util.source('loader.R')
features.rawBinFile <- function(symbol, binSize) sprintf('../data/binned_%sMins_%s.csv.xz', binSize, toupper(symbol))
features.fltBinFile <- function(symbol, binSize) sprintf('../data/fltBinned_%sMins_%s.csv.xz', binSize, toupper(symbol))
features.PCADetailsFile <- function(symbol, binSize) sprintf('../data/PCA_%sMins_%s.rds', binSize, toupper(symbol))
features.featuresFile <- function(symbol, binSize) sprintf('../data/features_%sMins_%s.csv', binSize, toupper(symbol))

features.genRawBins <- function(symbol, binSize) {
    system.time(dt <- load.raw(symbol))
    
    cat(sprintf('preparing dt\n'))
    dt <- dt[between(time, 930, 1600) & date >= 20030101][, date:=as.character(date)]
    dt <- dt[, ':='(date        =dtime.dateToStr(date),
                   mktOpenTime  =as.POSIXct(paste(date, '09:30:00'), format='%Y%m%d %H:%M:%S')),
                   date]
    dt <- dt[, ':='(min         =substr(time, nchar(time) - 1, nchar(time)),
                    hr          =paste0(ifelse(time < 1000, '0', ''), substr(time, 1, nchar(time) - 2))),
                    time]
    dt <- dt[, ':='(time        =as.POSIXct(paste0(date, ' ', hr, ':', min, ':00'), format='%Y%m%d %H:%M:%S'))]
    dt <- dt[, ':='(mbin        =round(as.numeric(difftime(time, mktOpenTime, units='mins'))))]
    dt <- dt[, ':='(bin         =ceiling(mbin / binSize) * binSize)]

    dt <- dt[order(date, bin)]

    cat(sprintf('preparing bindt\n'))
    bindt <- dt[, .(binEndTime  =time[.N], 
                    binEndPx    =close[.N], 
                    binVolume   =stat.sum(volume * close[.N])), 
                    .(date, bin)]
    
    bindt <- bindt[, ':='(idx           =seq_len(.N),
                          volumeSOD     =stat.cumsum(binVolume)), date]
    bindt <- bindt[, ':='(binReturn     =log(binEndPx) - shift(log(binEndPx), 1, type='lag'),
                          return3       =log(binEndPx) - shift(log(binEndPx), 3, type='lag'),
                          return12      =log(binEndPx) - shift(log(binEndPx), 12, type='lag'),
                          returnSOD     =log(binEndPx) - log(binEndPx[1]),
                          volume3       =volumeSOD - shift(volumeSOD, 3, type='lag'),
                          volume12      =volumeSOD - shift(volumeSOD, 12, type='lag')),
                          date]

    bindt <- bindt[idx <= 3, ':='(return3   =returnSOD,
                                  volume3   =volumeSOD)]
    bindt <- bindt[idx <= 12, ':='(return12  =returnSOD,
                                   volume12  =volumeSOD)]

    ###for bin == 0
    bindt <- bindt[, ':='(rollReturn        =log(binEndPx) - shift(log(binEndPx), 1, type='lag'),
                          rollReturn3       =log(binEndPx) - shift(log(binEndPx), 3, type='lag'),
                          rollReturn12      =log(binEndPx) - shift(log(binEndPx), 12, type='lag'))]
    bindt <- bindt[idx == 1, ':='(binReturn =rollReturn,
                                  return3   =rollReturn3,
                                  return12  =rollReturn12)]
    bindt <- bindt[, ':='(rollReturn = NULL, rollReturn3 = NULL, rollReturn12 = NULL)]
    
    ###previous day return/volume
    dailyStat <- dt[, .(open=open[1], close=close[.N], dailyVolume=stat.sum(volume * close[.N])), date]
    dailyStat <- dailyStat[, ':='(prevDayReturn =log(open) - shift(log(close), 2, type='lag'), 
                                  dailyReturn   =c(log(close[1]/open[1]), log(close[2:(.N)]/close[1:(.N - 1)])))]
    bindt <- merge(bindt, dailyStat[, .(date, prevDayReturn, dailyReturn, dailyVolume)], by='date', all=T, sort=F)

    ### load earnings 
    ### distance from earnings 1 prev, 0/-1 if after hours or before
    earnings <- util.fread('../data/earnings.csv')
    if(any(earnings$symbol == symbol)) {
        idx <- which(earnings$symbol == symbol)
        edt <- merge(bindt[, .(daysFromEarnings = as.integer(NA)), date], earnings[idx, .(date=dtime.dateToStr(date), eTime=time)], by=c('date'), all.x=T)
        edt <- edt[eTime == 'after close', daysFromEarnings := 0] 
        edt <- edt[eTime == 'before open', daysFromEarnings := -1] 
        eidx <- edt[, which(!is.na(eTime))]
        for(i in -10:10) {
            edt <- edt[eidx + i, daysFromEarnings := edt[eidx]$daysFromEarnings - i]
        }
        edt <- edt[is.na(daysFromEarnings), daysFromEarnings := 30]
        bindt <- merge(bindt, edt[, .(date, daysFromEarnings)], by='date', all.x=T)
        dailyStat <- merge(dailyStat, edt[, .(date, daysFromEarnings)], by='date', all.x=T)
    }

    ### return distribution around earnings
    cat(sprintf('%f percent data is less than 79 bucks\n', bindt[, .N, date][, stat.sum(N < 79)/(.N) * 100]))
    bindt <- bindt[, shortDays:=date %in% bindt[, .N, date][N <= 390 / binSize]$date]

    misc.removeCols(bindt, c('idx'))
    bindt <- bindt[order(date, bin)]
    util.write(bindt[date >= '201001'], features.rawBinFile(symbol, binSize))
    #util.write(bindt, binFile)
}

###backward looking
features.genFltBins <- function(symbol, binSize, ndays=63L) {
    bindt <- util.fread(features.rawBinFile(symbol, binSize))
    bindt <- bindt[!(shortDays)]

    cat(sprintf('preparing daily avg Statistics\n'))
    minAggDays <- 21L
    dates <- bindt[, sort(unique(date))]
    dailyStats <- bindt[!duplicated(date), .(date, prevDayReturn, dailyReturn, dailyVolume)]
    dailyAvgs <- rbindlist(lapply((minAggDays + 1):length(dates), function(i) dailyStats[max((i-ndays), 1):i, .(date = dailyStats$date[i], dailySigma = sd(dailyReturn[-(.N)]), dailyAvgVolume = mean(dailyVolume[-(.N)]))]), fill=T)
    dailyStats <- merge(dailyStats, dailyAvgs, by='date', sort=F, all=T)
    dailyStats <- dailyStats[1:(minAggDays + 1), ':='(dailySigma=dailySigma[.N], dailyAvgVolume=dailyAvgVolume[.N])]
    bindt <- merge(bindt, dailyStats[, .(date, dailyAvgVolume, dailySigma)], by='date', all=T, sort=F)

    cat(sprintf('preparing bin avg Statistics\n'))
    bindt <- bindt[, ':='(absReturnNorm=stat.winsorize(abs(binReturn)/stat.sum(abs(binReturn))),
                          volumeNorm    =binVolume / stat.sum(binVolume)), date]
    bindt <- bindt[, ':='(absReturnNorm=absReturnNorm / stat.sum(absReturnNorm)), date]
    cat(sprintf('running PCA for bin volatility \n'))
    print(system.time(fltBinSigma <- lapply((minAggDays + 1):length(dates), function(i) c(list(date=dates[i]), model.getPrincipalComponents(bindt, dates[max(1, i - ndays):(i-1)], 'absReturnNorm')))))
    cat(sprintf('running PCA for bin volatility \n'))
    print(system.time(fltBinVolume <- lapply((minAggDays + 1):length(dates), function(i) c(list(date=dates[i]), model.getPrincipalComponents(bindt, dates[max(1, i - ndays):(i-1)], 'volumeNorm')))))
        
    ### merge all the stats
    bins <- bindt[, unique(bin)]
    binStats <- merge(util.rbindlist(lapply(fltBinSigma, function(l) data.table(bin=bins, date=l$date, binSigma=l$fltStat[, 1])), fill=T), 
                      util.rbindlist(lapply(fltBinVolume, function(l) data.table(bin=bins, date=l$date, binAvgVolume=l$fltStat[, 1])), fill=T), 
                      all.x=T, by=c('bin','date'))
    bindt <- merge(bindt, binStats, by=c('date','bin'), all.x=T, sort=F)
    bindt <- bindt[date %in% dates[1:(minAggDays + 1)], ':='(binSigma=binSigma[.N], binAvgVolume=binAvgVolume[.N]), .(bin)]
    bindt <- bindt[, norm := dailySigma * binSigma]
    
    bindt <- bindt[order(date, bin)]
    util.write(bindt, features.fltBinFile(symbol, binSize))

    saveRDS(list(fltBinVolume=fltBinVolume, fltBinSigma=fltBinSigma), features.PCADetailsFile(symbol, binSize))
}

features.plotBinFeatures <- function(symbol='AAPL', binSize, dvEarningsTrend=NULL, bvEarningsTrend=NULL) {
    bindt <- util.fread(features.fltBinFile(symbol, binSize))
    pcaInfo <- readRDS(features.PCADetailsFile(symbol, binSize))
    qout <- 0.02
    bindt <- bindt[, out:=!between(binReturn, stat.quantile(binReturn, qout), stat.quantile(binReturn, 1-qout)) & 
                          !between(binReturn/binSigma, stat.quantile(binReturn/binSigma, qout), stat.quantile(binReturn/binSigma, 1-qout))]
    bindt <- bindt[, year:=format(dtime.toDate(date), '%Y')]
    
    yearsToPlot <- c('2010', '2012', '2016', '2017')
    plt11 <- ggplot(bindt[!(out) & year %in% yearsToPlot], aes(binReturn, group=year, color=year)) + geom_density(size=1, adjust=3) 
    plt11 <- plt11 + xlab(sprintf('[%s] intra-day 5min return distribution', symbol)) + ggtitle('[Non-stationary returns] intra-day return') + gg.legendInside(pos=c(0.2, 0.6))

    plt12 <- ggplot(bindt[!(out) & year %in% yearsToPlot], aes(binReturn/(dailySigma * binSigma), group=year, color=year)) + geom_density(size=1, adjust=3) 
    plt12 <- plt12 + xlab(sprintf('[%s] intra-day 5min normalized return distribution', symbol)) + ggtitle('[decresed non-stationarity] return/PCA_filtered_volatility') + gg.legendInside(pos=c(0.2, 0.6))

    sampleDates <- bindt[, sample(unique(date), 50)]
    plt21 <- ggplot(bindt[date %in% sampleDates & bin != 0], aes(bin, volumeNorm, group=date, color=date)) + geom_line() + ylab('bin Volume') + xlab(sprintf('[%s] intraday minute bins', symbol)) + ggtitle('realized intra-day volume profile')
    plt21 <- plt21 + gg.legendInside(pos=c(0.2, 0.6))
    plt22 <- ggplot(bindt[date %in% sampleDates & bin != 0], aes(bin, binAvgVolume, group=date, color=date)) + geom_line() + ylab('bin Volume') + xlab(sprintf('[%s] intraday minute bins', symbol)) + ggtitle('PCA filtered intra-day volume profile')
    plt22 <- plt22 + gg.legendInside(pos=c(0.2, 0.6))

    plt31 <- ggplot(bindt[date %in% sampleDates & bin != 0], aes(bin, absReturnNorm, group=date, color=date)) + geom_line() + ylab('bin Volatility') + xlab(sprintf('[%s] intraday minute bins', symbol)) + ggtitle('realized intra-day volatility profile')
    plt31 <- plt31 + gg.legendInside(pos=c(0.9, 0.9))
    plt32 <- ggplot(bindt[date %in% sampleDates & bin != 0], aes(bin, binSigma, group=date, color=date)) + geom_line() + ylab('bin Volatility') + xlab(sprintf('[%s] intraday minute bins', symbol)) + ggtitle('PCA filtered intra-day volatility profile')
    plt32 <- plt32 + gg.legendInside(pos=c(0.9, 0.9))

    plotfile <- file.path('../output', sprintf('%s-intraday-dynamics.pdf', symbol))
    plotlist <- list(plt11, plt21, plt31, plt12, plt22, plt32)
    pdf(plotfile, width=10, height=8)
    plot.multiplot(plotlist=plotlist, cols=length(plotlist) / 3)
    dev.off()

    pcaInfo <- readRDS(features.PCADetailsFile(symbol, binSize))

    eigsInfo <- util.rbindlist(list(data.table(eigValueType='volume Principal EigVector', eigenNumber=1:79, eigenVal=pcaInfo[[1]][[1000]]$eigenValues/stat.sum(pcaInfo[[1]][[1000]]$eigenValues)),
                                    data.table(eigValueType='volatility Principal EigVector', eigenNumber=1:79, eigenVal=pcaInfo[[2]][[1000]]$eigenValues/stat.sum(pcaInfo[[2]][[1000]]$eigenValues))), fill=T)
    plotfile <- file.path('../output', sprintf('%s-eigen-value.pdf', symbol))
    pdf(plotfile, width=10, height=8)
    ggplot(eigsInfo, aes(eigenNumber, eigenVal, color=eigValueType)) + geom_point(size=1) + gg.legendInside(pos=c(0.8, 0.8)) + ggtitle('Eigen Value Distribution\nOnly one significant & stable eigen vector')
    dev.off()
}

features.generate <- function(symbol = 'AAPL', binSize=5L, plot=F) {
    cat('loading binned data\n')
    bindt <- util.fread(features.fltBinFile(symbol, binSize))
    spydt <- util.fread(features.fltBinFile('SPY', binSize))
    xlkdt <- util.fread(features.fltBinFile('XLK', binSize))
    keyCols <- c('date', 'bin')
    retCols <- grep('(R|r)eturn', names(spydt), value=T)

    spydt <- spydt[, c(keyCols, retCols), with=F]
    setnames(spydt, retCols, paste0(retCols, '.spy')) 

    xlkdt <- xlkdt[, c(keyCols, retCols), with=F]
    setnames(xlkdt, retCols, paste0(retCols, '.xlk')) 
    
    bindt <- merge(bindt, spydt, by=keyCols, all.x=T)
    bindt <- merge(bindt, xlkdt, by=keyCols, all.x=T)
    qout <- 0.0025 
    if(is.null(bindt$daysFromEarnings) || bindt[, all(is.na(daysFromEarnings))]) {
        bindt <- bindt[, daysFromEarnings := 0]
    }

    bindt <- bindt[!(shortDays)]
    bindt <- bindt[, year:=format(dtime.toDate(date), '%Y')]
    
    if(plot) {
        features.correl.plot(bindt, symbol)
    }

    bindt <- bindt[!is.na(norm * binReturn * return3 * return12 * prevDayReturn *returnSOD *
                          binReturn.spy * return3.spy * return12.spy * prevDayReturn.spy *returnSOD.spy *
                          binReturn.xlk * return3.xlk * return12.xlk * prevDayReturn.xlk * returnSOD.xlk)]
    cat('computing features\n')
    fdt <- features.compute(bindt)
    fdt <- fdt[order(date, bin)]
    fdt <- fdt[!(date %in% fdt[is.na(y * binReturn.spy * return3.spy * return12.spy * returnSOD.spy)]$date)]

    featureFile <- features.featuresFile(symbol, binSize)
    cat(sprintf('saving features file %s\n', featureFile))
    util.write(fdt, featureFile)
}

features.correl.plot <- function(bindt, symbol) {
    qout <- 0.02
    bindt <- bindt[, out:=!between(binReturn, stat.quantile(binReturn, qout), stat.quantile(binReturn, 1-qout)) & 
                          !between(binReturn/binSigma, stat.quantile(binReturn/binSigma, qout), stat.quantile(binReturn/binSigma, 1-qout))]

    plt1 <- ggplot(bindt[date > '20151001'][!(out)] , aes(binReturn.spy, binReturn, color=bin)) + geom_point() 
    plt1 <- plt1 + ggtitle(sprintf('[bin return] SPY vs %s correlation', symbol)) + gg.legendInside(pos=c(0.2, 0.7)) 

    plt2 <- ggplot(bindt[date > '20151001'][!(out)] , aes(binReturn.xlk, binReturn, color=bin)) + geom_point() 
    plt2 <- plt2 + ggtitle(sprintf('[bin return] XLK vs %s correlation', symbol)) + gg.legendInside(pos=c(0.2, 0.7)) 

    plt3 <- ggplot(bindt[date > '20151001'][!(out)][, ':='(binReturn=binReturn/norm, binReturn.spy=binReturn.spy/norm)] , aes(binReturn.spy, binReturn, color=bin)) 
    plt3 <- plt3 + geom_point() + ggtitle(sprintf('[bin normalized return] SPY vs %s correlation', symbol)) + gg.legendInside(pos=c(0.2, 0.7)) 

    plt4 <- ggplot(bindt[date > '20151001'][!(out)][, ':='(binReturn=binReturn/norm, binReturn.xlk=binReturn.xlk/norm)] , aes(binReturn.xlk, binReturn, color=bin)) 
    plt4 <- plt4 + geom_point() + ggtitle(sprintf('[bin normalized return] XLK vs %s correlation', symbol)) + gg.legendInside(pos=c(0.2, 0.7)) 
    
    plt5 <- ggplot(bindt[date > '20151001' & between(daysFromEarnings, -10, 10)][!(out)] , aes(binReturn.xlk, binReturn, color=daysFromEarnings)) + geom_density(adjust=3)
    plt5 <- plt5 + geom_point() + ggtitle(sprintf('[bin normalized return] XLK vs %s correlation', symbol)) + gg.legendInside(pos=c(0.2, 0.7)) 
    
    plotlist <- list(plt1, plt2, plt3, plt4)
    plotfile <- sprintf('../output/%s-intraday-correl.pdf', symbol)
    cat(sprintf('plotting correlation plots %s\n', plotfile))
    pdf(plotfile, width=10, height=8)
    plot.multiplot(plotlist=plotlist, cols=length(plotlist) / 2)
    dev.off()
    invisible() 
}

## Note: this spits out winsorized features/returns.
##       only column not winsorized is nextBinReturn
features.compute <- function(bindt) {
    dt <- copy(bindt)
    dt <- dt[, ':='(nextBinReturn   =c(shift(binReturn, 1, type='lead')[-(.N)], 0))]
    dt <- dt[, ':='(cumBinAvgVolume =stat.cumsum(binAvgVolume)), date]
    dt <- dt[, ':='(binVolume3      =volumeSOD - stat.pmax(shift(volumeSOD, 2, type='lag'), 0),
                    binVolume12     =volumeSOD - stat.pmax(shift(volumeSOD, 11, type='lag'), 0),
                    binAvgVolume3   =cumBinAvgVolume - stat.pmax(shift(cumBinAvgVolume, 2, type='lag'), 0),
                    binAvgVolume12  =cumBinAvgVolume - stat.pmax(shift(cumBinAvgVolume, 11, type='lag'), 0)), date]

    qout <- 0.005
    retCols <- setdiff(grep('return', names(dt), value=T, ignore.case=T), 'nextBinReturn')
    .winsr <- function(col) {
        dt <- dt[, (col):=stat.winsorize(get(col), qout)]
    }
    invisible(lapply(retCols, .winsr))
    
    fdt <- dt[, .(year,
                  date,
                  bin,
                  nextBinReturn,
                  daysFromEarnings,
                  time                  =binEndTime,
                  norm                  =norm,
                  #outAll,
                  binSqrt               =sqrt(bin),
                  binVolNorm            =binVolume / (dailyAvgVolume * binAvgVolume),
                  binVol3Norm           =binVolume3 / (dailyAvgVolume * binAvgVolume3),
                  binVol12Norm          =binVolume12 / (dailyAvgVolume * binAvgVolume12),
                  binVolSODNorm         =volumeSOD / (dailyAvgVolume * cumBinAvgVolume),
                  y                     =c(shift(binReturn, 1, type='lead')[-(.N)], 0),
                  ynorm                 =c(shift(binReturn, 1, type='lead')[-(.N)] / norm[-(.N)], 0),

                  binReturn             =binReturn,
                  return3               =return3,
                  return12              =return12,
                  returnSOD             =returnSOD,
                  prevDayReturn         =prevDayReturn,
                  binReturnNorm         =binReturn / norm,
                  return3Norm           =return3 / norm,
                  return12Norm          =return12 / norm,
                  returnSODNorm         =returnSOD / norm,
                  prevDayReturnNorm     =prevDayReturn / norm,

                  y.xlk                 =c(shift(binReturn.xlk[-(.N)], 1, type='lead'), 0),
                  binReturn.xlk         =binReturn.xlk,
                  return3.xlk           =return3.xlk,
                  return12.xlk          =return12.xlk,
                  returnSOD.xlk         =returnSOD.xlk,
                  prevDayReturn.xlk     =prevDayReturn.xlk,
                  binReturnNorm.xlk     =binReturn.xlk / norm,
                  return3Norm.xlk       =return3.xlk / norm,
                  return12Norm.xlk      =return12.xlk / norm,
                  returnSODNorm.xlk     =returnSOD.xlk / norm,
                  prevDayReturnNorm.xlk =prevDayReturn.xlk / norm,

                  y.spy                 =c(shift(binReturn.spy[-(.N)], 1, type='lead'), 0),
                  binReturn.spy         =binReturn.spy,
                  return3.spy           =return3.spy,
                  return12.spy          =return12.spy,
                  returnSOD.spy         =returnSOD.spy,
                  prevDayReturn.spy     =prevDayReturn.spy,
                  binReturnNorm.spy     =binReturn.spy / norm,
                  return3Norm.spy       =return3.spy / norm,
                  return12Norm.spy      =return12.spy / norm,
                  returnSODNorm.spy     =returnSOD.spy / norm,
                  prevDayReturnNorm.spy =prevDayReturn.spy / norm,

                  binReturnVol          =binReturn * sqrt(binVolume / (dailyAvgVolume * binAvgVolume)),
                  return3Vol            =return3 * sqrt(binVolume3 / (dailyAvgVolume * binAvgVolume3)),
                  return12Vol           =return12 * sqrt(binVolume12 / (dailyAvgVolume * binAvgVolume12)),
                  returnSODVol          =returnSOD * sqrt(volumeSOD / (dailyAvgVolume * cumBinAvgVolume)),
                  binReturnNormVol      =binReturn / norm * sqrt(binVolume / (dailyAvgVolume * binAvgVolume)),
                  return3NormVol        =return3 / norm * sqrt(binVolume3 / (dailyAvgVolume * binAvgVolume3)),
                  return12NormVol       =return12 / norm * sqrt(binVolume12 / (dailyAvgVolume * binAvgVolume12)),
                  returnSODNormVol      =returnSOD / norm * sqrt(volumeSOD / (dailyAvgVolume * cumBinAvgVolume)),

                  binReturnEarn         =binReturn * sign(daysFromEarnings + 0.1) * (1 / ( 1 + abs(daysFromEarnings))),
                  return3Earn           =return3 * sign(daysFromEarnings + 0.1) * (1 / (1 + abs(daysFromEarnings))),
                  return12Earn          =return12 * sign(daysFromEarnings + 0.1) * (1 / (1 + abs(daysFromEarnings))),
                  returnSODEarn         =returnSOD * sign(daysFromEarnings + 0.1) * (1 / (1 + abs(daysFromEarnings))),
                  binReturnNormEarn     =binReturn / norm * sign(daysFromEarnings + 0.1) * (1 / ( 1 + abs(daysFromEarnings))),
                  return3NormEarn       =return3 / norm * sign(daysFromEarnings + 0.1) * (1 / (1 + abs(daysFromEarnings))),
                  return12NormEarn      =return12 / norm * sign(daysFromEarnings + 0.1) * (1 / (1 + abs(daysFromEarnings))),
                  returnSODNormEarn     =returnSOD / norm * sign(daysFromEarnings + 0.1) * (1 / (1 + abs(daysFromEarnings))),

                  binReturnBin          =binReturn * sqrt(bin),
                  return3Bin            =return3 * sqrt(bin), 
                  return12Bin           =return12 * sqrt(bin),
                  returnSODBin          =returnSOD * sqrt(bin),
                  binReturnNormBin      =binReturn / norm * sqrt(bin),
                  return3NormBin        =return3 / norm * sqrt(bin), 
                  return12NormBin       =return12 / norm * sqrt(bin),
                  returnSODNormBin      =returnSOD / norm * sqrt(bin))]

    fdt <- fdt[,':='(dEarnState         =cut(daysFromEarnings, breaks=c(-10, -3, -2, -1, 0, 1, 2, 60), labels=paste0('[', c('-10-3','-3-2','-2-1','-1-0','0-1','1-2','2-60'), ']'), include.lowest=T),
                     binState           =cut(bin, breaks=c(0, 15, 30, 60, 120, 330, 360, 375, 385, 390), labels=paste0('[',c('0-15','15-30','30-60','60-120','120-330','330-360','360-375','375-385','385-390'), ']'), include.lowest=T),
                     binVolState        =cut(binVolNorm, breaks=c(0, 0.25, 1, 2, Inf), labels=paste0('[',c('<0.25','0.25-1','1-2','>2'), ']'), include.lowest=T),
                     binVol3State       =cut(binVol3Norm, breaks=c(0, 0.25, 1, 2, Inf), labels=paste0('[',c('<0.25','0.25-1','1-2','>2'), ']'), include.lowest=T),
                     binVol12State      =cut(binVol12Norm, breaks=c(0, 0.25, 1, 2, Inf), labels=paste0('[',c('<0.25','0.25-1','1-2','>2'), ']'), include.lowest=T),
                     binVolSODState     =cut(binVolSODNorm, breaks=c(0, 0.25, 1, 2, Inf), labels=paste0('[',c('<0.25','0.25-1','1-2','>2'), ']'), include.lowest=T))]

    qout <- 0.005
    colsToWnsr <- setdiff(c('y', 'ynorm', grep('return', names(fdt), value=T, ignore.case=T)), c(retCols, 'nextBinReturn'))
    for(col in colsToWnsr) {
        fdt <- fdt[, (col):=stat.winsorize(get(col), qout)]
    }
    fdt
}
