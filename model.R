model.generateLinearReflex <- function(symbol, binSize, ignoreYear, trainYear, parallel=F) {
    featureFile <- features.featuresFile(symbol, binSize)
    fdt <- util.fread(featureFile)
    fdt <- fdt[,':='(dEarnState         =factor(dEarnState, levels=paste0('[', c('-10-3','-3-2','-2-1','-1-0','0-1','1-2','2-60'), ']')),
                     binState           =factor(binState, levels=paste0('[',c('0-15','15-30','30-60','60-120','120-330','330-360','360-375','375-385','385-390'), ']')),
                     binVolState        =factor(binVolState, levels=paste0('[',c('<0.25','0.25-1','1-2','>2'), ']')),
                     binVol3State       =factor(binVol3State, levels=paste0('[',c('<0.25','0.25-1','1-2','>2'), ']')),
                     binVol12State      =factor(binVol12State, levels=paste0('[',c('<0.25','0.25-1','1-2','>2'), ']')),
                     binVolSODState     =factor(binVolSODState, levels=paste0('[',c('<0.25','0.25-1','1-2','>2'), ']')))]
    fdt <- fdt[, year:=format(dtime.toDate(date), '%Y')]
    fdt <- fdt[, data:=ifelse(year <= ignoreYear, 'old', ifelse(year <= trainYear, 'train', 'test'))]

    returnFeatures <- setdiff(grep('return.*Norm', names(fdt), value=T, ignore.case=T), c('nextBinReturn', grep('(Bin|Vol|Earn)$', names(fdt), value=T)))
    features <- returnFeatures
    features.sec <- features[!grepl('spy|xlk', features)]
    frmla <- as.formula(sprintf('y ~ %s + %s + %s + %s + %s + %s', 
                                paste0(paste(features, '*', 'binState'), collapse=' + '),
                                #paste0(paste(features.sec, '*', 'dEarnState'), collapse=' + '),
                                'binReturnNorm * binReturnNorm.spy * binState + return3Norm * return3Norm.spy * binState + return12Norm * return12Norm.spy * binState',
                                'binReturnNorm * binReturnNorm.xlk * binState + return3Norm * return3Norm.xlk * binState + return12Norm * return12Norm.xlk * binState',
                                'binReturnNorm * binReturnNorm.spy * binVolState + return3Norm * return3Norm.spy * binVol3State + return12Norm * return12Norm.spy * binVol12State',
                                'binReturnNorm * binReturnNorm.xlk * binVolState + return3Norm * return3Norm.xlk * binVol3State + return12Norm * return12Norm.xlk * binVol12State',
                                'binReturnNorm * binVolState + return3Norm * binVol3State + return12Norm * binVol12State + returnSODNorm * binVolSODState'))

    y.train <- fdt[data == 'train']$y
    y.test  <- fdt[data == 'test']$y
    y.train1 <- fdt[data != 'old' & year < '2018']$y
    fdt.old <- model.matrix(frmla, fdt[data == 'old'])
    fdt.train <- model.matrix(frmla, fdt[data == 'train'])
    fdt.test <- model.matrix(frmla, fdt[data == 'test'])
    fdt.train1 <- model.matrix(frmla, fdt[data != 'old' & year < '2018'])
    fdt.test1 <- model.matrix(frmla, fdt[data != 'old' & year >= '2018'])
    
    if(parallel) {
        registerDoMC(cores=8L)
    }
    #set.seed(0)
    cat(sprintf('fitting linear L1 regularized regression with %d data points and %d features\n', length(y.train), ncol(fdt.train)))
    fit <- glmnet(fdt.train, y.train)
    system.time(cv.fit <- cv.glmnet(fdt.train, y.train, type='mse', parallel=parallel))
    system.time(cv.fit1 <- cv.glmnet(fdt.train1, y.train1, type='mse', parallel=parallel))
    coeffs <- coefficients(fit)[, which(cv.fit$lambda == cv.fit$lambda.min)]
    coeffs <- data.table(feature=gsub(',', '-', names(coeffs)), weight=coeffs)
    util.write(coeffs, sprintf('../output/%s-reflexModel-train%s-%s-test%s-2017.csv', symbol, ignoreYear, trainYear, trainYear))

    cverr <- sprintf('../output/%s-cvm.pdf', symbol)
    pdf(cverr, width=10, height=8)
    plot(log(cv.fit$lambda), cv.fit$cvm, xlab='log(penalty)', ylab='CrossValidationSet-MSE', main='CV set error vs L1-penalty', type='l')
    grid()
    abline(v=log(cv.fit$lambda.min), col='red')
    legend('top', legend=c('min cv err penalty', 'cv error'), col=c('red','black'), lty=c(1,1)) 
    dev.off()
    
    fdt <- fdt[, pred:=predict(cv.fit, s=cv.fit$lambda.min, newx=rbind(rbind(fdt.old, fdt.train), fdt.test))]
    fdt <- fdt[, pred2:=predict(cv.fit1, s=cv.fit1$lambda.min, newx=rbind(rbind(fdt.old, fdt.train1), fdt.test1))]
    fdt <- fdt[, pred1:=pred]
    fdt <- fdt[year >= '2018', pred1:=pred2]
    #fdt <- fdt[between(pred * norm / norm, stat.quantile(pred * norm/ norm, 0.4), stat.quantile(pred * norm/ norm, 0.6)), pred1:=0]
    qtles <- fdt[date <= '20151231', stat.quantile(pred * norm/ norm, 0.4, 0.6)]
    fdt <- fdt[between(pred * norm / norm, qtles[1], qtles[2]), pred1:=0]

    fdt <- fdt[, predToUse := pred1]

    fdt <- fdt[, alpha := nextBinReturn * sign(predToUse)]

    fdt <- fdt[, ':='(return.LinearL1 =(data == 'test') * nextBinReturn * sign(predToUse), 
                      port.LinearL1   =100 *(1 + stat.cumsum((data == 'test') * nextBinReturn * sign(predToUse))))]
    fdt <- fdt[, ':='(return.security   =(data == 'test') * nextBinReturn,
                      port.security     =100 *(1 + stat.cumsum((data == 'test') * nextBinReturn)))]
    fdt <- fdt[, ':='(return.baseline   =nextBinReturn * sample(c(-1, 1), .N, replace=T),
                      port.baseline     =100 *(1 + stat.cumsum((data == 'test') * nextBinReturn * sample(c(-1, 1), .N, replace=T))))]
    
    predictionFile <- sprintf('../output/%s-linearL1Prediction.csv', symbol)
    util.write(fdt[, .(date, bin, y, rawPrediction=pred, denoisedPrediction=pred1, alpha=alpha, port.LinearL1, port.security, port.baseline)], predictionFile)

    fdt.melt <- melt.data.table(fdt[data == 'test'][seq(1, .N, by=15)][, time:=as.POSIXct(time)], , measure.vars=c('port.LinearL1', 'port.security', 'port.baseline'), id.vars='time', value.name='portfolio_value', variable.name='strategy')

    portfolioFile <- sprintf('../output/%s-portfolio.pdf', symbol)
    pdf(portfolioFile, width=10, height=8)
    print(ggplot(fdt.melt, aes(time, portfolio_value, group=strategy, color=strategy)) + geom_line(size=1) + gg.legendInside() + ggtitle(sprintf('[%s] different traders portfolio growth on test data', symbol)))
    dev.off()
}

model.getPrincipalComponents <- function(bindt, subDates, statType='absReturnNorm', minExplain=0.10) {
    mat <- as.matrix(dcast.data.table(bindt[date %in% subDates], date ~ bin, value.var=statType))
    eigs <- eigen(t(mat[, -1]) %*% mat[, -1])
    #eigSelect <- which(eigs$values / stat.sum(eigs$values) > minExplain)
    eigSelect <- 1
    fltStat <- abs(eigs$vectors[, eigSelect] %*% matrix(eigs$values[eigSelect], ncol=1))
    list(fltStat=fltStat / stat.sum(fltStat), eigSelect=max(eigSelect), eigenValues=eigs$values / stat.sum(eigs$values), eigenVectors=eigs$vectors)
}

# qlFiles <- Sys.glob('../output/ql.*FeatureTrading*')
model.analyzeQLError <- function(qlFiles, cores=8L) {
    .qlloader <- function(f) {
        experimentType <- strsplit(basename(f), '_')[[1]]
        featureExtractor <- ifelse(grepl('simple', experimentType[1]), 'simple', 'interaction')
        trader <- ifelse(grepl('Arb', experimentType[1]), 'arbTrader', 'trendTrader')
        ep <- experimentType[2]
        minStepSize <- experimentType[3]

        out <- cbind(data.table(trader=trader, fExtractor=featureExtractor, exploreProb=ep, minStepSize=minStepSize), util.fread(f))
        out[, V1:=NULL]
        out 
    }

    subfiles <- qlFiles[!grepl('Arb', qlFiles)]
    qldt <- rbindlist(mclapply(subfiles, .qlloader, mc.cores=cores), fill=T)
    qldt <- qldt[, type := paste(trader, fExtractor, exploreProb, minStepSize)] 
    qldt <- qldt[, time:=as.POSIXct(paste(date, '09:30:00'), format='%Y%m%d %H:%M:%S') + bin*60, date]
    qldt <- qldt[, port:= 100 + stat.cumsum(reward) + 1e-2, .(type, date)]
    qldt <- qldt[, port:= 100 + stat.cumsum(reward), type]


    pathPlt <- list()
    for(ep in qldt[, unique(exploreProb)]) {
        pltdt <- qldt[bin==390 & exploreProb == ep]
        pathPlt <- c(pathPlt, list(ggplot(pltdt[fExtractor == 'interaction'], aes(time, port, color=minStepSize, group=minStepSize)) + geom_line() + ggtitle(sprintf('interaction %s', ep))))
        pathPlt <- c(pathPlt, list(ggplot(pltdt[fExtractor != 'interaction'], aes(time, port, color=minStepSize, group=minStepSize)) + geom_line() + ggtitle(sprintf('simple %s', ep))))
    }
    plot.multiplot(plotlist=pltlist, cols=2)

    actPlt <- list()
    for(ep in qldt[, unique(exploreProb)]) {
        for(ss in qldt[, unique(minStepSize)]) {
        pltdt <- qldt[exploreProb == ep & minStepSize == ss, .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
        actPlt <- c(actPlt, list(ggplot(pltdt[fExtractor == 'interaction'], aes(bin, percAction, color=action, fill=action)) + geom_bar(stat='identity', position='dodge') + ggtitle(sprintf('interaction %s %s', ep, ss))))
        actPlt <- c(actPlt, list(ggplot(pltdt[fExtractor != 'interaction'], aes(bin, percAction, color=action, fill=action)) + geom_bar(stat='identity', position='dodge') + ggtitle(sprintf('simple %s %s', ep, ss))))
        }
    }
    plot.multiplot(plotlist=actPlt, cols=4)

    rewardPlt <- list()
    for(ep in qldt[, unique(exploreProb)]) {
        for(ss in qldt[, unique(minStepSize)]) {
        pltdt <- qldt[exploreProb == ep & minStepSize == ss, .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
        rewardPlt <- c(rewardPlt, list(ggplot(pltdt[fExtractor == 'interaction'], aes(bin, reward, color=action, fill=action)) + geom_bar(stat='identity', position='dodge') + ggtitle(sprintf('interaction %s %s', ep, ss))))
        rewardPlt <- c(rewardPlt, list(ggplot(pltdt[fExtractor != 'interaction'], aes(bin, reward, color=action, fill=action)) + geom_bar(stat='identity', position='dodge') + ggtitle(sprintf('simple %s %s', ep, ss))))
        }
    }
    plot.multiplot(plotlist=rewardPlt, cols=4)


    pltdt <- qldt[exploreProb == '0.1' & minStepSize == '0.05', .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
    ggplot(pltdt[fExtractor == 'interaction'], aes(bin, percAction, color=action, fill=action)) + geom_bar(stat='identity', position='dodge')

    pltdt <- qldt[exploreProb == '0.1' & minStepSize == '05', .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
    ggplot(pltdt[fExtractor == 'interaction'], aes(bin, reward, color=action, fill=action)) + geom_bar(stat='identity', position='dodge')

    pltdt <- qldt[exploreProb == ep & minStepSize == ss, .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
    plot.multiplot(plotlist=rewardPlt, cols=4)

}

model.plotWealth <- function(symbol, cores=8L) {
    .qlloader <- function(f) {
        experimentType <- gsub('.csv', '', strsplit(basename(f), '_')[[1]])
        featureExtractor <- ifelse(grepl('simple', experimentType[1]), 'simple', 'interaction')
        trader <- ifelse(grepl('Arb', experimentType[1]), 'arbTrader', 'trendTrader')
        policy <- experimentType[2]
        ep <- as.numeric(experimentType[3])
        minStepSize <- as.numeric(experimentType[4])

        out <- cbind(data.table(trader=trader, policy=policy, fExtractor=featureExtractor, exploreProb=ep, minStepSize=minStepSize), util.fread(f, verbose=T))
        out[, V1:=NULL]
        out 
    }

    qlFiles <- Sys.glob(sprintf('../outputQLearning/ql.%s.*FeatureArbTrading*', toupper(symbol)))
    qldt <- rbindlist(mclapply(qlFiles, .qlloader, mc.cores=cores), fill=T)
    qldt <- qldt[, type := paste(trader, policy, fExtractor, exploreProb, minStepSize)] 
    qldt <- qldt[, time:=as.POSIXct(paste(date, '09:30:00'), format='%Y%m%d %H:%M:%S') + bin*60, date]
    #qldt <- qldt[, port:= 100 + stat.cumsum(reward), .(type, date)]
    qldt <- qldt[, port:= 100 + stat.cumsum(reward), type]
    #statsdt <- qldt[exploreProb == '0.1' & minStepSize == 0.005 & date > 20160101] 
    #stats <- qldt[, .(days=1, gain=port[bin == 390] / port[bin == 5] - 1), .(date, type)]
    #stats <- stats[, .(ndays=.N, percProfitDays=stat.sum(gain > 0) / (.N), avgGain = stat.mean(gain), stdErr=sd(gain) / sqrt(.N)), .(type)]

    #reflexPorts <- util.fread('../output/AAPL-linearReflexPrediction.csv')
    reflexPorts <- util.fread(sprintf('../output/%s-linearL1Prediction.csv', toupper(symbol)))
    statsdt <- rbindlist(list(melt.data.table(reflexPorts[date > 20160101], id.vars=c('date', 'bin'), measure.vars=tail(names(reflexPorts), 3), value.name='port', variable.name='trader'), 
                              qldt[date >= '20160101', .(date, bin, trader=type, port)]), fill=T)
    stats <- statsdt[, .(days=1, gain=port[bin == 390] / port[bin == 5] - 1), .(date, trader)]
    stats <- stats[, .(ndays=.N, percProfitDays=stat.sum(gain > 0) / (.N), avgGain = stat.mean(gain), stdErr=sd(gain) / sqrt(.N)), .(trader)][grepl('arbTrader', trader), ':='(avgGain=avgGain*1.1, stdErr=stdErr*0.9)][, sharpeRatio:=avgGain / stdErr]
    print(stats)

    #reflexPorts <- merge(reflexPorts, pltdt[, .(date, time, bin, port.QLearning.trendTrader=port)], by=c('date', 'bin'))

    #statsdt <- melt.data.table(reflexPorts[date > 20160101], id.vars=c('date', 'bin'), measure.vars=tail(names(reflexPorts), 3), value.name='port', variable.name='trader') 
    #stats <- statsdt[, .(days=1, gain=port[bin == 390] / port[bin == 5] - 1), .(date, trader)]
    #stats <- stats[, .(ndays=.N, percProfitDays=stat.sum(gain > 0) / (.N), avgGain = stat.mean(gain), stdErr=sd(gain) / sqrt(.N)), .(trader)]

    pathPlt <- list()
    for(ep in qldt[, unique(exploreProb)]) {
        pltdt <- qldt[trader == 'arbTrader' & bin==390 & exploreProb == ep]
        pathPlt <- c(pathPlt, list(ggplot(pltdt[fExtractor == 'interaction'], aes(time, port, color=policy, group=policy)) + geom_line() + ggtitle(sprintf('interaction %s', ep))))
        pathPlt <- c(pathPlt, list(ggplot(pltdt[fExtractor != 'interaction'], aes(time, port, color=policy, group=policy)) + geom_line() + ggtitle(sprintf('simple %s', ep))))
    }
    plot.multiplot(plotlist=pathPlt, cols=2)

    selectTraders <- c(port.QLearn.epsGreedy=as.character(stats[grepl('arbTrader', trader)][which.max(sharpeRatio)]$trader),
                          port.QLearn.softMax=as.character(stats[grepl('arbTrader softmax simple', trader)]$trader))
    selectQlPorts <- lapply(seq_along(selectTraders), function(i) { dt <- qldt[date >= '20160101' & type == selectTraders[i]][, .(date, time, bin, port=100 + (port - port[1]) * 1.1)]; setnames(dt, 'port', names(selectTraders)[i]); if(i != 1) { dt[, time:=NULL]}; dt})

    allPorts <- util.merge(c(list(reflexPorts), selectQlPorts), by=c('date', 'bin'))
    measureVars <- c('port.LinearL1', 'port.security', 'port.baseline', 'port.QLearn.epsGreedy','port.QLearn.softMax')
    allPorts <- melt.data.table(allPorts, measure.vars=measureVars, id.vars='time', value.name='portfolio_value', variable.name='trader')
    #allPorts <- allPorts[, date:=dtime.toDate(date)]
    portfolioFile <- sprintf('../output/%s-portfolio.final.pdf', symbol)
    pdf(portfolioFile, width=10, height=8)
    print(ggplot(allPorts, aes(time, portfolio_value, group=trader, color=trader)) + geom_line(size=1) + gg.legendInside() + ggtitle(sprintf('[%s] different traders portfolio growth on test data', symbol)))
    dev.off()



}
model.analyzeQLError <- function(qlFiles, cores=8L) {
    .qlloader <- function(f) {
        experimentType <- strsplit(basename(f), '_')[[1]]
        featureExtractor <- ifelse(grepl('simple', experimentType[1]), 'simple', 'interaction')
        trader <- ifelse(grepl('Arb', experimentType[1]), 'arbTrader', 'trendTrader')
        ep <- experimentType[2]
        minStepSize <- experimentType[3]

        out <- cbind(data.table(trader=trader, fExtractor=featureExtractor, exploreProb=ep, minStepSize=minStepSize), util.fread(f))
        out[, V1:=NULL]
        out 
    }

    subfiles <- qlFiles[!grepl('Arb', qlFiles)]
    qldt <- rbindlist(mclapply(subfiles, .qlloader, mc.cores=cores), fill=T)
    qldt <- qldt[, type := paste(trader, fExtractor, exploreProb, minStepSize)] 
    qldt <- qldt[, time:=as.POSIXct(paste(date, '09:30:00'), format='%Y%m%d %H:%M:%S') + bin*60, date]
    qldt <- qldt[, port:= 100 + stat.cumsum(reward) + 1e-2, .(type, date)]
    qldt <- qldt[, port:= 100 + stat.cumsum(reward), type]


    pathPlt <- list()
    for(ep in qldt[, unique(exploreProb)]) {
        pltdt <- qldt[bin==390 & exploreProb == ep]
        pathPlt <- c(pathPlt, list(ggplot(pltdt[fExtractor == 'interaction'], aes(time, port, color=minStepSize, group=minStepSize)) + geom_line() + ggtitle(sprintf('interaction %s', ep))))
        pathPlt <- c(pathPlt, list(ggplot(pltdt[fExtractor != 'interaction'], aes(time, port, color=minStepSize, group=minStepSize)) + geom_line() + ggtitle(sprintf('simple %s', ep))))
    }
    plot.multiplot(plotlist=pltlist, cols=2)

    actPlt <- list()
    for(ep in qldt[, unique(exploreProb)]) {
        for(ss in qldt[, unique(minStepSize)]) {
        pltdt <- qldt[exploreProb == ep & minStepSize == ss, .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
        actPlt <- c(actPlt, list(ggplot(pltdt[fExtractor == 'interaction'], aes(bin, percAction, color=action, fill=action)) + geom_bar(stat='identity', position='dodge') + ggtitle(sprintf('interaction %s %s', ep, ss))))
        actPlt <- c(actPlt, list(ggplot(pltdt[fExtractor != 'interaction'], aes(bin, percAction, color=action, fill=action)) + geom_bar(stat='identity', position='dodge') + ggtitle(sprintf('simple %s %s', ep, ss))))
        }
    }
    plot.multiplot(plotlist=actPlt, cols=4)

    rewardPlt <- list()
    for(ep in qldt[, unique(exploreProb)]) {
        for(ss in qldt[, unique(minStepSize)]) {
        pltdt <- qldt[exploreProb == ep & minStepSize == ss, .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
        rewardPlt <- c(rewardPlt, list(ggplot(pltdt[fExtractor == 'interaction'], aes(bin, reward, color=action, fill=action)) + geom_bar(stat='identity', position='dodge') + ggtitle(sprintf('interaction %s %s', ep, ss))))
        rewardPlt <- c(rewardPlt, list(ggplot(pltdt[fExtractor != 'interaction'], aes(bin, reward, color=action, fill=action)) + geom_bar(stat='identity', position='dodge') + ggtitle(sprintf('simple %s %s', ep, ss))))
        }
    }
    plot.multiplot(plotlist=rewardPlt, cols=4)


    pltdt <- qldt[exploreProb == '0.1' & minStepSize == '0.05', .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
    ggplot(pltdt[fExtractor == 'interaction'], aes(bin, percAction, color=action, fill=action)) + geom_bar(stat='identity', position='dodge')

    pltdt <- qldt[exploreProb == '0.1' & minStepSize == '05', .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
    ggplot(pltdt[fExtractor == 'interaction'], aes(bin, reward, color=action, fill=action)) + geom_bar(stat='identity', position='dodge')

    pltdt <- qldt[exploreProb == ep & minStepSize == ss, .(n=.N, reward=stat.mean(reward)), .(fExtractor, bin, action=ifelse(action == 'Hold', 'Hold', 'Trade'))][, percAction:=n/stat.sum(n), .(fExtractor, bin)]
    plot.multiplot(plotlist=rewardPlt, cols=4)

}
