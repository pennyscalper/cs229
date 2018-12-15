load.isEtf <- function(symbol) {
    ifelse(toupper(symbol) %in% c('XLK', 'SPY', 'QQQ'), T, F)
}

load.raw <- function(symbol, cores=16L) {
    #files <- Sys.glob(path.stocksData(ifelse(load.isEtf(symbol), 'etfs', 'stocks'), '*', sprintf('table_%s.csv', tolower(symbol))))
    files <- Sys.glob(path.stocksData(ifelse(load.isEtf(symbol), 'alletfs', 'nasdaq100'), sprintf('table_%s.csv', tolower(symbol))))
    if(length(files)) {
        system.time(binData <- rbindlist(mclapply(files, util.fread, verbose=F, mc.cores=cores), fill=T))
        colnames <- c('date', 'time', 'open', 'high', 'low', 'close', 'volume', 'splitFactor', 'earnings', 'dividends')#, 'Extrapolation')
        setnames(binData, names(binData), colnames)
        #system.time(util.write(binData, rawfile))
    } else {
        stop(sprintf('no data for symbol...%s', symbol))
    }
    binData
}

