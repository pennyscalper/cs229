#!/usr/bin/env Rscript

# set up R env
source('util.R')
packageErrs <- util.loadPackages()
if(interactive()) util.setWidth()

# get optional arguments
option_list <- list(make_option(c("-s", "--symbol"), type='character', default='MSFT', help="symbol to run feature generation"),
                    make_option(c("-b", "--bin_size"), type='integer', default=5L, help="bin size in minutes"),
                    make_option(c("-i", "--ignore_year"), type='character', default='2009', help="some stocks have issues so selectively data is thrown away"),
                    make_option(c("-t", "--train_year"), type='character', default='2015', help="year from (ignore_year to train_year) will be used to train the reflex model"),
                    make_option(c("-p", "--gen_plots"), type='logical', default=TRUE, help="generate plots"),
                    make_option(c("-m", "--gen_reflex"), type='logical', default=TRUE, help="generate reflex model"))

optArgs <- parse_args(OptionParser(option_list=option_list))
print(optArgs)
symbol <- optArgs$symbol
binSize <- optArgs$bin_size
ignoreYear <- optArgs$ignore_year
trainYear <- optArgs$train_year
plot <- optArgs$gen_plots
reflexModel <- optArgs$gen_reflex

# generate features/data/linearReflexModel
if(length(packageErrs)) {
    cat(sprintf('error loading R packages: %s\n', paste0(packageErrs, collapse=', ')))
} else {
    outDir <- '../output'
    if(!file.exists(outDir)) {
        dir.create('../output', recursive=T)
    }
    source('features.R')      
    source('path.R')
    source('stats.R')
    source('dateTime.R')
    source('plot.R')
    source('misc.R')
    source('model.R')
    cat('generating raw features\n')
    if(!file.exists(features.rawBinFile(symbol, binSize))) features.genRawBins(symbol=symbol, binSize=binSize)
    if(!file.exists(features.rawBinFile('XLK', binSize))) features.genRawBins(symbol='XLK', binSize=binSize) 
    if(!file.exists(features.rawBinFile('SPY', binSize))) features.genRawBins(symbol='SPY', binSize=binSize)

    cat('generating filtered features\n')
    if(!file.exists(features.fltBinFile(symbol, binSize)) || T) features.genFltBins(symbol=symbol, binSize=binSize)
    if(!file.exists(features.fltBinFile('XLK', binSize)) || T) features.genFltBins(symbol='XLK', binSize=binSize) 
    if(!file.exists(features.fltBinFile('SPY', binSize)) || T) features.genFltBins(symbol='SPY', binSize=binSize)

    cat('generating final features\n')
    features.generate(symbol=symbol, binSize=binSize, plot=plot)

    if(plot) {
        cat('plotting intraday dynamics\n')
        features.plotBinFeatures(symbol, binSize, dvEarningsTrend=NULL, bvEarningsTrend=NULL)
    }

    cat('generating linear reflex model\n')
    model.generateLinearReflex(symbol, binSize, ignoreYear=ignoreYear, trainYear=trainYear)
}


