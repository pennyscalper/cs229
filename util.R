util.loadPackages <- function(verbose=T) {
    err <- NULL    
    loadPkg <- try(library(stringr, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'stringr')
    loadPkg <- try(library(data.table, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'data.table')
    loadPkg <- try(library(ggplot2, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'ggplot2')
    loadPkg <- try(library(TTR, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'TTR')
    try(library(colorout, verbose=verbose), silent=T)
    loadPkg <- try(library(parallel, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'parallel')
    loadPkg <- try(library(tools, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'tools')
    loadPkg <- try(library(glmnet, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'glmnet')
    loadPkg <- try(library(doMC, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'doMC')
    loadPkg <- try(library(optparse, verbose=verbose), silent=T)
    if(class(loadPkg) == 'try-error') err <- c(err, 'optparse')
    invisible(err)
}

util.loadUtils <- function(verbose=T) {
    util.source('path.R', verbose=T)
    util.source('stats.R', verbose=T)
    util.source('dateTime.R', verbose=T)
    util.source('plot.R', verbose=T)
    util.source('misc.R', verbose=T)
    invisible()
}

util.findFile <- function(f) {
    for(dir in R_RESEARCH_PATH) {
        if(file.exists(file.path(dir, f))) {
            return(file.path(dir, f))
        }
    }
    stop('cant find f')
}

util.source <- function(f, verbose=T, ...) {
    fnames <- str_split(f, ',')[[1]]
    invisible(lapply(fnames, function(fname) { filePath <- util.findFile(fname)
                                    if(verbose) {
                                        cat(sprintf('loading...%s\n', filePath))
                                    }
                                    invisible(source(filePath))}))
}

util.rPaths <- function() {
    R_RESEARCH_PATH
}

util.setWidth <- function(width=NULL) {
    if(is.null(width)) {
        options(width=system("echo $COLUMNS", intern = TRUE))
    } else {
        options(width=width)
    }
}

util.page <- function(dt) {
    page(as.data.frame(dt), method='print')
}

util.file.conn <- function(file, mode='w') {
    if(file_ext(file) %in% 'xz') {
        xzfile(file, open=mode, compression = -9)
    } else if(file_ext(file) %in% 'gz') {
        gzfile(file, open=mode, compresssion=9)
    } else if(file_ext(file) %in% 'bz') {
        bzfile(file, open=mode, compression = 9)
    } else {
        file(file, open=mode)
    }
}

util.write <- function(data, file, sep=',', row.names=F, col.names=T, quote=F, ..., verbose=T) {
    conn <- util.file.conn(file, mode='w') 
    if(verbose) {
        cat('writing')
        if(file_ext(file) %in% c('xz', 'bz', 'gz')) { 
            cat(' and compressing ')
        }
        cat(sprintf(' ==> %s\n', file))
    }
    write.table(data, conn, sep=sep, row.names=row.names, col.names=col.names, quote=quote, ...)
    close(conn)
    invisible(file)
}

util.cat <- function(file) {
    if(file_ext(file) %in% 'xz') {
        sprintf('xzcat %s', file)
    } else if(file_ext(file) %in% 'gz') {
        sprintf('gzcat %s', file)
    } else if(file_ext(file) %in% 'bz') {
        sprintf('bzcat %s', file)
    } else {
        sprintf('cat %s', file)
    }
}

util.fread <- function(file, ..., verbose=T) {
    loadCmd <- util.cat(file)
    if(verbose) {
        cat(sprintf('loading...%s\n', loadCmd))
    }

    fread(loadCmd, ...) 
}

util.loadFiles <- function(files, cores=4L, ..., verbose=T) {
    if(length(files) == 1) {
        util.fread(files, ..., verbose=verbose)
    } else {
        util.rbindlist(mclapply(files, util.fread, mc.cores=cores, ..., verbose=verbose), fill=T)
    }
}

util.rbindlist <- function(...) rbindlist(...)

util.merge <- function(dtlist, ..., filterNUll=T) {
    if(filterNUll) {
        dtlist <- dtlist[!unlist(lapply(dtlist, is.null))]
    }
    Reduce(function(x, y) merge(x, y, ...), dtlist)
}
