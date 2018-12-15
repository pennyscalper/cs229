stat.winsorize <- function(x, q=0.02) {
    low <- quantile(x, q, na.rm=T)
    high <- quantile(x, 1 - q, na.rm=T)
    ifelse(x < low, rep(low, length(x)), ifelse(x > high, rep(high, length(x)), x)) 
}

stat.sum <- function(x, na.rm=T) {
    sum(x, na.rm=na.rm)
}

stat.cumsum <- function(x, na.rm=T) {
    if(na.rm) {
        x[is.na(x)] <- 0
    }
    cumsum(x)
}

stat.mean <- function(x, na.rm=T) {
    mean(x, na.rm=na.rm)
}

stat.wt.mean <- function(x, w, na.rm=T) {
    if(na.rm) {
        id.na <- is.na(x) | is.na(w)
        x <- x[!(id.na)]
        w <- w[!(id.na)]
    }

    stat.sum(x * w, na.rm=na.rm) / stat.sum(w, na.rm=na.rm)
}

stat.quantile <- function(x, q, na.rm=T) {
    quantile(x, q, na.rm=na.rm)
}

stat.pmax <- function(x, y, na.rm=T) {
    pmax(x, y, na.rm=na.rm)
}

stat.pmin <- function(x, y, na.rm=T) {
    pmin(x, y, na.rm=na.rm)
}

stat.summary <- function(x, qtiles=NULL) {
    summ <- list(n      =length(x),
                 min    =min(x, na.rm=T),
                 q1     =quantile(x, 0.25, na.rm=T),
                 median =quantile(x, 0.5, na.rm=T),
                 mean   =mean(x, na.rm=T),
                 q3     =quantile(x, 0.75, na.rm=T),
                 max    =max(x, na.rm=T),
                 sd     =sd(x, na.rm=T),
                 serr   =sd(x, na.rm=T) / sqrt(stat.sum(!is.na(x))),
                 NAs    =sum(is.na(x)))
    if(!is.null(qtiles)) {
        summQtiles <- lapply(qtiles, quantile, x=x, na.rm=T)
        names(summQtiles) <- paste0('q', qtiles)
        summ <- append(summ, summQtiles)
    }
    summ
}

stat.sigmoid <- function(x) {
    1 / (1  + exp(-x))
}
