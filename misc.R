misc.removeCols <- function(dt, cols) {
    cols <- unlist(lapply(cols, function(x) {if(!is.null(dt[[x]])) return(x); return(NULL)}))
    lapply(cols, function(x) dt[, (x):=NULL])
    invisible(dt)
}


