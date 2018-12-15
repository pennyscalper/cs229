path.research <- function(...) {
    file.path('~/Desktop/scpd/research', ...)
}

path.stocksData <- function(...) {
    path.research('rawData', ...) 
}
path.stocksData1 <- function(...) {
    path.research('rawData', 'nasdaq100', ...) 
}
