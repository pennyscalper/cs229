dtime.toDate <- function(date) {
    if(class(date) == 'Date') {
        return(date)
    }

    dt <- data.table(dateChar=as.character(date))
    if(any(dt[, grepl('^(19|20)[0-9]{6}$', dateChar)])) {
        dt[, date:=as.Date(dateChar, format='%Y%m%d'), dateChar]$date
    } else if(any(dt[, grepl('^(19|20)[0-9]{2}-[0-9]{2}-[0-9]{2}$', dateChar)])) {
        dt[, date:=as.Date(dateChar, format='%Y-%m-%d'), dateChar]$date
    } else if(any(dt[, grepl('^(19|20)[0-9]{2}/[0-9]{2}/[0-9]{2}$', dateChar)])) {
        dt[, date:=as.Date(dateChar, format='%Y/%m/%d'), dateChar]$date
    } else if(any(dt[, grepl('^[0-9]{2}-[0-9]{2}-(19|20)[0-9]{2}$', dateChar)])) {
        dt[, date:=as.Date(dateChar, format='%m-%d-%Y'), dateChar]$date
    } else if(any(dt[, grepl('^[0-9]{2}/[0-9]{2}/(19|20)[0-9]{2}$', dateChar)])) {
        dt[, date:=as.Date(dateChar, format='%m/%d/%Y'), dateChar]$date
    } else {
        stop('unknown date format')
    }
}

dtime.dateToStr <- function(date) {
    if(class(date) != 'Date') {
        date <- dtime.toDate(date)
    }

    data.table(date=date)[, dateChar := format(date, '%Y%m%d'), date]$dateChar
}

dtime.dateToInt <- function(date) {
    if(class(date) != 'Date') {
        date <- dtime.toDate(date)
    }

    data.table(date=date)[, dateInt := as.integer(format(date, '%Y%m%d')), date]$dateInt
}
