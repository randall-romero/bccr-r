


#========================================================================================
#' Makes a lubridate date
#'
#' \code{YMD} returns the last date of the month, for given year and month.
#'
#' @param y Year (integer).
#' @param m Month (integer).
#' @param d Day (integer). If zero (default), set d to last day of month
#'
#' @return A lubridate date
#' @examples
#' YMD(2010, 4)  # -> April 30th, 2010
#' YMD(2016, 2)  # -> February 29th, 2016
YMD <- function(y, m, d=0){
  dd <- rep(lubridate::ymd('20150128'), length(y))
  lubridate::year(dd) <- as.integer(y)
  lubridate::month(dd) <- as.integer(m)

  if (length(d)==1 & d==0){
    dd <- lubridate::ceiling_date(dd,"month") - lubridate::days(1)
  } else {
    lubridate::day(dd) <- d
  }

  return(dd)
}


#========================================================================================
#' Fix the decimal separator
#'
#' In Costa Rica, the decimal separator is indicated by a comma (,) while R
#' expects a period (.). \code{subs_commas} replaces commas by periods.
#'
#' @param value A vector of numbers (with comma decimals)
#'
#' @return The same vector of numbers, but with period decimals
#' @export
#' @examples
#' subs_commas(3,14) # -> 3.14
subs_commas <- function(value){
  return(as.numeric(gsub(",",".",value)))
}



#========================================================================================
#' Remove empty columns
#'
#' @param datos A data.frame
#'
#' @return Same data.frame, after removing columns with no data
#'
#' @examples
#' remove_empty_columns(mydata)
remove_empty_columns <- function(datos){
  kk <- 1
  while (kk  <= ncol(datos)){
    if (all(is.na(datos[[kk]]))){
      datos[[kk]] <- NULL
    } else {
      kk <- kk + 1
    }
  }
  return(datos)
}





#========================================================================================
#' Convert daily data to monthly
#'
#' @param datos A data.table where dates are indicated by a lubridate vector "fecha"
#' @param func A function to summarize the data
#'
#' @return A data.table with monthly data
#' @export
#' @import data.table
#'
#' @examples
#' ss <- read_daily_series(list(tbasica=17,tc=367), 2010, 2015)
#' daily_to_monthly(ss, dplyr::last)
#' daily_to_monthly(ss, mean)
daily_to_monthly <- function(datos, func){

  lubridate::day(datos$fecha) <- lubridate::days_in_month(datos$fecha)
  return(datos[, lapply(.SD, func), by=.(fecha)])
}



#========================================================================================
#' Table to list
#'
#' Converts a table to a (dictionary) list. Labels are taken from first column and values from second column.
#' Used by \code{read_daily_series} to accept table input instead of lists.
#'
#' @param tab A table, whose first column is used as labels (variable names) and second column as values ("cuadro" number)
#'
#' @return A list
#' @export
#'
#' @examples
series.as.list <- function(tab){

  # tab is given as as data.frame: use column 1 for names and column 2 for table numbers
  if (is.data.frame(tab)){
    ltab <- list()
    tab <- as.data.frame(tab)

    for (k in 1:nrow(tab)){
      v <- as.character(tab[k,1])
      ltab[v] <- tab[k, 2]
      }
    return(ltab)
  }


  # tab is given as vector OR list: make sure all entries have names
  if (is.vector(tab) |  is.list(tab)){

    if (is.null(names(tab))){
      v <- sapply(tab, makeNameFromNumber)
    } else {
      v <- names(tab)
    }

    ltab <- list()

    for (k in 1:length(tab)){
      vi <- ifelse(v[k]=="", makeNameFromNumber(tab[k]), v[k])
      ltab[vi] <- tab[k]
    }

    return(ltab)
  }

}


#========================================================================================
#' Trim a data frame
#'
#' Removes initial and last observations where all variables have missing values.
#'
#' @param df A data.table
#' @param long
#'
#' @return A data.table
#' @export
#'
#' @examples
trim_dataframe <- function(df, long=FALSE){
  nr <- nrow(df)
  nc <- ncol(df)

  idx <- rep(TRUE, nr)
  k <- 1
  while (all(is.na(df[k,2:nc, with=FALSE]))){
    idx[k] <- FALSE
    k <- k + 1
  }

  k <- nr
  while (all(is.na(df[k,2:nc, with=FALSE]))){
    idx[k] <- FALSE
    k <- k - 1
  }

  df <- df[idx,]

  if (long){
    df <- gather.data(df)
  }


  return(df)
  }



#========================================================================================
#' Multiple replacement
#'
#' @param pattern A vector of strings to be replaced
#' @param replacement A matching vector of strings with the replacements
#' @param x Text to be modified
#' @param ... Additional parameters to be passed to gsub
#' @export
#' @return The modified text
#'
#' @examples
#' mgsub(wrong, right, text)
mgsub <- function(pattern, replacement, x, ...){
  if (length(pattern)!=length(replacement)){
    stop("pattern and replacement do not have the same length.")
    }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
    }
  return(result)
}



#========================================================================================
#' Fix wrong Spanish characters
#'
#' @param txt (Vector of) strings with wrong characters
#'
#' @return txt (vector of) strings with proper Spanish characters
#' @export
#'
#' @examples
fix.spanish.chars <- function(txt){
  result <- mgsub(iso.8859.1$Wrong, iso.8859.1$Character, txt)
  return(result)
  }



#========================================================================================
parse_quarter <- function(v){
  trimestres <- c('[Tt]rimestre 1', '[Tt]rimestre 2', '[Tt]rimestre 3', '[Tt]rimestre 4')
  quarters <- c('31/3', '30/6', '30/9', '31/12')
  fechas <- mgsub(trimestres, quarters, v)
  return(lubridate::dmy(fechas))
}


#========================================================================================
#' gather.data
#' Transforms a wide data.table to a long data.table
#'
#' @param datos A (wide) data.frame
#'
#' @return A long data.frame
#' @export
#'
#' @examples
gather.data <- function(datos){
  n = ncol(datos)
  return(tidyr::gather(datos,"indicador", "valor", 2:n))
}


#========================================================================================
#' Returns the title given by BCCR to a specified table
#'
#' @param x Scalar, series (table) number
#'
#' @return
#' @export
#'
#' @examples
makeNameFromNumber <- function (x){
  idx <- min(which(indicadores$CUADRO==x))
  vname <- indicadores$TITULO[idx]
  return(vname)
  }



#========================================================================================
#' Last day of current period
#'
#' @param tt A vector of lubridate dates
#' @param period String, size of period: "week", "month", "quarter", "year"
#'
#' @return A vector of lubridate dates, with dates corresponding to end of specified period
#'
#' @examples
lastday <- function(tt, period="year"){
  if (period %in% c("day", "week", "month", "quarter", "year")){
    tt <- tt + lubridate::hours(1)
    return(lubridate::ceiling_date(tt, period) - lubridate::days(1))
  } else {
    stop('period must be "day", "week", "month",  "year", or "quarter". ')
  }
}



#========================================================================================
lower_frequency <- function(datos, func, period="year"){

  datos$fecha <- lastday(datos$fecha, period)
  return(datos[, lapply(.SD, func), by=.(fecha)])
}


#========================================================================================
merge_datatables <- function(tables){
  s <- names(tables)
  n <- length(tables)

  all_series <- tables[[s[1]]]
  if (n>1){
    for (k in 2:n){
      all_series %<>% merge(tables[[s[k]]], all=TRUE)
    }
  }

  return(all_series)
}

#========================================================================================
tidy <- function(listOfTables, freq=NULL, func=mean, long=FALSE){
  cleanTable <- listOfTables %>%
    merge_datatables() %>%
    trim_dataframe()

  if (!is.null(freq)){
    cleanTable %<>% lower_frequency(func, freq)
  }

  if (long){
    cleanTable %<>% gather.data()
  }

  return(cleanTable)
}
