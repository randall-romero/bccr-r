



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
table_to_list <- function(tab){
  ltab <- list()
  tab <- as.data.frame(tab)

  for (k in 1:nrow(tab)){
    v <- as.character(tab[k,1])
    ltab[v] <- tab[k, 2]
  }

  return(ltab)
}



#' Trim a data frame
#'
#' Removes initial and last observations where all variables have missing values.
#'
#' @param df A data.table
#'
#' @return A data.table
#' @export
#'
#' @examples
trim_dataframe <- function(df){
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

  return(df[idx,])


  }
