###
# This script downloads data from Banco Central de Costa Rica and creates a tidy data.frame.

# Randall Romero-Aguilar
# January-February 2016




# ====DEFINE FUNCTIONS TO DOWNLOAD AND TIDY-UP DATA FROM CENTRAL BANK WEBSITE=================







#' Download a table from the BCCR website.
#'
#' \code{download_series} downloads data from the BCCR website, stripping header rows
#'
#' @param cuadro A number identifying the BCCR's data table (integer).
#' @param first The first year to download (integer, default=1990).
#' @param last The last year to download (integer, default=2015).
#'
#' @return A data.table with data in same format as in BCCR website.
#' @export
#' @examples
#' download_series(125)
#' download_series(138, first=1995, last=2016)
#' download_series(367, header=5)
download_series <- function(cuadro, first=2012, last=2015){
  bccr_web <- "http://indicadoreseconomicos.bccr.fi.cr/indicadoreseconomicos/Cuadros/frmVerCatCuadro.aspx?"
  api <- paste("&FecInicial=",first, "/01/01&FecFinal=", last, "/12/31&Exportar=True&Excel=True", sep='')
  url_series <- paste(bccr_web,"CodCuadro=",cuadro,api,sep="")

  datos <- rvest::html_table(xml2::read_html(url_series),fill=TRUE)[[1]]

  # clean column that have no data
  kk <- 1
  while (kk  <= ncol(datos)){
    if (all(is.na(datos[kk]))){
      datos <- datos[-kk]
    } else {
      kk <- kk + 1
    }
  }

  return(data.table::data.table(datos))
}







make_monthly <- function(ini, db){
  return(ymd(ini) + months(1:nrow(db)) - days(1))
}







#' Reads monthly series.
#'
#' \code{read_montly_series} is used to download tables of montly data series,
#' where each row is a month and each column a year.
#'
#' @inheritParams download_series
#' @param series A list of name=cuadro pairs. Name (a string) is the name of a
#'   series, cuadro (integer) the number of table in the BCCR website.
#'
#' @return A data frame with given series.
#' @export
#'
#' @examples
#' read_month_year(list(M1=125))
read_month_year <- function(series, first=1950, last=lubridate::year(Sys.Date())){

  if (is.data.frame(series)){
    series <- table_to_list(series)
  }

  FIRST_SERIES <- TRUE
  for (ss in names(series)){

    raw_series <- download_series(series[ss], first, last)

    ## set headers
    h <- match('Enero', raw_series$X1) - 1  # raw that has headers (year number)
    t0 <- lubridate::ymd(paste(as.integer(raw_series[h,X2]), '01 01'))  # initial date
    raw_series[h, 1] <- "mes"
    colnames(raw_series) <- as.character(raw_series[h,])
    raw_series <- raw_series[-h:-1,]
    raw_series$mes <- 1:12

    raw_series %<>% data.table::melt(id="mes", measure=2:ncol(raw_series))
    raw_series <- raw_series[,.(fecha=t0 + months(1:.N) - lubridate::days(1),
                               value=subs_commas(value))]

    colnames(raw_series) <- c('fecha', ss)
    setkey(raw_series,'fecha')

    if (FIRST_SERIES){
      all_series <- raw_series
      FIRST_SERIES <- FALSE
    } else {
      all_series %<>% merge(raw_series, all=TRUE)
    }


  }
  return(trim_dataframe(all_series))
}




#' Reads daily series.
#'
#' \code{read_daily_series} is used to download tables of a daily series, where
#' each row is a day of the year and each column a year.
#'
#' @inheritParams download_series
#' @param series A list or a table. If a list is provided, its entries should be
#'   "cuadro" numbers, labeled with the desired variable name. If a table is
#'   provided, its first column must have the desired variable names, and its
#'   second column the \emph{cuadro} numbers.
#'
#'   In all case, \emph{cuadro} is the number of a table in the BCCR website.
#'
#' @return A data.table with given series.
#' @export
#' @importFrom magrittr %>% %<>%
#' @import data.table
#'
#' @examples
#' mylist <- list(tc=367, tbasica=17)
#' dd <- read_daily_series(mylist)
read_daily_series <- function(series, first=1950, last=lubridate::year(Sys.Date()), freq='d', func=mean){


  if (is.data.frame(series)){
    series <- table_to_list(series)
  }

  FIRST_SERIES <- TRUE
  for (ss in names(series)){
    raw_series <- download_series(series[ss], first, last)

    ## set headers
    h <- match('1 Ene', raw_series$X1) - 1  # raw that has headers (year number)
    t0 <- lubridate::ymd(paste(as.integer(raw_series[h,X2]) - 1, '12 31'))  # initial date
    raw_series[h, 1] <- "dia"
    colnames(raw_series) <- as.character(raw_series[h,])
    raw_series <- raw_series[-h:-1,]


    ## code for non-leap-years
    for (kk in 2:ncol(raw_series)){
      this_year <- as.integer(colnames(raw_series)[kk])
      if (!lubridate::leap_year(this_year)){
        raw_series[60, kk] = 'DELETE ME'      # 60 = feb29
      }
    }

      raw_series %<>% data.table::melt(id="dia", measure=2:ncol(raw_series))
      raw_series <- raw_series[value != "DELETE ME",
                               .(fecha=t0 + lubridate::days(1:.N),
                                 value=subs_commas(value))]


      colnames(raw_series) <- c('fecha', ss)
      setkey(raw_series,'fecha')

      if (FIRST_SERIES){
        all_series <- raw_series
        FIRST_SERIES <- FALSE
      } else {
        all_series %<>% merge(raw_series, all=TRUE)
      }

  }

  ## Change frequency
  if (tolower(freq) == 'm'){
    all_series %<>% daily_to_monthly(func)
  }

  return(trim_dataframe(all_series))
}



#' Reads monthly series.
#'
#' \code{read_year_month} is used to download tables of montly data series,
#' where each row is a year and each column a month.
#'
#' @inheritParams read_month_year
#'
#' @return A data frame with given series.
#' @export
#' @importFrom magrittr %>% %<>%
#' @import data.table
#'
#' @examples
#' read_year_month(list(lmn=95, lme=96))
read_year_month <- function(series, first=1950, last=lubridate::year(Sys.Date())){

  if (is.data.frame(series)){
    series <- table_to_list(series)
  }

  FIRST_SERIES <- TRUE
  for (ss in names(series)){

    raw_series <- download_series(series[ss], first, last)

    ## set headers
    h <- match('Enero', raw_series$X2)   # raw that has headers (year number)
    t0 <- lubridate::ymd(paste(as.integer(raw_series[h+1,X1]), '01 01'))  # initial date
    colnames(raw_series) <- c('anno', 1:12)
    raw_series <- raw_series[-h:-1,]
    #raw_series$mes <- 1:12

    raw_series %<>% data.table::melt(id="anno", measure=2:ncol(raw_series))
    raw_series <- raw_series[,.(fecha=YMD(anno, variable),
                                value=subs_commas(value))]

    colnames(raw_series) <- c('fecha', ss)
    setkey(raw_series,'fecha')

    if (FIRST_SERIES){
      all_series <- raw_series
      FIRST_SERIES <- FALSE
    } else {
      all_series %<>% merge(raw_series, all=TRUE)
    }
  }

  return(trim_dataframe(all_series))
}






#' Title
#'
#' Extracts title information (first two lines) for given table numbers
#'
#' @param series A vector of table numbers
#'
#' @return A table
#' @export
#'
#' @examples
#' read_titles(c(125, 96))
read_titles <- function(series){

  series <- as.character(series)
  raw_series <- data.table(series, NaN,NaN)
  colnames(raw_series) <- c('series','title','subtitle')
  setkey(raw_series, 'series')


  bccr_web <- "http://indicadoreseconomicos.bccr.fi.cr/indicadoreseconomicos/Cuadros/frmVerCatCuadro.aspx?"
  api <- "&Exportar=True&Excel=True"

  for (ss in series){
    url_series <- paste(bccr_web,"CodCuadro=",ss,api,sep="")

    tryCatch(
      {
        titulos <- rvest::html_table(xml2::read_html(url_series),fill=TRUE)[[1]]$X1[1:2]
        raw_series[ss, title:= titulos[1]]
        raw_series[ss, subtitle:= titulos[2]]
        print(paste(ss, titulos[1], sep = "   &   "))
      },
      error=function(cond){
        print(paste(ss, "ERROR -- Could not find the title", sep = "   &   "))
      }

    )


  }

  return(raw_series)
}





#' Find indicators by name
#'
#' @param name A string text with part of the indicator name
#'
#' @return A data.frame with indicators whose names match the requested value
#' @export
#'
#' @examples
#' find_series("ipc")
#' find_series("agricultura")
find_series <- function(name){
  idx <- grep(name, bccr::indicadores$TITULO,ignore.case = TRUE)
  return(bccr::indicadores[idx,])
}
