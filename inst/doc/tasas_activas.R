## ---- results='hide'-----------------------------------------------------
library(bccr)
library(ggplot2)   # to make plots
library(magrittr)  # for pipe operators
library(tidyr)     # to manipulate the data frame
library(dplyr)
library(data.table)

## ------------------------------------------------------------------------
vars <- list(public=495, private=496)
vivienda <- read_daily_series(vars)
print(vivienda)

## ---- cache=TRUE---------------------------------------------------------
vivienda <- read_daily_series(vars, 2000, 2015, 'm', mean)
print(vivienda)

## ---- fig.width=8--------------------------------------------------------
vivienda %>% 
  gather('owner','rate',2:3) %>%
  ggplot(aes(x=fecha, y=rate, color=owner)) + 
  geom_line(size=2.0) + 
  labs(title='Lending interest rate for housing, by type of bank', x='',y='% rate ')

## ------------------------------------------------------------------------
setkey(bancarias, tipo, entidad)
myvars <- bancarias[.('Activas','Bancos Estatales'), .(rate=paste(actividad, moneda,sep = "_"), cuadro)]

## ---- cache=TRUE---------------------------------------------------------
estatal <- read_daily_series(myvars)

## ---- fig.show='hold', fig.width=8, fig.height=8-------------------------
estatal %>% 
  gather("activ_moneda", "rate", 2:ncol(estatal)) %>% 
  separate(activ_moneda,c("actividad", "moneda"),"_") %>% 
  ggplot(aes(fecha, rate, color=moneda)) + 
  geom_line() + 
  facet_wrap(~actividad)

