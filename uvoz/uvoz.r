# 2. faza: Uvoz podatkov

library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(bigchess)
library(reticulate)
library(tmap)
library(rvest)
source("lib/libraries.r")

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# DELOVNO AKTICNO PREBIVALSTVO

delovno.aktivno.prebivalstvo = read_csv2(file = "podatki/delovno_aktivno_prebivalstvo(spol,leto,regije).csv",
                                          skip=2, locale = locale(encoding = "Windows-1250"), col_names=TRUE, na="-") 

delovno.aktivno.prebivalstvo.1 = pivot_longer( delovno.aktivno.prebivalstvo,
                                              cols = colnames(delovno.aktivno.prebivalstvo)[c(-1,-2)],
                                              names_to = "LETO",
                                              values_to = "DELOVNO.AKTIVNO.PREBIVALSTVO")

colnames(delovno.aktivno.prebivalstvo.1) = c('REGIJA','SPOL',"LETO","DELOVNO.AKTIVNO.PREBIVALSTVO")

novi_spol = delovno.aktivno.prebivalstvo.1["SPOL"]

delovno.aktivno.prebivalstvo.1["SPOL"] = replace( novi_spol,novi_spol == "Spol - SKUPAJ", "Skupaj" )

delovno.aktivno.prebivalstvo.2 = as.data.frame(delovno.aktivno.prebivalstvo.1)

# PAZI MORAŠ DAT !RUN! POSEBAJ
delovno.aktivno.prebivalstvo.koncno = delovno.aktivno.prebivalstvo.2 %>%  mutate( LETO = str_replace_all(LETO, "(\\d{4}) 2 Delovno aktivno prebivalstvo po prebivališču - SKUPAJ", "\\1" ) )
# ^^^^^^^
delovno.aktivno.prebivalstvo.koncno$LETO = as.numeric(delovno.aktivno.prebivalstvo.koncno$LETO)

