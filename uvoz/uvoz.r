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

#######################################

# MESEČNO GIBANJE BREZPOSELNIH


library("readxl")
mesecno.gibanje.brezposelnih <- read_xls("podatki/Mesecno_gibanje_Brezposelnih_1992-2022.xls",skip = 2 , col_names = TRUE) %>% as.data.frame()
mesecno.gibanje.brezposelnih = mesecno.gibanje.brezposelnih[1:12,]
mesecno.gibanje.brezposelnih.1 = pivot_longer(mesecno.gibanje.brezposelnih ,cols = colnames(mesecno.gibanje.brezposelnih)[-1], names_to = "LETO", values_to = "MESECNA_BREZPOSELNOST" )
colnames(mesecno.gibanje.brezposelnih.1)[1] = "MESEC"
mesecno.gibanje.brezposelnih.1$LETO = as.numeric(mesecno.gibanje.brezposelnih.1$LETO)
mesecno.gibanje.brezposelnih.koncno = mesecno.gibanje.brezposelnih.1[,c(2,1,3)]

########################################

# POVPREČNI DOHODEK NA ČLANA GOSPODINJSTVA


povp.dohod.na.clana.gospodinjstva = read_csv2(file = "podatki/povp_dohodek_na _clana_gospodinjstva(leto,regija).csv",
                                         skip=2, locale = locale(encoding = "Windows-1250"), col_names=TRUE) %>% as.data.frame()

povp.dohod.na.clana.gospodinjstva = povp.dohod.na.clana.gospodinjstva[,-1]
povp.dohod.na.clana.gospodinjstva.1 = pivot_longer(povp.dohod.na.clana.gospodinjstva ,cols = colnames(povp.dohod.na.clana.gospodinjstva)[-1], names_to = "LETO", values_to = "POVP.DOHOD.NA.CLANA.GOSPODINJSTVA" )
colnames(povp.dohod.na.clana.gospodinjstva.1)[1] = "REGIJA"
povp.dohod.na.clana.gospodinjstva.1$LETO = as.numeric(povp.dohod.na.clana.gospodinjstva.1$LETO)
povp.dohod.na.clana.gospodinjstva.koncno = povp.dohod.na.clana.gospodinjstva.1

#######################################

# PREBIVALSTVO

prebivalstvo = read_csv2(file = "podatki/prebivalstvo(regija,spol,leto).csv",
                                              skip=2, locale = locale(encoding = "Windows-1250"), col_names=TRUE) %>% as.data.frame()
prebivalstvo.1 = pivot_longer(prebivalstvo, cols = colnames(prebivalstvo)[-c(1,2)], names_to = "LETO", values_to = "PREBIVALSTVO")
colnames(prebivalstvo.1)[2] = "REGIJA"
prebivalstvo.1["SPOL"] = replace(prebivalstvo.1["SPOL"],prebivalstvo.1["SPOL"] == "Spol - SKUPAJ", "Skupaj" )
prebivalstvo.1$LETO = as.numeric(prebivalstvo.1$LETO)
prebivalstvo.koncno = prebivalstvo.1

########################################

# ŠTEVILO OSEB POD PRAGOM REVŠČINE

st.oseb.pod.pragom = read_csv2(file = "podatki/stevilo_oseb_pod_pragom_revscine(leto,regija).csv",
                         skip=2, locale = locale(encoding = "Windows-1250"), col_names=TRUE) %>% as.data.frame()
st.oseb.pod.pragom = st.oseb.pod.pragom[,-1]
colnames(st.oseb.pod.pragom)[1] = "REGIJA"
st.oseb.pod.pragom.1 = pivot_longer(st.oseb.pod.pragom ,cols = colnames(st.oseb.pod.pragom)[-1], names_to = "LETO", values_to = "ST.OSEB.POD.PRAGOM.REVSCINE" )
st.oseb.pod.pragom.1$LETO = as.numeric(st.oseb.pod.pragom.1$LETO)
st.oseb.pod.pragom.koncno = st.oseb.pod.pragom.1

########################################

# ŠTEVILO RESNO MATERIALNO PRIKRAJŠANIH

st.resno.materialno.prikrajsanih = read_csv2(file = "podatki/stevilo_resno_materialno_prikrajsanih(leto,regije).csv",
                               skip=2, locale = locale(encoding = "Windows-1250"), col_names=TRUE) %>% as.data.frame()
st.resno.materialno.prikrajsanih = st.resno.materialno.prikrajsanih[,-1]
colnames(st.resno.materialno.prikrajsanih)[1] = "REGIJA"
st.resno.materialno.prikrajsanih.1 = pivot_longer(st.resno.materialno.prikrajsanih ,cols = colnames(st.resno.materialno.prikrajsanih)[-1], names_to = "LETO", values_to = "ST.RESNO.MATERIALNO.PRIKRAJSANIH" )
st.resno.materialno.prikrajsanih.1$LETO = as.numeric(st.resno.materialno.prikrajsanih.1$LETO)
st.resno.materialno.prikrajsanih.koncno = st.resno.materialno.prikrajsanih.1


########################################



website <- read_html("https://www.racunovodstvo.net/tabelice/62/podatki-za-obracun-plac-osnovni-podatki")

vse_tabele <- website %>% html_table(fill = TRUE)
dva_ena = vse_tabele[[2]][2:13,] %>% as.data.frame() %>% select(c(1,2,4))  %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
dva_nic = vse_tabele[[3]][2:13,] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_devet = vse_tabele[[4]][2:13,] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_osem = vse_tabele[[5]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_sedem = vse_tabele[[6]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_sest = vse_tabele[[7]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_pet = vse_tabele[[8]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_stiri = vse_tabele[[9]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_tri = vse_tabele[[10]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_dva = vse_tabele[[11]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_ena = vse_tabele[[12]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
ena_nic = vse_tabele[[13]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
devet = vse_tabele[[14]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
osem = vse_tabele[[15]] %>% as.data.frame() %>% select(c(1,2,4))%>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )
sedem = vse_tabele[[16]] %>% as.data.frame() %>% select(c(1,2,4)) %>% rename(MESEC = 1, POVP.BRUTO.PLACA = 2, POVP.MIN.PLACA = 3 )

for (i in c(2,3)) {
  
  dva_ena[[i]] =  str_extract(dva_ena[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  dva_nic[[i]] = str_extract(dva_nic[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_devet[[i]] = str_extract(ena_devet[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_osem[[i]] = str_extract(ena_osem[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_sedem[[i]] = str_extract(ena_sedem[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_sest[[i]] = str_extract(ena_sest[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_pet[[i]] = str_extract(ena_pet[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_stiri[[i]] = str_extract(ena_stiri[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_tri[[i]] = str_extract(ena_tri[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_dva[[i]] = str_extract(ena_dva[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_ena[[i]] = str_extract(ena_ena[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  ena_nic[[i]] = str_extract(ena_nic[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  devet[[i]] = str_extract(devet[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  osem[[i]] = str_extract(osem[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  sedem[[i]] = str_extract(sedem[[i]],"(\\d*\\.\\d*\\,\\d*|\\d*\\,\\d*)")
  
  dva_ena[[i]] = dva_ena[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  dva_nic[[i]] = dva_nic[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_devet[[i]] = ena_devet[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_osem[[i]] = ena_osem[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_sedem[[i]] = ena_sedem[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_sest[[i]] = ena_sest[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_pet[[i]] = ena_pet[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_stiri[[i]] = ena_stiri[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_tri[[i]] = ena_tri[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_dva[[i]] = ena_dva[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_ena[[i]] = ena_ena[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  ena_nic[[i]] = ena_nic[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  devet[[i]] = devet[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  osem[[i]] = osem[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  sedem[[i]] = sedem[[i]] %>% str_replace_all( "\\.", "") %>% str_replace_all( ",", ".") %>% as.numeric()
  
}

sedem_letno = sedem %>% summarise(LETO = 2007 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
osem_letno = osem %>% summarise(LETO = 2008 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
devet_letno = devet %>% summarise(LETO = 2009 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_nic_letno = ena_nic%>% summarise(LETO = 2010 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_ena_letno = ena_ena %>% summarise(LETO = 2011 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_dva_letno = ena_dva %>% summarise(LETO = 2012 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_tri_letno = ena_tri %>% summarise(LETO = 2013 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_stiri_letno = ena_stiri %>% summarise(LETO = 2014 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = 792.05)
ena_pet_letno = ena_pet %>% summarise(LETO = 2015 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_sest_letno = ena_sest %>% summarise(LETO = 2016 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_sedem_letno = ena_sedem %>% summarise(LETO = 2017 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_osem_letno = ena_osem %>% summarise(LETO = 2018 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
ena_devet_letno = ena_devet %>% summarise(LETO = 2019 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
dva_nic_letno = dva_nic %>% summarise(LETO = 2020 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)
dva_ena_letno = dva_ena %>% summarise(LETO = 2021 ,POVP.BRUTO.PLACA = sum(POVP.BRUTO.PLACA)/12, POVP.MIN.PLACA = sum(POVP.MIN.PLACA)/12)

povp.placa = sedem_letno
povp.placa[2,] = osem_letno
povp.placa[3,] = devet_letno
povp.placa[4,] = ena_nic_letno
povp.placa[5,] = ena_dva_letno
povp.placa[6,] = ena_tri_letno
povp.placa[7,] = ena_stiri_letno
povp.placa[8,] = ena_pet_letno
povp.placa[9,] = ena_sest_letno
povp.placa[10,] = ena_sedem_letno
povp.placa[11,] = ena_osem_letno
povp.placa[12,] = ena_devet_letno
povp.placa[13,] = dva_nic_letno
povp.placa[14,] = dva_ena_letno
povp.placa[15,] = ena_ena_letno


vec = c (dva_ena , dva_nic, ena_devet, ena_osem, ena_sedem, ena_sest, ena_pet, ena_stiri, ena_tri, ena_dva, ena_ena, ena_nic, devet, osem, sedem)
######################################

# ZDRUŽEVANJE TABEL

# tabela 1

data_merge1 <- inner_join(st.oseb.pod.pragom.koncno, st.resno.materialno.prikrajsanih.koncno, by = c("LETO", "REGIJA")) 
data_merge2 <- inner_join(data_merge1, povp.dohod.na.clana.gospodinjstva.koncno, by = c("LETO", "REGIJA")) 

priprava = delovno.aktivno.prebivalstvo.koncno %>% filter(SPOL == "Skupaj") %>% select(REGIJA, LETO, DELOVNO.AKTIVNO.PREBIVALSTVO)

data_merge3 = priprava %>% left_join(data_merge2, by=c("LETO", "REGIJA") )

priprava2 = prebivalstvo.koncno %>% filter(SPOL == "Skupaj") %>% select(REGIJA, LETO, PREBIVALSTVO)

tabela_1 = data_merge3 %>% left_join(priprava2, by=c("LETO", "REGIJA"))

# tabela 2

tabela_2 = delovno.aktivno.prebivalstvo.koncno %>% left_join(prebivalstvo.koncno, by = c("REGIJA", "LETO", "SPOL"))

# tabela 3

brezposelni_letno = mesecno.gibanje.brezposelnih.koncno %>% group_by(LETO) %>% summarise(LETNA_BREZPOSELNOST = sum(MESECNA_BREZPOSELNOST))
delovno_aktivno_slo = delovno.aktivno.prebivalstvo.koncno %>% filter(SPOL == "Skupaj" ) %>% filter(REGIJA == "SLOVENIJA" ) %>% select(LETO, DELOVNO.AKTIVNO.PREBIVALSTVO)
povp_dohod_slo = povp.dohod.na.clana.gospodinjstva.koncno %>% filter(REGIJA == "SLOVENIJA") %>% select(LETO, POVP.DOHOD.NA.CLANA.GOSPODINJSTVA)
prebivalstvo_slo = prebivalstvo.koncno %>% filter(REGIJA == "SLOVENIJA") %>% filter(SPOL == "Skupaj" ) %>% select(LETO, PREBIVALSTVO)
st_oseb_pod_pragom_slo = st.oseb.pod.pragom.koncno %>% filter(REGIJA == "SLOVENIJA") %>% select(LETO, ST.OSEB.POD.PRAGOM.REVSCINE)
st_resno_prikrajsanih_slo = st.resno.materialno.prikrajsanih.koncno %>% filter(REGIJA == "SLOVENIJA") %>% select(ST.RESNO.MATERIALNO.PRIKRAJSANIH,LETO)

tabela_3 = st_oseb_pod_pragom_slo %>% left_join(st_resno_prikrajsanih_slo, by = "LETO") %>% 
                                      left_join(brezposelni_letno, by = "LETO")  %>%
                                      left_join(delovno_aktivno_slo, by = "LETO") %>%
                                      left_join(prebivalstvo_slo, by = "LETO") %>%
                                      left_join(povp_dohod_slo, by = "LETO") %>%
                                      left_join(povp.placa, by = "LETO")
