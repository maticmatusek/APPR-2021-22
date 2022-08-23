# 3. faza: Vizualizacija podatkov


library(ggpubr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(tmap)
source("lib/libraries.r")
library(tidyverse)

leto_regija = read_csv("leto_regija.csv")
leto_regija_spol = read_csv("leto_regija_spol.csv")
leto_slo = read_csv("leto_slo.csv")

#########################

leto_regija$AKTIVNO.PREB.proti.CELOTNEM.PREB = leto_regija$DELOVNO.AKTIVNO.PREBIVALSTVO / leto_regija$PREBIVALSTVO
leto_regija$POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB = leto_regija$ST.OSEB.POD.PRAGOM.REVSCINE / leto_regija$PREBIVALSTVO
leto_regija$RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB = leto_regija$ST.RESNO.MATERIALNO.PRIKRAJSANIH / leto_regija$PREBIVALSTVO

########################

leto_regija_spol$AKTIVNO.PREB.proti.CELOTNEM.PREB = leto_regija_spol$DELOVNO.AKTIVNO.PREBIVALSTVO / leto_regija_spol$PREBIVALSTVO

########################

leto_slo$AKTIVNO.PREB.proti.CELOTNEM.PREB = leto_slo$DELOVNO.AKTIVNO.PREBIVALSTVO / leto_slo$PREBIVALSTVO
leto_slo$POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB = leto_slo$ST.OSEB.POD.PRAGOM.REVSCINE / leto_slo$PREBIVALSTVO
leto_slo$RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB = leto_slo$ST.RESNO.MATERIALNO.PRIKRAJSANIH / leto_slo$PREBIVALSTVO
leto_slo$BREZPOSELNOST.proti.CELOTNO.PREB = leto_slo$LETNA.BREZPOSELNOST / leto_slo$PREBIVALSTVO
leto_slo$MIN.PLACA.proti.BRUTO.PLACA = leto_slo$POVP.MIN.PLACA / leto_slo$POVP.BRUTO.PLACA

########################
########################


# leto_slo

g_primerjava_stopenj_zaposlenosti_slovenija = leto_slo %>%
  ggplot( aes(x=LETO)) +                    
  geom_line(aes(y=BREZPOSELNOST.proti.CELOTNO.PREB , color = "Stopnja brezposelnosti"),lwd=1) +  
  geom_line(aes(y=AKTIVNO.PREB.proti.CELOTNEM.PREB, color = "Stopnja aktivnega prebivalstva"),lwd=1) +
  labs(
    x = "Leto",
    title = "Slovenija",
    y = "%"
  ) +
  scale_x_continuous(breaks=seq(2007, 2020, 1)) + rremove("legend.title")+
  theme(axis.text.x = element_text(angle = 30))

g_primerjava_stopenj_zaposlenosti_slovenija

#######################

g_stevilo_oseb_slovenija = leto_slo %>%
  ggplot(aes(x=LETO)) +
  geom_line(aes(y= ST.OSEB.POD.PRAGOM.REVSCINE , color = "Število ljudi pod pragom revščine"),lwd=1) +  
  geom_line(aes(y=ST.RESNO.MATERIALNO.PRIKRAJSANIH, color = "Število resno materialno prikrajšanih"),lwd=1) +
  geom_line(aes(y=LETNA.BREZPOSELNOST, color = "Število brezposelnih"),lwd=1) +
  geom_line(aes(y=DELOVNO.AKTIVNO.PREBIVALSTVO, color = "Število delovno aktivnih ljudi"),lwd=1) +
  labs(
    x = "Leto",
    title = "Slovenija",
    y = "Število ljudi"
  ) +
  scale_x_continuous(breaks=seq(2007, 2020, 1)) + 
  scale_y_continuous(breaks=seq(0, 1500000, 150000)) + rremove("legend.title")


g_stevilo_oseb_slovenija

########################

g_place = leto_slo %>%
  ggplot(aes(x=LETO)) +
  geom_line(aes(y= POVP.BRUTO.PLACA , color = "Povprečna bruto plača"),lwd=1) +  
  geom_line(aes(y= POVP.MIN.PLACA , color = "Povprečna minimalna plača"),lwd=1)+
  labs(
    x = "Leto",
    title = "Slovenija",
    y = "€"
  ) +
  scale_x_continuous(breaks=seq(2007, 2020, 1))  + 
  scale_y_continuous(breaks=seq(0, 2500, 250)) + rremove("legend.title")+
  theme(axis.text.x = element_text(angle = 30))

g_place

#######################

g_revscina = leto_slo %>%
  ggplot(aes(x=LETO)) +
  geom_line(aes(y= ST.OSEB.POD.PRAGOM.REVSCINE , color = "Število ljudi pod pragom revščine"),lwd=1) +  
  geom_line(aes(y=ST.RESNO.MATERIALNO.PRIKRAJSANIH, color = "Število resno materialno prikrajšanih"),lwd=1) +
  labs(
    x = "Leto",
    title = "Slovenija",
    y = "Število ljudi"
  ) +
  scale_x_continuous(breaks=seq(2007, 2020, 1)) +
  scale_y_continuous(breaks=seq(0, 300000, 30000))+ rremove("legend.title")+
  theme(axis.text.x = element_text(angle = 30))


g_revscina

#######################

g_primerjava_stopenj_revscine = leto_slo %>%
  ggplot(aes(x=LETO)) +
  geom_line(aes(y= POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB , color = "Stopnja ljudi pod pragom revščine"),lwd=1) +  
  geom_line(aes(y= RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB, color = "Stopnja resno materialno prikrajšanih"),lwd=1) +
  labs(
    x = "Leto",
    title = "Slovenija",
    y = "%"
  ) +
  scale_x_continuous(breaks=seq(2007, 2020, 1)) +
  scale_y_continuous(breaks=seq(0, 0.15, 0.015))+ rremove("legend.title")+
  theme(axis.text.x = element_text(angle = 30))

g_primerjava_stopenj_revscine

########################

g_revscina_placa_zaposlenost = ggarrange(g_primerjava_stopenj_revscine + rremove("x.title") + rremove("legend.title"), g_place+ rremove("x.title") + rremove("legend.title") + labs(title=""),g_primerjava_stopenj_zaposlenosti_slovenija+ rremove("x.title") + rremove("legend.title") + labs(title=""), nrow = 3, align = "v", heights = c(5,5,5))
g_revscina_placa_zaposlenost

#########################
# slab scenarij

max_brezposelnost =  leto_slo %>% filter(leto_slo$BREZPOSELNOST.proti.CELOTNO.PREB == max(leto_slo$BREZPOSELNOST.proti.CELOTNO.PREB))
max_pod_pragom = leto_slo %>% filter(leto_slo$POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB == max(leto_slo$POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB))
max_materialno_prikrajsani =  leto_slo %>% filter(leto_slo$RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB == max(leto_slo$RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB))
min_aktivno_preb =  leto_slo %>% filter(leto_slo$AKTIVNO.PREB.proti.CELOTNEM.PREB == min(leto_slo$AKTIVNO.PREB.proti.CELOTNEM.PREB))
min_bruto =  leto_slo %>% filter(leto_slo$POVP.BRUTO.PLACA == min(leto_slo$POVP.BRUTO.PLACA))
min_min_placa =  leto_slo %>% filter(leto_slo$POVP.MIN.PLACA == min(leto_slo$POVP.MIN.PLACA))

#########################
# dober scenarij
min_brezposelnost =   leto_slo %>% filter(leto_slo$BREZPOSELNOST.proti.CELOTNO.PREB == min(leto_slo$BREZPOSELNOST.proti.CELOTNO.PREB))
min_pod_pragom =  leto_slo %>% filter(leto_slo$POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB == min(leto_slo$POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB))
min_materialno_prikrajsani =  leto_slo %>% filter(leto_slo$RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB == min(leto_slo$RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB))
max_aktivno_preb =  leto_slo %>% filter(leto_slo$AKTIVNO.PREB.proti.CELOTNEM.PREB == max(leto_slo$AKTIVNO.PREB.proti.CELOTNEM.PREB))
max_bruto_placa =  leto_slo %>% filter(leto_slo$POVP.BRUTO.PLACA == max(leto_slo$POVP.BRUTO.PLACA))
max_min_placa =  leto_slo %>% filter(leto_slo$POVP.MIN.PLACA == max(leto_slo$POVP.MIN.PLACA))


#########################    Mislim, da z leto_slo več ali manj konec
#########################

# leto_regija

resno_materialno_prikrajsani_regija = leto_regija %>% filter(RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB != is.na(RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB)) %>%
  ggplot(
    mapping = aes(x = LETO, y = RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB, color = REGIJA)
  ) +
  geom_line() +
  scale_x_continuous(
    breaks = 2008:2020)+
  theme(
    axis.text.x = element_text(angle = 60, size=5.5, vjust = 1,hjust = 1),
  ) +
  labs(
    x = "Leto",
    y = "Stopnja resno materialno prikrajšanih (%)",
    title = "Stopnja resno materialno prikrajšanih"
  ) +
  facet_wrap(~ REGIJA) 

resno_materialno_prikrajsani_regija

#########################

dohodek_na_clana_regija = leto_regija %>% filter(POVP.DOHOD.NA.CLANA.GOSPODINJSTVA != is.na(POVP.DOHOD.NA.CLANA.GOSPODINJSTVA)) %>%
  ggplot(
    mapping = aes(x = LETO, y = POVP.DOHOD.NA.CLANA.GOSPODINJSTVA, fill = REGIJA)
  ) +
  geom_col() +
  scale_x_continuous(
    breaks = 2008:2020)+
  theme(
    axis.text.x = element_text(angle = 60, size=5.5, vjust = 1, hjust=1),
  ) +
  labs(
    x = "Leto",
    y = "Dohodek na člana gospodinjstva",
    title = "Dohodek na člana gospodinjstva"
  ) +
  facet_wrap(~ REGIJA) 

dohodek_na_clana_regija

############################

g_aktivno_preb_regije = leto_regija %>% filter(AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>%
  ggplot( mapping = aes(x = LETO, y = AKTIVNO.PREB.proti.CELOTNEM.PREB, color=REGIJA)) +
  geom_line(lwd=1) +
  scale_x_continuous(breaks = 2005:2020)+
  labs( x = "Leto", y = "%", title = "Stopnja aktivnega prebivalstva")

g_aktivno_preb_regije

############################

g_pod_pragom_regije = leto_regija %>% filter(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB != is.na(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB)) %>%
  ggplot( mapping = aes(x = LETO, y = POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB, color=REGIJA)) +
  geom_line() +
  scale_x_continuous(breaks = 2005:2020)+
  labs( x = "Leto", y = "%", title = "Stopnja ljudi pod pragom revščine")+
  facet_wrap(~ REGIJA)+
  theme(
    axis.text.x = element_text(angle = 60, size=5.5, vjust = 1, hjust=1),
  ) 

g_pod_pragom_regije

############################# 
# slab scenarij
Letna_regija_max_pod_pragom_revscine = leto_regija %>% filter(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB != is.na(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB) ) %>% filter(REGIJA != "SLOVENIJA" ) %>%
  group_by(LETO) %>%
  mutate(MAX_POD_PRAGOM_REVSCINE = max(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB)) %>% filter(MAX_POD_PRAGOM_REVSCINE == POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB) %>% dplyr::select(LETO,REGIJA,MAX_POD_PRAGOM_REVSCINE) %>% arrange(-desc(LETO))

Letna_regija_max_materialno_prikrjsani = leto_regija %>% filter(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB != is.na(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB) ) %>% filter(REGIJA != "SLOVENIJA" ) %>%
  group_by(LETO) %>%
  mutate(MAX_MATERIALNO_PRIKRAJSANIH = max(RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB)) %>% filter(MAX_MATERIALNO_PRIKRAJSANIH == RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB) %>% dplyr::select(LETO,REGIJA,MAX_MATERIALNO_PRIKRAJSANIH) %>% arrange(-desc(LETO))

Letna_regija_min_aktivno_prebivalstvo = leto_regija %>% filter(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB != is.na(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB) ) %>% filter(REGIJA != "SLOVENIJA" ) %>%
  group_by(LETO) %>%
  mutate(MIN_AKTIVNO_PREB = min(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(MIN_AKTIVNO_PREB == AKTIVNO.PREB.proti.CELOTNEM.PREB) %>% dplyr::select(LETO,REGIJA,MIN_AKTIVNO_PREB) %>% arrange(-desc(LETO))

min_dohodek_na_clana = leto_regija %>% filter(POVP.DOHOD.NA.CLANA.GOSPODINJSTVA != is.na(POVP.DOHOD.NA.CLANA.GOSPODINJSTVA) ) %>% filter(REGIJA != "SLOVENIJA" ) %>%
  group_by(LETO) %>%
  mutate(MIN_DOHOD_NA_CLANA = min(POVP.DOHOD.NA.CLANA.GOSPODINJSTVA)) %>% filter(MIN_DOHOD_NA_CLANA == POVP.DOHOD.NA.CLANA.GOSPODINJSTVA) %>% dplyr::select(LETO,REGIJA,MIN_DOHOD_NA_CLANA) %>% arrange(-desc(LETO))


############################
# dober scenarij
Letna_regija_min_pod_pragom_revscine = leto_regija %>% filter(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB != is.na(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB) ) %>% filter(REGIJA != "SLOVENIJA" ) %>%
  group_by(LETO) %>%
  mutate(MIN_POD_PRAGOM_REVSCINE = min(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB)) %>% filter(MIN_POD_PRAGOM_REVSCINE == POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB) %>% dplyr::select(LETO,REGIJA,MIN_POD_PRAGOM_REVSCINE) %>% arrange(-desc(LETO))

Letna_regija_min_materialno_prikrjsani = leto_regija %>% filter(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB != is.na(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB) ) %>% filter(REGIJA != "SLOVENIJA" ) %>%
  group_by(LETO) %>%
  mutate(MIN_MATERIALNO_PRIKRAJSANIH = min(RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB)) %>% filter(MIN_MATERIALNO_PRIKRAJSANIH == RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB) %>% dplyr::select(LETO,REGIJA,MIN_MATERIALNO_PRIKRAJSANIH) %>% arrange(-desc(LETO))

Letna_regija_max_aktivno_prebivalstvo = leto_regija %>% filter(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB != is.na(POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB) ) %>% filter(REGIJA != "SLOVENIJA" ) %>%
  group_by(LETO) %>%
  mutate(MAX_AKTIVNO_PREB = max(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(MAX_AKTIVNO_PREB == AKTIVNO.PREB.proti.CELOTNEM.PREB) %>% dplyr::select(LETO,REGIJA,MAX_AKTIVNO_PREB) %>% arrange(-desc(LETO))

max_dohodek_na_clana = leto_regija %>% filter(POVP.DOHOD.NA.CLANA.GOSPODINJSTVA != is.na(POVP.DOHOD.NA.CLANA.GOSPODINJSTVA) ) %>% filter(REGIJA != "SLOVENIJA" ) %>%
  group_by(LETO) %>%
  mutate(MAX_DOHOD_NA_CLANA = max(POVP.DOHOD.NA.CLANA.GOSPODINJSTVA)) %>% filter(MAX_DOHOD_NA_CLANA == POVP.DOHOD.NA.CLANA.GOSPODINJSTVA) %>% dplyr::select(LETO,REGIJA,MAX_DOHOD_NA_CLANA) %>% arrange(-desc(LETO))


########################### Mislim, da z leto_regija več ali manj konec
###########################
# leto_regija_spol

g_aktivno = leto_regija_spol %>%   filter(leto_regija_spol$AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(leto_regija_spol$AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(SPOL != "Skupaj") %>% filter(REGIJA != "SLOVENIJA") %>%
  ggplot( mapping = aes(x = LETO, y = AKTIVNO.PREB.proti.CELOTNEM.PREB, color=REGIJA, shape=SPOL, size = AKTIVNO.PREB.proti.CELOTNEM.PREB)) +
  geom_point() +
  scale_x_continuous(breaks = 2005:2020)+
  labs( x = "Leto", y = "%", title = "Stopnja delovno aktivnih moških in žensk po regijah", shape = "Spol", size="Stopnja zaposlenosti", color="Regija")

g_aktivno

#############################

g_aktivno_2 = leto_regija_spol %>%   filter(leto_regija_spol$AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(leto_regija_spol$AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(SPOL != "Skupaj") %>% filter(REGIJA != "SLOVENIJA") %>%
  ggplot( mapping = aes(x = LETO, y = AKTIVNO.PREB.proti.CELOTNEM.PREB, color=REGIJA, shape=SPOL)) +
  geom_point() +
  scale_x_continuous(breaks = 2005:2020)+
  labs( x = "Leto", y = "%", title = "Stopnja delovno aktivnih moških in žensk po regijah", shape = "Spol", size="Stopnja zaposlenosti", color="Regija")+
  facet_wrap(~ REGIJA)+
  theme(
    axis.text.x = element_text(angle = 60, size=6.5, vjust = 1, hjust=1),
  )


g_aktivno_2

#############################
# slab scenarij

min_aktivne_zenske =  leto_regija_spol %>% filter(AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(SPOL != "Skupaj") %>% filter(SPOL != "Moški") %>%
  group_by(LETO) %>%
  mutate(MIN_AKTIVNE_ZENSKE = min(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(MIN_AKTIVNE_ZENSKE == AKTIVNO.PREB.proti.CELOTNEM.PREB) %>% dplyr::select(LETO,REGIJA,SPOL,MIN_AKTIVNE_ZENSKE) %>% arrange(-desc(LETO))

min_aktivni_moski =  leto_regija_spol %>% filter(AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(SPOL == "Moški") %>%
  group_by(LETO) %>%
  mutate(MIN_AKTIVNI_MOSKI = min(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(MIN_AKTIVNI_MOSKI == AKTIVNO.PREB.proti.CELOTNEM.PREB) %>% dplyr::select(LETO,REGIJA,SPOL,MIN_AKTIVNI_MOSKI) %>% arrange(-desc(LETO))



#############################
# dober scenarij

max_aktivne_zenske =  leto_regija_spol %>% filter(AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(SPOL != "Skupaj") %>% filter(SPOL != "Moški") %>%
  group_by(LETO) %>%
  mutate(MAX_AKTIVNE_ZENSKE = max(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(MAX_AKTIVNE_ZENSKE == AKTIVNO.PREB.proti.CELOTNEM.PREB) %>% dplyr::select(LETO,REGIJA,SPOL,MAX_AKTIVNE_ZENSKE) %>% arrange(-desc(LETO))

max_aktivni_moski =  leto_regija_spol %>% filter(AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(SPOL == "Moški") %>%
  group_by(LETO) %>%
  mutate(MAX_AKTIVNI_MOSKI = max(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(MAX_AKTIVNI_MOSKI == AKTIVNO.PREB.proti.CELOTNEM.PREB) %>% dplyr::select(LETO,REGIJA,SPOL,MAX_AKTIVNI_MOSKI) %>% arrange(-desc(LETO))

#############################
#############################
# ZEMLJEVIDI

zemljevid <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                             "SVN_adm1", mapa = 'zemljevid', encoding = "UTF-8")

zemljevid$NAME_1 <- c("Gorenjska", "Goriška","Jugovzhodna Slovenija", "Koroška", "Primorsko-notranjska", "Obalno-kraška", "Osrednjeslovenska", "Podravska", "Pomurska", "Savinjska", "Posavska", "Zasavska")

zemljevid <- fortify(zemljevid)   

# 1

zenske = leto_regija_spol %>% filter(AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(SPOL != "Skupaj") %>% filter(REGIJA != "SLOVENIJA") %>% filter(SPOL != "Moški") %>% dplyr::select(REGIJA,LETO,AKTIVNO.PREB.proti.CELOTNEM.PREB)

library(ggiraph)

zemljevid_zaposlenost_zensk <- ggplot() + geom_polygon_interactive(data=left_join( zenske, zemljevid, by=c("REGIJA"="NAME_1")),
                                                aes(x=long, y=lat, group=group, fill=AKTIVNO.PREB.proti.CELOTNEM.PREB)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Stopnja zaposlenosti žensk")) +
  ggtitle("Zaposlenost žensk") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "red", high = "green",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill")+
  facet_wrap(~ LETO)
  
zemljevid_zaposlenost_zensk

# 2

moski = leto_regija_spol %>% filter(AKTIVNO.PREB.proti.CELOTNEM.PREB != is.na(AKTIVNO.PREB.proti.CELOTNEM.PREB)) %>% filter(REGIJA != "SLOVENIJA") %>% filter(SPOL == "Moški") %>% dplyr::select(REGIJA,LETO,AKTIVNO.PREB.proti.CELOTNEM.PREB)


zemljevid_zaposlenost_moski <- ggplot() + geom_polygon_interactive(data=left_join( moski, zemljevid, by=c("REGIJA"="NAME_1")),
                                                                   aes(x=long, y=lat, group=group, fill=AKTIVNO.PREB.proti.CELOTNEM.PREB)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Stopnja zaposlenosti moških")) +
  ggtitle("Zaposlenost moških") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "red", high = "green",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill")+
  facet_wrap(~ LETO)

zemljevid_zaposlenost_moski

# 3

materialno_prikrajsani = leto_regija %>% filter(LETO %in% 2008:2020 ) %>% filter(REGIJA != "SLOVENIJA")  %>% dplyr::select(REGIJA,LETO,RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB)

zemljevid_materialno_prikrajsani <- ggplot() + geom_polygon(data=left_join( materialno_prikrajsani, zemljevid, by=c("REGIJA"="NAME_1")),
                                                                   aes(x=long, y=lat, group=group, fill=RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Stopnja resno materialno prikrajšanih")) +
  ggtitle("Resna materialna prikrajšanost") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "green", high = "red",
                      space = "Lab", na.value = "green", guide = "black",
                      aesthetics = "fill")+
  facet_wrap(~ LETO)

zemljevid_materialno_prikrajsani




