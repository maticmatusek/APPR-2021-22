# 3. faza: Vizualizacija podatkov

source("lib/libraries.r")

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

g_primerjava_stopenj_zaposlenosti_slovenija = leto_slo %>%
  ggplot( aes(x=LETO)) +                    
  geom_line(aes(y=BREZPOSELNOST.proti.CELOTNO.PREB , color = "Stopnja brezposelnosti"),lwd=1) +  
  geom_line(aes(y=AKTIVNO.PREB.proti.CELOTNEM.PREB, color = "Stopnja aktivnega prebivalstva"),lwd=1) +
  labs(
    x = "Leto",
    title = "Slovenija",
    y = "%"
  ) +
  scale_x_continuous(breaks=seq(2007, 2020, 1))

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
  scale_y_continuous(breaks=seq(0, 1500000, 150000))


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
  scale_y_continuous(breaks=seq(0, 2500, 100))

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
  scale_y_continuous(breaks=seq(0, 300000, 30000))

g_revscina

#######################

g_primerjava_stopenj_revscine = leto_slo %>%
  ggplot(aes(x=LETO)) +
  geom_line(aes(y= POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB , color = "Stopnja ljudi pod pragom revščine"),lwd=1) +  
  geom_line(aes(y= RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB, color = "Stopnja resno materialno prikrajšanih"),lwd=1) +
  labs(
    x = "Leto",
    title = "Slovenija",
    y = "Število ljudi"
  ) +
  scale_x_continuous(breaks=seq(2007, 2020, 1)) +
  scale_y_continuous(breaks=seq(0, 0.15, 0.015))

g_primerjava_stopenj_revscine

#########################  