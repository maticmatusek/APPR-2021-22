# 4. faza: Napredna analiza podatkov

library(ggpubr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(tmap)
source("lib/libraries.r")
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(NbClust)
library(GGally)

leto_regija = read_csv("leto_regija.csv")
leto_regija_spol = read_csv("leto_regija_spol.csv")
leto_slo = read_csv("leto_slo.csv")

leto_regija$AKTIVNO.PREB.proti.CELOTNEM.PREB = leto_regija$DELOVNO.AKTIVNO.PREBIVALSTVO / leto_regija$PREBIVALSTVO
leto_regija$POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB = leto_regija$ST.OSEB.POD.PRAGOM.REVSCINE / leto_regija$PREBIVALSTVO
leto_regija$RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB = leto_regija$ST.RESNO.MATERIALNO.PRIKRAJSANIH / leto_regija$PREBIVALSTVO

leto_slo$AKTIVNO.PREB.proti.CELOTNEM.PREB = leto_slo$DELOVNO.AKTIVNO.PREBIVALSTVO / leto_slo$PREBIVALSTVO
leto_slo$POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB = leto_slo$ST.OSEB.POD.PRAGOM.REVSCINE / leto_slo$PREBIVALSTVO
leto_slo$RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB = leto_slo$ST.RESNO.MATERIALNO.PRIKRAJSANIH / leto_slo$PREBIVALSTVO
leto_slo$BREZPOSELNOST.proti.CELOTNO.PREB = leto_slo$LETNA.BREZPOSELNOST / leto_slo$PREBIVALSTVO
leto_slo$MIN.PLACA.proti.BRUTO.PLACA = leto_slo$POVP.MIN.PLACA / leto_slo$POVP.BRUTO.PLACA


############################
############################
# 2008

dva_osem = leto_regija %>% filter(LETO == 2008) %>% filter(REGIJA != "SLOVENIJA") %>% dplyr::select(REGIJA,POVP.DOHOD.NA.CLANA.GOSPODINJSTVA, POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB, AKTIVNO.PREB.proti.CELOTNEM.PREB, RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB)
imena2008 = dva_osem[,1]
dva_osem = dva_osem[,-1] %>% as.matrix()
rownames(dva_osem) = imena2008[[1]]
dva_osem = scale(dva_osem)


# Elbow method
fviz_nbclust(dva_osem, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(dva_osem, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(dva_osem, kmeans, nstart = 25,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")

# Izbral k=2 zaradi Silhuette method 

kmeans2008 <- kmeans(dva_osem, centers = 2)
cluster_2008 = fviz_cluster(kmeans2008, dva_osem, main = "2008") 

############################
############################
# 2014

ena_stiri = leto_regija %>% filter(LETO == 2014) %>% filter(REGIJA != "SLOVENIJA") %>% dplyr::select(REGIJA,POVP.DOHOD.NA.CLANA.GOSPODINJSTVA, POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB, AKTIVNO.PREB.proti.CELOTNEM.PREB, RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB)
imena2014 = ena_stiri[,1]
ena_stiri = ena_stiri[,-1] %>% as.matrix()
rownames(ena_stiri) = imena2014[[1]]
ena_stiri = scale(ena_stiri)


# Elbow method
fviz_nbclust(ena_stiri, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(ena_stiri, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(ena_stiri, kmeans, nstart = 25,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")

# Izbral k=3 zaradi Silhuette method 

kmeans2014 <- kmeans(ena_stiri, centers = 3)
cluster_2014 = fviz_cluster(kmeans2014, ena_stiri, main = "2014")

############################
############################
# 2019

ena_devet = leto_regija %>% filter(LETO == 2019) %>% filter(REGIJA != "SLOVENIJA") %>% dplyr::select(REGIJA,POVP.DOHOD.NA.CLANA.GOSPODINJSTVA, POD.PRAGOM.REVSCINE.proti.CELOTNEM.PREB, AKTIVNO.PREB.proti.CELOTNEM.PREB, RESNO.MATERIALNO.PRIKRAJSANI.proti.CELOTNO.PREB)
imena2019 = ena_devet[,1]
ena_devet = ena_devet[,-1] %>% as.matrix()
rownames(ena_devet) = imena2019[[1]]
ena_devet = scale(ena_devet)


# Elbow method
fviz_nbclust(ena_devet, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(ena_devet, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(ena_devet, kmeans, nstart = 25,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")

# Izbral k=2 zaradi Silhuette method 

kmeans2019 <- kmeans(ena_devet, centers = 2)
cluster_2019 = fviz_cluster(kmeans2019, ena_devet, main = "2019") 
cluster_2019

############################
############################
# LINEARNA REGRESIJA

ggpairs(leto_slo %>% dplyr::select(c(2,3,4,5,7,8,9)))

############################
# število oseb pod pragom , letna brezposelnost



g_napoved_1 <- ggplot(leto_slo, aes(x=ST.OSEB.POD.PRAGOM.REVSCINE, y=LETNA.BREZPOSELNOST)) + geom_point()

linearni =  lm(data=leto_slo, LETNA.BREZPOSELNOST ~ ST.OSEB.POD.PRAGOM.REVSCINE)
g_napoved_1 + geom_smooth(method="lm", formula = y ~ x )

kvadrat = lm(data=leto_slo, LETNA.BREZPOSELNOST ~ ST.OSEB.POD.PRAGOM.REVSCINE + I(ST.OSEB.POD.PRAGOM.REVSCINE^2))
g_napoved_1 + geom_smooth(method="lm", formula = y ~ x + I(x^2))

kubik = lm(data=leto_slo, LETNA.BREZPOSELNOST ~ ST.OSEB.POD.PRAGOM.REVSCINE+ I(ST.OSEB.POD.PRAGOM.REVSCINE^2)+ I(ST.OSEB.POD.PRAGOM.REVSCINE^3))
g_napoved_1 + geom_smooth(method="lm", formula = y ~ x + I(x^2) + I(x^3))

cetrti = lm(data=leto_slo, LETNA.BREZPOSELNOST ~ ST.OSEB.POD.PRAGOM.REVSCINE+ I(ST.OSEB.POD.PRAGOM.REVSCINE^2)+ I(ST.OSEB.POD.PRAGOM.REVSCINE^3)+ I(ST.OSEB.POD.PRAGOM.REVSCINE^4))
g_napoved_1 + geom_smooth(method="lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4))

peti = lm(data=leto_slo, LETNA.BREZPOSELNOST ~ ST.OSEB.POD.PRAGOM.REVSCINE + I(ST.OSEB.POD.PRAGOM.REVSCINE^2)+ I(ST.OSEB.POD.PRAGOM.REVSCINE^3)+ I(ST.OSEB.POD.PRAGOM.REVSCINE^4)+ I(ST.OSEB.POD.PRAGOM.REVSCINE^5))
g_napoved_1 + geom_smooth(method="lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))

zlepljanje = lowess(leto_slo$ST.OSEB.POD.PRAGOM.REVSCINE, leto_slo$LETNA.BREZPOSELNOST)
zlepek = loess(data=leto_slo, LETNA.BREZPOSELNOST ~ ST.OSEB.POD.PRAGOM.REVSCINE)
g_napoved_1 + geom_line(data=as.data.frame(zlepljanje), aes(x=x, y=y), color="green")

sapply(list(linearni, kvadrat, kubik, cetrti, peti, zlepek), function(x) mean((x$residuals^2)))
min(sapply(list(linearni, kvadrat, kubik, cetrti, peti, zlepek), function(x) mean((x$residuals^2))))


# Najmanjso napako ima zlepek

g_napoved_1 = g_napoved_1 + geom_line(data=as.data.frame(zlepljanje), aes(x=x, y=y), color="green")+
  labs(
    x = "Število oseb pod pragom revščine",
    title = "Slovenija",
    y = "Število brezposelnih"
  )


g_napoved_1

################################
# materialno prikrajšani, povprečna bruto plača


g_napoved_2 <- ggplot(leto_slo, aes(x=ST.RESNO.MATERIALNO.PRIKRAJSANIH, y=POVP.BRUTO.PLACA)) + geom_point()

linearni1 =  lm(data=leto_slo, POVP.BRUTO.PLACA ~ ST.RESNO.MATERIALNO.PRIKRAJSANIH)
g_napoved_2 + geom_smooth(method="lm", formula = y ~ x )

kvadrat1 = lm(data=leto_slo, POVP.BRUTO.PLACA ~ ST.RESNO.MATERIALNO.PRIKRAJSANIH + I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^2))
g_napoved_2 + geom_smooth(method="lm", formula = y ~ x + I(x^2))

kubik1 = lm(data=leto_slo, POVP.BRUTO.PLACA ~ ST.RESNO.MATERIALNO.PRIKRAJSANIH+ I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^2)+ I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^3))
g_napoved_2 + geom_smooth(method="lm", formula = y ~ x + I(x^2) + I(x^3))

cetrti1 = lm(data=leto_slo, POVP.BRUTO.PLACA ~ ST.RESNO.MATERIALNO.PRIKRAJSANIH+ I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^2)+ I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^3)+ I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^4))
g_napoved_2 + geom_smooth(method="lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4))

peti1 = lm(data=leto_slo, POVP.BRUTO.PLACA ~ ST.RESNO.MATERIALNO.PRIKRAJSANIH + I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^2)+ I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^3)+ I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^4)+ I(ST.RESNO.MATERIALNO.PRIKRAJSANIH^5))
g_napoved_2 + geom_smooth(method="lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))

zlepljanje1 = lowess(leto_slo$ST.RESNO.MATERIALNO.PRIKRAJSANIH, leto_slo$POVP.BRUTO.PLACA)
zlepek1 = loess(data=leto_slo, POVP.BRUTO.PLACA ~ ST.RESNO.MATERIALNO.PRIKRAJSANIH)
g_napoved_2 + geom_line(data=as.data.frame(zlepljanje1), aes(x=x, y=y), color="green")

sapply(list(linearni1, kvadrat1, kubik1, cetrti1, peti1, zlepek1), function(x) mean((x$residuals^2)))
min(sapply(list(linearni1, kvadrat1, kubik1, cetrti1, peti1, zlepek1), function(x) mean((x$residuals^2))))

# Najbolj se prilagaja peti1

g_napoved_2 = g_napoved_2 + geom_smooth(method="lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5)) +
  labs(
    x = "Število resno materialno prikrajšanih oseb",
    title = "Slovenija",
    y = "Povprečna bruto plača"
  )
g_napoved_2
