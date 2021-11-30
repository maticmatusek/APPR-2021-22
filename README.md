# Analiza zaposlenosti, plač in revščine v Sloveniji

## Osnovna ideja
Ob pregledu podatkov na SiStatu sem opazil kategorijo zaposlenost in podkategorijo revščina, kar je takoj vzbudilo zanimanje, kako je z revščino v Sloveniji in od česa vse je le ta odvisna. Ker nimam veliko znanja na področju revščine v Sloveniji, se mi je zdelo poučno, da bi raziskal kako na revščino vplivata plača in zaposlenost. Za lažje razumevanje dejanskega stanja bom vpliv plač in zaposlenosti na revščino pregledal na vseh regijah Slovenije.

## Opis podatkovnih virov
Vse podatke sem našel na Sistatu. Odločil sem se, da jih bom prenesel v obliki csv datotek, medtem ko so bili podatki na voljo v .px, .csv, .xlsx in .jstat oblikah. 
Povezave:
* Delovno aktivno prebivalstvo:
  https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0775355S.px/table/tableViewLayout2/
* Stopnja resne materialne prikrajšanosti:
  https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0867641S.px/table/tableViewLayout2/
* Stopnja tveganja revščine:
  https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0867636S.px/table/tableViewLayout2/
* Razpoložljiv dohodek:
  https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0867385S.px/table/tableViewLayout2/
* Prebivalstvo:
  https://pxweb.stat.si/SiStatData/pxweb/sl/Data/Data/05C2002S.px/table/tableViewLayout2/

## Zasnova podatkovnega modela
Podatki v tidy data obliki bodo izgledali tako, da bodo stolpci oziroma spremenljivke :
* leto
* spol
* regija
* število ljudi(moški, ženske,skupno)
* število delovno aktivnega prebivalstva
* število resno materialno prikrajšanih
* število ljudi, ki so pod pragom revščine
* dohodek na člana gospodinjstva
Spremenljivka spol ni kompatibilna z vsemi ostalimi podatki. Verjetno tudi nebom čisto vsega spravil v eno tabelo.

## Plan dela
Rad bi analiziral revščino v različnih regijah slovenije v odvisnosti od zaposlenosti in dohodka. Najprej bom seveda moral podatke očistiti in jih urditi v tidy data obliko. Potrebno bo dodati tudi kakšne nove stolpce, kot so razmerje zaposlenih v regiji proti vsem ljudem v regiji, razmerje med resno materialno oškodovanimi in številom ljudi v regiji. Na koncu me bo seveda zanimalo katera regija ima najslabši in najboljši procent revščine, če so kakšna zanimiva odstopanja, kot npr. visoka povprečna plača in velik procent resno materialno oškodovanih ljudi.
## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
