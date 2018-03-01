setwd("C:/users/nathalie_cools/Documents/Heide_herstel/R_herstel")
Piloot <- read.csv2("ImportLaboResultatenv15.csv")
summary(Piloot)
str(Piloot)

# variabelen in juist formaat plaatsen:
Piloot$Year <- as.factor(Piloot$Year)
Piloot$DateSampling <- as.Date(Piloot$DateSampling)
Piloot$DateLiming <- as.Date(Piloot$DateLiming)
#Piloot$Ndays <- Piloot$DateSampling - Piloot$DateLiming
#Piloot$Ndays <- as.numeric(Piloot$Ndays)
Piloot$Pair <- as.factor(Piloot$Pair)
Piloot$SurveyPoint <- as.factor(Piloot$SurveyPoint)
Piloot$Depth.cm. <- as.factor(Piloot$Depth.cm.)
Piloot$LOIOS<- as.numeric(as.character(Piloot$LOIOS))
Piloot$PHH2O<- as.numeric(as.character(Piloot$PHH2O))
Piloot$PHCACL2<- as.numeric(as.character(Piloot$PHCACL2))
Piloot$EC<- as.numeric(as.character(Piloot$EC))
Piloot$NMIN<- as.numeric(as.character(Piloot$NMIN))
Piloot$NKJEL<- as.numeric(as.character(Piloot$NKJEL))
Piloot$NOx<- as.numeric(as.character(Piloot$NOx))
Piloot$NH4<- as.numeric(as.character(Piloot$NH4))
Piloot$TC<- as.numeric(as.character(Piloot$TC))
Piloot$TIC<- as.numeric(as.character(Piloot$TIC))
Piloot$TOC<- as.numeric(as.character(Piloot$TOC))
Piloot$POLSEN<- as.numeric(as.character(Piloot$POLSEN))
Piloot$PAUM<- as.numeric(as.character(Piloot$PAUM))
Piloot$Ca <- as.numeric(as.character(Piloot$Ca))
Piloot$K <- as.numeric(as.character(Piloot$K))
Piloot$Mg <- as.numeric(as.character(Piloot$Mg))
Piloot$Na <- as.numeric(as.character(Piloot$Na))
Piloot$CEC <- as.numeric(as.character(Piloot$CEC))
Piloot$BS <- as.numeric(as.character(Piloot$BS))
# Piloot$NHy.NOx <- as.numeric(as.character(Piloot$NHy.NOx))
Piloot$C.N <- as.numeric(as.character(Piloot$C.N))
Piloot$Al<- as.numeric(as.character(Piloot$Al))
Piloot$Fe <- as.numeric(as.character(Piloot$Fe))
Piloot$Mn <- as.numeric(as.character(Piloot$Mn))
Piloot$ACE <- as.numeric(as.character(Piloot$ACE))
Piloot$AS <- as.numeric(as.character(Piloot$AS))
Piloot$sumcations <- as.numeric(as.character(Piloot$sumcations))
Piloot$freeH <- as.numeric(as.character(Piloot$freeH))

str(Piloot)
summary(Piloot)
library(ggplot2)

ggplot(Piloot, aes(x= Ndays, y = CEC, colour = Area)) + geom_jitter()
ggplot(Piloot, aes(x= Ndays, y = PHCACL2, colour = Area)) + geom_jitter()
ggplot(Piloot, aes(x= Ndays, y = PHCACL2, colour = Limed)) + geom_jitter()
ggplot(Piloot, aes(x= PHH2O, y = NH4, colour = Ndays)) + geom_jitter()
ggplot(Piloot, aes(x= PHH2O, y = NH4, colour = Limed)) + geom_jitter()
ggplot(Piloot, aes(x= PHCACL2, y = NH4, colour = Area)) + geom_jitter()
ggplot(Piloot, aes(x= PHCACL2, y = NHy.NOx, colour = Area)) + geom_jitter()
ggplot(Piloot, aes(x= CEC, y = NH4, colour = Area)) + geom_jitter() + facet_wrap (~Area, scales = "free_y")
ggplot(Piloot, aes(x= Ndays, y = C.N, colour = Area)) + geom_jitter() + facet_wrap (~Area, scales = "free_y")
ggplot(Piloot, aes(x= CEC, y = Al, colour = Area)) + geom_jitter() + facet_wrap (~Area, scales = "free_y")
ggplot(Piloot, aes(x= BS, y = Fe, colour = Area)) + geom_jitter() + facet_wrap (~Area, scales = "free_y")
# hier zie je niet veel verband, ik zou dit eens willen doen met betere CEC bepaling.

library(INBOtheme)
# grafiekjes maken per Area



##################
# Zandvoordebos ##
##################
Zandvoordebos<- subset(Piloot, Area == "Zandvoordebos")

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = PHH2O, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = PHCACL2, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = CEC, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("CEC (cmol(+).kg-1)")+ scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = K, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("K (cmol(+).kg-1)")+ scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = Ca, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Ca (cmol(+).kg-1)")+ scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = Mg, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Mg (cmol(+).kg-1)")+ scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = Na, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Na (cmol(+).kg-1)")+ scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = BS, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + scale_y_continuous("Basenverzadiging (%)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = LOIOS, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Organische stof (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = TC, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("totale koolstof (g.kg-1)") +  scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = NH4, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("ammoniakale-N (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = NOx, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("NOx-N (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = NKJEL, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Kjeldahl-N (g.kg-1)")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = C.N, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + scale_y_continuous("C/N verhouding")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = POLSEN, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + scale_y_continuous("P Olsen (mg.kg-1)")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

p <- ggplot(Zandvoordebos, aes(x= Ndays, y = PAUM, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + scale_y_continuous("P AUM (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

# CEC was wel vergelijkbaar in het begin en daar zien we geen evolutie over tijd.
# voor NH4 was er op beide plots en op alle diepte een NH4 piek rond een 800 dagen na de bekalking.
# Dit was in het najaar in 2014 en oorzaak is te zoeken in omzetting tijdens het bewaren van de stalen op het INBO
# stalen van november waren pas in april gemeten


p <- ggplot(Zandvoordebos, aes(x= Ndays, y = EC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + scale_y_continuous("Electrische geleidbaarheid (µs/cm)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

###################
# Haverven        #
###################

Haverven<- subset(Piloot, Area == "Haverven")

p <- ggplot(Haverven, aes(x= Ndays, y = PHH2O, colour = Limed, shape = Depth), size = 5) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")   + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = PHCACL2, colour = Limed, shape = Depth), size = 5) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = K, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("K (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = CEC, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("CEC (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = Mg, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Mg (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = Mg, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Mg (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = Ca, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Ca (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = BS, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Basenverzadiding (%)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

# de initi?le pH(H2O) lag op de verschillende plots al wel wat uit elkaar en was eigenlijk ook al
# redelijk neutraal. De bekalking heeft een effect gehad op de bovenste 5 cm, en er is een vertraging
# in de 5 - 10 cm laag maar uiteindelijk ziet het ernaar uit dat het terug naar hetzelfde niveau
# gaat evolueren.En hetzelfde voor pH(CaCl2)

# Minerale stikstof kent een piek in november 2014, hier 750 dagen na de bekalking op het moment
# dat de vegetatiegroei is stilgevallen en de bodem waterverzadigd begint te geraken. Maar een jaar
# later wordt datzelfde hoge NMIN niveau echter niet gehaald. Een jaar vroeger, zijn de stalen nog
# wel in het groeiseizoen (september) genomen.

p <- ggplot(Haverven, aes(x= Ndays, y = TC, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Totale koolstof (g.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = LOIOS, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Organische stof (g.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = NKJEL, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Kjeldahl N (g.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = NMIN, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Minerale N (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = NH4, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Ammoniakale N (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = NOx, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("NOx-N (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = C.N, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("C/N verhouding") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

# Kjeldahl N en NOx-N opvolgen heeft geen zin want dit zit meestal onder de LOQ

p <- ggplot(Haverven, aes(x= Ndays, y = POLSEN, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("P Olsen (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Haverven, aes(x= Ndays, y = PAUM, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("P AUM (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")


p <- ggplot(Haverven, aes(x= Ndays, y = EC, colour = Limed, shape = Depth), size = 5) + geom_point(size = 2) +geom_line()
p + scale_y_continuous("Electrische geleidbaarheid (?s/cm)") + facet_wrap (~Pair, scales = "free_y") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking") 

##############
# Heiberg   ##
##############

Heiberg<- subset(Piloot, Area == "Heiberg")

summary(Heiberg)

p <- ggplot(Heiberg, aes(x= Ndays, y = PHH2O, colour = Limed, shape = Depth), size = 5) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")   + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = PHCACL2, colour = Limed, shape = Depth), size = 5) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = K, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("K (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = CEC, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("CEC (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = Mg, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Mg (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = Ca, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Ca (cmol(+).kg-1 soil)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = BS, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Basenverzadiding (%)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = TC, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Totale koolstof (g.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = LOIOS, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Organische stof (g.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = NKJEL, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Kjeldahl N (g.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = NMIN, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Minerale N (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = NH4, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("Ammoniakale N (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = NOx, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("NOx-N (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = C.N, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("C/N verhouding") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = POLSEN, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("P Olsen (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Heiberg, aes(x= Ndays, y = PAUM, colour = Limed, shape = Depth), size = 5) + geom_point() +geom_line()
p + scale_y_continuous("P AUM (mg.kg-1)") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")


# je ziet een sterke seizonaliteit in de pHH2O en sterker in de bekalkte plots
# dat is minder aanwezig bij de pH(CaCl2) maar je ziet toch nog fluctuaties en meer in de wel dan in de 
# niet bekalkte plots; 
# toch nog eens nagaan of in koppels 43 na 500 dagen de twee dieptes in het bekalkte plotje niet gewisseld zijn
# best bakje eens bekijken, koppel E bovenaan de helling mogelijk wel wat afspoeling & erosie
# en bij koppel 39 in de  0 - 5 cm laag tussen de twee plotjes

# ammoniumpiek was te zien eind november 2014 maar een jaar later was die er niet; blijkt artefact in labo
# er zit precies ook een seizonaliteit in het plant beschikbaar P gehalte
# niet dat het iets met bekalken zou te maken hebben maar in verschillende koppels gaat het OS gehalte achteruit

p <- ggplot(Heiberg, aes(x= Ndays, y = EC, colour = Limed, shape = Depth)) + geom_point() + geom_line()
p + facet_wrap (~Pair, scales = "free_y")  

# ggsave(p, filename = "Heiberg.jpg")

##############
# Tielenkamp #
##############

Tielenkamp<- subset(Piloot, Area == "Tielenkamp")

p <- ggplot(Tielenkamp, aes(x= Ndays, y = PHH2O, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("pH-H2O") + facet_wrap (~Pair) + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = PHCACL2, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("pH-CaCl2")+ facet_wrap (~Pair) + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")    

p <- ggplot(Tielenkamp, aes(x= Ndays, y = CEC, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("CEC (cmol(+).kg-1) ") + facet_wrap (~Pair)  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = Ca, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Ca (cmol(+).kg-1) ") + facet_wrap (~Pair) + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = K, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Ka (cmol(+).kg-1) ") + facet_wrap (~Pair) + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = Mg, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Mg (cmol(+).kg-1) ") + facet_wrap (~Pair, scales = "free_y") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = BS, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Basenverzadiging (%) ") + facet_wrap (~Pair, scales = "free_y") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = EC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  
p + scale_y_continuous("Electrische geleidbaarheid (?s/cm) ") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = NMIN, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Minerale stikstof mg.kg-1 ") + facet_wrap (~Pair, scales ="free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = NH4, colour = Limed, shape = Depth)) + geom_point(size = 2)  + geom_line()
p + scale_y_continuous("Ammoniakale-N mg.kg-1 ") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = NOx, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Geoxideerde stikstof mg.kg-1 ") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = LOIOS, colour = Limed, shape = Depth)) + geom_point(size = 2) 
p + scale_y_continuous("Organische stof g.kg-1 ") + facet_wrap (~Pair, scales = "free_y")  + geom_line() + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = TC, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + scale_y_continuous("Totale koolstof mg.kg-1 ") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

#  p <- ggplot(Tielenkamp, aes(x= Ndays, y = TOC, colour = Limed, shape = Depth)) + geom_point(size = 2) 
# p + scale_y_continuous("Totale organische koolstof g.kg-1 ") + facet_wrap (~Pair, scales = "free_y")  + geom_smooth()

p <- ggplot(Tielenkamp, aes(x= Ndays, y = TOC, colour = Limed, shape = Depth)) + geom_point(size = 2) 
p + scale_y_continuous("Totale organische koolstof g.kg-1 ") + facet_wrap (~Pair, scales = "free_y")  + geom_line()

p <- ggplot(Tielenkamp, aes(x= Ndays, y = TIC, colour = Limed, shape = Depth)) + geom_point(size = 2) 
p + scale_y_continuous("Totale inorganische koolstof g.kg-1 ") + facet_wrap (~Pair)  + geom_line() + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = NKJEL, colour = Limed, shape = Depth)) + geom_point(size = 2) 
p + scale_y_continuous("Kjeldahl N g.kg-1 ") + facet_wrap (~Pair, scale = "free_y")  + geom_line() + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = POLSEN, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + scale_y_continuous("P Olsen mg.kg-1 ") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = PAUM, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + scale_y_continuous("P AUM mg.kg-1 ") + facet_wrap (~Pair, scales = "free_y")  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  

p <- ggplot(Tielenkamp, aes(x= Ndays, y = C.N, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()  + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")  
p + scale_y_continuous("C/N verhouding ") + facet_wrap (~Pair, scales = "free_y") 

# ggsave(p, filename = "Tielenkamp.jpg")


##################
# Aanwijsputten  #
##################

Aanwijsputten<- subset(Piloot, Area == "Aanwijsputten")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = PHCACL2, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") +scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = PHH2O, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") +scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = CEC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("CEC (cmol(+).kg-1)") +scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = Ca, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Ca (cmol(+).kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = K, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("K (cmol(+).kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = Mg, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Mg (cmol(+).kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = BS, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Base Saturation (%)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = EC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")  + scale_y_continuous("Electrische conductiviteit (?s/cm)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = LOIOS, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Organische stof (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = TOC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("TOC (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = TIC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("TIC (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = TC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Totale koolstof (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = NH4, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("NH4-N (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = NOx, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("NOx-N (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = NMIN, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Mineral-N (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = NKJEL, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Kjeldahl N (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = C.N, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("C:N ratio") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = POLSEN, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")  + scale_y_continuous("P Olsen (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Aanwijsputten, aes(x= Ndays, y = PAUM, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")  + scale_y_continuous("P AUM (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")


##############
# Averbode  ##
##############
Averbode<- subset(Piloot, Area == "Averbode")

p <- ggplot(Averbode, aes(x= Ndays, y = PHCACL2, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") +scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = PHH2O, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") +scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = CEC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("CEC (cmol(+).kg-1)") +scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = Ca, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Ca (cmol(+).kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = K, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("K (cmol(+).kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = Mg, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Mg (cmol(+).kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = BS, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Base Saturation (%)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = EC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")  + scale_y_continuous("Electrische conductiviteit (?s/cm)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = LOIOS, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Organische stof (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = TOC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("TOC (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = TIC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("TIC (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = TC, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Totale koolstof (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = NH4, colour = Limed, shape = Depth.cm.)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("NH4-N (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = NOx, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("NOx-N (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = NMIN, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Mineral-N (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = NKJEL, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("Kjeldahl N (g.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = C.N, colour = Limed, shape = Depth)) + geom_point(size = 2) + geom_line()
p + facet_wrap (~Pair, scales = "free_y") + scale_y_continuous("C:N ratio") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = POLSEN, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")  + scale_y_continuous("P Olsen (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")

p <- ggplot(Averbode, aes(x= Ndays, y = PAUM, colour = Limed, shape = Depth)) + geom_point(size = 2) +geom_line()
p + facet_wrap (~Pair, scales = "free_y")  + scale_y_continuous("P AUM (mg.kg-1)") + scale_colour_discrete ("Bekalkt") + scale_shape_discrete("Diepte (cm)") + scale_x_continuous("Aantal dagen na bekalking")








############# tests op trends (??n aanhoudende richting) in pH ##########

############################
# ZANDVOORDEBOS koppel 17  #
############################

# cijfers in de trend zijn na de bekalking #

install.packages("Kendall")
library(Kendall)
x <- c(4, 10, 11, 5, 8, 6.9)
MannKendall(x)

# pH(H2O)reeks 0 - 5 cm bekalkt  #
pHH2OtrueM05 <- c(4.13, 4.98, 4.64, 4.87, 5.09, 5.62, 5.17, 5.64, 5.13, 5.35)
MannKendall(pHH2OtrueM05)
# tau = 0.644, 2-sided pvalue =0.012266

# pH(H2O) reeks 5 - 10 cm bekalkt  #
pHH2OtrueM51 <- c(4.13,4.18, 4.24, 4.26, 4.54, 5.16, 4.44, 4.74, 4.81, 4.55)
MannKendall(pHH2OtrueM51)
# tau = 0.689, 2-sided pvalue =0.0072904

# pH(H2O) reeks 0 - 5 cm niet bekalkt  #
pHH2OfalseM05 <- c(4.06, 4.09, 4.12, 4.16, 4.25, 4.66, 4.29, 4.31, 4.31, 4.35)
MannKendall(pHH2OfalseM05)
# tau = 0.809, 2-sided pvalue =0.0016717

# pH(H2O)reeks 5 - 10 cm niet bekalkt  #
pHH2OfalseM51 <- c(4.13, 4.13, 4.12, 4.15, 4.28, 4.83, 4.35, 4.29, 4.3, 4.35)
MannKendall(pHH2OfalseM51)
# tau = 0.614, 2-sided pvalue =0.019061

# pH(CaCl2) reeks 0 - 5 cm bekalkt  #
pHCaCl2trueM05 <- c(3.75, 4.00, 3.94, 4.13, 4.26, 4.30, 4.23, 4.76, 4.22, 4.46)
MannKendall(pHCaCl2trueM05)
# tau = 0.644, 2-sided pvalue =0.012266

# pH(CaCl2) reeks 5 - 10 cm bekalkt  #
pHCaCl2trueM51 <- c(3.58, 3.79, 3.83, 3.83, 3.90, 3.99, 3.78, 4.12, 3.95, 3.94)
MannKendall(pHCaCl2trueM51)
# tau = 0.539, 2-sided pvalue =0.038879

# pH(CaCl2) reeks 0 - 5 cm niet bekalkt  #
pHCaCl2falseM05 <- c(3.6, 3.59, 3.63, 3.61,3.64, 3.55, 3.67, 3.67, 3.7)
MannKendall(pHCaCl2falseM05)
# tau = 0.592, 2-sided pvalue =0.036032

# pH(CaCl2) reeks 5 - 10 cm niet bekalkt  #
pHCaCl2falseM51 <- c(3.72, 3.66, 3.66, 3.72, 3.68, 3.69, 3.63, 3.74, 3.03,3.78)
MannKendall(pHCaCl2falseM51)
# tau = 0.0682, 2-sided pvalue =0.85689


#####################################
# trends in uitwisselbare kationen  #
#####################################

# CEC reeks 0 - 5 cm niet bekalkt  #
CECgeen05cm <- c(4.18, 10.1, 4.5, 8.2, 7.0, 4.0, 7.0, 5.0, 6.0, 6.1)
MannKendall(CECgeen05cm)

# CEC reeks 5 - 10 cm niet bekalkt  #
CECgeen510cm <- c(3.99, 9.3, 3.8, 7.0, 6.3, 5.0, 7.0, 5.0, 5.0, 5.6)
MannKendall(CECgeen510cm)

# CEC reeks 0 - 5 cm wel bekalkt  #
CECwel05cm <- c(4.81, 10.4, 3.8, 5.9, 4.6, 15.0, 6.0, 7.0, 5.0, 5.1)
MannKendall(CECwel05cm)

# CEC reeks 5 - 10 cm wel bekalkt Zandvoordebos #
CEC510cm <- c(7.22, 12.9, 3.5, 3.9, 4.6, 4.0, 5.0, 3.0, 5.0, 4.5)
MannKendall(CEC510cm)


# individuele kationen Ca, K en Mg vaak onder LOQ: even werken met de LOQ waarden? 
# als er dan zo een dalende trend in zit, dat zal dit ook wel echt zo zijn

# Ca reeks 0 - 5 cm niet bekalkt  #
Cageen05cm <- c(0.7, 0.84, 0.32, 0.33, 0.35, 0.30, 0.41, 0.59, 0.37, 0.43)
MannKendall(Cageen05cm)

# Ca reeks 5 - 10 cm niet bekalkt  #
Cageen510cm <- c(0.56, 0.64, 0.2, 0.2, 0.28, 0.35, 0.2, 0.82, 1.09, 0.26)
MannKendall(Cageen510cm)

# Ca reeks 0 - 5 cm wel bekalkt  #
Cawel05cm <- c(0.62, 2.25, 2.01, 2.21, 1.46, 1.59, 1.59, 3.22, 1.51, 2.27)
MannKendall(Cawel05cm)

# Ca reeks 5 - 10 cm wel bekalkt Zandvoordebos #
Ca510cm <- c(0.56, 0.64, 0.2, 0.2, 0.28, 0.35, 0.2, 0.82,1.09, 0.26)
MannKendall(Ca510cm)

# Mg reeks 0 - 5 cm niet bekalkt  #
Mggeen05cm <- c(0.05, 0.06, 0.07, 0.05, 0.17, 0.14, 0.28, 0.29, 0.16, 0.25)
MannKendall(Mggeen05cm)

# Mg reeks 5 - 10 cm niet bekalkt  #
Mggeen510cm <- c(0.05, 0.05, 0.05, 0.05, 0.12, 0.09, 0.15, 0.14, 0,10, 0.16)
MannKendall(Mggeen510cm)

# Mg reeks 0 - 5 cm wel bekalkt  #
Mgwel05cm <- c(0.05, 0.48, 0.32, 0.57, 1.18, 1.35, 1.27, 2.51, 1.11, 1.78)
MannKendall(Mgwel05cm)

# Mg reeks 5 - 10 cm wel bekalkt  #
Mg510cm <- c(0.05, 0.05, 0.05, 0.05, 0.25, 0.28, 0.15, 0.72, 0.92, 0.26)
MannKendall(Mg510cm)

# K reeks 0 - 5 cm niet bekalkt  #
Kgeen05cm <- c(0.39, 0.59, 0.64,0.44, 0.2,0.2, 0.2, 0.25, 0.2, 0.18)
MannKendall(Kgeen05cm)

# K reeks 5 - 10 cm niet bekalkt  #
Kgeen510cm <- c(0.35, 0.57, 0.56, 0.35, 0.2, 0.2, 0.2, 0.2, 0.17, 0.13)
MannKendall(Kgeen510cm)

# K reeks 0 - 5 cm wel bekalkt  #
Kwel05cm <- c(0.34, 0.58, 0.54, 0.47, 0.2, 0.2, 0.2, 0.2, 0.14, 0.15)
MannKendall(Kwel05cm)

# K reeks 5 - 10 cm wel bekalkt  #
K510cm <- c(0.26, 0.56, 0.40, 0.32, 0.2, 0.2, 0.2, 0.2, 0.11, 0.1)
MannKendall(K510cm)



############# tests op trends ##########

####################################
# stat sign trends in bodem pH    #
####################################

###################
# AANWIJSPUTTEN   #
###################



install.packages("Kendall")
library(Kendall)
x <- c(4, 10, 11, 5, 8, 6.9)
MannKendall(x)
?MannKendall

###############
# pH(H2O)     #
###############

#############
# koppel 60 #
#############

# pH(H2O) M05 bekalkt   #
pHH2OtrueM05 <- c(4.02, 4.99, 4.51,4.08,5.02,5.23,4.16)
MannKendall(pHH2OtrueM05)
# tau = 0.333, 2-sided pvalue =0.36752

# pH(H2O) M05 niet bekalkt   #
pHH2OfalseM05 <- c(4.05, 4.16, 3.96, 4.56, 4.2, 4.39, 4.8)
MannKendall(pHH2OfalseM05)
# tau = 0.619, 2-sided pvalue =0.071505

# pH(H2O) M51 bekalkt #
pHH2OtrueM51 <- c(4.10, 4.25, 4.22, 4.15, 4.4, 4.56, 4.29)
MannKendall(pHH2OtrueM51)
# tau = 0.524, 2-sided pvalue =0.13313

# pH(H2O) M51 niet bekalkt #
pHH2OfalseM51 <- c(4.10, 4.32, 4.07, 4.26, 4.2, 4.33, 4.43)
MannKendall(pHH2OfalseM51)
# tau = 0.524, 2-sided pvalue =0.13313

#############
# koppel 61 #
#############

pHH2OtrueM05 <- c(4.17, 4.18, 4.08, 4.18, 4.24, 4.17, 4.2)
MannKendall(pHH2OtrueM05)
# tau = 0.35, 2-sided pvalue =0.35644

pHH2OfalseM05 <- c(4.29, 4.47, 4.27, 4.17, 4.31, 4.29)
MannKendall(pHH2OfalseM05)
# tau = -0.138, 2-sided pvalue =0.84831

pHH2OtrueM51 <- c(4.18, 4.20, 4.08, 4.16, 4.25, 4.34, 4.22)
MannKendall(pHH2OtrueM51)
# tau = 0.429, 2-sided pvalue =0.22956

pHH2OfalseM51 <- c(4.32, 4.50, 4.30, 4.27, 4.38, 4.31, 4.51)
MannKendall(pHH2OfalseM51)
# tau = 0.143, 2-sided pvalue =0.76389

#############
# koppel 62 #
#############

pHH2OtrueM05 <- c(4.42, 4.76, 4.66, 4.50, 4.56, 5.34, 4.48)
MannKendall(pHH2OtrueM05)
# tau = 0.0476, 2-sided pvalue =1

pHH2OfalseM05 <- c(4.48, 4.41, 4.39, 4.45, 4.5, 4.47, 4.48)
MannKendall(pHH2OfalseM05)
# tau = 0.293, 2-sided pvalue =0.44752

pHH2OtrueM51 <- c(4.39, 4.89, 5.08, 4.56, 4.66, 4.68, 4.61)
MannKendall(pHH2OtrueM51)
# tau = 0.0476, 2-sided pvalue =1

pHH2OfalseM51 <- c(4.49, 4.54, 4.48, 4.53, 4.53, 4.46, 4.37)
MannKendall(pHH2OfalseM51)
# tau = -0.488, 2-sided pvalue =0.17156

# dus geen stat sign trends in pH(H2O) buiten koppel 60 M05 niet bekalkt p = 0.07

###############
# pH(CaCl2)   #
###############

#############
# koppel 60 #
#############

pHCaCl2trueM05 <- c(3.30, 3.71, 3.57, 3.34, 3.89, 4.3, 3.35)
MannKendall(pHCaCl2trueM05)
# tau = 0.333, 2-sided pvalue =0.36752

pHCaCl2falseM05 <- c(3.27, 3.27, 3.20, 3.71, 3.32, 3.55, 3.84)
MannKendall(pHCaCl2falseM05)
# tau = 0.586, 2-sided pvalue =0.094718

pHCaCl2trueM51 <- c(3.43, 3.51, 3.55, 3.55, 3.79, 3.77, 3.63)
MannKendall(pHCaCl2trueM51)
# tau = 0.683, 2-sided pvalue =0.048286

pHCaCl2falseM51 <- c(3.46, 3.48, 4.07, 4.26, 4.2, 4.33, 4.43)
MannKendall(pHCaCl2falseM51)
# tau = 0.905, 2-sided pvalue =0.0068638

#############
# koppel 61 #
#############

pHCaCl2trueM05 <- c(3.42, 3.44, 3.36, 3.50, 3.49, 3.46, 3.49)
MannKendall(pHCaCl2trueM05)
# tau = 0.39, 2-sided pvalue =0.28761

pHCaCl2falseM05 <- c(3.55, 3.60, 3.54, 3.53, 3.79, 3.56, 3.59)
MannKendall(pHCaCl2falseM05)
# tau = 0.143, 2-sided pvalue =0.76389

pHCaCl2trueM51 <- c(3.49, 3.50, 3.43, 3.56, 3.61, 3.59, 3.57)
MannKendall(pHCaCl2trueM51)
# tau = 0.524, 2-sided pvalue =0.13313

pHCaCl2falseM51 <- c(3.64, 3.68, 3.62, 3.68, 3.8, 3.68, 3.96)
MannKendall(pHCaCl2falseM51)
# tau = 0.617, 2-sided pvalue =0.084539


#############
# koppel 62 #
#############

pHCaCl2trueM05 <- c(3.62, 3.72, 3.74, 3.78, 3.75, 4.29, 3.71)
MannKendall(pHCaCl2trueM05)
# tau = 0.429, 2-sided pvalue =0.22956

pHCaCl2falseM05 <- c(3.62, 3.57, 3.61, 3.66, 3.72, 3.68, 3.8)
MannKendall(pHCaCl2falseM05)
# tau = 0.714, 2-sided pvalue =0.035498

pHCaCl2trueM51 <- c(3.79, 3.89, 4.10, 3.97, 3.99, 4.03, 3.96)
MannKendall(pHCaCl2trueM51)
# tau = 0.333, 2-sided pvalue =0.36752

pHCaCl2falseM51 <- c(3.73, 3.69, 3.77, 4.04, 3.84, 3.74, 3.75)
MannKendall(pHCaCl2falseM51)
# tau = 0.238, 2-sided pvalue =0.54801

############################
# Tielenkamp               #
############################

#############
# koppel 52 #
#############
pHH2OtrueM05 <- c(6.53, 5.90, 5.68, 5.53, 4.84, 5.17, 5.09)
MannKendall(pHH2OtrueM05)
# tau = -0.81, 2-sided pvalue =0.016261

pHH2OtrueM51 <- c(5.86, 4.87, 5.36, 4.79, 4.52, 4.83, 4.72)
MannKendall(pHH2OtrueM51)
# tau = -0.619, 2-sided pvalue =0.071505

pHH2OfalseM05 <- c(4.40, 4.49, 5.42, 4.45, 4.63, 4.64, 4.32)
MannKendall(pHH2OfalseM05)
# tau = -0.619, 2-sided pvalue =0.071505

pHH2OfalseM51 <- c(4.23, 4.37, 5.06, 4.35, 4.52, 4.59, 4.4)
MannKendall(pHH2OfalseM51)
# tau = 0.333, 2-sided pvalue =0.36752

pHCaCl2trueM05 <- c(5.13, 4.73, 4.22, 4.41, 4.08, 4.31, 4.18)
MannKendall(pHCaCl2trueM05)
# tau = -0.619, 2-sided pvalue =0.071505

pHCaCl2trueM51 <- c(4.66, 3.88, 3.98, 4.11, 4.04, 4.13, 4.26)
MannKendall(pHCaCl2trueM51)
# tau = 0.333, 2-sided pvalue =0.36752

pHCaCl2falseM05 <- c(3.54, 3.51, 3.69, 3.71, 3.81, 3.96, 3.37)
MannKendall(pHCaCl2falseM05)
# tau = 0.333, 2-sided pvalue =0.36752

pHCaCl2falseM51 <- c(3.63, 3.49, 3.62, 3.60, 3.87, 4.03, 3.58)
MannKendall(pHCaCl2falseM51)
# tau = 0.143, 2-sided pvalue =0.76389


#############
# koppel 57 #
#############
pHH2OtrueM05 <- c(5.98, 4.97, 6.48, 5.14, 5.22, 5.18, 4.6)
MannKendall(pHH2OtrueM05)
# tau = -0.333, 2-sided pvalue =0.3675

pHH2OtrueM51 <- c(4.20, 4.25, 5.41, 4.52, 4.73, 4.8, 4.59)
MannKendall(pHH2OtrueM51)
# tau = 0.429, 2-sided pvalue =0.22956

pHH2OfalseM05 <- c(4.38, 4.56, 5.59, 4.53, 4.78, 4.67, 4.61)
MannKendall(pHH2OfalseM05)
# tau = 0.238, 2-sided pvalue =0.54801

pHH2OfalseM51 <- c(4.31, 4.43, 5.14, 4.48, 4.66, 4.59, 4.55)
MannKendall(pHH2OfalseM51)
# tau = 0.333, 2-sided pvalue =0.36752

pHCaCl2trueM05 <- c(4.95, 4.03, 4.91, 4.06, 4.34, 4.28, 3.7)
MannKendall(pHCaCl2trueM05)
# tau = -0.429, 2-sided pvalue =0.22956

pHCaCl2trueM51 <- c(3.61, 3.67, 3.99, 3.95, 4.17, 4.23, 3.98)
MannKendall(pHCaCl2trueM51)
# tau = 0.619, 2-sided pvalue =0.071505

pHCaCl2falseM05 <- c(3.80, 3.70, 3.94, 3.85, 3.90, 3.94, 3.76)
MannKendall(pHCaCl2falseM05)
# tau = 0.195, 2-sided pvalue =0.64858

pHCaCl2falseM51 <- c(3.76, 3.70, 3.84, 3.90, 4.11, 4.15, 3.85)
MannKendall(pHCaCl2falseM51)
# tau = 0.619, 2-sided pvalue =0.071505

#############
# koppel 58 #
#############
pHH2OtrueM05 <- c(4.06, 5.03, 5.18, 5.50, 4.93, 4.96, 4.38, 5.08)
MannKendall(pHH2OtrueM05)
# tau = 0.0714, 2-sided pvalue =0.90154

pHH2OtrueM51 <- c(4.14, 4.26, 4.37, 4.82, 4.34, 4.55, 4.42, 4.59)
MannKendall(pHH2OtrueM51)
# tau = 0.571, 2-sided pvalue =0.063487

pHH2OfalseM05 <- c(4.05, 4.23, 4.34, 4.90, 4.32, 4.27, 4.3, 4.21)
MannKendall(pHH2OfalseM05)
# tau = 0, 2-sided pvalue =1

pHH2OfalseM51 <- c(4.16, 4.28, 4.45, 5.10, 4.41, 4.50, 4.34, 4.53)
MannKendall(pHH2OfalseM51)
# tau = 0.333, 2-sided pvalue =0.36752

pHCaCl2trueM05 <- c(3.13, 4.24, 4.09, 4.20, 3.86, 3.99, 3.88, 4.1)
MannKendall(pHCaCl2trueM05)
# tau = 0, 2-sided pvalue =1

pHCaCl2trueM51 <- c(3.57, 3.63, 3.63, 3.77, 3.61, 4.00, 3.77, 3.97)
MannKendall(pHCaCl2trueM51)
# tau = 0.593, 2-sided pvalue =0.059451

pHCaCl2falseM05 <- c(3.16, 3.19, 3.15, 3.30, 3.22, 3.49, 3.25, 3.49)
MannKendall(pHCaCl2falseM05)
# tau = 0.618, 2-sided pvalue =0.046063

pHCaCl2falseM51 <- c(3.67, 3.64, 3.71, 3.81, 3.71, 3.81, 3.75, 3.96)
MannKendall(pHCaCl2falseM51)
# tau = 0.667, 2-sided pvalue =0.032667

#############
# koppel 59 #
#############

pHH2OtrueM05 <- c(4.16, 4.87, 5.19, 5.08, 5.41, 5.27, 5.24, 5.29)
MannKendall(pHH2OtrueM05)
# tau = 0.643, 2-sided pvalue =0.035448

pHH2OtrueM51 <- c(4.24, 4.29, 4.53, 4.85, 4.44, 4.66, 4.61, 4.75)
MannKendall(pHH2OtrueM51)
# tau = 0.571, 2-sided pvalue =0.063487

pHH2OfalseM05 <- c(4.27, 4.30, 4.70, 5.32, 4.25, 4.57, 3.83, 4.39)
MannKendall(pHH2OfalseM05)
# tau = -0.0714, 2-sided pvalue =0.90154

pHH2OfalseM51 <- c(4.29, 4.36, 4.64, 5.05, 4.51, 4.61, 4.48, 4.52)
MannKendall(pHH2OfalseM51)
# tau = 0.214, 2-sided pvalue =0.53619

pHCaCl2trueM05 <- c(3.19, 3.96, 4.13, 3.58, 4.27, 4.25, 4.16, 4.29)
MannKendall(pHCaCl2trueM05)
# tau = 0.643, 2-sided pvalue =0.035448

pHCaCl2trueM51 <- c(3.63, 3.71, 3.76, 3.70, 3.70, 3.87, 3.94, 4.00)
MannKendall(pHCaCl2trueM51)
# tau = 0.691, 2-sided pvalue =0.024822

pHCaCl2falseM05 <- c(3.28, 3.35, 3.46, 4.02, 3.17, 3.41, 3.22, 3.49)
MannKendall(pHCaCl2falseM05)
# tau = 0.143, 2-sided pvalue =0.71052

pHCaCl2falseM51 <- c(3.64, 3.64, 3.75, 3.76, 3.68, 3.84, 4.28, 3.95)
MannKendall(pHCaCl2falseM51)
# tau = 0.764, 2-sided pvalue =0.012649
#############
# koppel 63 #
#############

pHH2OtrueM05 <- c(4.08, 4.70, 4.37, 4.66, 4.61, 4.33)
MannKendall(pHH2OtrueM05)
# tau = -0.0667, 2-sided pvalue =1

pHH2OtrueM51 <- c(4.14, 3.80, 4.02, 3.96, 3.88, 3.98)
MannKendall(pHH2OtrueM51)
# tau = -0.2, 2-sided pvalue =0.70711

pHH2OfalseM05 <- c(3.88, 4.29, 4.33, 4.3, 4.32, 4.32)
MannKendall(pHH2OfalseM05)
# tau = 0.552, 2-sided pvalue =0.1806

pHH2OfalseM51 <- c(3.93, 3.98, 3.97, 4.03, 4.03, 4.11)
MannKendall(pHH2OfalseM51)
# tau = 0.828, 2-sided pvalue =0.035378

pHCaCl2trueM05 <- c(3.40, 3.60, 3.66, 3.75, 3.77, 3.67)
MannKendall(pHCaCl2trueM05)
# tau = 0.733, 2-sided pvalue =0.060289

pHCaCl2trueM51 <- c(3.28, 3.21, 3.47, 3.39, 3.36, 3.36)
MannKendall(pHCaCl2trueM51)
# tau = 0.138, 2-sided pvalue =0.84831

pHCaCl2falseM05 <- c(3.27, 3.46, 3.54, 3.48, 3.55, 3.55)
MannKendall(pHCaCl2falseM05)
# tau = 0.828, 2-sided pvalue =0.035378

pHCaCl2falseM51 <- c(3.21, 3.35, 3.43, 3.42, 3.45, 3.49)
MannKendall(pHCaCl2falseM51)
# tau = 0.867, 2-sided pvalue =0.024171


#############
# koppel 64 #
#############

pHH2OtrueM05 <- c(4.02, 4.79, 4.81, 4.95, 4.59, 4.66)
MannKendall(pHH2OtrueM05)
# tau = 0.2, 2-sided pvalue =0.70711

pHH2OtrueM51 <- c(4.07, 3.93, 3.96, 4.25, 4.21, 4.11)
MannKendall(pHH2OtrueM51)
# tau = 0.333, 2-sided pvalue =0.45237

pHH2OfalseM05 <- c(4.26, 3.96, 4.02, 4.08, 4.01, 4.13)
MannKendall(pHH2OfalseM05)
# tau = 0.0667, 2-sided pvalue =1

pHH2OfalseM51 <- c(4.28, 3.95, 3.97, 4.00, 3.92, 4.13)
MannKendall(pHH2OfalseM51)
# tau = -0.0667, 2-sided pvalue =1

pHCaCl2trueM05 <- c(3.22, 3.75, 3.87, 3.83, 3.67, 3.71)
MannKendall(pHCaCl2trueM05)
# tau = 0.0667, 2-sided pvalue =1

pHCaCl2trueM51 <- c(3.33, 3.24, 3.36, 3.49, 3.47, 3.41)
MannKendall(pHCaCl2trueM51)
# tau = 0.467, 2-sided pvalue =0.25966

pHCaCl2falseM05 <- c(3.25, 3.21, 3.32, 3.32, 3.27, 3.37)
MannKendall(pHCaCl2falseM05)
# tau = 0.552, 2-sided pvalue =0.1806

pHCaCl2falseM51 <- c(3.38, 3.26, 3.37, 3.35, 3.31, 3.41)
MannKendall(pHCaCl2falseM51)
# tau = 0.0667, 2-sided pvalue =1



#############
# koppel 65 #
#############

pHH2OtrueM05 <- c(3.92, 4.27, 4.45, 4.64, 4.3, 4.25)
MannKendall(pHH2OtrueM05)
# tau = 0.2, 2-sided pvalue =0.70711

pHH2OtrueM51 <- c(3.97, 3.87, 3.98, 4.03, 3.96, 4.03)
MannKendall(pHH2OtrueM51)
# tau = 0.414, 2-sided pvalue =0.33889

pHH2OfalseM05 <- c(3.90, 3.88, 3.97, 4.04, 4.04, 4.06)
MannKendall(pHH2OfalseM05)
# tau = 0.828, 2-sided pvalue =0.035378

pHH2OfalseM51 <- c(4.20, 3.93, 4.01, 4.05, 4.05, 4.08)
MannKendall(pHH2OfalseM51)
# tau = 0.276, 2-sided pvalue =0.56609

pHCaCl2trueM05 <- c(3.92, 3.43, 3.74, 3.73, 3.53, 3.47)
MannKendall(pHCaCl2trueM05)
# tau = -0.467, 2-sided pvalue =0.25966

pHCaCl2trueM51 <- c(3.32, 3.26, 3.45, 3.88, 3.4, 3.39)
MannKendall(pHCaCl2trueM51)
# tau = 0.2, 2-sided pvalue =0.70711

pHCaCl2falseM05 <- c(3.25, 3.20, 3.38, 3.35, 3.4, 3.39)
MannKendall(pHCaCl2falseM05)
# tau = 0.6, 2-sided pvalue =0.13285

pHCaCl2falseM51 <- c(3.45, 3.33, 3.53, 3.44, 3.48, 3.45)
MannKendall(pHCaCl2falseM51)
# tau = 0.138, 2-sided pvalue =0.84831



##############
# Heiberg  ##
##############

#############
# koppel 39 #
#############

pHH2OtrueM05 <- c(4.33, 5.10, 6.05, 6.32, 5.43, 4.76, 5.54, 5.81)
MannKendall(pHH2OtrueM05)
#N tau = 0.286, 2-sided pvalue =0.38648

pHH2OtrueM51 <- c(4.53, 4.52, 4.77, 5.42, 4.65, 4.60, 4.81, 4.79)
MannKendall(pHH2OtrueM51)
# tau = 0.357, 2-sided pvalue =0.26551

pHH2OfalseM05 <- c(4.38, 4.30, 4.53, 5.19, 4.68, 6.49, 4.89, 4.6)
MannKendall(pHH2OfalseM05)
# tau = 0.429, 2-sided pvalue =0.17355

pHH2OfalseM51 <- c(4.45, 4.31, 4.42, 4.74, 4.52, 5.03, 4.42, 4.49)
MannKendall(pHH2OfalseM51)
# tau = 0.255, 2-sided pvalue =0.45443

pHCaCl2trueM05 <- c(3.87, 4.47, 4.65, 4.96, 4.36, 4.01, 4.52, 4.86)
MannKendall(pHCaCl2trueM05)
# tau = 0.286, 2-sided pvalue =0.38648

pHCaCl2trueM51 <- c(4.08, 4.11, 4.06, 4.20, 3.98, 4.10, 4.06, 4.14)
MannKendall(pHCaCl2trueM51)
# tau = 0.0364, 2-sided pvalue =1

pHCaCl2falseM05 <- c(3.89, 3.96, 3.89, 4.01, 3.98, 5.23, 4.09, 4.01)
MannKendall(pHCaCl2falseM05)
# tau = 0.593, 2-sided pvalue =0.059451

pHCaCl2falseM51 <- c(4.04, 4.01, 4.01, 4.12, 4.03, 4.23, 4.15, 4.15)
MannKendall(pHCaCl2falseM51)
# tau = 0.519, 2-sided pvalue =0.10236

#############
# koppel 40 #
#############

pHH2OtrueM05 <- c(4.16, 5.23, 6.36, 5.72, 6.25, 5.32, 5.92, 5.67)
MannKendall(pHH2OtrueM05)
# tau = 0.214, 2-sided pvalue =0.53619

pHH2OtrueM51 <- c(4.29, 4.39, 4.70, 4.74, 4.88, 4.69, 4.91, 4.83)
MannKendall(pHH2OtrueM51)
# tau = 0.643, 2-sided pvalue =0.035448

pHH2OfalseM05 <- c(4.16, 4.18, 4.49, 5.03, 4.56, 4.92, 4.64, 4.61)
MannKendall(pHH2OfalseM05)
# tau = 0.5, 2-sided pvalue =0.10776

pHH2OfalseM51 <- c(4.29, 4.21, 4.42, 4.92, 4.44, 4.44, 4.55, 4.45)
MannKendall(pHH2OfalseM51)
# tau = 0.546, 2-sided pvalue =0.080905

pHCaCl2trueM05 <- c(3.67, 4.12, 4.85, 4.41, 4.77, 4.55, 4.5, 4.54)
MannKendall(pHCaCl2trueM05)
# tau = 0.286, 2-sided pvalue =0.38648

pHCaCl2trueM51 <- c(3.86, 3.88, 3.93, 4.12, 4.01, 4.00, 4.04, 4.05)
MannKendall(pHCaCl2trueM51)
# tau = 0.643, 2-sided pvalue =0.035448

pHCaCl2falseM05 <- c(3.60, 3.71, 3.71, 3.89, 3.67, 4.28, 3.79, 3.77)
MannKendall(pHCaCl2falseM05)
# tau = 0.4, 2-sided pvalue =0.21249

pHCaCl2falseM51 <- c(3.79, 3.83, 3.76, 3.90, 3.78, 3.84, 3.89, 3.95)
MannKendall(pHCaCl2falseM51)
# tau = 0.5, 2-sided pvalue =0.10776

#############
# koppel 43 #
#############

pHH2OtrueM05 <- c(4.12, 4.60, 5.98, 5.29, 5.83, 5.80, 5.9, 6.18)
MannKendall(pHH2OtrueM05)
# tau = 0.643, 2-sided pvalue =0.035448

pHH2OtrueM51 <- c(4.27, 4.39, 4.64, 6.60, 4.59, 5.16, 5.32, 5.37)
MannKendall(pHH2OtrueM51)
# tau = 0.643, 2-sided pvalue =0.035448

pHH2OfalseM05 <- c(4.08, 4.17, 4.35, 4.78, 4.38, 4.97, 4.85, 5.03)
MannKendall(pHH2OfalseM05)
# tau = 0.857, 2-sided pvalue =0.004434

pHH2OfalseM51 <- c(4.16, 4.17, 4.33, 4.75, 4.39, 4.75, 4.78, 4.79)
MannKendall(pHH2OfalseM51)
# tau = 0.909, 2-sided pvalue =0.0027696

pHCaCl2trueM05 <- c(3.50, 3.86, 4.46, 4.02, 4.49, 4.79, 4.63, 4.69)
MannKendall(pHCaCl2trueM05)
# tau = 0.786, 2-sided pvalue =0.0093747

pHCaCl2trueM51 <- c(3.70, 3.89, 3.86, 5.02, 3.83, 4.38, 4.39, 4.35)
MannKendall(pHCaCl2trueM51)
# tau = 0.357, 2-sided pvalue =0.26551

pHCaCl2falseM05 <- c(3.46, 3.61, 3.59, 3.69, 3.52, 4.30, 4.22, 4.22)
MannKendall(pHCaCl2falseM05)
# tau = 0.546, 2-sided pvalue =0.080905

pHCaCl2falseM51 <- c(3.66, 3.73, 3.71, 3.66, 4.22, 4.27, 4.21)
MannKendall(pHCaCl2falseM51)
# tau = 0.488, 2-sided pvalue =0.17156


#############
# koppel 44 #
#############

pHH2OtrueM05 <- c(4.16, 5.19, 5.74, 6.30, 5.50, 5.93, 5.45, 5.69)
MannKendall(pHH2OtrueM05)
# tau = 0.286, 2-sided pvalue =0.38648

pHH2OtrueM51 <- c(4.19, 4.26, 4.44, 5.50, 4.36, 4.80, 4.61, 4.58)
MannKendall(pHH2OtrueM51)
# tau = 0.429, 2-sided pvalue =0.17355

pHH2OfalseM05 <- c(4.06, 4.15, 4.27, 4.83, 4.32, 4.37, 4.41, 4.33)
MannKendall(pHH2OfalseM05)
# tau = 0.571, 2-sided pvalue =0.063487

pHH2OfalseM51 <- c(4.14, 4.08, 4.19, 4.86, 4.49, 4.42, 4.49, 4.44)
MannKendall(pHH2OfalseM51)
# tau = 0.4, 2-sided pvalue =0.21249

pHCaCl2trueM05 <- c(3.54, 4.03, 4.12, 4.54, 4.10, 4.60, 4.17, 4.27)
MannKendall(pHCaCl2trueM05)
# tau = 0.571, 2-sided pvalue =0.063487

pHCaCl2trueM51 <- c(3.67, 3.78, 3.70, 3.95, 3.69, 3.98, 3.81, 3.75)
MannKendall(pHCaCl2trueM51)
# tau = 0.286, 2-sided pvalue =0.38648

pHCaCl2falseM05 <- c(3.20, 3.40, 3.32, 3.41, 3.42, 3.61, 3.52, 3.5)
MannKendall(pHCaCl2falseM05)
# tau = 0.714, 2-sided pvalue =0.018741

pHCaCl2falseM51 <- c(3.56, 3.62, 3.52, 3.66, 3.70, 3.82, 3.86, 3.77)
MannKendall(pHCaCl2falseM51)
# tau = 0.714, 2-sided pvalue =0.018741

#############
# koppel 46 #
#############

pHH2OtrueM05 <- c(4.13, 4.28, 4.92, 5.54, 5.26, 6.70, 5.51, 4.45)
MannKendall(pHH2OtrueM05)
# tau = 0.429, 2-sided pvalue =0.17355

pHH2OtrueM51 <- c(4.13, 4.09, 4.36, 4.58, 4.34, 5.84, 4.75, 4.4)
MannKendall(pHH2OtrueM51)
# tau = 0.5, 2-sided pvalue =0.10776

pHH2OfalseM05 <- c(4.01, 4.17, 4.35, 4.68, 4.38, 4.59, 4.25, 4.94)
MannKendall(pHH2OfalseM05)
# tau = 0.571, 2-sided pvalue =0.063487

pHH2OfalseM51 <- c(4.11, 4.12, 4.32, 4.82, 4.32, 4.23, 4.3, 4.34)
MannKendall(pHH2OfalseM51)
# tau = 0.4, 2-sided pvalue =0.21249

pHCaCl2trueM05 <- c(3.42, 3.67, 3.66, 4.40, 4.21, 5.07, 4.4, 3.7)
MannKendall(pHCaCl2trueM05)
# tau = 0.473, 2-sided pvalue =0.13463

pHCaCl2trueM51 <- c(3.61, 3.65, 3.63, 3.75, 3.66, 4.39, 3.43, 3.76)
MannKendall(pHCaCl2trueM51)
# tau = 0.357, 2-sided pvalue =0.26551

pHCaCl2falseM05 <- c(3.46, 3.72, 3.64, 3.94, 3.63, 3.83, 3.72, 4.01)
MannKendall(pHCaCl2falseM05)
# tau = 0.473, 2-sided pvalue =0.13463

pHCaCl2falseM51 <- c(3.61, 3.62, 3.72, 3.78, 3.65, 3.68, 3.75, 3.74)
MannKendall(pHCaCl2falseM51)
# tau = 0.5, 2-sided pvalue =0.10776

#########################
#           Haverven    #
#########################

#############
# koppel 32 #
#############

pHH2OtrueM05 <- c(6.58, 7.45, 7.05, 7.34, 7.18, 7.05, 7.01, 6.68, 6.76)
MannKendall(pHH2OtrueM05)
# tau = -0.366, 2-sided pvalue =0.20841

pHH2OtrueM51 <- c(6.37, 6.36, 6.54, 6.91, 6.59, 6.66, 6.59, 6.49)
MannKendall(pHH2OtrueM51)
# tau = 0.255, 2-sided pvalue =0.45443

pHH2OfalseM05 <- c(6.01, 60.06, 6.08, 6.17, 6.39, 6.11, 6.28, 6.11, 6.15)
MannKendall(pHH2OfalseM05)
# tau = 0.0845, 2-sided pvalue =0.83394

pHH2OfalseM51 <- c(5.90, 5.87, 5.93, 6.04, 6.30, 6.02, 6.00, 6.17, 6.1)
MannKendall(pHH2OfalseM51)
# tau = 0.5, 2-sided pvalue =0.076333

pHCaCl2trueM05 <- c(5.70, 6.57, 6.12, 6.30, 6.01, 5.98, 6.38, 5.59, 5.82)
MannKendall(pHCaCl2trueM05)
# tau = -0.278, 2-sided pvalue =0.34808

pHCaCl2trueM51 <- c(5.43, 5.62, 5.55, 5.59, 5.65, 5.48, 5.66, 5.5, 5.5)
MannKendall(pHCaCl2trueM51)
# tau = 0.0845, 2-sided pvalue =0.83394

pHCaCl2falseM05 <- c(5.30, 5.26, 5.36, 5.26, 5.27, 5.13, 5.35, 5.18, 5.21)
MannKendall(pHCaCl2falseM05)
# tau = -0.31, 2-sided pvalue =0.29451

pHCaCl2falseM51 <- c(5.09, 5.10, 5.11, 5.11, 5.16, 5.01, 5.05, 5.08, 5.11)
MannKendall(pHCaCl2falseM51)
# tau = 0.029, 2-sided pvalue =1


#############
# koppel 33 #
#############

pHH2OtrueM05 <- c(6.00, 6.96, 7.02, 6.96, 6.79, 7.02, 7.10, 6.72, 6.55)
MannKendall(pHH2OtrueM05)
# tau = 0, 2-sided pvalue =1

pHH2OtrueM51 <- c(5.76, 5.81, 5.96, 6.17, 6.44, 6.10, 6.38, 6.00, 5.93)
MannKendall(pHH2OtrueM51)
# tau = 0.278, 2-sided pvalue =0.34808

pHH2OfalseM05 <- c(6.48, 6.48, 6.48, 6.55, 6.76, 6.40, 6.66, 6.52, 6.57)
MannKendall(pHH2OfalseM05)
# tau = 0.319, 2-sided pvalue =0.28733

pHH2OfalseM51 <- c(6.31, 6.35, 6.34, 6.48, 7.11, 6.36, 6.48, 6.38, 6.06)
MannKendall(pHH2OfalseM51)
# tau = 0.141, 2-sided pvalue =0.67499

pHCaCl2trueM05 <- c(5.25, 5.89, 5.94, 5.94, 5.70, 5.73, 6.01, 5.69, 5.53)
MannKendall(pHCaCl2trueM05)
# tau = -0.0845, 2-sided pvalue =0.83394

pHCaCl2trueM51 <- c(5.00, 5.10, 5.17, 5.20, 5.32, 5.09, 5.37, 5.03, 4.95)
MannKendall(pHCaCl2trueM51)
# tau = 0, 2-sided pvalue =1

pHCaCl2falseM05 <- c(5.62, 5.62, 5.68, 5.59, 5.68, 5.38,5.61, 5.44, 4.92 )
MannKendall(pHCaCl2falseM05)
# tau = -0.514, 2-sided pvalue =0.07314

pHCaCl2falseM51 <- c(5.48, 5.55, 5.54, 5.54, 5.97, 5.32, 5.45, 5.31, 5.1)
MannKendall(pHCaCl2falseM51)
# tau = -0.535, 2-sided pvalue =0.059172

###############################
# Paired t-tests              #
###############################

Paired <- read.csv("pairedt-1tend.csv", sep =";")
summary(Paired)
str(Paired)
Paired$PAUMtend<- as.numeric(as.character(Paired$PAUMtend))

?split

PairedSplit <-split(Paired,Paired$Limed)
PairedLimed <-PairedSplit$`True`
PairedRef <-PairedSplit$`False`
PairedLimedSplit <-split(PairedLimed,PairedLimed$Depth.cm.)
PairedLimed0 <-PairedLimedSplit$`0`
PairedLimed5 <-PairedLimedSplit$`5`
PairedRefSplit <-split(PairedRef,PairedRef$Depth.cm.)
PairedRef0 <-PairedRefSplit$`0`
PairedRef5 <-PairedRefSplit$`5`


t.test(Paired$PHH2Ot.1,Paired$PHH2Otend, paired=TRUE)
# op ganse dataset is pH(H2O) stat sign gestegen met 0.38 pH eenheden
# Paired t-test
# 
# data:  Paired$PHH2Ot.1 and Paired$PHH2Otend
# t = -7.2447, df = 71, p-value = 4.17e-10
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4789188 -0.2721923
# sample estimates:
#   mean of the differences 
# -0.3755556 

t.test(PairedLimed$PHH2Ot.1,PairedLimed$PHH2Otend, paired=TRUE)
# op bekalkte plotjes dataset is pH(H2O) stat sign gestegen met 0.53 pH eenheden
# Paired t-test
# data:  PairedLimed$PHH2Ot.1 and PairedLimed$PHH2Otend
# t = -6.067, df = 35, p-value = 6.302e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.7040095 -0.3509905
# sample estimates:
#   mean of the differences 
# -0.5275 

t.test(PairedRef$PHH2Ot.1,PairedRef$PHH2Otend, paired=TRUE)
# maar ook op referentiesites is de pH stat sign gestegen met 0.22 pH eenheden
# Paired t-test
# data:  PairedRef$PHH2Ot.1 and PairedRef$PHH2Otend
# t = -4.9708, df = 35, p-value = 1.752e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3149361 -0.1322861
# sample estimates:
#   mean of the differences 
# -0.2236111 


t.test(PairedLimed0$PHH2Ot.1,PairedLimed0$PHH2Otend, paired=TRUE)
# bovenste 5 cm van bekalkte plotjes, gem stijging van 0.75 pH eenheden
# data:  PairedLimed0$PHH2Ot.1 and PairedLimed0$PHH2Otend
# t = -5.2355, df = 17, p-value = 6.715e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -1.0600297 -0.4510814
# sample estimates:
#  mean of the differences 
# -0.7555556 

t.test(PairedLimed5$PHH2Ot.1,PairedLimed5$PHH2Otend, paired=TRUE)
# 5 -10 cm van bekalkte plotjes, gem stijging van 0.30 pH eenheden
# data:  PairedLimed5$PHH2Ot.1 and PairedLimed5$PHH2Otend
# t = -4.6301, df = 17, p-value = 0.0002392
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4358936 -0.1629953
# sample estimates:
#  mean of the differences 
# -0.2994444 

t.test(PairedRef0$PHH2Ot.1,PairedRef0$PHH2Otend, paired=TRUE)
# 0-5 cm op niet bekalkte plots, stijging van 0.29 pH eenheden, dus dezelfde
# als op bekalkte 5 - 10 cm
# Paired t-test
# data:  PairedRef0$PHH2Ot.1 and PairedRef0$PHH2Otend
# t = -4.0179, df = 17, p-value = 0.0008918
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4448202 -0.1385132
# sample estimates:
#  mean of the differences 
# -0.2916667
# t.test(PairedRef5$PHH2Ot.1,PairedRef5$PHH2Otend, paired=TRUE)

t.test(PairedRef5$PHH2Ot.1,PairedRef5$PHH2Otend, paired=TRUE)
# Paired t-test
# 
# data:  PairedRef5$PHH2Ot.1 and PairedRef5$PHH2Otend
# t = -3.0997, df = 17, p-value = 0.006509
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.26143479 -0.04967632
# sample estimates:
#   mean of the differences 
# -0.1555556 

wilcox.test(Paired$PHH2Ot.1,Paired$PHH2Otend)
wilcox.test(PairedLimed$PHH2Ot.1,PairedLimed$PHH2Otend)
wilcox.test(PairedRef$PHH2Ot.1,PairedRef$PHH2Otend)
wilcox.test(PairedLimed0$PHH2Ot.1,PairedLimed0$PHH2Otend)
wilcox.test(PairedLimed5$PHH2Ot.1,PairedLimed5$PHH2Otend)
wilcox.test(PairedRef0$PHH2Ot.1,PairedRef0$PHH2Otend)
wilcox.test(PairedRef5$PHH2Ot.1,PairedRef5$PHH2Otend)

t.test(Paired$PHCACL2t.1,Paired$PHCACL2tend, paired=TRUE)
t.test(PairedLimed$PHCACL2t.1,PairedLimed$PHCACL2tend, paired=TRUE)
t.test(PairedLimed0$PHCACL2t.1,PairedLimed0$PHCACL2tend, paired=TRUE)
t.test(PairedLimed5$PHCACL2t.1,PairedLimed5$PHCACL2tend, paired=TRUE)
t.test(PairedRef$PHCACL2t.1,PairedRef$PHCACL2tend, paired=TRUE)
t.test(PairedRef0$PHCACL2t.1,PairedRef0$PHCACL2tend, paired=TRUE)
t.test(PairedRef5$PHCACL2t.1,PairedRef5$PHCACL2tend, paired=TRUE)


# t.test(Paired$PHCACL2t.1,Paired$PHCACL2tend, paired=TRUE)
# 
# Paired t-test
# 
# data:  Paired$PHCACL2t.1 and Paired$PHCACL2tend
# t = -6.9992, df = 71, p-value = 1.178e-09
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3349617 -0.1864271
# sample estimates:
#   mean of the differences 
# -0.2606944 
# 
# > t.test(PairedLimed$PHCACL2t.1,PairedLimed$PHCACL2tend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedLimed$PHCACL2t.1 and PairedLimed$PHCACL2tend
# t = -6.3851, df = 35, p-value = 2.409e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4784862 -0.2476249
# sample estimates:
#   mean of the differences 
# -0.3630556 
# 
# > t.test(PairedLimed0$PHCACL2t.1,PairedLimed0$PHCACL2tend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedLimed0$PHCACL2t.1 and PairedLimed0$PHCACL2tend
# t = -5.6933, df = 17, p-value = 2.64e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.7073696 -0.3248527
# sample estimates:
#   mean of the differences 
# -0.5161111 
# 
# > t.test(PairedLimed5$PHCACL2t.1,PairedLimed5$PHCACL2tend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedLimed5$PHCACL2t.1 and PairedLimed5$PHCACL2tend
# t = -4.3426, df = 17, p-value = 0.0004426
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3120279 -0.1079721
# sample estimates:
#   mean of the differences 
# -0.21 
# 
# > t.test(PairedRef$PHCACL2t.1,PairedRef$PHCACL2tend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedRef$PHCACL2t.1 and PairedRef$PHCACL2tend
# t = -3.7353, df = 35, p-value = 0.0006663
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.24438563 -0.07228103
# sample estimates:
#   mean of the differences 
# -0.1583333 
# 
# > t.test(PairedRef0$PHCACL2t.1,PairedRef0$PHCACL2tend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedRef0$PHCACL2t.1 and PairedRef0$PHCACL2tend
# t = -2.3964, df = 17, p-value = 0.02833
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.32698433 -0.02079345
# sample estimates:
#   mean of the differences 
# -0.1738889 
# 
# > t.test(PairedRef5$PHCACL2t.1,PairedRef5$PHCACL2tend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedRef5$PHCACL2t.1 and PairedRef5$PHCACL2tend
# t = -3.1124, df = 17, p-value = 0.006334
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.23956401 -0.04599154
# sample estimates:
#   mean of the differences 
# -0.1427778 

summary(PairedLimed0)
summary(PairedLimed5)
summary(PairedRef0)
summary(PairedRef5)


t.test(Paired$BSt.1,Paired$BStend, paired=TRUE)
t.test(PairedLimed$BSt.1,PairedLimed$BStend, paired=TRUE)
t.test(PairedLimed0$BSt.1,PairedLimed0$BStend, paired=TRUE)
t.test(PairedLimed5$BSt.1,PairedLimed5$BStend, paired=TRUE)
t.test(PairedRef$BSt.1,PairedRef$BStend, paired=TRUE)
t.test(PairedRef0$BSt.1,PairedRef0$BStend, paired=TRUE)
t.test(PairedRef5$BSt.1,PairedRef5$BStend, paired=TRUE)

t.test(Paired$TCt.1,Paired$Tctend, paired=TRUE)
t.test(PairedLimed$TCt.1,PairedLimed$Tctend, paired=TRUE)
t.test(PairedLimed0$TCt.1,PairedLimed0$Tctend, paired=TRUE)
t.test(PairedLimed5$TCt.1,PairedLimed5$Tctend, paired=TRUE)
# stat sign daling van 2.8375 g.kg-1 p-value = 0.004765
t.test(PairedRef$TCt.1,PairedRef$Tctend, paired=TRUE)
t.test(PairedRef0$TCt.1,PairedRef0$Tctend, paired=TRUE)
t.test(PairedRef5$TCt.1,PairedRef5$Tctend, paired=TRUE)

t.test(Paired$CECt.1,Paired$CECtend, paired=TRUE)
t.test(PairedLimed$CECt.1,PairedLimed$CECtend, paired=TRUE)
t.test(PairedLimed0$CECt.1,PairedLimed0$CECtend, paired=TRUE)
t.test(PairedLimed5$CECt.1,PairedLimed5$CECtend, paired=TRUE)
t.test(PairedRef$CECt.1,PairedRef$CECtend, paired=TRUE)
t.test(PairedRef0$CECt.1,PairedRef0$CECtend, paired=TRUE)
t.test(PairedRef5$CECt.1,PairedRef5$CECtend, paired=TRUE)


t.test(Paired$POLSENt.1,Paired$POLSENtend, paired=TRUE)
t.test(PairedLimed$POLSENt.1,PairedLimed$POLSENtend, paired=TRUE)
t.test(PairedLimed0$POLSENt.1,PairedLimed0$POLSENtend, paired=TRUE)
t.test(PairedLimed5$POLSENt.1,PairedLimed5$POLSENtend, paired=TRUE)
t.test(PairedRef$POLSENt.1,PairedRef$POLSENtend, paired=TRUE)
t.test(PairedRef0$POLSENt.1,PairedRef0$POLSENtend, paired=TRUE)
t.test(PairedRef5$POLSENt.1,PairedRef5$POLSENtend, paired=TRUE)

t.test(Paired$PAUMt.1,Paired$PAUMtend, paired=TRUE)
t.test(PairedLimed$PAUMt.1,PairedLimed$PAUMtend, paired=TRUE)
t.test(PairedLimed0$PAUMt.1,PairedLimed0$PAUMtend, paired=TRUE)
t.test(PairedLimed5$PAUMt.1,PairedLimed5$PAUMtend, paired=TRUE)
t.test(PairedRef$PAUMt.1,PairedRef$PAUMtend, paired=TRUE)
t.test(PairedRef0$PAUMt.1,PairedRef0$PAUMtend, paired=TRUE)
t.test(PairedRef5$PAUMt.1,PairedRef5$PAUMtend, paired=TRUE)

# 
# t.test(Paired$CECt.1,Paired$CECtend, paired=TRUE)
# 
# Paired t-test
# 
# data:  Paired$CECt.1 and Paired$CECtend
# t = 4.2137, df = 54, p-value = 9.605e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.7028069 1.9786476
# sample estimates:
#   mean of the differences 
# 1.340727 
# 
# > t.test(PairedLimed$CECt.1,PairedLimed$CECtend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedLimed$CECt.1 and PairedLimed$CECtend
# t = 2.6111, df = 27, p-value = 0.01455
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.2440386 2.0345329
# sample estimates:
#   mean of the differences 
# 1.139286 
# 
# > t.test(PairedLimed0$CECt.1,PairedLimed0$CECtend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedLimed0$CECt.1 and PairedLimed0$CECtend
# t = 1.5198, df = 14, p-value = 0.1508
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3964696  2.3244696
# sample estimates:
#   mean of the differences 
# 0.964 
# 
# > t.test(PairedLimed5$CECt.1,PairedLimed5$CECtend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedLimed5$CECt.1 and PairedLimed5$CECtend
# t = 2.192, df = 12, p-value = 0.04884
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.008046757 2.675030166
# sample estimates:
#   mean of the differences 
# 1.341538 
# 
# > t.test(PairedRef$CECt.1,PairedRef$CECtend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedRef$CECt.1 and PairedRef$CECtend
# t = 3.3037, df = 26, p-value = 0.0027 83
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.5854693 2.5137899
# sample estimates:
#   mean of the differences 
# 1.54963 
# 
# > t.test(PairedRef0$CECt.1,PairedRef0$CECtend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedRef0$CECt.1 and PairedRef0$CECtend
# t = 2.4424, df = 13, p-value = 0.02963
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1755897 2.8658389
# sample estimates:
#   mean of the differences 
# 1.520714 
# 
# > t.test(PairedRef5$CECt.1,PairedRef5$CECtend, paired=TRUE)
# 
# Paired t-test
# 
# data:  PairedRef5$CECt.1 and PairedRef5$CECtend
# t = 2.1559, df = 12, p-value = 0.05209
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01679218  3.17833064
# sample estimates:
#   mean of the differences 
# 1.580769 

# dalende trend (?) of toch lagere CECwaarden in 2016-2017 tegenover in 2012-2013
# kan verklaring zijn waarom de BS in 2016-2017 hoger is dan in 2012 -2013


t.test(Paired$LOIOSt.1,Paired$LOIOStend, paired=TRUE)
t.test(PairedLimed$LOIOSt.1,PairedLimed$LOIOStend, paired=TRUE)
t.test(PairedLimed0$LOIOSt.1,PairedLimed0$LOIOStend, paired=TRUE)
t.test(PairedLimed5$LOIOSt.1,PairedLimed5$LOIOStend, paired=TRUE)
t.test(PairedRef$LOIOSt.1,PairedRef$LOIOStend, paired=TRUE)
t.test(PairedRef0$LOIOSt.1,PairedRef0$LOIOStend, paired=TRUE)
t.test(PairedRef5$LOIOSt.1,PairedRef5$LOIOStend, paired=TRUE)

ggplot(Paired, aes(x=LOIOSt.1)) + geom_histogram()
ggplot(Paired, aes(x=LOIOStend)) + geom_histogram()


##########################################################################
# Verschillen in startcondities tussen bekalkte en niet-bekalkte plots?  #
##########################################################################

Paired <- read.csv("paired.ref.limed.csv", sep =";")
summary(Paired)
str(Paired)

library(ggplot2)
ggplot (Paired, aes(x= EC.ref)) + geom_histogram()
ggplot (Paired, aes(x= Eclimed)) + geom_histogram()

# gegevens zijn niet normaal verdeeld dus je mag eigenlijk geen t-test doen
wilcox.test(Paired$LOI.ref, Paired$LOIlimed)
wilcox.test(Paired$PHH2O.ref, Paired$PHH2Olimed)
wilcox.test(Paired$PHCACL2.ref, Paired$PHCACL2limed)
wilcox.test(Paired$EC.ref, Paired$Eclimed)
wilcox.test(Paired$NKJEL.ref, Paired$NKJELlimed)
wilcox.test(Paired$NMIN.ref, Paired$NMINlimed)
wilcox.test(Paired$NOx.ref, Paired$Noxlimed)
wilcox.test(Paired$NH4.ref, Paired$NH4limed)
wilcox.test(Paired$TC.ref, Paired$Tclimed)
wilcox.test(Paired$POLSEN.ref, Paired$POLSENlimed)
wilcox.test(Paired$PAUM.ref, Paired$PAUMlimed)
wilcox.test(Paired$Ca.ref, Paired$Calimed)
wilcox.test(Paired$K.ref, Paired$Klimed)
wilcox.test(Paired$Mg.ref, Paired$Mglimed)
wilcox.test(Paired$C.N.ref, Paired$C.N.limed)

t.test(Paired$LOI.ref, Paired$LOIlimed, paired=TRUE)
t.test(Paired$PHH2O.ref, Paired$PHH2Olimed, paired=TRUE)
t.test(Paired$PHCACL2.ref, Paired$PHCACL2limed, paired=TRUE)
t.test(Paired$EC.ref, Paired$Eclimed, paired=TRUE)
t.test(Paired$NKJEL.ref, Paired$NKJELlimed, paired=TRUE)
t.test(Paired$NMIN.ref, Paired$NMINlimed, paired=TRUE)
t.test(Paired$NOx.ref, Paired$Noxlimed, paired=TRUE)
t.test(Paired$NH4.ref, Paired$NH4limed, paired=TRUE)
t.test(Paired$TC.ref, Paired$Tclimed, paired=TRUE)
t.test(Paired$POLSEN.ref, Paired$POLSENlimed, paired=TRUE)
t.test(Paired$PAUM.ref, Paired$PAUMlimed, paired=TRUE)
t.test(Paired$Ca.ref, Paired$Calimed, paired=TRUE)
t.test(Paired$K.ref, Paired$Klimed, paired=TRUE)
t.test(Paired$Mg.ref, Paired$Mglimed, paired=TRUE)
t.test(Paired$C.N.ref, Paired$C.N.limed, paired=TRUE)

############################
# P Olsen problematiek     #
############################

ggplot(Piloot, aes(x= POLSEN, y = PAUM)) + geom_jitter() + facet_wrap (~Area, scales = "free_y") 
ggplot(Piloot, aes(x= POLSEN, y = PAUM)) + geom_jitter() + facet_wrap (~Area, scales = "free_x") 

ggplot(Zandvoordebos, aes(x= POLSEN, y = PAUM, colour = Depth, shape = Limed)) + geom_jitter()
ggplot(Aanwijsputten, aes(x= POLSEN, y = PAUM, colour = Depth, shape = Limed)) + geom_jitter()
ggplot(Haverven, aes(x= POLSEN, y = PAUM, colour = Depth, shape = Limed)) + geom_jitter()
ggplot(Heiberg, aes(x= POLSEN, y = PAUM, colour = Depth.cm., shape = Limed)) + geom_jitter()
ggplot(Tielenkamp, aes(x= POLSEN, y = PAUM, colour = Depth, shape = Limed)) + geom_jitter()
# hier zie je een beter correlatie in de 5 - 10 cm stalen dat in de 0 - 5 cm stalen
ggplot(Averbode, aes(x= POLSEN, y = PAUM, colour = Depth, shape = Limed)) + geom_jitter()

cor(Piloot$POLSEN, y = Piloot$PAUM, use = "pairwise.complete.obs",
    method = c("pearson"))
cor(Zandvoordebos$POLSEN, y = Zandvoordebos$PAUM, use = "pairwise.complete.obs",
    method = c("pearson"))
cor(Haverven$POLSEN, y = Haverven$PAUM, use = "pairwise.complete.obs",
    method = c("pearson"))
cor(Tielenkamp$POLSEN, y = Tielenkamp$PAUM, use = "pairwise.complete.obs",
    method = c("pearson"))
cor(Heiberg$POLSEN, y = Heiberg$PAUM, use = "pairwise.complete.obs",
    method = c("pearson"))
cor(Aanwijsputten$POLSEN, y = Aanwijsputten$PAUM, use = "pairwise.complete.obs",
    method = c("pearson"))
cor(Averbode$POLSEN, y = Averbode$PAUM, use = "pairwise.complete.obs",
    method = c("pearson"))



######################################################
# P Olsen problematiek
######################################################

# zien we een verband tussen P Olsen, PAUM en P totaal?

Allchemical<-read.csv2("Allchemicalonset.csv")
str(Allchemical)

Allchemical$Pair <- as.factor(Allchemical$Pair)
Allchemical$SurveyPoint <- as.factor(Allchemical$SurveyPoint)
Allchemical$Depth <- as.factor(Allchemical$Depth)
Allchemical$LOIOS<- as.numeric(as.character(Allchemical$LOIOS))
Allchemical$PHH2O<- as.numeric(as.character(Allchemical$PHH2O))
Allchemical$PHCACL2<- as.numeric(as.character(Allchemical$PHCACL2))
Allchemical$EC<- as.numeric(as.character(Allchemical$EC))
Allchemical$NMIN<- as.numeric(as.character(Allchemical$NMIN))
Allchemical$NKJEL<- as.numeric(as.character(Allchemical$NKJEL))
Allchemical$NOx<- as.numeric(as.character(Allchemical$NOx))
Allchemical$NH4<- as.numeric(as.character(Allchemical$NH4))
Allchemical$TC<- as.numeric(as.character(Allchemical$TC))
Allchemical$POLSEN<- as.numeric(as.character(Allchemical$POLSEN))
Allchemical$PAUM<- as.numeric(as.character(Allchemical$PAUM))
Allchemical$Ca <- as.numeric(as.character(Allchemical$Ca))
Allchemical$K <- as.numeric(as.character(Allchemical$K))
Allchemical$Mg <- as.numeric(as.character(Allchemical$Mg))
Allchemical$Na <- as.numeric(as.character(Allchemical$Na))
Allchemical$CEC <- as.numeric(as.character(Allchemical$CEC))
Allchemical$BS <- as.numeric(as.character(Allchemical$BS))
Allchemical$C.N <- as.numeric(as.character(Allchemical$C.N))
Allchemical$Al.tot <- as.numeric(as.character(Allchemical$Al.tot))
Allchemical$As.tot <- as.numeric(as.character(Allchemical$As.tot))
Allchemical$Cd.tot <- as.numeric(as.character(Allchemical$Cd.tot))
Allchemical$Co.tot <- as.numeric(as.character(Allchemical$Co.tot))
Allchemical$Cr.tot <- as.numeric(as.character(Allchemical$Cr.tot))
Allchemical$Cu.tot <- as.numeric(as.character(Allchemical$Cu.tot))
Allchemical$Fe.tot <- as.numeric(as.character(Allchemical$Fe.tot))
Allchemical$Mn.tot <- as.numeric(as.character(Allchemical$Mn.tot))
Allchemical$Ni.tot <- as.numeric(as.character(Allchemical$Ni.tot))
Allchemical$P.tot <- as.numeric(as.character(Allchemical$P.tot))
Allchemical$Pb.tot <- as.numeric(as.character(Allchemical$Pb.tot))
Allchemical$S.tot <- as.numeric(as.character(Allchemical$S.tot))
Allchemical$Zn.tot <- as.numeric(as.character(Allchemical$Zn.tot))
Allchemical$Ca.tot <- as.numeric(as.character(Allchemical$Ca.tot))
Allchemical$K.tot <- as.numeric(as.character(Allchemical$K.tot))
Allchemical$Mg.tot <- as.numeric(as.character(Allchemical$Mg.tot))
Allchemical$Na.tot <- as.numeric(as.character(Allchemical$Na.tot))
Allchemical$Al.Ca <- as.numeric(as.character(Allchemical$Al.Ca))
Allchemical$C.P <- as.numeric(as.character(Allchemical$C.P))

library(ggplot2)
p <- ggplot (Allchemical, aes(x= POLSEN, y=PAUM, colour = Area)) + geom_point(size = 2)


p <- ggplot (Allchemical, aes(x= POLSEN, y=P.tot, colour = Area)) + geom_point(size = 2)
ggplot (Allchemical, aes(x= PAUM, y=P.tot, colour = Area)) + geom_point(size = 2)
p + scale_x_continuous("P AUM (mg.kg-1)") + scale_y_continuous("P semit-total (mg.kg-1)")
  
model <- lm (POLSEN ~ PAUM, data = Allchemical)
anova (model)
summary (model)

model <- lm (P.tot ~ PAUM, data = Allchemical)
anova (model)
summary (model)

p <- ggplot (Allchemical, aes(x= POLSEN, y=P.tot, colour = Area)) + geom_point(size = 2)
p + scale_x_continuous("P Olsen (mg.kg-1)") + scale_y_continuous("P semit-total (mg.kg-1)")

model <- lm (P.tot ~ POLSEN, data = Allchemical)
anova (model)
summary (model)