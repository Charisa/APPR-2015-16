# 4. faza: Analiza podatkov

source("analiza/funkcije_analiza.r", encoding = "UTF-8")
source("lib/libraries.r", encoding = "UTF-8")


# Grupiranje podatkov po Quality of Life Index od leta 2012 do leta 2015

zemljevid_2012 <- grupiranje(tabela_quality_of_life_2012, 4, "Quality of Life Index 2012")
zemljevid_2013 <- grupiranje(tabela_quality_of_life_2013, 4, "Quality of Life Index 2013")
zemljevid_2014 <- grupiranje(tabela_quality_of_life_2014, 4, "Quality of Life Index 2014")
zemljevid_2015 <- grupiranje(tabela_quality_of_life_2015, 4, "Quality of Life Index 2015")



# Uvozimo 4 HTML tabele, ki prikazujejo Human Index Development v letu 2014 (najnovešji podatki na voljo).

human_development_index1 <- html_razpredelnica('podatki/human_development_index1.csv','http://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index', 3, c(1, 2, 28, 29))
human_development_index2 <- html_razpredelnica('podatki/human_development_index2.csv','http://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index', 6, c(1, 2, 32, 33, 55))
human_development_index3 <- html_razpredelnica('podatki/human_development_index3.csv','http://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index', 9, c(1, 2, 23, 24))
human_development_index4 <- html_razpredelnica('podatki/human_development_index4.csv','http://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index', 12, c(1, 2, 25, 26))


# Tabela human_development_index1 vsebuje podatke za HDI vseh držav, ki imajo visok HDI.
# V to skupino spada tudi Slovenija in večino evropskih držav.

# Združimo tabele z vsemi kategorijami indeksov v eno tabelo: human_development_index.

human_development_index <- join_all(list(human_development_index1, human_development_index2, human_development_index3, 
                                         human_development_index4), by = NULL, type = 'full')


# Združimo novo tabelo s "tabela_2014", ki vsebuje vse indekse leta 2014. Tabela bo služila
# za napovedovanje ostalih indeksov.

table_2014 <- join_all(list(leto_2014, human_development_index), by = NULL, type = 'full')
table_2014 <- table_2014[,-length(table_2014)]
table_2014 <- table_2014[,-length(table_2014)+1]
write.csv2(table_2014, "podatki/2014", fileEncoding = "UTF-8")

# Tabelo uredimo po HDI. Pogledamo dve največji in dve najmanjši vrednost HDI pri državah, 
# ki imajo na voljo podatke vseh ostalih indeksov.
# Dve največji vrednosti sta Norveška in Avstralija (72, 35),
# dve najmanjši pa Indija in Pakistan (9, 50).

# Naredimo še manjšo tabelo, ki jo bomo uporabili za približno napoved ostalih indeksov 
# na podlagi HDI (največjih in najmanjših vrednosti). 

tabela_HDI <- table_2014[c(72, 35, 9, 50) ,]

tabela_HDI <- data.frame(tabela_HDI, row.names = tabela_HDI[, 1])
tabela_HDI <- tabela_HDI[,- c(1)]
tabela_HDI$CODE <- NULL


# S podatki iz razpredelnice tabela_HDI naredimo 'dotchart'. 
# Graf in analizo grafa naredimo v datoteki projekt.Rmd.



# Iz razpredelnice 'tabela' sem izbrala 10 držav, katerih indekse bom napovedala (s pomočjo grafa).
# Podatki so od leta 2012 do leta 2015.
# Funkcija se nahaja v funkcije_grupiranje, kjer funkcija indeks_graf nariše graf za izbrane 
# države od leta 2012 do leta 2015 pri izbranem indeksu.

drzave_10 <- c("Slovenia", "Australia", "Croatia", "Japan", "Norway", "United Kingdom", "United States", "Austria",
            "Argentina", "China")
tabela_indeksov_10 <- dplyr::filter(tabela, Country %in% drzave_10)
tabela_indeksov_10$CODE <- NULL





# For zanka za izpis zemljevidov indeksov po letih, ki jih uporabim v shiny-ju.
# (Uporabimo le v primeru, če funkcija vrni_zemljevid v shiny ne deluje.)

#indeksi_9 <- c("Traffic Index", "Quality of Life Index", "Pollution Index", "Health Care Index",
#              "Crime Index", "Rent Index", "Consumer Price Index", "Groceries Index", "Restaurant Price Index")
#for (i in indeksi_9){
#  for (l in seq(2012, 2015)){
#    assign(paste("year", l, gsub(" ", "", i), sep = "_"), vrni_zemljevid(i, l))
#  }
#}
