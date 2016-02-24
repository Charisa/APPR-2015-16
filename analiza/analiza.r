# 4. faza: Analiza podatkov

source("analiza/funkcije_grupiranje.r", encoding = "UTF-8")


# Grupiranje podatkov po Quality of Life Index od leta 2012 do leta 2015

zemljevid_2012 <- grupiranje(tabela_quality_of_life_2012, 4, "Quality of Life Index 2012")

zemljevid_2013 <- grupiranje(tabela_quality_of_life_2013, 4, "Quality of Life Index 2013")

zemljevid_2014 <- grupiranje(tabela_quality_of_life_2014, 4, "Quality of Life Index 2014")

zemljevid_2015 <- grupiranje(tabela_quality_of_life_2015, 4, "Quality of Life Index 2015")



# Tabela: table_2014

table_2014

# Tabela: human_development_index

human_development_index

# Tabela human_development_index vsebuje podatke za HDI vseh dražv, ki imajo visok HDI.
# V to skupino spada tudi Slovenija in večino evropskih držav.


# Združimo novo tabelo s "tabela_2014", ki vsebuje vse indekse leta 2014. Tabela bo služila
# za napovedovanje ostalih indeksov.

table_2014 <- join_all(list(leto_2014, human_development_index), by = NULL, type = 'full')
table_2014 <- table_2014[,-length(table_2014)]
table_2014 <- table_2014[,-length(table_2014)+1]
write.csv2(table_2014, "podatki/2014", fileEncoding = "UTF-8")


# Naredimo še manjšo tabelo, ki jo bomo uporabili za napovedovanje ostalih indeksov s podobnim
# HDI (Human Development Index). Izberemo državi z najvišjima vrednostima HDI (Norveška, Avstralija) 
# in najmanjšima vrednostima HDI pri zbranih podatkih (Črna Gora, Kuvait). 
# Napoved bomo določili samo za države z visokim HDI.

tabela_HDI <- table_2014[c(72, 35, 61, 112) ,]

tabela_HDI <- data.frame(tabela_HDI, row.names = tabela_HDI[, 1])
tabela_HDI <- tabela_HDI[,-1]


# S podatki iz razpredelnice naredimo 'dotchart'.

dotchart(
  t(tabela_HDI), 
  color=c("Red","Blue","Darkgreen", "Midnightblue", "Pink", "Darkmagenta", 
          "Goldenrod", "Darkorange", "Cyan", "Coral"),
  main = "Indexes", cex = 0.9, gcolor = "Black", pch = 19)




