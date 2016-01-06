# 2. faza: Uvoz podatkov

# Knjižnice

library(XML)
library(RCurl)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)



# RAZPREDELNICA za graf(CSV)

# Poimenovanje stolpcev:

stolpci <- c("dobrine/storitve","januar2015.december2014","januar2015.januar2014","povprečje_januar_2015.povprečje_januar_2014",
             "povprečna12-mesečna_rast",
             "februar2015.januar2015","februar2015.februar2014",
             "povprečje_januar,februar_2015.povprečje_januar,februar_2014","povprečna12-mesečna_rast",
             "marec2015.februar2015","marec2015.marec2014",
             "povprečje_januar-marec_2015.povprečje_januar-marec_2014","povprečna12-mesečna_rast",
             "april2015.marec2015","april2015.april2014",
             "povprečje_januar-april_2015.povprečje_januar-april_2014","povprečna12-mesečna_rast",
             "maj2015.april2015","maj2015.maj2014",
             "povprečje_januar-maj_2015.povprečje_januar-maj_2014","povprečna12-mesečna_rast",
             "junij2015.maj2015","junij2015.junij2014",
             "povprečje_januar-junij_2015.povprečje_januar-junij_2014","povprečna12-mesečna_rast",
             "julij2015.junij2015","julij2015.julij2014",
             "povprečje_januar-julij_2015.povprečje_januar-julij_2014","povprečna12-mesečna_rast",
             "avgust2015.julij2015","avgust2015.avgust2014",
             "povprečje_januar-avgust_2015.povprečje_januar-avgust_2014","povprečna12-mesečna_rast",
             "september2015.avgust2015","september2015.september2014",
             "povprečje_januar-september_2015.povprečje_januar-september_2014","povprečna12-mesečna_rast",
             "oktober2015.september2015","oktober2015.oktober2014",
             "povprečje_januar-oktober_2015.povprečje_januar-oktober_2014","povprečna12-mesečna_rast",
             "november2015.oktober2015","november2015.oktober2014",
             "povprečje_januar-november_2015.povprečje_januar-november_2014","povprečna12-mesečna_rast")

razpredelnica = read.csv2("podatki/indeksi_cen.csv", fileEncoding = "Windows-1250")
                                                                  # Odpremo datoteko .csv.
razpredelnica <- data.frame(razpredelnica, row.names = razpredelnica[, 1])
                                                                  # Izbrišemo stolpec z imeni dobrin/storitev
                                                                  # in preimenujemo imena vrstic v ta stolpec.
rownames(razpredelnica) <- gsub("^[0-9]+ ", "", rownames(razpredelnica))
                                                                  # Odstranimo številke v imenih vrstic
razpredelnica$X01.Hrana.in.brezalkoholne.pijače <- row.names(razpredelnica)
colnames(razpredelnica) <- stolpci                                # Preimenujemo imena stolpcev z imeni iz vektorja stolpci.

indx <- sapply(razpredelnica, is.factor)                          # 'Vrednosti' so faktorji. 
razpredelnica[indx] <- lapply(razpredelnica[indx], function(x) as.numeric(gsub("[.]", ".", x)))
                                                                  # Faktorje spremenimo v numerične vrednosti.

razpredelnica <- razpredelnica[seq(-5, -length(stolpci), by = -4)]# Odstranimo vsak četrti stolpec.

write.csv2(razpredelnica, "podatki/razpredelnica.csv", fileEncoding = "UTF-8", row.names = FALSE)
                                                                  # Ustvarimo datoteko .csv.

grupiranje <- c("januar2015.december2014","februar2015.januar2015",
                "marec2015.februar2015","april2015.marec2015",
                "maj2015.april2015","junij2015.maj2015",
                "julij2015.junij2015","avgust2015.julij2015",
                "september2015.avgust2015","oktober2015.september2015",
                "november2015.oktober2015")                       # Ustvarimo nov vektor z imeni stolpcev, ki jih 
                                                                  # bomo uporabili v novi tabeli.




# NOVA TABELA osnovne_dobrine: spreminjanje indeksa 16-ih dobrin/storitev v letu 2015 (po mesecih)

osnovne_dobrine <- subset(razpredelnica, select = grupiranje)     # Podrazpredelnica (spremembe po zaporednih mesecih)

osnovne_dobrine <- osnovne_dobrine[-c(2:20, 22:27, 29:32, 34:37, 39:57, 59:65, 67:84, 
                                      86:104, 106:109, 111:118, 120:123, 126:128),]    
                                                                  # Izbrani stolpci originalne razpredelnice
imena_dobrin <- row.names(osnovne_dobrine)                        # V imena vrstic shranimo dobrine/storitve

osnovne_dobrine <- arrange(osnovne_dobrine, desc(januar2015.december2014), 
                           desc(februar2015.januar2015))          # Ureditev po velikosti (od največje do najmanjše vrednosti
                                                                  # po prvem, nato po drugem stolpcu).
row.names(osnovne_dobrine) <- imena_dobrin     
                                                                  # Ker se v prejšnjem koraku spremenijo imena vrstic, 
                                                                  # jih preimenujemo nazaj (s pomočjo prej definiranega vektorja
                                                                  # imena_dobrin).



# NOVI TABELI ZA GRAF osnovne_dobrine_graf, dobrine_graf

osnovne_dobrine_graf <- subset(osnovne_dobrine, select = (colnames(osnovne_dobrine))[c(1, 2, 10, 11)])
                                                                  # Prva in zadnja dva stolpca.

osnovne_dobrine_graf <- osnovne_dobrine_graf[c(1, 2, length(rownames(osnovne_dobrine)) - 1, 
                                               length((rownames(osnovne_dobrine)))),]
                                                                  # 2 največja in 2 najmanjši vrednosti tabele osnovne_dobrine 
                                                                  # (iz katerih bo sestavljen graf), 
                                                                  # torej prvi dve in zadnji dve vrstici.

                                                                  # Preoblikovanje osnovne_dobrine_graf za uporabo v grafu
osnovne_dobrine_graf <- t(osnovne_dobrine_graf)                   # Transponiranje
osnovne_dobrine_graf <- melt(osnovne_dobrine_graf, id = row.names(osnovne_dobrine_graf))  
                                                                  # Imena samo v vrsticah, 
                                                                  # spremenljivke se ponavljajo večkrat.

# S pomočjo konzole poiščemo minimum in maksimum indeksa v celem letu, nato pa jih zapišemo
# v dobrine_graf.

dobrine <- subset(razpredelnica, select = grupiranje)             # Nova razpredelnica s stolpci iz grupiranja.
dobrine_graf <- subset(dobrine, select = (colnames(osnovne_dobrine))[c(1, 5, 7, 11)])
dobrine_graf <- dobrine_graf[c(7, 21),]

dobrine_graf <- t(dobrine_graf)
dobrine_graf <- melt(dobrine_graf, id = row.names(dobrine_graf))



# RISANJE GRAFOV

graf_osnovnih_dobrin <- ggplot(osnovne_dobrine_graf, stat = "identity", main = "Indeksi cen") + 
  aes(x = Var1, y = value, fill = Var2, stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_bar(stat = "identity", position = "dodge") + xlab("obdobja") + ylab("vrednost indeksa") + 
  scale_fill_manual("Dobrine in storitve", values = c("darkred", "darkblue", "yellow", "darkgreen"))


graf <- ggplot(dobrine_graf, stat = "identity", main = "Indeksi cen") + 
  aes(x = Var1, y = value, fill = Var2, stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_bar(stat = "identity", position = "dodge") + xlab("obdobja") + ylab("vrednost indeksa") + 
  scale_fill_manual("Dobrine in storitve", values = c("darkred", "darkblue"))


# Preoblikovanje razpredelnice v tidy.data

tidy <- arrange(razpredelnica,desc(januar2015.december2014))
razpredelnica_tidy <- melt(tidy, id = "dobrine/storitve")
razpredelnica_tidy <- rename(razpredelnica_tidy, "mesečni indeksi" = variable, "vrednost indeksa" = value)





# RAZPREDELNICE (HTML).

# Uvožene razpredelnice s pomočjo funkcije html_razpredelnice, kateri podamo,
# kam shrani novo tabelo in iz katere spletne strani jo uvozimo.
# Osnovne tabele od leta 2012 do leta 2015 (indeksi: CPI, Rent index,
# CPI + rent index, Groceries index, Restaurant price index,
# Local purchasing power index).
# Teh razpredelnic je sicer še več (od leta 2009 do 2015), ampak kasneje
# uporabljene tabele časovno ne segajo dlje od leta 2012. Zato vzamemo
# samo to časovno obdobje.


# Zaženemo funkcijo html_razpredelnice, ki sprejme podatka, kam naj shrani tabelo
# in na kateri spletni strani se nahaja. Ostali ukazi znotraj funkcije ostanejo za te
# primere nespremenljivi.

source("uvoz/html_razpredelnice.R", encoding = "UTF-8")


# Uvoz razpredelnic po letih

tabela_2012 <- html_razpredelnice("podatki/tabela_2012",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2012")

tabela_2013 <- html_razpredelnice("podatki/tabela_2013",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2013")

tabela_2014 <- html_razpredelnice("podatki/tabela_2014", 
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2014")

tabela_2015 <- html_razpredelnice("podatki/tabela_2015",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2015")


# Izbrane razpredelnice po določenih indeksih in letih



# Zaženemo funkcijo html_svetovni_indeksi, ki sprejme podatke, kam naj tabelo shrani
# in iz katere spletne strani naj sploh dobi tabelo.

source("uvoz/html_svetovni_indeksi.R", encoding = "UTF-8")


# Uvoz razpredelnic po indeksih in letih (od leta 2012 do leta 2015).
# Originalne razpredelnic je bilo več, vsaka pa je vsebovala en indeks,
# ki ga želim vključiti v tabelo, zato so nastale tabele z eno vrednostjo stolpca.
# Indeksi: Traffic index, Crime index, Health Care Index, Pollution Index, Quality of Life index.
# Rubrike ločimo po indeksih.




# TRAFFIC index od leta 2012 do 2015

tabela_traffic_2012 <- html_svetovni_indeksi("podatki/tabela_traffic_2012.",
                                             "http://www.numbeo.com/traffic/rankings_by_country.jsp?title=2012-Q1")

tabela_traffic_2013 <- html_svetovni_indeksi("podatki/tabela_trafiic_2013",
                                             "http://www.numbeo.com/traffic/rankings_by_country.jsp?title=2013-Q1")

tabela_traffic_2014 <- html_svetovni_indeksi("podatki/tabela_traffic_2014",
                                             "http://www.numbeo.com/traffic/rankings_by_country.jsp?title=2014")

tabela_traffic_2015 <- html_svetovni_indeksi("podatki/tabela_traffic_2015",
                                             "http://www.numbeo.com/traffic/rankings_by_country.jsp?title=2015")


# CRIME index od leta 2012 do 2015

tabela_crime_2012 <- html_svetovni_indeksi("podatki/tabela_crime_2012",
                                           "http://www.numbeo.com/crime/rankings_by_country.jsp?title=2012-Q1")

tabela_crime_2013 <- html_svetovni_indeksi("podatki/tabela_crime_2013",
                                           "http://www.numbeo.com/crime/rankings_by_country.jsp?title=2013-Q1")

tabela_crime_2014 <- html_svetovni_indeksi("podatki/tabela_crime_2014",
                                           "http://www.numbeo.com/crime/rankings_by_country.jsp?title=2014")

tabela_crime_2015 <- html_svetovni_indeksi("podatki/tabela_crime_2015",
                                           "http://www.numbeo.com/crime/rankings_by_country.jsp?title=2015")


# HEALTH CARE index od leta 2012 do 2015

tabela_health_care_2012 <- html_svetovni_indeksi("podatki/tabela_health_care_2012",
                                                 "http://www.numbeo.com/health-care/rankings_by_country.jsp?title=2012-Q1")

tabela_health_care_2013 <- html_svetovni_indeksi("podatki/tabela_health_care_2013",
                                                 "http://www.numbeo.com/health-care/rankings_by_country.jsp?title=2013-Q1")

tabela_health_care_2014 <- html_svetovni_indeksi("podatki/tabela_health_care_2014",
                                                 "http://www.numbeo.com/health-care/rankings_by_country.jsp?title=2014")

tabela_health_care_2015 <- html_svetovni_indeksi("podatki/tabela_health_care_2015",
                                                 "http://www.numbeo.com/health-care/rankings_by_country.jsp?title=2015")


# POLLUTION index od leta 2012 do 2015

tabela_pollution_2012 <- html_svetovni_indeksi("podatki/tabela_pollution_2012",
                                               "http://www.numbeo.com/pollution/rankings_by_country.jsp?title=2012-Q1")

tabela_pollution_2013 <- html_svetovni_indeksi("podatki/tabela_pollution_2013",
                                               "http://www.numbeo.com/pollution/rankings_by_country.jsp?title=2013-Q1")

tabela_pollution_2014 <- html_svetovni_indeksi("podatki/tabela_pollution_2014",
                                               "http://www.numbeo.com/pollution/rankings_by_country.jsp?title=2014")

tabela_pollution_2015 <- html_svetovni_indeksi("podatki/tabela_pollution_2015",
                                               "http://www.numbeo.com/pollution/rankings_by_country.jsp?title=2015")


# QUALITY OF LIFE index od leta 2012 do 2015

tabela_quality_of_life_2012 <- html_svetovni_indeksi("podatki/tabela_quality_of_life_2012",
                                                     "http://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2012-Q1")

tabela_quality_of_life_2013 <- html_svetovni_indeksi("podatki/tabela_quality_of_life_2013",
                                                     "http://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2013-Q1")

tabela_quality_of_life_2014 <- html_svetovni_indeksi("podatki/tabela_quality_of_life_2014",
                                                     "http://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014")

tabela_quality_of_life_2015 <- html_svetovni_indeksi("podatki/tabela_quality_of_life_2015",
                                                     "http://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015")



# Naslednje indekse dobimo iz tabel: tabela_2012, tabela_2013, tabela_2014 in tabela_2015.
# Za boljši pregled nad posameznimi indeksi, sem vse indekse dala v svojo tabelo po letih,
# ne pa jih pustila vse skupaj in jih ločevala samo po letih.
# V tem primeru si nisem pomagala z novo funkcijo, saj časovno ne bi kaj prida profitirala.


# RENT index od leta 2012 do 2015

tabela_rent_2012 <- subset(tabela_2012, select = c("Country", "Rent Index"))

tabela_rent_2013 <- subset(tabela_2013, select = c("Country", "Rent Index"))

tabela_rent_2014 <- subset(tabela_2014, select = c("Country", "Rent Index"))

tabela_rent_2015 <- subset(tabela_2015, select = c("Country", "Rent Index"))


# CPI od leta 2012 do 2015

tabela_CPI_2012 <- subset(tabela_2012, select = c("Country", "Consumer Price Index"))

tabela_CPI_2013 <- subset(tabela_2013, select = c("Country", "Consumer Price Index"))

tabela_CPI_2014 <- subset(tabela_2014, select = c("Country", "Consumer Price Index"))

tabela_CPI_2015 <- subset(tabela_2015, select = c("Country", "Consumer Price Index"))


# GROCERIES index od leta 2012 do 2015

tabela_groceries_2012 <- subset(tabela_2012, select = c("Country", "Groceries Index"))

tabela_groceries_2013 <- subset(tabela_2013, select = c("Country", "Groceries Index"))

tabela_groceries_2014 <- subset(tabela_2014, select = c("Country", "Groceries Index"))

tabela_groceries_2015 <- subset(tabela_2015, select = c("Country", "Groceries Index"))


# RESTAURANT PRICE index od leta 2012 do 2015

tabela_restaurant_price_2012 <- subset(tabela_2012, select = c("Country", "Restaurant Price Index"))

tabela_restaurant_price_2013 <- subset(tabela_2013, select = c("Country", "Restaurant Price Index"))

tabela_restaurant_price_2014 <- subset(tabela_2014, select = c("Country", "Restaurant Price Index"))

tabela_restaurant_price_2015 <- subset(tabela_2015, select = c("Country", "Restaurant Price Index"))




# ZDRUŽEVANJE TABEL PO LETIH

# Uvožene in obdelane skrajšane tabele združimo za posamezno leto (od 2012 do 2015).
# Najprej združujemo 5 tabel z indeksi:
# traffic index, quality of life index, pollution index, health care index, crime index.


# Leto 2012

leto_2012_1 <- join_all(list(tabela_traffic_2012,tabela_quality_of_life_2012,
                             tabela_pollution_2012, tabela_health_care_2012,tabela_crime_2012),
                    by = NULL, type = 'full')

# Leto 2013

leto_2013_1 <- join_all(list(tabela_traffic_2013,tabela_quality_of_life_2013,
                             tabela_pollution_2013, tabela_health_care_2013,tabela_crime_2013),
                    by = NULL, type = 'full')

# Leto 2014

leto_2014_1 <- join_all(list(tabela_traffic_2014,tabela_quality_of_life_2014,
                             tabela_pollution_2014, tabela_health_care_2014,tabela_crime_2014),
                    by = NULL, type = 'full')

# Leto 2015

leto_2015_1 <- join_all(list(tabela_traffic_2015,tabela_quality_of_life_2015,
                             tabela_pollution_2015, tabela_health_care_2015,tabela_crime_2015),
                    by = NULL, type = 'full')




# Sedaj pa te združene tabele združimo še s preostalimi (dobljenimi iz razpredelnic:
# tabela_2012, tabela_2013, tabela_2014, tabela_2015).
# S tem dobimo tabele, ki jih bomo kasneje uporabili za analize,
# zato te tabele tudi posebej shranimo.


# Leto 2012

leto_2012 <- join_all(list(leto_2012_1, tabela_rent_2012, tabela_CPI_2012, tabela_groceries_2012,
                       tabela_restaurant_price_2012),
                    by = NULL, type = 'full')
leto_2012$Year <- as.numeric(2012)        # Dodamo stolpec Year, ki v vsaki vrstici zapiše leto.
write.csv2(leto_2012, "podatki/leto_2012", fileEncoding = "UTF-8")


# Leto 2013

leto_2013 <- join_all(list(leto_2013_1, tabela_rent_2013, tabela_CPI_2013, tabela_groceries_2013,
                           tabela_restaurant_price_2013),
                      by = NULL, type = 'full')
leto_2013$Year <- as.numeric(2013)        # Dodamo stolpec Year, ki v vsaki vrstici zapiše leto.
write.csv2(leto_2013, "podatki/leto_2013", fileEncoding = "UTF-8")


# Leto 2014

leto_2014 <- join_all(list(leto_2014_1, tabela_rent_2014, tabela_CPI_2014, tabela_groceries_2014,
                           tabela_restaurant_price_2014),
                      by = NULL, type = 'full')
leto_2014$Year <- as.numeric(2014)        # Dodamo stolpec Year, ki v vsaki vrstici zapiše leto.
write.csv2(leto_2014, "podatki/leto_2014", fileEncoding = "UTF-8")


# Leto 2015

leto_2015 <- join_all(list(leto_2015_1, tabela_rent_2015, tabela_CPI_2015, tabela_groceries_2015,
                           tabela_restaurant_price_2015),
                      by = NULL, type = 'full')
leto_2015$Year <- as.numeric(2015)        # Dodamo stolpec Year, ki v vsaki vrstici zapiše leto.
write.csv2(leto_2015, "podatki/leto_2015", fileEncoding = "UTF-8")




# Na novo narejene tabele damo v eno, tako da jih ne združujemo,
# ampak jih zapišemo eno pod drugo v veliko tabelo,
# nato pa spravimo v tidy.data.

tabela <- rbind(leto_2012, leto_2013, leto_2014, leto_2015)

tabela_tidy <- melt(tabela, id = c("Country", "Year"))
tabela_tidy[,4] <- as.numeric(tabela_tidy[,4])



# Preimenujemo imena stolpcev value in variable.

tabela_tidy <- rename(tabela_tidy, Index_Value = value, Index = variable)



