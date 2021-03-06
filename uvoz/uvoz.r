# 2. faza: Uvoz podatkov

# Knjižnice

source("lib/libraries.r", encoding = "UTF-8")
source("uvoz/iso_kratice.R", encoding = "UTF-8")


# RAZPREDELNICA za graf(CSV)

# Poimenovanje stolpcev:

stolpci <- c("dobrine/storitve","jan15/dec14","jan15/jan14","avg.jan15/avg.jan14",
             "avg.12-month",
             "feb15/jan15","feb15/feb14",
             "avg.jan-feb15/avg.jan-feb14","avg.12-month",
             "mar15/feb15","mar15/mar14",
             "avg.jan-mar15/avg.jan-mar14","avg.12-month",
             "apr15/mar15","apr15/apr14",
             "avg.jan-apr15/avg.jan-apr14","avg.12-month",
             "maj15/apr15","maj15/maj14",
             "avg.jan-maj15/avg.jan-maj14","avg.12-month",
             "jun15/maj15","jun15/jun14",
             "avg.jan-jun15/avg.jan-jun14","avg.12-month",
             "jul15/jun15","jul15/jul14",
             "avg.jan-jul15/avg.jan-jul14","avg.12-month",
             "avgu15/jul15","avgu15/avgu14",
             "avg.jan-avgu15/avg.jan-avgu14","avg.12-month",
             "sep15/avgu15","sep15/sep14",
             "avg.jan-sep15/avg.jan-sep14","avg.12-month",
             "okt15/sep15","okt15/okt14",
             "avg.jan-okt15/avg.jan-okt14","avg.12-month",
             "nov15/okt15","nov15/nov14",
             "avg.jan-nov15/avg.jan-nov14","avg.12-month")

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

izbira <- c("jan15/dec14","feb15/jan15", "mar15/feb15","apr15/mar15",
            "maj15/apr15","jun15/maj15", "jul15/jun15","avgu15/jul15",
            "sep15/avgu15","okt15/sep15", "nov15/okt15")          # Ustvarimo nov vektor z imeni stolpcev, ki jih 
                                                                  # bomo uporabili v novi tabeli.




# NOVA TABELA osnovne_dobrine: spreminjanje indeksa 16-ih dobrin/storitev v letu 2015 (po mesecih)

osnovne_dobrine <- subset(razpredelnica, select = izbira)         # Podrazpredelnica (spremembe po zaporednih mesecih)
osnovne_dobrine <- osnovne_dobrine[-c(2:20, 22:27, 29:32, 34:37, 39:57, 59:65, 67:84, 
                                      86:104, 106:109, 111:118, 120:123, 126:128),]    
                                                                  # Izbrani stolpci originalne razpredelnice
imena_dobrin <- row.names(osnovne_dobrine)                        # V imena vrstic shranimo dobrine/storitve
osnovne_dobrine <- arrange(osnovne_dobrine, desc(`jan15/dec14`), 
                           desc(`feb15/jan15`))          # Ureditev po velikosti (od največje do najmanjše vrednosti
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

dobrine <- subset(razpredelnica, select = izbira)                  # Nova razpredelnica s stolpci iz grupiranja.
dobrine_graf <- subset(dobrine, select = (colnames(osnovne_dobrine))[c(1, 5, 7, 11)])
dobrine_graf <- dobrine_graf[c(7, 21),]
dobrine_graf <- t(dobrine_graf)
dobrine_graf <- melt(dobrine_graf, id = row.names(dobrine_graf))


# Preoblikovanje razpredelnice v tidy.data

tidy <- arrange(razpredelnica,desc(`jan15/dec14`))
razpredelnica_tidy <- melt(tidy, id = "dobrine/storitve")
razpredelnica_tidy <- rename(razpredelnica_tidy, "mesecni indeksi" = variable, "vrednost indeksa" = value)


# Priprava za graf graf_slovenski_indeksi

osnovne <- row.names(osnovne_dobrine)[1:5]
slovenski_indeksi <- filter(razpredelnica_tidy, `dobrine/storitve` %in% osnovne, `mesecni indeksi` %in% izbira)


# RISANJE GRAFOV

graf_osnovnih_dobrin <- ggplot(osnovne_dobrine_graf, stat = "identity", main = "Indeksi cen") + 
  aes(x = Var1, y = value, fill = Var2, stat = "identity") + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  geom_bar(stat = "identity", position = "dodge") + xlab("obdobja") + ylab("vrednost indeksa") + 
  scale_fill_manual("Dobrine in storitve", values = c("darkred", "darkblue", "yellow", "darkgreen"))

graf <- ggplot(dobrine_graf, stat = "identity", main = "Indeksi cen") + 
  aes(x = Var1, y = value, fill = Var2, stat = "identity") + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  geom_bar(stat = "identity", position = "dodge") + xlab("obdobja") + ylab("vrednost indeksa") + 
  scale_fill_manual("Dobrine in storitve", values = c("darkred", "darkblue"))

graf_slovenski_indeksi <- ggplot(data = slovenski_indeksi) + 
  geom_line(aes(x = `mesecni indeksi`, y = `vrednost indeksa`, group = `dobrine/storitve`, color = `dobrine/storitve`)) +
  theme(axis.text.x = element_blank())




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

tabela_2012 <- html_razpredelnice("podatki/tabela_2012.csv",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2012")
tabela_2013 <- html_razpredelnice("podatki/tabela_2013.csv",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2013")
tabela_2014 <- html_razpredelnice("podatki/tabela_2014.csv", 
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2014")
tabela_2015 <- html_razpredelnice("podatki/tabela_2015.csv",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2015")


# Izbrane razpredelnice po določenih indeksih in letih


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

tabela_rent_2012 <- subset(tabela_2012, select = c("Country", "Rent Index", "CODE"))
tabela_rent_2013 <- subset(tabela_2013, select = c("Country", "Rent Index", "CODE"))
tabela_rent_2014 <- subset(tabela_2014, select = c("Country", "Rent Index", "CODE"))
tabela_rent_2015 <- subset(tabela_2015, select = c("Country", "Rent Index", "CODE"))


# CPI od leta 2012 do 2015

tabela_CPI_2012 <- subset(tabela_2012, select = c("Country", "Consumer Price Index", "CODE"))
tabela_CPI_2013 <- subset(tabela_2013, select = c("Country", "Consumer Price Index", "CODE"))
tabela_CPI_2014 <- subset(tabela_2014, select = c("Country", "Consumer Price Index", "CODE"))
tabela_CPI_2015 <- subset(tabela_2015, select = c("Country", "Consumer Price Index", "CODE"))


# GROCERIES index od leta 2012 do 2015

tabela_groceries_2012 <- subset(tabela_2012, select = c("Country", "Groceries Index", "CODE"))
tabela_groceries_2013 <- subset(tabela_2013, select = c("Country", "Groceries Index", "CODE"))
tabela_groceries_2014 <- subset(tabela_2014, select = c("Country", "Groceries Index", "CODE"))
tabela_groceries_2015 <- subset(tabela_2015, select = c("Country", "Groceries Index", "CODE"))


# RESTAURANT PRICE index od leta 2012 do 2015

tabela_restaurant_price_2012 <- subset(tabela_2012, select = c("Country", "Restaurant Price Index", "CODE"))
tabela_restaurant_price_2013 <- subset(tabela_2013, select = c("Country", "Restaurant Price Index", "CODE"))
tabela_restaurant_price_2014 <- subset(tabela_2014, select = c("Country", "Restaurant Price Index", "CODE"))
tabela_restaurant_price_2015 <- subset(tabela_2015, select = c("Country", "Restaurant Price Index", "CODE"))




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
leto_2012 <- leto_2012[, c(1, 2, 4:11, 3)]
leto_2012$Year <- as.numeric(2012)        # Dodamo stolpec Year, ki v vsaki vrstici zapiše leto.
write.csv2(leto_2012, "podatki/leto_2012.csv", fileEncoding = "UTF-8")


# Leto 2013

leto_2013 <- join_all(list(leto_2013_1, tabela_rent_2013, tabela_CPI_2013, tabela_groceries_2013,
                           tabela_restaurant_price_2013),
                      by = NULL, type = 'full')
leto_2013 <- leto_2013[, c(1, 2, 4:11, 3)]
leto_2013$Year <- as.numeric(2013)        # Dodamo stolpec Year, ki v vsaki vrstici zapiše leto.
write.csv2(leto_2013, "podatki/leto_2013.csv", fileEncoding = "UTF-8")


# Leto 2014

leto_2014 <- join_all(list(leto_2014_1, tabela_rent_2014, tabela_CPI_2014, tabela_groceries_2014,
                           tabela_restaurant_price_2014),
                      by = NULL, type = 'full')
leto_2014 <- leto_2014[, c(1, 2, 4:11, 3)]
leto_2014$Year <- as.numeric(2014)        # Dodamo stolpec Year, ki v vsaki vrstici zapiše leto.
write.csv2(leto_2014, "podatki/leto_2014.csv", fileEncoding = "UTF-8")


# Leto 2015

leto_2015 <- join_all(list(leto_2015_1, tabela_rent_2015, tabela_CPI_2015, tabela_groceries_2015,
                           tabela_restaurant_price_2015),
                      by = NULL, type = 'full')
leto_2015 <- leto_2015[, c(1, 2, 4:11, 3)]
leto_2015$Year <- as.numeric(2015)        # Dodamo stolpec Year, ki v vsaki vrstici zapiše leto.
write.csv2(leto_2015, "podatki/leto_2015.csv", fileEncoding = "UTF-8")


# Na novo narejene tabele damo v eno, tako da jih ne združujemo,
# ampak jih zapišemo eno pod drugo v veliko tabelo,
# nato pa spravimo v tidy.data.

tabela <- rbind(leto_2012, leto_2013, leto_2014, leto_2015)
tabela_tidy <- melt(tabela, id = c("Country", "Year"))
tabela_tidy[,4] <- as.numeric(tabela_tidy[,4])
                                          # Preimenujemo imena stolpcev value in variable.
tabela_tidy <- rename(tabela_tidy, Index_Value = value, Index = variable)
                                                                          


# Tabele za Albertov problem

sort_quality_of_life_2015 <- arrange(tabela_quality_of_life_2015, desc(`Quality of Life Index`))
sort_quality_of_life_2015 <- sort_quality_of_life_2015[,-3]


# Izbrano tabelo uvozimo in jo razvrstimo po ceni bivanja (od najvišje do najnižje).
# Uporabimo "if", da tabele ne vsakič uvažamo iz interneta, ampak jo samo preberemo.

if(file.exists("podatki/cost_of_living_cities_Albert.csv")){
  cost_of_living_cities_Albert <- read.csv2("podatki/cost_of_living_cities_Albert.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  names(cost_of_living_cities_Albert) <- base::gsub("\\.", " ", names(cost_of_living_cities_Albert))
}else{
  povezava = getURL("http://www.numbeo.com/cost-of-living/basket_of_goods.jsp?currency=EUR&quantity_itemId_1=30&quantity_itemId_2=4&quantity_itemId_3=3&quantity_itemId_4=&quantity_itemId_5=&quantity_itemId_114=20&quantity_itemId_6=&quantity_itemId_7=&quantity_itemId_8=4&quantity_itemId_9=8&quantity_itemId_115=&quantity_itemId_11=2&quantity_itemId_12=&quantity_itemId_19=4&quantity_itemId_121=2&quantity_itemId_110=1&quantity_itemId_118=10&quantity_itemId_111=1&quantity_itemId_116=2&quantity_itemId_112=2&quantity_itemId_119=1&quantity_itemId_113=3&quantity_itemId_13=3&quantity_itemId_14=2&quantity_itemId_15=&quantity_itemId_16=16&quantity_itemId_17=&quantity_itemId_18=&quantity_itemId_20=&quantity_itemId_107=&quantity_itemId_108=&quantity_itemId_109=&quantity_itemId_24=50&quantity_itemId_25=&quantity_itemId_30=2&quantity_itemId_32=500&quantity_itemId_33=1&quantity_itemId_40=1&quantity_itemId_42=8&quantity_itemId_44=&quantity_itemId_60=2&quantity_itemId_62=&quantity_itemId_64=1&quantity_itemId_66=1&quantity_itemId_26=&quantity_itemId_27=&quantity_itemId_28=1&quantity_itemId_29=&quantity_itemId_100=&quantity_itemId_101=")
  tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
  cost_of_living_cities_Albert = tables[[3]]
  cost_of_living_cities_Albert <- cost_of_living_cities_Albert[,-c(1,4)]            # Izbrišemo odvečna stolpca, vrednosti v stolpcu s 
  # cenami banan spremenimo v class numeric.
  cost_of_living_cities_Albert$`Custom Basket Price` <- as.numeric(as.character(cost_of_living_cities_Albert$`Custom Basket Price`))
  # Uredimo tabelo po vrednostih cen.
  cost_of_living_cities_Albert <- arrange(cost_of_living_cities_Albert, desc(`Custom Basket Price`))
  write.csv2(cost_of_living_cities_Albert, "podatki/cost_of_living_cities_Albert.csv", fileEncoding = "UTF-8", row.names = FALSE)
}



# Tabela cen banan v evropskih državah (razpredelnica .csv).

banane_Albert <- read.csv2("podatki/banane.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
                                                        # Izberemo library "dplyr", iz katere kličemo
                                                        # funkcijo "rename", s katero spremenimo imena stolpcev.
banane_Albert <- dplyr::rename(banane_Albert, Country = X, `Banana_Price (1 kg)` = X.1)
                                                        # Tabela ima še prazne stolpce, zato izberemo samo 
                                                        # tiste z vrednostmi.
banane_Albert <- subset(banane_Albert, select = c(Country, `Banana_Price (1 kg)`))
                                                        # Zadnjemu stolpcu spremenimo class v numeric,
                                                        # za kasnejšo pravilno rabo vrednosti.
banane_Albert$`Banana_Price (1 kg)` <- as.numeric(as.character(banane_Albert$`Banana_Price (1 kg)`))
