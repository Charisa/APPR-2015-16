# 2. faza: Uvoz podatkov

library(XML)
library(RCurl)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)


# RAZPREDELNICA za graf(CSV)

# Poimenovanje stolpcev:

stolpci <- c("januar2015.december2014","januar2015.januar2014","povprečje_januar_2015.povprečje_januar_2014",
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
razpredelnica <- data.frame(razpredelnica[, -1], row.names = razpredelnica[, 1])
# Izbrišemo stolpec z imeni dobrin/storitev
# in preimenujemo imena vrstic v ta stolpec.
rownames(razpredelnica) <- gsub("^[0-9]+ ", "", rownames(razpredelnica))
# Odstranimo številke v imenih vrstic
colnames(razpredelnica) <- stolpci                                # Preimenujemo imena stolpcev z imeni iz vektorja stolpci.
indx <- sapply(razpredelnica, is.factor)                          # 'Vrednosti' so faktorji. 
razpredelnica[indx] <- lapply(razpredelnica[indx], function(x) as.numeric(gsub("[.]", ".", x)))
# Faktorje spremenimo v numerične vrednosti.

razpredelnica <- razpredelnica[seq(-4, -length(stolpci), by = -4)]# Odstranimo vsak četrti stolpec.

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


# NOVA TABELA ZA GRAF osnovne_dobrine_graf: 4 dobrine, spremembe v prvih in zadnjh dveh mesecih

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


# RISANJE GRAFA

graf <- ggplot(osnovne_dobrine_graf, stat = "identity", main = "Indeksi cen") + 
  aes(x = Var1, y = value, fill = Var2, stat = "identity") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.5)) +
  geom_bar(stat = "identity", position = "dodge") + xlab("obdobja") + ylab("vrednost indeksa") + 
  scale_fill_manual("Dobrine in storitve", values = c("darkred", "darkblue", "yellow", "darkgreen"))











# RAZPREDELNICE (HTML).

# Uvožene razpredelnice s pomočjo funkcije html_razpredelnice, kateri podamo,
# kam shrani novo tabelo in iz katere spletne strani jo uvozimo.
# Osnovne tabele od leta 2009 do leta 2015 (indeksi: 1. CPI, 2. Rent index,
# 3. CPI + rent index, 4. groceries index, 5. restaurant price index,
# 6. local purchasing power index)

source("uvoz/html_razpredelnice.R", encoding = "UTF-8")

tabela_2012 <- html_razpredelnice("podatki/tabela_2012",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2012")

tabela_2013 <- html_razpredelnice("podatki/tabela_2013",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2013")

tabela_2014 <- html_razpredelnice("podatki/tabela_2014", 
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2014")

tabela_2015 <- html_razpredelnice("podatki/tabela_2015",
                                  "http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2015")


# Izbrane razpredelnice po določenih indeksih in letih

source("uvoz/html_svetovni_indeksi.R", encoding = "UTF-8")

# TRAFFIC index od leta 2012 do 2015

tabela_traffic_2012 <- html_svetovni_indeksi("podatki/tabela_traffic_2012.",
                                             "http://www.numbeo.com/traffic/rankings_by_country.jsp?title=2012-Q1",
                                             "2012")

tabela_traffic_2013 <- html_svetovni_indeksi("podatki/tabela_trafiic_2013",
                                             "http://www.numbeo.com/traffic/rankings_by_country.jsp?title=2013-Q1",
                                             "2013")

tabela_traffic_2014 <- html_svetovni_indeksi("podatki/tabela_traffic_2014",
                                             "http://www.numbeo.com/traffic/rankings_by_country.jsp?title=2014",
                                             "2014")

tabela_traffic_2015 <- html_svetovni_indeksi("podatki/tabela_traffic_2015",
                                             "http://www.numbeo.com/traffic/rankings_by_country.jsp?title=2015",
                                             "2015")

# CRIME index od leta 2012 do 2015

tabela_crime_2012 <- html_svetovni_indeksi("podatki/tabela_crime_2012",
                                           "http://www.numbeo.com/crime/rankings_by_country.jsp?title=2012-Q1",
                                           "2012")

tabela_crime_2013 <- html_svetovni_indeksi("podatki/tabela_crime_2013",
                                           "http://www.numbeo.com/crime/rankings_by_country.jsp?title=2013-Q1",
                                           "2013")

tabela_crime_2014 <- html_svetovni_indeksi("podatki/tabela_crime_2014",
                                           "http://www.numbeo.com/crime/rankings_by_country.jsp?title=2014",
                                           "2014")

tabela_crime_2015 <- html_svetovni_indeksi("podatki/tabela_crime_2015",
                                           "http://www.numbeo.com/crime/rankings_by_country.jsp?title=2015",
                                           "2015")

# HEALTH CARE index od leta 2012 do 2015

tabela_health_care_2012 <- html_svetovni_indeksi("podatki/tabela_health_care_2012",
                                                 "http://www.numbeo.com/health-care/rankings_by_country.jsp?title=2012-Q1",
                                                 "2012")

tabela_health_care_2013 <- html_svetovni_indeksi("podatki/tabela_health_care_2013",
                                                 "http://www.numbeo.com/health-care/rankings_by_country.jsp?title=2013-Q1",
                                                 "2013")

tabela_health_care_2014 <- html_svetovni_indeksi("podatki/tabela_health_care_2014",
                                                 "http://www.numbeo.com/health-care/rankings_by_country.jsp?title=2014",
                                                 "2014")

tabela_health_care_2015 <- html_svetovni_indeksi("podatki/tabela_health_care_2015",
                                                 "http://www.numbeo.com/health-care/rankings_by_country.jsp?title=2015",
                                                 "2015")

# POLLUTION index od leta 2012 do 2015

tabela_pollution_2012 <- html_svetovni_indeksi("podatki/tabela_pollution_2012",
                                               "http://www.numbeo.com/pollution/rankings_by_country.jsp?title=2012-Q1",
                                               "2012")

tabela_pollution_2013 <- html_svetovni_indeksi("podatki/tabela_pollution_2013",
                                               "http://www.numbeo.com/pollution/rankings_by_country.jsp?title=2013-Q1",
                                               "2013")

tabela_pollution_2014 <- html_svetovni_indeksi("podatki/tabela_pollution_2014",
                                               "http://www.numbeo.com/pollution/rankings_by_country.jsp?title=2014",
                                               "2014")

tabela_pollution_2015 <- html_svetovni_indeksi("podatki/tabela_pollution_2015",
                                               "http://www.numbeo.com/pollution/rankings_by_country.jsp?title=2015",
                                               "2015")

# QUALITY OF LIFE index od leta 2012 do 2015

tabela_quality_of_life_2012 <- html_svetovni_indeksi("podatki/tabela_quality_of_life_2012",
                                                     "http://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2012-Q1",
                                                     "2012")

tabela_quality_of_life_2013 <- html_svetovni_indeksi("podatki/tabela_quality_of_life_2013",
                                                     "http://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2013-Q1",
                                                     "2013")

tabela_quality_of_life_2014 <- html_svetovni_indeksi("podatki/tabela_quality_of_life_2014",
                                                     "http://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014",
                                                     "2014")

tabela_quality_of_life_2015 <- html_svetovni_indeksi("podatki/tabela_quality_of_life_2015",
                                                     "http://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015",
                                                     "2015")

# RENT index od leta 2012 do 2015

tabela_rent_2012 <- subset(tabela_2012, select = c("Rent Index"))
write.csv2(tabela_rent_2012, "podatki/tabela_rent_2012", fileEncoding = "UTF-8")

tabela_rent_2013 <- subset(tabela_2013, select = c("Rent Index"))
write.csv2(tabela_rent_2013, "podatki/tabela_rent_2013", fileEncoding = "UTF-8")

tabela_rent_2014 <- subset(tabela_2014, select = c("Rent Index"))
write.csv2(tabela_rent_2014, "podatki/tabela_rent_2014", fileEncoding = "UTF-8")

tabela_rent_2015 <- subset(tabela_2015, select = c("Rent Index"))
write.csv2(tabela_rent_2015, "podatki/tabela_rent_2015", fileEncoding = "UTF-8")


# CPI od leta 2012 do 2015

tabela_CPI_2012 <- subset(tabela_2012, select = c("Consumer Price Index"))
write.csv2(tabela_CPI_2012, "podatki/tabela_CPI_2012", fileEncoding = "UTF-8")

tabela_CPI_2013 <- subset(tabela_2013, select = c("Consumer Price Index"))
write.csv2(tabela_CPI_2013, "podatki/tabela_CPI_2013", fileEncoding = "UTF-8")

tabela_CPI_2014 <- subset(tabela_2014, select = c("Consumer Price Index"))
write.csv2(tabela_CPI_2014, "podatki/tabela_CPI_2014", fileEncoding = "UTF-8")

tabela_CPI_2015 <- subset(tabela_2015, select = c("Consumer Price Index"))
write.csv2(tabela_CPI_2015, "podatki/tabela_CPI_2015", fileEncoding = "UTF-8")


# GROCERIES index od leta 2012 do 2015

tabela_groceries_2012 <- subset(tabela_2012, select = c("Groceries Index"))
write.csv2(tabela_groceries_2012, "podatki/tabela_groceries_2012", fileEncoding = "UTF-8")

tabela_groceries_2013 <- subset(tabela_2013, select = c("Groceries Index"))
write.csv2(tabela_groceries_2013, "podatki/tabela_groceries_2013", fileEncoding = "UTF-8")

tabela_groceries_2014 <- subset(tabela_2014, select = c("Groceries Index"))
write.csv2(tabela_groceries_2014, "podatki/tabela_groceries_2014", fileEncoding = "UTF-8")

tabela_groceries_2015 <- subset(tabela_2015, select = c("Groceries Index"))
write.csv2(tabela_groceries_2015, "podatki/tabela_groceries_2015", fileEncoding = "UTF-8")


# RESTAURANT PRICE index od leta 2012 do 2015

tabela_restaurant_price_2012 <- subset(tabela_2012, select = c("Restaurant Price Index"))
write.csv2(tabela_restaurant_price_2012, "podatki/tabela_restaurant_price_2012", fileEncoding = "UTF-8")

tabela_restaurant_price_2013 <- subset(tabela_2013, select = c("Restaurant Price Index"))
write.csv2(tabela_restaurant_price_2013, "podatki/tabela_restaurant_price_2013", fileEncoding = "UTF-8")

tabela_restaurant_price_2014 <- subset(tabela_2014, select = c("Restaurant Price Index"))
write.csv2(tabela_restaurant_price_2014, "podatki/tabela_restaurant_price_2014", fileEncoding = "UTF-8")

tabela_restaurant_price_2015 <- subset(tabela_2015, select = c("Restaurant Price Index"))
write.csv2(tabela_restaurant_price_2015, "podatki/tabela_restaurant_price_2015", fileEncoding = "UTF-8")


# ZDRUŽEVANJE tabel po letih

#a <- merge(tabela_traffic_2012, tabela_crime_2012, tabela_health_care_2012,
 #          tabela_pollution_2012, tabela_quality_of_life_2012, tabela_rent_2012,
  #         tabela_CPI_2012, tabela_groceries_2012, tabela_restaurant_price_2012, 
   #        by = , all = TRUE)


#indeksi_2012 <- indeksi(tabela_traffic_2012, tabela_crime_2012, tabela_health_care_2012,
 #                       tabela_pollution_2012, tabela_quality_of_life_2012, tabela_rent_2012,
  #                      tabela_CPI_2012, tabela_groceries_2012, tabela_restaurant_price_2012,
   #                     "podatki/indeksi_2012", indeksi_2012)



#tabela_traffic_2012 <- data.frame(tabela_traffic_2012)
#tabela_quality_of_life_2012 <- data.frame(tabela_quality_of_life_2012)
#tabela_groceries_2012 <- data.frame(tabela_groceries_2012)
#tabela_health_care_2012 <- data.frame(tabela_health_care_2012)

#tabela_traffic_2012$Country <- rownames(tabela_traffic_2012)
#tabela_quality_of_life_2012$Country <- rownames(tabela_quality_of_life_2012)
#tabela_groceries_2012$Country <- rownames(tabela_groceries_2012)
#tabela_health_care_2012$Country <- rownames(tabela_health_care_2012)

#df <- join_all(list(tabela_traffic_2012,tabela_quality_of_life_2012,
 #                   tabela_groceries_2012, tabela_health_care_2012),
  #             by = 'Country', type = 'full')


#s <- c("Country","Consumer Price Index", "Rent Index", "Groceries Index", "Restaurant Price Index")
#r <- list(tabela_2012, tabela_2013, tabela_2014, tabela_2015)
#leta <- 2012:2015
#names(r) <- leta
#sk <- lapply(leta, function(ll)
 # lapply(s, function(ss)
  #  data.frame(leto = ll, index = ss, vrednost = r[[paste0(ll)]][[ss]], stringsAsFactors = FALSE)) %>%
   # bind_rows()
  #) %>% bind_rows()

# k <- lapply(r,function(r) r[s])


