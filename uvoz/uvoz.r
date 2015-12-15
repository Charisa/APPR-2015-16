# 2. faza: Uvoz podatkov

library(XML)
library(RCurl)
library(ggplot2)
library(dplyr)

# RAZPREDELNICA (HTML).

povezava = getURL("http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2015-mid&region=150")
tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
names(tables)
tabela = tables[[3]]
tabela <- tabela[,-1]
kategorije <- c(names(tabela))[-1] # Vektor imen stolpcev; c je vektor; names(tmp) - imena stolpcev
                                # [-1] - se znebimo Country
kategorije

tabela <- data.frame(tabela[, -1], row.names = tabela[, 1]) # Znebimo se prejšnjega stolpca (držav), vsem
                  # vrsticam (row) damo vektor vseh imen. Čeprav smo ga že odstranli, ga lahko
                  # spet uporabimo, ker smo naredili vse v enem koraku
colnames(tabela) <- kategorije   # Stolpcem damo imena po kategorijah.
write.csv2(tabela, "podatki/tabela.csv", fileEncoding = "UTF-8")

indx <- sapply(tabela, is.factor)
tabela[indx] <- lapply(tabela[indx], function(x) as.numeric(gsub("[.]", ".", x)))


# RAZPREDELNICA (CSV)

# Poimenovanje stolpcev:

stolpci <- c("januar 2015 / december 2014", "januar 2015 / januar 2014",
             "povprečje (januar) 2015 / povprečje (januar) 2014", "povprečna 12-mesečna rast",
             "februar 2015 / januar 2015", "februar 2015 / februar 2014",
             "povprečje (januar, februar) 2015 / povprečje (januar, februar) 2014", "povprečna 12-mesečna rast",
             "marec 2015 / februar 2015", "marec 2015 / marec 2014",
             "povprečje (januar-marec) 2015 / povprečje (januar-marec) 2014", "povprečna 12-mesečna rast",
             "april 2015 / marec 2015", "april 2015 / april 2014",
             "povprečje (januar-april) 2015 / povprečje (januar-april) 2014", "povprečna 12-mesečna rast",
             "maj 2015 / april 2015", "maj 2015 / maj 2014",
             "povprečje (januar-maj) 2015 / povprečje (januar-maj) 2014", "povprečna 12-mesečna rast",
             "junij 2015 / maj 2015", "junij 2015 / junij 2014",
             "povprečje (januar-junij) 2015 / povprečje (januar-junij) 2014", "povprečna 12-mesečna rast",
             "julij 2015 / junij 2015", "julij 2015 / julij 2014",
             "povprečje (januar-julij) 2015 / povprečje (januar-julij) 2014", "povprečna 12-mesečna rast",
             "avgust 2015 / julij 2015", "avgust 2015 / avgust 2014",
             "povprečje (januar-avgust) 2015 / povprečje (januar-avgust) 2014", "povprečna 12-mesečna rast",
             "september 2015 / avgust 2015", "september 2015 / september 2014",
             "povprečje (januar-september) 2015 / povprečje (januar-september) 2014", "povprečna 12-mesečna rast",
             "oktober 2015 / september 2015", "oktober 2015 / oktober 2014",
             "povprečje (januar-oktober) 2015 / povprečje (januar-oktober) 2014", "povprečna 12-mesečna rast",
             "november 2015 / oktober 2015", "november 2015 / oktober 2014",
             "povprečje (januar-november) 2015 / povprečje (januar-november) 2014", "povprečna 12-mesečna rast")

razpredelnica = read.csv2("podatki/indeksi_cen.csv", fileEncoding = "Windows-1250")
razpredelnica <- data.frame(razpredelnica[, -1], row.names = razpredelnica[, 1])
rownames(razpredelnica) <- gsub("^[0-9]+ ", "", rownames(razpredelnica))
colnames(razpredelnica) <- stolpci
indx <- sapply(razpredelnica, is.factor)
razpredelnica[indx] <- lapply(razpredelnica[indx], function(x) as.numeric(gsub("[.]", ".", x)))
write.csv2(razpredelnica, "podatki/razpredelnica.csv", fileEncoding = "UTF-8", row.names = FALSE)

razpredelnica <- razpredelnica[seq(-4, -length(stolpci), by = -4)]

grupiranje <- c("januar 2015 / december 2014", "februar 2015 / januar 2015",
                "marec 2015 / februar 2015", "april 2015 / marec 2015",
                "maj 2015 / april 2015", "junij 2015 / maj 2015",
                "julij 2015 / junij 2015", "avgust 2015 / julij 2015",
                "september 2015 / avgust 2015", "oktober 2015 / september 2015",
                "november 2015 / oktober 2015")

# NOVA TABELA osnovne_dobrine: spreminjanje indeksa 16-ih dobrin/storitev v letu 2015 (po mesecih)

osnovne_dobrine <- subset(razpredelnica, select = grupiranje)

osnovne_dobrine <- osnovne_dobrine[-c(2:20, 22:27, 29:32, 34:37, 39:57, 59:65, 67:84, 
                                    86:104, 106:109, 111:118, 120:123, 126:128),]

# Histogram tabele osnovne_dobrine v zadnjem mesecu / predzaden mesec

graf <- ggplot(osnovne_dobrine, aes(x = rownames(osnovne_dobrine), y = "november 2015 / oktober 2015")) + 
  geom_line(binwidth = 0.001, color = "red", fill = "white")




#graf <- ggplot(osnovne_dobrine$`november 2015 / oktober 2015`, geom = 'histogram')
#graf <- ggplot(data = osnovne_dobrine, aes(x = created_at)) + 
 # geom_bar(aes(fill=..count..), alpha=0.5, size=0.5, binwidth=60*5) 
#hgraf <- ggplot(data = osnovne_dobrine, aes(osnovne_dobrine$`november 2015 / oktober 2015`), binwidth = length(rownames(osnovne_dobrine)), size = 0.5)
#hgraf + (geom_histogram())
#hgraf + aes(x = rownames(osnovne_dobrine), y = osnovne_dobrine$`november 2015 / oktober 2015`)


# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
