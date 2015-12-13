# 2. faza: Uvoz podatkov

library(XML)
library(RCurl)

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
write.csv2(razpredelnica, "podatki/razpredelnica.csv", fileEncoding = "UTF-8")

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
