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

stolpci <- c("januar2015.december2014","januar2015.januar2014","povprečje_januar_2015.povprečje_januar_2014","povprečna12-mesečnarast",
             "februar2015.januar2015","februar2015.februar2014",
             "povprečje_januar,februar_2015.povprečje_januar,februar_2014","povprečna12-mesečnarast",
             "marec2015.februar2015","marec2015.marec2014",
             "povprečje_januar-marec_2015.povprečje_januar-marec_2014","povprečna12-mesečnarast",
             "april2015.marec2015","april2015.april2014",
             "povprečje_januar-april_2015.povprečje_januar-april_2014","povprečna12-mesečnarast",
             "maj2015.april2015","maj2015.maj2014",
             "povprečje_januar-maj_2015.povprečje_januar-maj_2014","povprečna12-mesečnarast",
             "junij2015.maj2015","junij2015.junij2014",
             "povprečje_januar-junij_2015.povprečje_januar-junij_2014","povprečna12-mesečnarast",
             "julij2015.junij2015","julij2015.julij2014",
             "povprečje_januar-julij_2015.povprečje_januar-julij_2014","povprečna12-mesečnarast",
             "avgust2015.julij2015","avgust2015.avgust2014",
             "povprečje_januar-avgust_2015.povprečje_januar-avgust_2014","povprečna12-mesečnarast",
             "september2015.avgust2015","september2015.september2014",
             "povprečje_januar-september_2015.povprečje_januar-september_2014","povprečna12-mesečnarast",
             "oktober2015.september2015","oktober2015.oktober2014",
             "povprečje_januar-oktober_2015.povprečje_januar-oktober_2014","povprečna12-mesečnarast",
             "november2015.oktober2015","november2015.oktober2014",
             "povprečje_januar-november_2015.povprečje_januar-november_2014","povprečna12-mesečnarast")

razpredelnica = read.csv2("podatki/indeksi_cen.csv", fileEncoding = "Windows-1250")
razpredelnica <- data.frame(razpredelnica[, -1], row.names = razpredelnica[, 1])
rownames(razpredelnica) <- gsub("^[0-9]+ ", "", rownames(razpredelnica))
colnames(razpredelnica) <- stolpci
indx <- sapply(razpredelnica, is.factor)
razpredelnica[indx] <- lapply(razpredelnica[indx], function(x) as.numeric(gsub("[.]", ".", x)))
write.csv2(razpredelnica, "podatki/razpredelnica.csv", fileEncoding = "UTF-8", row.names = FALSE)

razpredelnica <- razpredelnica[seq(-4, -length(stolpci), by = -4)]

grupiranje <- c("januar2015.december2014","februar2015.januar2015",
                "marec2015.februar2015","april2015.marec2015",
                "maj2015.april2015","junij2015.maj2015",
                "julij2015.junij2015","avgust2015.julij2015",
                "september2015.avgust2015","oktober2015.september2015",
                "november2015.oktober2015")

# NOVA TABELA osnovne_dobrine: spreminjanje indeksa 16-ih dobrin/storitev v letu 2015 (po mesecih)

osnovne_dobrine <- subset(razpredelnica, select = grupiranje)

osnovne_dobrine <- osnovne_dobrine[-c(2:20, 22:27, 29:32, 34:37, 39:57, 59:65, 67:84, 
                                    86:104, 106:109, 111:118, 120:123, 126:128),]

# Največja in najmanjša vrednost tabele osnovne_dobrine (iz katerih bo sestavljen graf)

najvecja_vrednost <- max(osnovne_dobrine$januar2015.december2014)
najmanjsa_vrednost <- min(osnovne_dobrine$januar2015.december2014)


x <- rownames(osnovne_dobrine)[osnovne_dobrine$januar2015.december2014 == najvecja_vrednost]
y <- rownames(osnovne_dobrine)[osnovne_dobrine$januar2015.december2014 == najmanjsa_vrednost]

group <- c(x, y)

zacetna_vrednost <- c(najvecja_vrednost, najmanjsa_vrednost)
meseci <- names(osnovne_dobrine)

vrstica_najvecje_vrednosti <- as.numeric(as.vector(osnovne_dobrine[13,]))
vrstica_najmanjse_vrednosti <- as.numeric(as.vector(osnovne_dobrine[2,]))

vrstici <- c(vrstica_najvecje_vrednosti, vrstica_najmanjse_vrednosti)





# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
