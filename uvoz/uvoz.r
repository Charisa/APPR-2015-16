# 2. faza: Uvoz podatkov

library(XML)
library(RCurl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(plyr)

# RAZPREDELNICA (HTML).

povezava = getURL("http://www.numbeo.com/cost-of-living/rankings_by_country.jsp?title=2015-mid&region=150")
tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
names(tables)
tabela = tables[[3]]
tabela <- tabela[,-1]
kategorije <- c(names(tabela))[-1]                                # Vektor imen stolpcev; c je vektor; names(tmp) - imena stolpcev
                                                                  # [-1] - se znebimo Country
kategorije

tabela <- data.frame(tabela[, -1], row.names = tabela[, 1]) 
                                                                  # Znebimo se prejšnjega stolpca (držav), vsem
                                                                  # vrsticam (row) damo vektor vseh imen. 
                                                                  # Čeprav smo ga že odstranli, ga lahko
                                                                  # spet uporabimo, ker smo naredili vse v enem koraku

colnames(tabela) <- kategorije                                    # Stolpcem damo imena po kategorijah.
write.csv2(tabela, "podatki/tabela.csv", fileEncoding = "UTF-8")
                                                                  # Ustvarimo datoteko .csv.

indx <- sapply(tabela, is.factor)                                 # "Vrednosti" so faktorji.
tabela[indx] <- lapply(tabela[indx], function(x) as.numeric(gsub("[.]", ".", x)))
                                                                  # Faktorje spremenimo v numerične vrednosti, da lahko
                                                                  # z njimi operiramo in rišemo grafe.




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
write.csv2(razpredelnica, "podatki/razpredelnica.csv", fileEncoding = "UTF-8", row.names = FALSE)
                                                                  # Ustvarimo datoteko .csv.

razpredelnica <- razpredelnica[seq(-4, -length(stolpci), by = -4)]# Odstranimo vsak četrti stolpec.

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
  geom_bar(stat = "identity", position = "dodge") + xlab("obdobja") + ylab("vrednost indeksa") + 
  scale_fill_manual("Dobrine in storitve", values = c("darkred", "darkblue", "yellow", "darkgreen"))
































#najvecja_vrednost <- max(osnovne_dobrine$januar2015.december2014)
#najmanjsa_vrednost <- min(osnovne_dobrine$januar2015.december2014)
#x <- rownames(osnovne_dobrine)[osnovne_dobrine$januar2015.december2014 == najvecja_vrednost]
#y <- rownames(osnovne_dobrine)[osnovne_dobrine$januar2015.december2014 == najmanjsa_vrednost]
#group <- c(x, y)
#zacetna_vrednost <- c(najvecja_vrednost, najmanjsa_vrednost)
#meseci <- (names(osnovne_dobrine))[c(1, 2, 3, 9, 10, 11)]
#vrstica_najvecje_vrednosti <- as.numeric(as.vector(osnovne_dobrine[x,]))
#vrstica_najmanjse_vrednosti <- as.numeric(as.vector(osnovne_dobrine[y,]))
#vrstici <- c(vrstica_najvecje_vrednosti, vrstica_najmanjse_vrednosti)
