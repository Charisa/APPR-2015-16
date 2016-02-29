# Funkcija za uvoz HTML razpredelnic


html_razpredelnice <- function(shrani, link){
  #if (file.exists(shrani)){                              
   #read.csv2(shrani, row.names = 1)                      
  #}
  povezava = getURL(link)
  tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
  names(tables)
  tabela = tables[[3]]
  tabela <- tabela[,-1]
  kategorije <- c(names(tabela)) #[-1]                    # Vektor imen stolpcev; c je vektor; names(tmp) - imena stolpcev
                                                          # [-1] - se znebimo Country
  kategorije
  
  #tabela <- data.frame(tabela[, -1],
                       #row.names = tabela[, 1])          # Znebimo se prejšnjega stolpca (držav), vsem
                                                          # vrsticam (row) damo vektor vseh imen. 
                                                          # Čeprav smo ga že odstranli, ga lahko
                                                          # spet uporabimo, ker smo naredili vse v enem koraku
  
  colnames(tabela) <- kategorije                          # Stolpcem damo imena po kategorijah.
  indx <- sapply(tabela, is.factor)                       # "Vrednosti" so faktorji.
  tabela[,-1] <- lapply(tabela[,-1],
                         function(x) as.numeric(gsub("[.]", ".", x)))
                                                          # Faktorje od 2. do končnega stolpca
                                                          # spremenimo v numerične vrednosti, da lahko
                                                          # z njimi operiramo in rišemo grafe.
  tabela <- merge(tabela, ISO)
  #write.csv2(tabela, shrani, fileEncoding = "UTF-8")     # Ustvarimo datoteko .csv.
  return (tabela)
}




# Funkcija za uvoz razpredelnice Very High Human Development Index

html_stolpci <- c('Country', 'HDI 2014', 'Change from previous year')

html_razpredelnica <- function(link, t, vrstice){
  setInternet2(use = TRUE)
  d <<- download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
  povezava = getURL(link, cainfo="cacert.pem")
  tables = readHTMLTable(povezava, fileEncoding = 'UTF-8')
  names(tables)
  tabelca = tables[[t]]                                   # Vzamemo t-to tabelo iz spletne strani.
  tabelca <- tabelca[,c(3:5)]
  tabelca <- tabelca[-vrstice,]
  colnames(tabelca) <- html_stolpci
  indx <- sapply(tabelca, is.factor)
  tabelca[,-1] <- lapply(tabelca[,-1],                    # Faktorje v 2. in 3. stolpcu spremenimo 
                                                          # v numerične vrednosti.
                        function(x) as.numeric(gsub('[.]', '.', x)))  
  rownames(tabelca) <- seq(length = nrow(tabelca))        # Osvežimo rownames, da so znova oštevilčene po vrsti.
  
  
  return (tabelca)
  
}