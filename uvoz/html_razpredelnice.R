# V sledečih funkcijah uporabimo if stavek, ki nam omogoči,
# da tabelo shranimo le enkrat, prvič, ne pa vedno, ko zaženemo program.
# To nam omogoči manjšo časovno zahtevnost programa.



# Funkcija za uvoz HTML razpredelnic (uporabljena v uvozu).

html_razpredelnice <- function(shrani, link){
  if(file.exists(shrani)){
    tabela <- read.csv2(shrani, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(tabela) <- base::gsub("\\.", " ", names(tabela))# Poskrbimo, da so imena stolpcev ista (pike 
                                                            # zamenjamo s prvotnimi presledki)
  }else{
    povezava = getURL(link)
    tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
    names(tables)
    tabela = tables[[3]]
    tabela <- tabela[,-1]
    kategorije <- c(names(tabela))                          # Vektor imen stolpcev; c je vektor; names(tmp) - imena stolpcev
                                                            # [-1] - se znebimo Country
    
    colnames(tabela) <- kategorije                          # Stolpcem damo imena po kategorijah.
    indx <- sapply(tabela, is.factor)                       # "Vrednosti" so faktorji.
    tabela[,-1] <- lapply(tabela[,-1],
                          function(x) as.numeric(gsub("[.]", ".", x)))
                                                            # Faktorje od 2. do končnega stolpca
                                                            # spremenimo v numerične vrednosti, da lahko
                                                            # z njimi operiramo in rišemo grafe.
    tabela <- merge(tabela, ISO)
    write.csv2(tabela, shrani, fileEncoding = "UTF-8", row.names = FALSE)
  }
  return (tabela)
}


# Funkcija za uvoz tabel svetovnih indeksov (uporabljena v uvozu).

html_svetovni_indeksi <- function(shrani, link){
  shrani <- paste0(shrani, ".csv")
  if(file.exists(shrani)){
    tabela <- read.csv2(shrani, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    names(tabela) <- base::gsub("\\.", " ", names(tabela))
  }else{
    povezava = getURL(link)
    tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
    names(tables)
    tabela <- tables[[3]]
    tabela <- tabela[,c(-1,-length(names(tabela)))]
    kategorije <- c(names(tabela)) 
    kategorije
    colnames(tabela) <- kategorije                          
    indx <- sapply(tabela, is.factor)
    tabela <- tabela[c(1:2)]
    tabela[2] <- as.character(tabela[,2])
    tabela[2] <- as.numeric(tabela[,2])
    tabela <- merge(tabela, ISO)                              # Tabeli dodamo ISO kratice držav.
    write.csv2(tabela, shrani, fileEncoding = "UTF-8", row.names = FALSE)
  }
  return (tabela)
}
