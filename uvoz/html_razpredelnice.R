html_razpredelnice <- function(shrani, link){
  povezava = getURL(link)
  tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
  names(tables)
  tabela = tables[[3]]
  tabela <- tabela[,-1]
  kategorije <- c(names(tabela))[-1]                      # Vektor imen stolpcev; c je vektor; names(tmp) - imena stolpcev
                                                          # [-1] - se znebimo Country
  kategorije
  
  tabela <- data.frame(tabela[, -1],
                       row.names = tabela[, 1])           # Znebimo se prejšnjega stolpca (držav), vsem
                                                          # vrsticam (row) damo vektor vseh imen. 
                                                          # Čeprav smo ga že odstranli, ga lahko
                                                          # spet uporabimo, ker smo naredili vse v enem koraku
  
  colnames(tabela) <- kategorije                          # Stolpcem damo imena po kategorijah.
  indx <- sapply(tabela, is.factor)                       # "Vrednosti" so faktorji.
  tabela[indx] <- lapply(tabela[indx],
                         function(x) as.numeric(gsub("[.]", ".", x)))
                                                          # Faktorje spremenimo v numerične vrednosti, da lahko
                                                          # z njimi operiramo in rišemo grafe.
  
  
  write.csv2(tabela, shrani, fileEncoding = "UTF-8")      # Ustvarimo datoteko .csv.
  return (tabela)
}

