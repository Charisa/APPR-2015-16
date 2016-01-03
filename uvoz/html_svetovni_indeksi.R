html_svetovni_indeksi <- function(shrani, link, leto){
  povezava = getURL(link)
  tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
  names(tables)
  tabela = tables[[3]]
  tabela <- tabela[,-1]
  kategorije <- c(names(tabela))[-1]
  
  kategorije
  
  tabela <- data.frame(tabela[, -1],
                       row.names = tabela[, 1])
  colnames(tabela) <- kategorije                          
  indx <- sapply(tabela, is.factor)                     
  tabela[indx] <- lapply(tabela[indx],
                         function(x) as.numeric(gsub("[.]", ".", x)))
  tabela <- tabela[seq(-2, -length(kategorije))]
  
  write.csv2(tabela, shrani, fileEncoding = "UTF-8")
  return (tabela)
  
}