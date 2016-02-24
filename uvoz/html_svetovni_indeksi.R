html_svetovni_indeksi <- function(shrani, link){

  povezava = getURL(link)
  tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
  names(tables)
  tabela <- tables[[3]]
  tabela <- tabela[,c(-1,-length(names(tabela)))]
  kategorije <- c(names(tabela)) # [-1]
  
  kategorije

  
  colnames(tabela) <- kategorije                          
  indx <- sapply(tabela, is.factor)

  tabela <- tabela[c(1:2)]
  tabela[2] <- as.character(tabela[,2])
  tabela[2] <- as.numeric(tabela[,2])
  #write.csv2(tabela, shrani, fileEncoding = "UTF-8")
  return (tabela)
  
}




