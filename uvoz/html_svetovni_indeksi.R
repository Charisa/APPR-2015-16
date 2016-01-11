html_svetovni_indeksi <- function(shrani, link){
  #if (file.exists(shrani)){                              
    #read.csv2(shrani, row.names = 1)                      
  #}
  povezava = getURL(link)
  tables = readHTMLTable(povezava, fileEncoding = "UTF-8")
  names(tables)
  tabela = tables[[3]]
  tabela <- tabela[c(-1,-length(names(tabela)))]
  kategorije <- c(names(tabela)) # [-1]
  
  kategorije
  
  #tabela <- data.frame(tabela[, -1],
  #                    row.names = tabela[, 1])
  
  colnames(tabela) <- kategorije                          
  indx <- sapply(tabela, is.factor)                     
  #tabela[,-1] <- lapply(tabela[,-1],
  #                      function(x) as.numeric(gsub("[.]", ".", x)))
  #tabela <- tabela[seq(-3, -length(kategorije))]
  tabela <- tabela[c(1:2)]
  tabela[2] <- as.numeric(tabela[,2])
  #write.csv2(tabela, shrani, fileEncoding = "UTF-8")
  return (tabela)
  
}