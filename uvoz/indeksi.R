indeksi <- function(tabela1, tabela2, tabela3, tabela4, tabela5, tabela6, tabela7, tabela8, tabela9,
                    shrani, ime_tabele){
  tabela <- merge(tabela1, tabela2, tabela3, tabela4, tabela5, tabela6, tabela7, 
                      tabela8, tabela9)
  tabela <- tabela[,-1]
  kategorije <- c(names(tabela))[-1]
  write.csv2(ime_tabele, shrani, fileEncoding = "UTF-8")
  return (tabela)
}

