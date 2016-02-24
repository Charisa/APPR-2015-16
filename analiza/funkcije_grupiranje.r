source("lib/uvozi.zemljevid.r", encoding = "UTF-8")


# Uvozimo zemljevid sveta

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                        "ne_50m_admin_0_countries", encoding = "Windows-1252")


grupiranje <- function(tabelica, stevilo_kategorij, naslov){
  tabelek <- data.frame(tabelica, row.names = tabelica[, 1])
  
  
  tabelek.norm <- scale(tabelek$`Quality of Life Index`)
  k <- kmeans(tabelek.norm, stevilo_kategorij, 1000)
  drzave <- tabelek$Country
  m <- match(svet$name_long, drzave)
  
  svet$skupina <<- factor(k$cluster[drzave[m]])
  sv <- pretvori.zemljevid(svet)
  l <- levels(svet$skupina)
  
  print(ggplot(sv, aes(x = long, y = lat, group = group, fill = skupina))  
        + geom_polygon(color = "Grey")
        + scale_fill_manual(name = 'Categories', limits = c('1', '2', '3', '4'),
                            values = c('1' = 'Red', '3' = 'Blue', '4' = 'Green', '2' = 'Yellow'))
        + theme_nothing(legend = TRUE)
        + labs(title = naslov))
  return(k$size)
  
}



