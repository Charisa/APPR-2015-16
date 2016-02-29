source("lib/uvozi.zemljevid.r", encoding = "UTF-8")
source("uvoz/iso_kratice.R", encoding = "UTF-8")

# Funkcija za grupiranje podatkov (klicana v analiza/analiza.R)

grupiranje <- function(tabelek, stevilo_kategorij, naslov){
  tabelek <- tabelek[,-3]
  row.names(tabelek) <-tabelek[,1]
  tabelek1 <- tabelek[-1]
  tabelek.tree <- hclust(dist(tabelek1, method = "euclidian"), method = "ward.D2")
  
  # Grupira v dano število kategorij.
  obrezano <- cutree(tabelek.tree, k = stevilo_kategorij)             
  tabelek$skupina <- obrezano
  tabelek$region <- tolower(rownames(tabelek))
  tabelek <- merge(tabelek, iso, by = "Country")
  tabelek$hover <- with(tabelek,paste(`Country`,"<br>", "Index:",`Quality of Life Index`))
  
  l <- list(color = toRGB("grey"), width = 0.5)
  g <- list(showframe = FALSE, projection = list(type = 'Mercator'))
  graf <- plot_ly(data=tabelek, z = `skupina`, text = hover, locations = CODE, type = 'choropleth',
                  color = skupina, colors = 'Blues', marker = list(line = l), showscale = FALSE) %>%
    layout(title = naslov,
           geo = g)
  
  return(graf)
}


# Funkcija za izris grafov (posamezni indeksi in njihova napoved za izbrane države v
# vektorju drzave_10) v shiny-ju.

indeks_graf <- function(indeks){
  tabelek <- subset(tabela_indeksov_10, select = c("Country", "Year", indeks))
  colnames(tabelek)[3] <- "Index"
  for (i in unique(tabelek$Country)){
    indeksiranje <- napovedovanje(i, indeks)
    tabelek <- rbind(tabelek, indeksiranje)
  }
  grafica <- plot_ly(tabelek, x = Year, y = Index, name = "index", 
                     line = list(shape = "spline"), color = Country)
  return(grafica)
}


napovedovanje <- function(drzava, indeks){
  tabelek <- subset(tabela_indeksov_10, select = c("Country", "Year", indeks))
  colnames(tabelek)[3] <- "Index"
  tabelek <- dplyr::filter(tabelek, Country %in% drzava)
  tabelek.lo <- loess(Index ~ Year, tabelek, control = loess.control(surface = "direct"))
  Index <- predict(tabelek.lo, data.frame(Year = 2016))
  tabelcica <- data.frame(Index)
  tabelcica$Country <- drzava
  tabelcica$Year <- 2016
  return(tabelcica)
}