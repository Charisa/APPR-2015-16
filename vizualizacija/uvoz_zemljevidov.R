# Funkcija za uvoz zemljevidov za prikaz posameznih indeksov v letu 2015.

uvoz_zemljevidov <- function(tabela, ime_stolpca, ime_zemljevida, barva = "RdPu"){
  classIntervals <- function(var, n, style = "hclust", rtimes = 3){
    if (any(is.na(var)))                      # warning("Podatki niso na voljo za vse države.")
    error = NULL
    }
                                              # Ime_stolpca: ime stolpca v tabeli
                                              # Tabela: leto_2015
                                              # ime_zemljevida: "zemljevid_indeks"
  
  ime <- joinCountryData2Map(tabela, joinCode = "NAME", nameJoinColumn = "Country", 
                             mapResolution = "coarse")
  classInt <- classIntervals(ime[[ime_stolpca]], 7)
  mapParams <- mapCountryData(ime, nameColumnToPlot = ime_stolpca, catMethod = "fixedWidth",
                              missingCountryCol = "grey", oceanCol = "lightblue", addLegend = FALSE, 
                              colourPalette = brewer.pal(7,barva))
  legenda <- do.call(addMapLegend, c(mapParams,legendLabels = "all", legendWidth = 0.5, 
                                     legendIntervals = "data",legendMar = 2))
  zemljevid <- (legenda)
  return (zemljevid)
}


# Funkcija za risanje zemljevidov v aplikaciji shiny.

vrni_zemljevid <- function(indeks, leto){
  tabelica <- tabela
  tabelica$CODE <- NULL
  indeks = which(colnames(tabelica) == indeks)
  tabelica <- filter(tabelica, Year %in% leto)
  
  tabelica <- tabelica[,c(1, indeks)]
  tabelica$index <- tabelica[,2]
  
  crte_1 <- list(color = toRGB("grey"), width = 2)
  gr <- list(showcoastlines = TRUE, projection = list(type = 'mercator'), resolution = "100",
            scope = "world", showland = "boolean")
  
  poker <- plot_ly(tabelica, z = index, text = Country, locations = Country,
                       type = 'choropleth',locationmode="country names", color = index, colors = "Blues", 
                       marker = list(line = crte_1), showscale = FALSE) %>%
    layout(title = paste(colnames(tabelica)[2], leto), geo = g)
  return(poker)
}