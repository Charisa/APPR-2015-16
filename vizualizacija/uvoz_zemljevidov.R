uvoz_zemljevidov <- function(tabela, ime_stolpca, ime_zemljevida, barva = "RdPu"){
  classIntervals <- function(var, n, style = "hclust", rtimes = 3){
    if (any(is.na(var))) 
    # warning("Podatki niso na voljo za vse države.")
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