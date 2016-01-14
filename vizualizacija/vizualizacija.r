# 3. faza: Izdelava zemljevidov

source("lib/libraries.r", encoding = "UTF-8")



# ZEMLJEVIDI ZA POSAMEZNE INDEKSE V LETO 2015


classIntervals <- function(var, n, style = "hclust", rtimes = 3, ...){
  if (any(is.na(var))) 
    #warning("Podatki niso na voljo za vse države.")
    error = NULL
}

zemljevid_traffic_index <- joinCountryData2Map(leto_2015, joinCode = "NAME", nameJoinColumn = "Country", mapResolution = "coarse")
classInt <- classIntervals(zemljevid_traffic_index[["Traffic Index"]], 7)

mapParams <- mapCountryData(zemljevid_traffic_index, nameColumnToPlot = "Traffic Index", catMethod = "fixedWidth",
                          missingCountryCol = "grey", oceanCol = "lightblue", addLegend = FALSE, colourPalette = brewer.pal(7,"RdPu"))

legenda <- do.call(addMapLegend, c(mapParams,legendLabels = "all", legendWidth = 0.5, legendIntervals = "data",legendMar = 2))

zemljevid <- (legenda)







#seznam <- list(color = col2rgb("grey", alpha = FALSE), width = 0.5)

#g <- list(showframe = FALSE, showcoastlines = FALSE,
 #         projection = list(type = 'Mercator'))

#plot_ly(drzave, z = "Quality of Life Index", text = Country, locations = Year,
 #       type = "choropleth", color = Quality.of.Life.Index, colors = "Blues",
  #      marker = list(line = 1), colorbar = list(tickprefix = "$", 
#                                                 title = "CPI"))
         
         
         
         #)
          #    text = "Country", locations = CODE, type = 'choropleth',
           #   color = "Counsumer Price Index", colors = 'Blues', marker = list(line = l),
            #  colorbar = list(tickprefix = '$', title = 'CPI'))) %>%
  #layout(title = '2015 CPI<br>Source:"http://www.numbeo.com/cost-of-living/rankings.jsp">NUMBEO</a>',
        # geo = g)












#colnames(drzave) <- colnames(leto_2015)
#head(drzave)

#library(rworldmap)
#library(maptools)
#nov_zemljevid <- getMap(resolution = "low")
#plot(nov_zemljevid, fill = )

#points(drzave$"Quality of Life Index", drzave$"Consumer Price Index", col = 'red', cex = .6)


