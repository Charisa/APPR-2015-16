# 3. faza: Izdelava zemljevidov

source("lib/libraries.r", encoding = "UTF-8")



# ZEMLJEVIDI ZA POSAMEZNE INDEKSE V LETO 2015


zemljevid_traffic <- uvoz_zemljevidov(leto_2015, "Traffic Index", zemljevid_traffic_index, 
                                      barva = "RdPu")
zemljevid_pollution <- uvoz_zemljevidov(leto_2015, "Pollution Index", zemljevid_pollution_index, 
                                      barva = "Oranges")
zemljevid_health_care <- uvoz_zemljevidov(leto_2015, "Health Care Index", zemljevid_health_care_index,
                                      barva = "BuGn")
zemljevid_crime <- uvoz_zemljevidov(leto_2015, "Crime Index", zemljevid_crime_index,
                                      barva = "Blues")
zemljevid_rent <- uvoz_zemljevidov(leto_2015, "Rent Index", zemljevid_rent_index,
                                      barva = "Reds")


poskus <- uvoz_zemljevidov(tabela_crime_2013, "Crime Index", zem,
                           barva = "RdPu")


# 




















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


