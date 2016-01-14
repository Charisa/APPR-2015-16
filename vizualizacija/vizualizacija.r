# 3. faza: Izdelava zemljevida

drzave <- leto_2015
seznam <- list(color = ("grey"), width = 0.5)

g <- list(showframe = FALSE, showcoastlines = FALSE,
          projection = list(type = 'Mercator'))
plot_ly(drzave, z = "Counsumer Price Index")
              text = Country, locations = CODE, type = 'choropleth',
              color = "Counsumer Price Index", colors = 'Blues', marker = list(line = l),
              colorbar = list(tickprefix = '$', title = 'CPI')) %>%
  layout(title = '2015 CPI<br>Source:"http://www.numbeo.com/cost-of-living/rankings.jsp">NUMBEO</a>',
         geo = g)












#colnames(drzave) <- colnames(leto_2015)
#head(drzave)

#library(rworldmap)
#library(maptools)
#nov_zemljevid <- getMap(resolution = "low")
#plot(nov_zemljevid, fill = )

#points(drzave$"Quality of Life Index", drzave$"Consumer Price Index", col = 'red', cex = .6)


