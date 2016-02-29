map_table_2015 <- merge(leto_2015, ISO, by = "Country")     # Dodamo ISO kratice v našo tabelo leto_2015.

# Ni kratic za vsa imena, zato posledično
# izgubimo podatke za 8 držav.

# Dobila sem razpredelnico map_2015, s katero
# bom naredila zemljevid.

# map_table_2015[is.na(map_table_2015)] <- 0                  # V tabeli vse NA spremenimo v ničle.


crte <- list(color = toRGB("grey"), width = 2)
g <- list(showcoastlines = TRUE, projection = list(type = 'mercator'), resolution = "100",
          scope = "world", showland = "boolean")

x <- plot_ly(map_table_2015, z = `Quality of Life Index`, text = Country, locations = Country,
             type = 'choropleth',locationmode="country names", color = `Quality of Life Index`, colors = terrain.colors(5), 
             marker = list(line = crte), colorbar = list(tickprefix = '', title = "Vrednosti indeksov")) %>%
  layout(title = "", geo = g)