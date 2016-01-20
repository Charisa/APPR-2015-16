# 3. faza: Izdelava zemljevidov

source("lib/libraries.r", encoding = "UTF-8")




# ZEMLJEVIDI ZA POSAMEZNE INDEKSE V LETU 2015


zemljevid_traffic <- uvoz_zemljevidov(tabela_traffic_2015, "Traffic Index", zemljevid_traffic_index, 
                                      barva = "RdPu")
zemljevid_pollution <- uvoz_zemljevidov(tabela_pollution_2015, "Pollution Index", zemljevid_pollution_index, 
                                      barva = "Oranges")
zemljevid_health_care <- uvoz_zemljevidov(tabela_health_care_2015, "Health Care Index", zemljevid_health_care_index,
                                      barva = "BuGn")
zemljevid_crime <- uvoz_zemljevidov(tabela_crime_2015, "Crime Index", zemljevid_crime_index,
                                      barva = "Blues")
zemljevid_rent <- uvoz_zemljevidov(tabela_rent_2015, "Rent Index", zemljevid_rent_index,
                                      barva = "Reds")
zemljevid_CPI <- uvoz_zemljevidov(tabela_CPI_2015, "Consumer Price Index", zemljevid_CP_index,
                                      barva = "YlGn")
zemljevid_groceries <- uvoz_zemljevidov(tabela_groceries_2015, "Groceries Index", zemljevid_groceris_index,
                                      barva = "BrBG")
zemljevid_restaurant_price <- uvoz_zemljevidov(tabela_restaurant_price_2015, "Restaurant Price Index", zemljevid_restaurant_price_index,
                                      barva = "Spectral")



# Iz tabele leto bom naredila zemljevid (PLOTLY) 



ISO <- read.csv("https://raw.githubusercontent.com/umpirsky/country-list/master/data/en_US/country.csv", encoding = "UTF-8")           
                                                            # Uvožena tabela z ISO kraticami.

colnames(ISO)[2] <- "Country"                               # Preimenujemo stolpec "value" v
colnames(ISO)[1] <- "CODE"                                  # v "Country" in "id" v "CODE".

map_table_2015 <- merge(leto_2015, ISO, by = "Country")     # Dodamo ISO kratice v našo tabelo leto_2015.
                                                            # Ni kratic za vsa imena, zato posledično
                                                            # izgubimo podatke za 8 držav.
                                                            
                                                            # Dobila sem razpredelnico map_2015, s katero
                                                            # bom naredila zemljevid.

map_table_2015[is.na(map_table_2015)] <- 0                  # V tabeli vse NA spremenimo v ničle.
 


crte <- list(color = toRGB("grey"), width = 2)
g <- list(showcoastlines = TRUE, projection = list(type = 'mercator'), resolution = "100",
         scope = "world", showland = "boolean")

x <- plot_ly(map_table_2015, z = `Quality of Life Index`, text = Country, locations = Country,
                    type = 'choropleth',locationmode="country names", color = `Quality of Life Index`, colors = terrain.colors(5), 
                    marker = list(line = crte), colorbar = list(tickprefix = '', title = "Vrednosti indeksov")) %>%
  layout(title = "Indeksi cen zivljenjskih potrebscin", geo = g)



