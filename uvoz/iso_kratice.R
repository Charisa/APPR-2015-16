# Uvožena tabela z ISO kraticami.

ISO <- read.csv("https://raw.githubusercontent.com/umpirsky/country-list/master/data/en_US/country.csv", encoding = "UTF-8")           

colnames(ISO)[2] <- "Country"                               # Preimenujemo stolpec "value" v
colnames(ISO)[1] <- "CODE"                                  # v "Country" in "id" v "CODE".
write.csv2(ISO, 'podatki/ISO.csv', fileEncoding = "UTF-8", row.names = FALSE)


# ISO kratice s 'tremi črkami'.

iso <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv", encoding = "UTF-8")           
iso <- iso[c(1,3)]
colnames(iso)[1] <- "Country"                               # Preimenujemo stolpec "value" v
colnames(iso)[2] <- "CODE"
write.csv2(iso, 'podatki/iso.csv', fileEncoding = "UTF-8", row.names = FALSE)