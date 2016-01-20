df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_ebola.csv')
# restrict from June to September
df <- subset(df, Month %in% 6:9)
# ordered factor variable with month abbreviations
df$abbrev <- ordered(month.abb[df$Month], levels = month.abb[6:9])
# September totals
df9 <- subset(df, Month == 9)

# common plot options
g <- list(
  scope = 'africa',
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)

g1 <- c(
  g,
  resolution = 50,
  showcoastlines = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white"),
  projection = list(type = 'Mercator'),
  list(lonaxis = list(range = c(-15, -5))),
  list(lataxis = list(range = c(0, 12))),
  list(domain = list(x = c(0, 1), y = c(0, 1)))
)

g2 <- c(
  g,
  showcountries = F,
  bgcolor = toRGB("white", alpha = 0),
  list(domain = list(x = c(0, .6), y = c(0, .6)))
)

plot_ly(df, type = 'scattergeo', mode = 'markers', locations = Country,
        locationmode = 'country names', text = paste(Value, "cases"),
        color = as.ordered(abbrev), marker = list(size = Value/50), inherit = F) %>%
  add_trace(type = 'scattergeo', mode = 'text', geo = 'geo2', showlegend = F,
            lon = 21.0936, lat = 7.1881, text = 'Africa') %>%
  add_trace(type = 'choropleth', locations = Country, locationmode = 'country names',
            z = Month, colors = "black", showscale = F, geo = 'geo2', data = df9) %>%
  layout(title = 'Ebola cases reported by month in West Africa 2014<br> Source: <a href="https://data.hdx.rwlabs.org/dataset/rowca-ebola-cases">HDX</a>',
         geo = g1, geo2 = g2)