library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250))
fig <- fig %>% add_markers(
  x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
  text = ~paste(df$name, "<br />", df$pop/1e6, " million")
)
fig <- fig %>% layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)

fig




library(rnaturalearth)
world_data <- ne_countries(scale = "medium", returnclass = "sf")
world_contour <- world_data$geometry

#random data
researchers <- c("Julia", "Zelda", "Munai", "Osiris", "Jimena")
countries <- c("Spain", "Japan", "Peru", "Egypt", "Nepal")
df <- data.frame(researcher = researchers, country = countries)

# check if countries in df are in world_data$name
countries %in% world_data$name

library(ggplot2)
library(dplyr)
library(sf)

selected_countries <- world_data %>%
  filter(name %in% df$country) %>%
  left_join(df, by = c("name" = "country")) %>%
  mutate(hover_text = paste("Country:", name, "<br>Researcher:", researcher))

g <- ggplot() +
  geom_sf(data = world_data, fill = NA, color = "#a9a9a9") +
  geom_sf(data = selected_countries, aes(fill = name, text = hover_text), color = "#a9a9a9") +
  scale_fill_viridis_d() +
  theme_minimal()

p <- ggplotly(g, tooltip = "text")
htmlwidgets::saveWidget(p, "mapa_interactivo.html")
