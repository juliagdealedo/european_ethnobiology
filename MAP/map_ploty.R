library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = "usa",
  projection = list(type = "albers usa"),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig <- plot_geo(df, locationmode = "USA-states", sizes = c(1, 250))
fig <- fig %>% add_markers(
  x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
  text = ~paste(df$name, "<br />", df$pop / 1e6, " million")
)
fig <- fig %>% layout(title = "2014 US city populations<br>(Click legend to toggle)", geo = g)

fig


htmlwidgets::saveWidget(fig, "mapa2_interactivo.html")


## packges
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(sf)
library(readxl)

## base map
# https://www.naturalearthdata.com/downloads/
# information of countries, cities and contour
world_data <- ne_countries(scale = "medium", returnclass = "sf")
world_cities <- ne_download(scale = "medium", type = "populated_places",
                            category = "cultural", returnclass = "sf")
world_contour <- world_data$geometry
# map's definition can be changed with scale argument
# such asplot(scale = "medium"/"small")

## data
# random data
researchers <- c("Julia", "Zelda", "Munai", "Osiris", "Jimena")
countries <- c("Spain", "Japan", "Peru", "Egypt", "Nepal")
df <- data.frame(researcher = researchers, country = countries)
# real data
df <- read_xlsx("MAP/European Network of Ethnobiology.xlsx")
# specify encoding!
# rename some columns of df at the same time
df <- df %>%
  rename(res_name = "Your name:",
         res_surname = "Your surname:",
         institution = "Your current institutional affiliation 1:",
         inst_country = "In which country is your current institution 1 located?",
         inst_coord = "Coordinates Institution",
         inst_mun_coord = "Coordinates Municipality",
         field = "Geography of your current and past fieldsites:")


# check if countries in df are in world_data$name
df$inst_country[!df$inst_country %in% world_data$name]
# need to modify the form, someone was able to put "Serbia and Montenegro"

# if there are several people in the same country,
# only the last one appears, we need to group them first
df_grouped <- df %>%
  group_by(inst_country) %>%
  summarise(
    researchers = paste(paste(res_name, res_surname), collapse = "<br>")
  )

inst_countries <- world_data %>%
  filter(name %in% df_grouped$inst_country) %>%
  left_join(df_grouped, by = c("name" = "inst_country")) %>%
  mutate(hover_text = paste("Country:", name, "<br>Researchers:", researchers))

# no sé porque solo va el hoover en el borde de las
# áreas, he probado poniendo el centroide, pero entonces
# solo sale en el centro... así que no lo he usado al final
centroids <- st_centroid(inst_countries)

g <- ggplot() +
  geom_sf(data = world_data, fill = NA, color = "#a9a9a9") +
  geom_sf(data = inst_countries,
          aes(fill = name, text = hover_text),
          color = "#a9a9a9") +
  scale_fill_viridis_d() +
  theme_minimal()

p <- ggplotly(g, tooltip = "text")
p
htmlwidgets::saveWidget(p, "mapa_interactivo.html")
