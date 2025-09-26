library(rnaturalearth)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)
library(sf)
library(readxl)
library(plotly)
library(leaflet)
library(leaflet.providers) # other base maps https://leaflet-extras.github.io/leaflet-providers/preview/
library(leaflet.extras) # for layers and sublayers
library(htmlwidgets)



# test
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


####################### PLOTLY/GGPLOTLY ####################
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

## ggplotly (scrollzoom no funciona)
g <- ggplot() +
  geom_sf(data = world_data, fill = NA, color = "#a9a9a9") +
  geom_sf(data = inst_countries,
          aes(fill = name, text = hover_text),
          color = "#a9a9a9") +
  scale_fill_viridis_d() +
  theme_minimal()

p <- ggplotly(g, tooltip = "text") %>%
  config(scrollZoom = TRUE)
p
htmlwidgets::saveWidget(p, "mapa_interactivo.html")


## plotly CACA CACA CACA CACA
library(plotly)
world_data$has_researchers <- world_data$name %in% inst_countries$name
world_data <- st_cast(world_data, "MULTIPOLYGON")

# hoover de cada país

# parece que plotly no entiende de sf, hay que extraer los polígonos
for_plotly <- lapply(seq_len(nrow(inst_countries)), function(i) {
  poly <- inst_countries$geometry[i][[1]]
  if (inherits(poly, "MULTIPOLYGON")) {
    lapply(poly, function(p) p[[1]])
  } else {
    list(poly[[1]])
  }
})

p <- plot_ly()

for (i in seq_len(nrow(world_data))) {
  geom <- world_data$geometry[i][[1]]
  for (poly in geom) {
    coords <- poly[[1]] # añade como dobles polígonos?
    p <- p %>%
      add_trace(
        type = "scattergeo",
        mode = "lines",
        lon = coords[, 1],
        lat = coords[, 2],
        fill = "toself",
        fillcolor = if (world_data$has_researchers[i])
          toRGB("skyblue", alpha = 0.5)
        else
          toRGB("gray85", alpha = 0.5),
        line = list(color = "#a9a9a9"),
        name = world_data$name[i],
        text = hover_texts[i],
        hoverinfo = "text"
      )
  }
}

p <- p %>%
  layout(
    geo = list(
      scope = "europe",
      showland = TRUE,
      landcolor = toRGB("gray85"),
      showcountries = TRUE,
      countrycolor = "#a9a9a9"
    )
  ) %>%
  config(scrollZoom = TRUE) # no funciona...?

htmlwidgets::saveWidget(p, "mapa_interactivo3.html")







################### LEAFLET ####################
#### data
df <- read_xlsx("MAP/European Network of Ethnobiology.xlsx")
# specify encoding! now some words are not well recognized

df <- df %>%
  rename(res_name = "Your name:",
         res_surname = "Your surname:",
         institution = "Your current institutional affiliation 1:",
         inst_country = "In which country is your current institution 1 located?",
         inst_coord = "Coordinates Institution",
         inst_mun_coord = "Coordinates Municipality",
         field = "Geography of your current and past fieldsites:",
         disc = "Main subdiscipines:") %>%
  mutate(name = paste(res_name, res_surname)) %>%
  separate(inst_coord,
           into = c("Latitude", "Longitude"),
           sep = ", ", convert = TRUE) %>%
  separate(inst_mun_coord,
           into = c("Mun_Latitude", "Mun_Longitude"),
           sep = ", ", convert = TRUE)


#### countries
world_data <- ne_countries(scale = "medium", returnclass = "sf")

# institutions
popup_inst_points <- df %>%
  group_by(institution) %>%
  summarise(
    text_popup = paste0("<b>", institution, "</b><br>",
                         paste(name, collapse = "<br>")),
    .groups = "drop"
  )
df <- df %>%
  left_join(popup_inst_points, by = "institution")

icon_inst <- makeIcon(
  iconUrl = "MAP/images/img.png",
  iconWidth = 15, iconHeight = 15,
  iconAnchorX = 0, iconAnchorY = 0 # needs to be adjusted
)

# institutions countries
# popup text for researchers institutions in each country
# grouped by institution
popup_inst <- df %>%
  group_by(inst_country, institution) %>%
  summarise(researchers = paste(name, collapse = "<br>"), .groups = "drop") %>%
  group_by(inst_country) %>%
  summarise(researchers = paste0("<b>", institution, "</b><br>",
                                 researchers, collapse = "<br><br>"))
inst_countries <- world_data %>% filter(admin %in% unique(df$inst_country))
inst_countries <- inst_countries %>%
  left_join(popup_inst, by = c("admin" = "inst_country"))

# fieldwork
df_field <- df %>%
  separate_rows(field, sep = ";") %>%
  mutate(researchers = paste(name, collapse = "<br>"))
# popup text for countries where researchers conduct fieldwork
popup_field <- df_field %>%
  group_by(field) %>%
  summarise(researchers = paste(name, collapse = "<br>"))
field_countries <- world_data %>% filter(admin %in% unique(df_field$field))
field_countries <- field_countries %>%
  left_join(popup_field, by = c("admin" = "field"))

# disciplines filter
df_disc <- df %>%
  separate_rows(disc, sep = ";")
disciplines <- unique(df_disc$disc)
for (d in disciplines) {
  df_d <- df_disc %>% filter(disc == d)
  lf <- lf %>%
    addMarkers(data = df_d,
               ~Longitude, ~Latitude,
               popup = ~paste0("<b>", institution, "</b><br>", name),
               group = d)
}

lf <- leaflet(df) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # jawg.sunny / CartoDB.VoyagerLabelsUnder ?
  # layer for countries
  addPolygons(data = inst_countries,
              fillColor = topo.colors(n = nrow(inst_countries)),
              fillOpacity = 0.2,
              color = "black",
              weight = 1,
              popup = ~paste0("<b>", admin, "</b><br>", researchers),
              group = "Countries") %>%
  # layer for institutions points
  addMarkers(~Longitude, ~Latitude,
             popup = ~text_popup,
             group = "Institutions",
             icon = icon_inst) %>%
  # layer for fieldwork countries
  addPolygons(data = field_countries,
              fillColor = "#1c8d1c",
              fillOpacity = 0.3,
              color = "darkgreen",
              weight = 1,
              popup = ~paste0("<b>", admin, "</b><br>", researchers),
              group = "Fieldwork") %>%
  # layer control
  addLayersControl(
    overlayGroups = c("Countries", "Institutions", "Fieldwork"),
    options = layersControlOptions(collapsed = FALSE)
  )

# layer for disciplines points not working!
lf <- lf %>%
  addMarkers(data = df_disc,
             ~Longitude, ~Latitude,
             popup = ~paste0("<b>", institution, "</b><br>",
                           name, "<br><i>", disc, "</i>"),
             group = "Disciplines") %>%
  # control de capas como antes
  addLayersControl(
    overlayGroups = c("Countries", "Fieldwork", "Institutions", "Disciplines"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # supuestamente añade buscador pero no va¿?
  addSearchFeatures(
    targetGroups = "Disciplines",
    options = searchFeaturesOptions(
      zoom = 10,
      openPopup = TRUE,
      firstTipSubmit = TRUE,
      autoType = TRUE,
      autoCollapse = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  )

saveWidget(lf, "mapa_interactivo_lf.html")
