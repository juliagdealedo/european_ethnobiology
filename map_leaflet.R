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
library(googlesheets4)


################### LEAFLET ####################
#### data
df <- read_xlsx("MAP/European Network of Ethnobiology.xlsx")

# sheet_url <- "https://docs.google.com/dfasdasd"
# df <- read_sheet(sheet_url)

# specify encoding! now some words are not well recognized
colnames(df)
df <- df %>%
  rename(res_name = "Your name:",
         res_surname = "Your surname:",
         institution = "Your current institutional affiliation 1:",
         inst_country = "In which country is your current institution 1 located?",
         inst_coord = "Coordinates Institution",
         inst_mun_coord = "Coordinates Municipality",
         field = "Geography of your current and past fieldsites:",
         disc = "Main subdiscipines:",
         link1 = "Link 1 - Your personal website or university page:",
         link2 = "Link 2 - Your article depository:") %>%
  mutate(name = paste(res_name, res_surname)) %>%
  separate(inst_coord,
           into = c("Latitude", "Longitude"),
           sep = ", ", convert = TRUE) %>%
  separate(inst_mun_coord,
           into = c("Mun_Latitude", "Mun_Longitude"),
           sep = ", ", convert = TRUE)



link_ok = df %>%
  group_by(name) %>%
  summarise(link_ok_pop = if_else(link2 != NA,
                                  paste ("<a href=", link2,">", name, "</a>" ,"<br>",
                                                     "- Discipline: ", disc, "<br>", "- Institution: ", institution),
                                  "") )

#
# link_ok = df %>%
#   group_by(name) %>%
#   summarise(link_ok_pop = paste ("<a href=", link2,">", name, "</a>" ,"<br>",
#                                  "- Discipline: ", disc, "<br>", "- Institution: ", institution))

df <- df %>%
  left_join(link_ok, by = "name")

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


# people and discipline
popup_people_disc <- df %>%
  group_by(name) %>%
  summarise(
    text_popup_people_disc = paste0("<b>", name, "</b><br>",
                        paste(disc, collapse = "<br>")),
    .groups = "drop"
  )

df <- df %>%
  left_join(popup_people_disc, by = "name")

# institutions countries
# popup text for researchers institutions in each country
# grouped by institution
popup_inst <- df %>%
  group_by(inst_country) %>%
  summarise(researchers = paste(name, collapse = "<br>"), .groups = "drop")

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

lf <-leaflet(df) %>%

  addProviderTiles(providers$CartoDB.Positron) %>%

  # PEOPLE
  addMarkers(
    ~Longitude, ~Latitude,
    popup=~link_ok_pop,
    label=~name,
    group = "People",
    clusterOptions = markerClusterOptions()) %>%

  # FIELDWORK
  addPolygons(data = field_countries,
              fillColor = "#FFE2C7",
              fillOpacity = 0.3,
              color = "#1A4548",
              weight = 1,
              popup = ~paste0("<b>", admin, "</b><br>", researchers),
              group = "Fieldwork") %>%

  # COUNTRIES AND PEOPLE
  addPolygons(data = inst_countries,
              fillColor = topo.colors(n = nrow(inst_countries)),
              fillOpacity = 0.2,
              color = "black",
              weight = 1,
              popup = ~paste0("<b>", admin, "</b><br>", researchers),
              group = "Countries") %>%

  # # INSTITUTIONS
  # addMarkers(~Longitude, ~Latitude,
  #            #popup = ~text_popup,
  #            #label = ~text_popup,
  #            #popup = ~institution,
  #            label = ~institution,
  #            group = "Institutions",
  #            icon = icon_inst,
  #            clusterOptions = markerClusterOptions())  %>%

  # # DISCIPLINES
  # addCircleMarkers(
  #            ~Longitude, ~Latitude,
  #            #popup = ~disc,
  #            label = ~disc,
  #            group = "Disciplines",
  #            clusterOptions = markerClusterOptions()) %>%
  #
  # LEYENDA
  addLayersControl(
    overlayGroups = c("People", "Countries", "Fieldwork"),
    options = layersControlOptions(collapsed = FALSE)) %>%

  # BUSCADOR
  addSearchFeatures(targetGroups = "People",
                    options = searchFeaturesOptions(zoom=10, openPopup = TRUE,
                                                    firstTipSubmit = F,
                                                    autoType = TRUE,
                                                    autoCollapse = TRUE,
                                                    hideMarkerOnCollapse = TRUE))


lf


saveWidget(lf, "mapa_interactivo_lf3.html")


