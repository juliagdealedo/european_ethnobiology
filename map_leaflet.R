library(rnaturalearth)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)
library(sf)
library(readxl)
library(plotly)
library(leaflet)
library(leaflet.providers) # other base maps
# https://leaflet-extras.github.io/leaflet-providers/preview/
library(leaflet.extras) # for layers and sublayers
library(htmlwidgets)
library(gsheet)
library(googlesheets4)


################### LEAFLET ####################
#### Data
df <- read_xlsx("MAP/European Network of Ethnobiology.xlsx")

# sheet_url <- "https://docs.google.com/dfasdasd"
# df <- read_sheet(sheet_url)

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
  mutate(disc = str_replace_all(disc, ";", ", ")) %>%
  mutate(name = paste(res_name, res_surname)) %>%
  separate(inst_coord,
           into = c("Latitude", "Longitude"),
           sep = ", ", convert = TRUE) %>%
  separate(inst_mun_coord,
           into = c("Mun_Latitude", "Mun_Longitude"),
           sep = ", ", convert = TRUE)

# google sheets
df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1G1VxvZ5e2StEOhqEdcYvsouRjzYAJxVaKUYlx96pEKM/edit?usp=sharing")

df <- df %>%
  rename(res_name = "Name",
         res_surname = "Surname",
         institution = "Institution name",
         institution2 = "Other institutions (Institution 2)",
         link1 = "Website",
         link2 = "Website 2",
         inst_country = "Country of your institutional affiliation",
         inst_coord = "Institution coordinates",
         inst_coord2 = "Institution 2 coordinates",
         field = "Geography of your current and past fieldsites:",
         disc = "Main subdiscipines:",
         date = "Marca temporal") %>%
  mutate(name = paste(res_name, res_surname),
         date = as.POSIXct(date,
                           format = "%d/%m/%Y %H:%M:%S")) %>% # another format
  group_by(name) %>%
  slice_max(date, n = 1) %>% # keep only the most recent entry per researcher
  ungroup() %>%
  # if inst_coord is NA, use inst_coord2
  mutate(inst_coord = ifelse(is.na(inst_coord) | inst_coord == "",
                             inst_coord2,
                             inst_coord)) %>%
  separate(inst_coord,
           into = c("Latitude", "Longitude"),
           sep = ", ", convert = TRUE)

# for now we use inst_coord2 if inst_coord is NA, but the name
# should be institution2 in that case


#### Researchers
## popup with links if available, discipline and institution
researchers_popup <- df %>%
  group_by(name) %>%
  summarise(
    link_ok_pop = case_when( # case_when as consecutive ifs
      !is.na(link2) & link2 != "" ~ paste0("<div style='max-width:300px; word-break:break-word;'>", #supuestamente para que el texto no se salga del cuadro
                                           "<a href='", link2, "' target='_blank'> ",
                                           "<span style='font-size:16px;'>",
                                           name, "</span></a><br>",
                                           "<b>Discipline: </b>", disc, "<br>",
                                           "<b>Institution: </b>",
                                           institution, "</div>"),
      !is.na(link1) & link1 != "" ~ paste0("<div style='max-width:300px; word-break:break-word;'>",
                                           "<a href='", link1, "' target='_blank'> ",
                                           "<span style='font-size:16px;'>",
                                           name, "</span></a><br>",
                                           "<b>Discipline: </b>", disc, "<br>",
                                           "<b>Institution: </b>",
                                           institution, "</div>"),
      TRUE ~ paste0("<div style='max-width:300px; word-break:break-word;'>",
                    "<span style='font-size:16px;'><b>", name,
                    "</b></span><br>", "<b>Discipline:</b> ",
                    disc, "<br>", "<b>Institution:</b> ", institution, "</div>")
    )
  )

df <- df %>%
  left_join(researchers_popup, by = "name")


#### Countries
world_data <- ne_countries(scale = "medium", returnclass = "sf")

## Institutions
# popup_inst_points <- df %>%
#   group_by(institution) %>%
#   summarise(
#     text_popup = paste0("<b>", institution, "</b><br>",
#                         paste(name, collapse = "<br>")),
#     .groups = "drop"
#   )
#
# df <- df %>%
#   left_join(popup_inst_points, by = "institution")
#

## Institutions countries
# popup text for researchers institutions in each country
# grouped by institution
popup_inst <- df %>%
  group_by(inst_country) %>%
  summarise(researchers = paste(name, collapse = "<br>"), .groups = "drop")

inst_countries <- world_data %>% filter(admin %in% unique(df$inst_country))
inst_countries <- inst_countries %>%
  left_join(popup_inst, by = c("admin" = "inst_country")) %>%
  mutate(popup_text = paste0("<span style='font-size:14px;'><b>",
                             admin, "</b></span><br>",
                             researchers))
icon_inst <- makeIcon(
  iconUrl = "MAP/images/img.png",
  iconWidth = 15, iconHeight = 15,
  iconAnchorX = 0, iconAnchorY = 0 # needs to be adjusted
)

## Fieldwork
df_field <- df %>%
  separate_rows(field, sep = ";") %>%
  mutate(researchers = paste(name, collapse = "<br>"))

# popup text for countries where researchers conduct fieldwork
popup_field <- df_field %>%
  group_by(field) %>%
  summarise(researchers = paste(name, collapse = "<br>"))

field_countries <- world_data %>% filter(admin %in% unique(df_field$field))
field_countries <- field_countries %>%
  left_join(popup_field, by = c("admin" = "field")) %>%
  mutate(popup_text = paste0("<span style='font-size:14px;'><b>",
                             admin, "</b></span><br>",
                             researchers))


#### PLOT
## base map with main layers
lf <- leaflet(df) %>%
  setView(lng = mean(df$Longitude, na.rm = TRUE),
          lat = mean(df$Latitude, na.rm = TRUE),
          zoom = 3) %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  # Researchers
  addMarkers(~Longitude, ~Latitude,
             popup = ~link_ok_pop,
             label = ~name,
             group = "Researchers",
             icon = icon_inst,
             clusterOptions = markerClusterOptions(
              iconCreateFunction = JS(
                "function(cluster) {
                  return L.divIcon({
                    html: '<div style=\"background-color: #FFE2C7; border-radius: 50%; width: 30px; height: 30px; display: flex; align-items: center; justify-content: center; color: white; font-size: 14px; border: 2px solid white;\">' + cluster.getChildCount() + '</div>',
                    className: 'marker-cluster',
                    iconSize: L.point(40, 40)
                  });
                }"
             ))) %>%

  # Fieldwork
  addPolygons(data = field_countries,
              fillColor = "#FFE2C7",
              fillOpacity = 0.3,
              color = "#e2c7ae",
              weight = 1,
              popup = ~popup_text,
              group = "Fieldwork") %>%

  # Institutions countries
  addPolygons(data = inst_countries,
              fillColor = "#1A4548",
              fillOpacity = 0.3,
              color = "#1A4548",
              weight = 1,
              popup = ~popup_text,
              group = "Institutions' countries")

## layer for each discipline NO FUNKA
# for (d in disciplines) {
#   df_d <- df_disc %>% filter(disc == d)
#   lf <- lf %>%
#     addCircleMarkers(data = df_d,
#                      ~Longitude, ~Latitude,
#                      popup = ~link_ok_pop,
#                      label = ~name,
#                      group = paste0("Discipline: ", d),
#                      radius = 5,
#                      fillOpacity = 0.8,
#                      color = "#1A4548")
# }
## Leyend and layers control
lf <- lf %>%
  addLayersControl(
    overlayGroups = c("Researchers",
                      "Institutions' countries",
                      "Fieldwork"),
    options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup("Fieldwork")

## Search bar
lf <- lf %>%
  addSearchFeatures(targetGroups = "Researchers",
                    options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                                    firstTipSubmit = FALSE,
                                                    autoType = TRUE,
                                                    autoCollapse = TRUE,
                                                    hideMarkerOnCollapse = TRUE))

lf
saveWidget(lf, paste0("mapa_interactivo_lf", Sys.time(), ".html"))





#### Disciplines filter
# df_disc <- df %>%
#   separate_rows(disc, sep = ";")
#
# disciplines <- unique(df_disc$disc)
# discipline_groups <- paste0("Discipline: ", disciplines)

