#### R Script for Sounds of Nature 2025 Shiny App

#### Importing Libraries

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(scales)
library(ggrepel)
library(stringr)
library(plotly)
library(leaflet)
library(leaflegend)
library(leaflet.extras)
library(bslib)
library(showtext)
library(fontawesome)
library(shinybrowser)
library(htmltools)
library(glue)
library(htmlwidgets)
library(favawesome)
library(hms)
library(ggh4x)

font_add_google("Libre Franklin", "libre")
showtext_auto()
showtext_opts(dpi = 96)

#### Reading Data

geogs = read_csv("aru_coords_2025.csv") %>%
  mutate("Biome Type" = case_when(
    `Forest, Grassland,` == "F" ~ "Forest",
    TRUE ~ "Grassland"
  )
  ) %>% 
  select(-Lat, -Long, -`Forest, Grassland,`) 

original_sb <- c("Song Birds")
replacement_sb <- c("Songbirds")
original_r <- c("Raptors")
replacement_r <- c("Owls & Other Raptors")
original_shb <- c("Shorebirds and Seabirds")
replacement_shb <- c("Shorebirds")
original_w <- c("Waterfowl and Wading Birds")
replacement_w <- c("Waterfowl & Wading Birds")

birdfamilies = read_csv("birdfamilies_updated.csv", col_names = FALSE) %>% 
  rename(`Scientific Name` = X1, `Common Name` = X2, Family = X3, `More Information` = X4)
birdfamilies$Family = replace(birdfamilies$Family, birdfamilies$Family %in% original_sb, replacement_sb)
birdfamilies$Family = replace(birdfamilies$Family, birdfamilies$Family %in% original_r, replacement_r)
birdfamilies$Family = replace(birdfamilies$Family, birdfamilies$Family %in% original_shb, replacement_shb)
birdfamilies$Family = replace(birdfamilies$Family, birdfamilies$Family %in% original_w, replacement_w)

privatesitedatalist = list()

for (i in list.files("private_sites_results")) {
  site_number = substr(i, 1, 5)
  filepath = paste0("private_sites_results/", i)
  temp_df = read_csv(filepath) %>% mutate(`Site Number` = site_number) %>% relocate(`Site Number`, .before = `Common Name`)
  privatesitedatalist[[i]] = temp_df
  rm(temp_df)
}

publicsitedatalist = list()

for (i in list.files("public_sites_results")) {
  site_number = substr(i, 1, 5)
  filepath = paste0("public_sites_results/", i)
  temp_df = read_csv(filepath) %>% mutate(`Site Number` = site_number) %>% relocate(`Site Number`, .before = `Common Name`)
  publicsitedatalist[[i]] = temp_df
  rm(temp_df)
}

privatesitedata = bind_rows(privatesitedatalist)
publicsitedata = bind_rows(publicsitedatalist)
rawsitedata = bind_rows(privatesitedata, publicsitedata)
rm(privatesitedatalist)
rm(publicsitedatalist)

#### Creating Icons for Map and Mapping Site Location Data (Default Map First Displayed on Tab 1)

geogss = geogs %>% mutate(
  Marker_color = case_when(
    `Public or Private` == "Public" ~ "lightgreen",
    TRUE ~ "green"  # If private
  ),
  Icon_type = case_when(
    `Biome Type` == "Forest" ~ "tree",
    TRUE ~ "leaf" # If grassland
  )
)

map_icon = makeAwesomeIcon(
  icon = geogss$Icon_type,
  library = "fa",
  markerColor = geogss$Marker_color,
  iconColor = "#264037"
)

globalmap = leaflet(options = leafletOptions(zoomSnap = 0.25,
                                             zoomControl = FALSE)) %>% 
  setView(lng = -94, lat = 45, zoom = 6.75) %>% 
  addTiles() %>% 
  addProviderTiles("OpenStreetMap.HOT") %>%  
  addAwesomeMarkers(lng = geogss$`Township Long`, 
                    lat = geogss$`Township Lat`, 
                    icon = map_icon,
                    label = glue("Site {geogss$ARU}"),
                    layerId = geogss$ARU,
                    group = "sites") %>%
  addControl(html = "<strong>Biome Type</strong> <br> <i class='fa fa-tree'></i> Forest <br> <i class='fa fa-leaf'></i> Grassland",
             position = "bottomright") %>% 
  addLegend(
    position = "bottomright",  # Position the legend
    colors = c("#70ac26", "#bbfa71"),
    labels = c("Private", "Public"),
    title = "Type of Site", # Set a title
    opacity = 1
  ) %>% 
  onRender(
    "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }") %>% 
  addResetMapButton() %>% 
  addSearchFeatures(targetGroups = "sites", options = searchFeaturesOptions(
    moveToLocation = TRUE,
    zoom = 10,
    autoCollapse = TRUE,
    hideMarkerOnCollapse = FALSE,
    textPlaceholder = "Search by Site Number...",
    marker = list(icon = NULL, animate = TRUE, circle = list(radius = 10, weight = 3, color
                                                             = "#fe6100", stroke = TRUE, fill = FALSE))
  ))

#### Cleaning Site Data

originalbirds = c("Cattle Egret", "Northern Goshawk", "Herring Gull", "Warbling Vireo", "Gray Jay", "House Wren", "Yellow Warbler", "Whimbrel", "Arctic Skua", "Pomarine Skua", "Long-tailed Skua", "Red-throated Diver", "Pacific Diver", "White-billed Diver", "Brent Goose", "Grey Phalarope", "Grey-crowned Rosy-Finch", "Black-throated Grey Warbler", "Red Grouse/Willow Grouse", "Common Redpoll", "Barn Owl", "Black-crowned Night-Heron", "Yellow-crowned Night-Heron")
replacementbirds = c("Western Cattle-Egret", "American Goshawk", "American Herring Gull", "Eastern Warbling Vireo", "Canada Jay", "Northern House Wren", "Northern Yellow Warbler", "Hudsonian Whimbrel", "Parasitic Jaeger", "Pomarine Jaeger", "Long-tailed Jaeger", "Red-throated Loon", "Pacific Loon", "Yellow-billed Loon", "Brant", "Red Phalarope", "Gray-crowned Rosy-Finch", "Black-throated Gray Warbler", "Willow Ptarmigan", "Redpoll", "American Barn Owl", "Black-crowned Night Heron", "Yellow-crowned Night Heron")
for (i in 1:length(originalbirds)) {
  original_name = c(originalbirds[i])
  replacement_name = c(replacementbirds[i])
  rawsitedata$`Common Name` = replace(rawsitedata$`Common Name`, rawsitedata$`Common Name` %in% original_name, replacement_name)
}

cleaningsites = left_join(rawsitedata, birdfamilies, by = join_by(`Common Name`)) %>%
  drop_na("Total Instances") %>% 
  relocate(`Scientific Name`, .after = `Site Number`) %>%
  relocate(Family, .before = Confidence) %>% 
  rename(`Number of Detections, 60% Confidence` = `Number of Instances, 60% Confidence`,
         `Number of Detections, 65% Confidence` = `Number of Instances, 65% Confidence`,
         `Number of Detections, 70% Confidence` = `Number of Instances, 70% Confidence`,
         `Number of Detections, 75% Confidence` = `Number of Instances, 75% Confidence`,
         `Number of Detections, 80% Confidence` = `Number of Instances, 80% Confidence`,
         `Number of Detections, 85% Confidence` = `Number of Instances, 85% Confidence`,
         `Number of Detections, 90% Confidence` = `Number of Instances, 90% Confidence`,
         `Number of Detections, 95% Confidence` = `Number of Instances, 95% Confidence`,
         `Total Detections` = `Total Instances`,
         `BirdNET Confidence Score` = Confidence,
         `Rare Detection` = Rare) %>% 
  mutate(
    Family = factor(Family, levels = c("Songbirds", "Waterfowl & Wading Birds", "Owls & Other Raptors", "Shorebirds", "Other Land Birds")),
    `Scientific Name` = paste0("<u>", "<em>", "<a href='", `More Information`,"' target='_blank'>", `Scientific Name`, "</a>", "</em>", "</u>"),
    `Total Detections` = case_when(
      `Rare Detection` == "no" ~ rowSums(across(c(`Number of Detections, 60% Confidence`,
                                                  `Number of Detections, 65% Confidence`,
                                                  `Number of Detections, 70% Confidence`,
                                                  `Number of Detections, 75% Confidence`,
                                                  `Number of Detections, 80% Confidence`,
                                                  `Number of Detections, 85% Confidence`,
                                                  `Number of Detections, 90% Confidence`,
                                                  `Number of Detections, 95% Confidence`))),
      `Rare Detection` == "yes" ~ as.numeric(`Total Detections`)
    ),
    `Lowest Confidence` = case_when(
      `Number of Detections, 60% Confidence` > 0 ~ "60%",
      `Number of Detections, 65% Confidence` > 0 ~ "65%",
      `Number of Detections, 70% Confidence` > 0 ~ "70%",
      `Number of Detections, 75% Confidence` > 0 ~ "75%",
      `Number of Detections, 80% Confidence` > 0 ~ "80%",
      `Number of Detections, 85% Confidence` > 0 ~ "85%",
      `Number of Detections, 90% Confidence` > 0 ~ "90%",
      `Number of Detections, 95% Confidence` > 0 ~ "95%",
      TRUE ~ "99%"
    ),
    `Highest Confidence` = case_when(
      `Number of Detections, 95% Confidence` > 0 ~ "95%",
      `Number of Detections, 90% Confidence` > 0 ~ "90%",
      `Number of Detections, 85% Confidence` > 0 ~ "85%",
      `Number of Detections, 80% Confidence` > 0 ~ "80%",
      `Number of Detections, 75% Confidence` > 0 ~ "75%",
      `Number of Detections, 70% Confidence` > 0 ~ "70%",
      `Number of Detections, 65% Confidence` > 0 ~ "65%",
      `Number of Detections, 60% Confidence` > 0 ~ "60%",
      TRUE ~ "99%"
    ),
    `Confidence Range` = case_when(
      `Lowest Confidence` == `Highest Confidence` ~ `Highest Confidence`,
      TRUE ~ paste0(`Lowest Confidence`, " - ", `Highest Confidence`)
    )
  )

sgcnbirds = c("Acadian Flycatcher", "American Barn Owl", "American Bittern", "American Black Duck", "American Goshawk", "American Kestrel", "American Three-toed Woodpecker", "American White Pelican", "Baird's Sparrow", "Bay-breasted Warbler", "Bell's Vireo", "Black Tern", "Black-backed Woodpecker", "Black-crowned Night Heron", "Black-throated Blue Warbler", "Bobolink", "Boreal Chickadee", "Boreal Owl", "Buff-breasted Sandpiper", "Bufflehead", "Burrowing Owl", "Canvasback", "Cape May Warbler", "Cerulean Warbler", "Chestnut-collared Longspur", "Chimney Swift", "Common Gallinule", "Common Loon", "Common Nighthawk", "Common Tern", "Connecticut Warbler", "Eared Grebe", "Eastern Meadowlark", "Eastern Whip-poor-will", "Evening Grosbeak", "Field Sparrow", "Forster's Tern", "Franklin's Gull", "Golden-winged Warbler", "Grasshopper Sparrow", "Great Gray Owl", "Greater Prairie Chicken", "Henslow's Sparrow", "Horned Grebe", "King Rail", "Lark Sparrow", "Least Bittern", "LeConte's Sparrow", "Lesser Scaup", "Loggerhead Shrike", "Louisiana Waterthrush", "Marbled Godwit", "Nelson's Sparrow", "Northern Harrier", "Northern Pintail", "Olive-sided Flycatcher", "Peregrine Falcon", "Philadelphia Vireo", "Piping Plover", "Prothonotary Warbler", "Purple Martin", "Red Knot", "Red-headed Woodpecker", "Red-necked Grebe", "Red-shouldered Hawk", "Sedge Wren", "Sharp-tailed Grouse", "Short-eared Owl", "Solitary Sandpiper", "Sprague's Pipit", "Spruce Grouse", "Swainson's Hawk", "Swainson's Thrush", "Upland Sandpiper", "Western Grebe", "Western Kingbird", "Western Meadowlark", "Wilson's Phalarope", "Yellow Rail")
sgcnbirds_wsymbol = c("Acadian Flycatcher¹", "American Barn Owl¹", "American Bittern¹", "American Black Duck¹", "American Goshawk¹", "American Kestrel¹", "American Three-toed Woodpecker¹", "American White Pelican¹", "Baird's Sparrow¹", "Bay-breasted Warbler¹", "Bell's Vireo¹", "Black Tern¹", "Black-backed Woodpecker¹", "Black-crowned Night Heron¹", "Black-throated Blue Warbler¹", "Bobolink¹", "Boreal Chickadee¹", "Boreal Owl¹", "Buff-breasted Sandpiper¹", "Bufflehead¹", "Burrowing Owl¹", "Canvasback¹", "Cape May Warbler¹", "Cerulean Warbler¹", "Chestnut-collared Longspur¹", "Chimney Swift¹", "Common Gallinule¹", "Common Loon¹", "Common Nighthawk¹", "Common Tern¹", "Connecticut Warbler¹", "Eared Grebe¹", "Eastern Meadowlark¹", "Eastern Whip-poor-will¹", "Evening Grosbeak¹", "Field Sparrow¹", "Forster's Tern¹", "Franklin's Gull¹", "Golden-winged Warbler¹", "Grasshopper Sparrow¹", "Great Gray Owl¹", "Greater Prairie Chicken¹", "Henslow's Sparrow¹", "Horned Grebe¹", "King Rail¹", "Lark Sparrow¹", "Least Bittern¹", "LeConte's Sparrow¹", "Lesser Scaup¹", "Loggerhead Shrike¹", "Louisiana Waterthrush¹", "Marbled Godwit¹", "Nelson's Sparrow¹", "Northern Harrier¹", "Northern Pintail¹", "Olive-sided Flycatcher¹", "Peregrine Falcon¹", "Philadelphia Vireo¹", "Piping Plover¹", "Prothonotary Warbler¹", "Purple Martin¹", "Red Knot¹", "Red-headed Woodpecker¹", "Red-necked Grebe¹", "Red-shouldered Hawk¹", "Sedge Wren¹", "Sharp-tailed Grouse¹", "Short-eared Owl¹", "Solitary Sandpiper¹", "Sprague's Pipit¹", "Spruce Grouse¹", "Swainson's Hawk¹", "Swainson's Thrush¹", "Upland Sandpiper¹", "Western Grebe¹", "Western Kingbird¹", "Western Meadowlark¹", "Wilson's Phalarope¹", "Yellow Rail¹")
for (i in 1:length(sgcnbirds)) {
  original_name = c(sgcnbirds[i])
  replacement_name = c(sgcnbirds_wsymbol[i])
  cleaningsites$`Common Name` = replace(cleaningsites$`Common Name`, cleaningsites$`Common Name` %in% original_name, replacement_name)
}
cleaningsites$`Common Name` = replace(cleaningsites$`Common Name`, cleaningsites$`Common Name` %in% "Eastern Screech-Owl", "Eastern Screech-Owl²")

cleanedsites = cleaningsites %>% 
  select(`Site Number`, `Scientific Name`, `Common Name`, Family, `Confidence Range`, 
         `Total Detections`, `List of Datetimes`) %>% 
  relocate(`Confidence Range`, .before = `Total Detections`)


unique_families = cleanedsites %>%
  select(Family) %>% 
  unique() %>% 
  mutate(Family =  factor(Family, levels = c("Songbirds", "Waterfowl & Wading Birds", "Owls & Other Raptors", "Shorebirds", "Other Land Birds"))) %>%
  arrange(Family)

unique_species = cleanedsites %>% 
  select(`Common Name`) %>% 
  unique() %>% 
  arrange(`Common Name`)

unique_biomes = geogs %>% 
  select(`Biome Type`) %>% 
  unique() %>% 
  mutate(`Biome Type` = factor(`Biome Type`, levels = c("Forest", "Grassland"))) %>% 
  arrange(`Biome Type`)

#### Summary Statistics for Shiny App

total_species = cleanedsites %>% 
  group_by(`Site Number`) %>% 
  count()

avg_species = round(mean(total_species$n))

total_detections = cleanedsites %>% 
  group_by(`Site Number`) %>% 
  summarize(`Total Site Detections` = sum(`Total Detections`))

avg_detections = prettyNum(round(mean(total_detections$`Total Site Detections`)), big.mark = ",")

alldetections = prettyNum(sum(cleanedsites$`Total Detections`), big.mark = ",")

#### Functions for Shiny App (zoom-adjusted)

filtersites = function(site, detection_prob="60") {
  filtered_df = cleaningsites %>% select(-`List of Datetimes`) %>% 
    filter(`Site Number` == site,
           as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob)) %>% 
    rename("60" = "Number of Detections, 60% Confidence",
           "65" = "Number of Detections, 65% Confidence",
           "70" = "Number of Detections, 70% Confidence",
           "75" = "Number of Detections, 75% Confidence",
           "80" = "Number of Detections, 80% Confidence",
           "85" = "Number of Detections, 85% Confidence",
           "90" = "Number of Detections, 90% Confidence",
           "95" = "Number of Detections, 95% Confidence") %>% 
    mutate(Prob_Detections = case_when(
      `Rare Detection` == "yes" ~ `Total Detections`,
      detection_prob == "60" ~ rowSums(across(c(`60`, `65`, `70`, `75`, `80`, `85`, `90`, `95`))),
      detection_prob == "70" ~ rowSums(across(c(`70`, `75`, `80`, `85`, `90`, `95`))),
      detection_prob == "80" ~ rowSums(across(c(`80`, `85`, `90`, `95`))),
      detection_prob == "90" ~ rowSums(across(c(`90`, `95`)))
    ),
    Filtered_Lowest_Conf = case_when(
      `Rare Detection` == "yes" ~ "99%",
      (detection_prob == "60") & (`60` > 0) ~ "60%",
      (detection_prob == "60") & (`65` > 0) ~ "65%",
      (detection_prob == "60") & (`70` > 0) ~ "70%",
      (detection_prob == "60") & (`75` > 0) ~ "75%",
      (detection_prob == "60") & (`80` > 0) ~ "80%",
      (detection_prob == "60") & (`85` > 0) ~ "85%",
      (detection_prob == "60") & (`90` > 0) ~ "90%",
      (detection_prob == "60") & (`95` > 0) ~ "95%",
      (detection_prob == "70") & (`70` > 0) ~ "70%",
      (detection_prob == "70") & (`75` > 0) ~ "75%",
      (detection_prob == "70") & (`80` > 0) ~ "80%",
      (detection_prob == "70") & (`85` > 0) ~ "85%",
      (detection_prob == "70") & (`90` > 0) ~ "90%",
      (detection_prob == "70") & (`95` > 0) ~ "95%",
      (detection_prob == "80") & (`80` > 0) ~ "80%",
      (detection_prob == "80") & (`85` > 0) ~ "85%",
      (detection_prob == "80") & (`90` > 0) ~ "90%",
      (detection_prob == "80") & (`95` > 0) ~ "95%",
      (detection_prob == "90") & (`90` > 0) ~ "90%",
      (detection_prob == "90") & (`95` > 0) ~ "95%",
    ),
    New_Conf_Range = case_when(
      `Filtered_Lowest_Conf` == `Highest Confidence` ~ `Highest Confidence`,
      TRUE ~ paste0(`Filtered_Lowest_Conf`, " - ", `Highest Confidence`)
    )) %>% 
    select(`Site Number`, `Scientific Name`, `Common Name`, Family, `New_Conf_Range`, 
           `Prob_Detections`) %>% 
    relocate(`New_Conf_Range`, .before = `Prob_Detections`)
  
  return(filtered_df)
}

filterfamily = function(site, family, detection_prob="60") {
  filtered_df = cleaningsites %>% select(-`List of Datetimes`) %>% 
    filter(`Site Number` == site,
           Family == family,
           as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob)) %>% 
    rename("60" = "Number of Detections, 60% Confidence",
           "65" = "Number of Detections, 65% Confidence",
           "70" = "Number of Detections, 70% Confidence",
           "75" = "Number of Detections, 75% Confidence",
           "80" = "Number of Detections, 80% Confidence",
           "85" = "Number of Detections, 85% Confidence",
           "90" = "Number of Detections, 90% Confidence",
           "95" = "Number of Detections, 95% Confidence") %>% 
    mutate(Prob_Detections = case_when(
      `Rare Detection` == "yes" ~ `Total Detections`,
      detection_prob == "60" ~ rowSums(across(c(`60`, `65`, `70`, `75`, `80`, `85`, `90`, `95`))),
      detection_prob == "70" ~ rowSums(across(c(`70`, `75`, `80`, `85`, `90`, `95`))),
      detection_prob == "80" ~ rowSums(across(c(`80`, `85`, `90`, `95`))),
      detection_prob == "90" ~ rowSums(across(c(`90`, `95`)))
    ),
    Filtered_Lowest_Conf = case_when(
      `Rare Detection` == "yes" ~ "99%",
      (detection_prob == "60") & (`60` > 0) ~ "60%",
      (detection_prob == "60") & (`65` > 0) ~ "65%",
      (detection_prob == "60") & (`70` > 0) ~ "70%",
      (detection_prob == "60") & (`75` > 0) ~ "75%",
      (detection_prob == "60") & (`80` > 0) ~ "80%",
      (detection_prob == "60") & (`85` > 0) ~ "85%",
      (detection_prob == "60") & (`90` > 0) ~ "90%",
      (detection_prob == "60") & (`95` > 0) ~ "95%",
      (detection_prob == "70") & (`70` > 0) ~ "70%",
      (detection_prob == "70") & (`75` > 0) ~ "75%",
      (detection_prob == "70") & (`80` > 0) ~ "80%",
      (detection_prob == "70") & (`85` > 0) ~ "85%",
      (detection_prob == "70") & (`90` > 0) ~ "90%",
      (detection_prob == "70") & (`95` > 0) ~ "95%",
      (detection_prob == "80") & (`80` > 0) ~ "80%",
      (detection_prob == "80") & (`85` > 0) ~ "85%",
      (detection_prob == "80") & (`90` > 0) ~ "90%",
      (detection_prob == "80") & (`95` > 0) ~ "95%",
      (detection_prob == "90") & (`90` > 0) ~ "90%",
      (detection_prob == "90") & (`95` > 0) ~ "95%",
    ),
    New_Conf_Range = case_when(
      `Filtered_Lowest_Conf` == `Highest Confidence` ~ `Highest Confidence`,
      TRUE ~ paste0(`Filtered_Lowest_Conf`, " - ", `Highest Confidence`)
    )) %>% 
    select(`Site Number`, `Scientific Name`, `Common Name`, Family, `New_Conf_Range`, 
           `Prob_Detections`) %>% 
    relocate(`New_Conf_Range`, .before = `Prob_Detections`)
  
  return(filtered_df)
}

species_stats = function(site) {
  val = total_species %>% filter(`Site Number` == site) %>% pull(n)
  return(val)
}

detections_stats = function(site) {
  val = total_detections %>% filter(`Site Number` == site) %>% pull(`Total Site Detections`)
  return(val)
}

site_ownership = function(site) {
  val = geogs %>% filter(ARU == site) %>% pull(`Public or Private`)
  return(val)
}

site_biome = function(site) {
  val = geogs %>% filter(ARU == site) %>% pull(`Biome Type`)
  return(val)
}

makesitemap = function(site) {
  
  # use existing geogs df to create new temp df where new col shows pin marker color depending on if site is what user has selected and if it is public or private
  
  geogs_sites = geogs %>% 
    mutate(Marker_color = case_when(
      ARU == site ~ "blue",  # User's selected site
      `Public or Private` == "Public" ~ "lightgreen",  # If public and not user's selected site
      TRUE ~ "green"  # If private and not user's selected site
    ),
    Icon_type = case_when(
      `Biome Type` == "Forest" ~ "tree",
      TRUE ~ "leaf" # If grassland
    ))
  
  tlat = geogs_sites %>% 
    filter(ARU == site) %>% 
    pull(`Township Lat`)
  
  tlong = geogs_sites %>% 
    filter(ARU == site) %>% 
    pull(`Township Long`)
  
  map_icon = makeAwesomeIcon(
    icon = geogs_sites$Icon_type,
    library = "fa",
    markerColor = geogs_sites$Marker_color,
    iconColor = "#264037"
  )
  
  map = leaflet(options = leafletOptions(zoomSnap = 0.25,
                                         zoomControl = FALSE)) %>% 
    setView(lng = tlong, lat = tlat, zoom = 14) %>% 
    addTiles() %>% 
    addProviderTiles("OpenStreetMap.HOT") %>%  
    addAwesomeMarkers(lng = geogs_sites$`Township Long`, 
                      lat = geogs_sites$`Township Lat`, 
                      icon = map_icon,
                      label = glue("Site {geogs_sites$ARU}"),
                      layerId = geogs_sites$ARU,
                      group = "sites") %>%
    addControl(html = "<strong>Biome Type</strong> <br> <i class='fa fa-tree'></i> Forest <br> <i class='fa fa-leaf'></i> Grassland",
               position = "bottomright") %>% 
    addLegend(
      position = "bottomright",  # Position the legend
      colors = c("#70ac26", "#bbfa71", "#2596be"),
      labels = c("Private", "Public", "Selected"),
      title = "Type of Site", # Set a title
      opacity = 1
    ) %>%
    onRender(
      "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }") %>%
    addResetMapButton() %>% 
    addSearchFeatures(targetGroups = "sites", options = searchFeaturesOptions(
      moveToLocation = FALSE,
      zoom = 14,
      autoCollapse = TRUE,
      hideMarkerOnCollapse = FALSE,
      textPlaceholder = "Search by Site Number...",
      marker = list(icon = NULL, animate = TRUE, circle = list(radius = 10, weight = 3, color
                                                               = "#fe6100", stroke = TRUE, fill = FALSE))
    ))
  
  return(map)
}

createbirdplot = function(site, detection_prob="60", text_scale = 1) {
  filtered_df = filtersites(site, detection_prob) %>% 
    group_by(Family) %>% 
    count()
  filtered_df$Family = factor(filtered_df$Family, levels = unique_families$Family) # ensures that even if a family has no birds at a site, the empty family bar is still shown on plot
  plottitle = paste0("Number of Species Detected within Each Avian Group at Site ", site)
  maxn = max(filtered_df$n) + 10
  adjusted_lineheight = 0.55 / text_scale
  ggplot(filtered_df) +
    geom_col(aes(x = Family, y = n, fill = Family),
             color = "#264037") +
    geom_text(aes(x = Family, y = n, label = n),
              color = "#264037",
              family = 'libre',
              fontface = "bold",
              size = 18/.pt * text_scale,
              vjust = -0.5) +
    scale_x_discrete(labels = label_wrap(11), drop = FALSE) +
    scale_y_continuous(limits = c(0, maxn), breaks = seq(0, maxn, by = 10)) +
    scale_fill_manual(values = c("Songbirds" = "#ffb000", "Waterfowl & Wading Birds" = "#fe6100", "Owls & Other Raptors" = "#dc267f", "Shorebirds" = "#785ef0", "Other Land Birds" = "#648fff")) +
    labs(x = "Avian Group", y = "Species Detected",
         title = str_wrap(plottitle, 50),
         subtitle = str_wrap("Click on a bar to explore the corresponding species in the data table!", 70)) +
    theme(legend.position = "none",
          axis.text = element_text(family = 'libre',
                                   size = 18 * text_scale,
                                   color = "#264037",
                                   lineheight = adjusted_lineheight),
          axis.title = element_text(family = 'libre',
                                    size = 20 * text_scale,
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          #legend.title = element_text(family = 'libre',
          #size = 20,
          #color = "#264037"),
          #legend.text = element_text(family = 'libre',
          #size = 16,
          #color = "#264037"),
          plot.title = element_text(family = 'libre',
                                    size = 24 * text_scale,
                                    face = 'bold',
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.subtitle = element_text(family = 'libre',
                                       size = 19 * text_scale,
                                       color = "#264037",
                                       lineheight = adjusted_lineheight),
          #panel.grid = element_line(color = "#ddebe6"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #legend.key = element_rect(color = "#f3f3ed",
          #size = 0.5),
          #legend.background = element_rect(fill = "#f3f3ed"),
          panel.background = element_rect(color = "#264037",
                                          fill = "#ddebe6"),
          plot.background = element_rect(linewidth = 1,
                                         color = "#c5d1cd",
                                         fill = "#fffffa"),
          plot.margin = margin(t = 14, r = 60, b = 14, l = 14)
    ) 
}

createdatehistogram = function(site, species="overall", detection_prob="60", text_scale = 1) {
  if (species == "overall") {
    df = cleaningsites %>% 
      filter(`Site Number` == site,
             as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob))
    
    overalldatelist = list()
    overallcountlist = list()
    
    for (f in 1:nrow(df)) {
      dtlistchar = str_sub(df[f, "List of Datetimes"], 3, -3)
      dtcharlist = (str_split(dtlistchar, "\\], \\["))[[1]]
      
      datelist = list()
      countlist = list()
      
      for (i in 1:length(dtcharlist)) {
        dtcharcount = (str_split(dtcharlist[[i]], ", "))[[1]]
        
        dtchar = str_sub(dtcharcount[[1]], 2, -2)
        date = ymd(substr(dtchar, 1, 8))
        datelist = append(datelist, list(date))
        
        count = as.numeric(dtcharcount[[2]])
        countlist = append(countlist, list(count))
      }
      
      overalldatelist = append(overalldatelist, datelist)
      overallcountlist = append(overallcountlist, countlist)
    }
    
    tempdf = data.frame("Dates" = as.Date(unlist(overalldatelist)),
                        "Counts" = unlist(overallcountlist)) %>%
      group_by(Dates) %>% 
      summarize(Count = sum(Counts)) %>% 
      arrange(Dates)
    
    plottitle = paste0("Bird Detections at Site ", site, " by Date")
    adjusted_lineheight = 0.55 / text_scale
    
    maxn = max(tempdf$Count) + 20
    
    ggplot(tempdf) +
      geom_histogram(aes(x = Dates, weight = Count),
                     color = "#264037",
                     fill = "#ffb000",
                     #bins = numbins,
                     binwidth = 1) +
      #geom_text(aes(x = Dates, y = Count, label = Count),
      #color = "#264037",
      #family = 'libre',
      #fontface = "bold",
      #size = 18/.pt, #* text_scale,
      #vjust = -0.5) +
      scale_x_date(date_breaks = "4 days") +
      scale_y_continuous(limits = c(0, maxn), n.breaks = 7) + #breaks = seq(0, maxn, by = 200)) +
      labs(x = "Dates", y = "Number of Detections",
           subtitle = "60% detection probability or higher; each bar represents 1 day",
           title = str_wrap(plottitle, 50)) +
      #theme_bw(base_size = 18) +
      theme(
        axis.text = element_text(family = 'libre',
                                 size = 18 * text_scale,
                                 color = "#264037",
                                 lineheight = adjusted_lineheight),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1),
        axis.title = element_text(family = 'libre',
                                  size = 20 * text_scale,
                                  color = "#264037",
                                  lineheight = adjusted_lineheight),
        plot.title = element_text(family = 'libre',
                                  size = 24 * text_scale,
                                  face = 'bold',
                                  color = "#264037",
                                  lineheight = adjusted_lineheight),
        plot.subtitle = element_text(family = 'libre',
                                     size = 19 * text_scale,
                                     color = "#264037",
                                     lineheight = adjusted_lineheight),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "#8fcdcc"),
        panel.grid.minor.y = element_line(color = "#8fcdcc"),
        panel.background = element_rect(color = "#264037",
                                        fill = "#ddebe6"),
        plot.background = element_rect(linewidth = 1,
                                       color = "#c5d1cd",
                                       fill = "#fffffa"),
        plot.margin = margin(t = 14, r = 30, b = 14, l = 14)
      )
  }
  else {
    df = cleaningsites %>% 
      filter(`Site Number` == site,
             `Common Name` == species,
             as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob))
    
    dtlistchar = str_sub(df %>% pull(`List of Datetimes`), 3, -3)
    dtcharlist = (str_split(dtlistchar, "\\], \\["))[[1]]
    
    datelist = list()
    countlist = list()
    
    for (i in 1:length(dtcharlist)) {
      dtcharcount = (str_split(dtcharlist[[i]], ", "))[[1]]
      
      dtchar = str_sub(dtcharcount[[1]], 2, -2)
      date = ymd(substr(dtchar, 1, 8))
      datelist = append(datelist, list(date))
      
      count = as.numeric(dtcharcount[[2]])
      countlist = append(countlist, list(count))
    }
    
    tempdf = data.frame("Dates" = as.Date(unlist(datelist)),
                        "Counts" = unlist(countlist)) %>%
      group_by(Dates) %>% 
      summarize(Count = sum(Counts)) %>% 
      arrange(Dates)
    
    val = if (species %in% sgcnbirds_wsymbol | species == "Eastern Screech-Owl²") {str_sub(species, 1, -2)} else {species}
    plottitle = paste0(val, " Detections at Site ", site, " by Date")
    adjusted_lineheight = 0.55 / text_scale
    
    maxn = max(tempdf$Count) + 2
    
    if (sum(tempdf$Count) < 8) {
      
      ggplot(tempdf) +
        geom_histogram(aes(x = Dates, weight = Count),
                       color = "#264037",
                       fill = "#ffb000",
                       #bins = numbins,
                       binwidth = 1) +
        #geom_text(aes(x = Dates, y = Count, label = Count),
        #color = "#264037",
        #family = 'libre',
        #fontface = "bold",
        #size = 18/.pt, #* text_scale,
        #vjust = -0.5) +
        scale_x_date(date_breaks = "1 day") +
        scale_y_continuous(limits = c(0, maxn), breaks = seq(0, maxn, 1)) +
        labs(x = "Dates", y = "Number of Detections",
             subtitle = "60% detection probability or higher; each bar represents 1 day",
             title = str_wrap(plottitle, 50)) +
        #theme_bw(base_size = 18) +
        theme(
          
          axis.text = element_text(family = 'libre',
                                   size = 18 * text_scale,
                                   color = "#264037",
                                   lineheight = adjusted_lineheight),
          axis.text.x = element_text(
            angle = 45,
            vjust = 1,
            hjust = 1),
          axis.title = element_text(family = 'libre',
                                    size = 20 * text_scale,
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.title = element_text(family = 'libre',
                                    size = 24 * text_scale,
                                    face = 'bold',
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.subtitle = element_text(family = 'libre',
                                       size = 19 * text_scale,
                                       color = "#264037",
                                       lineheight = adjusted_lineheight),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "#8fcdcc"),
          panel.grid.minor.y = element_line(color = "#8fcdcc"),
          panel.background = element_rect(color = "#264037",
                                          fill = "#ddebe6"),
          plot.background = element_rect(linewidth = 1,
                                         color = "#c5d1cd",
                                         fill = "#fffffa"),
          plot.margin = margin(t = 14, r = 30, b = 14, l = 14)
        )
    }
    
    else {
      ggplot(tempdf) +
        geom_histogram(aes(x = Dates, weight = Count),
                       color = "#264037",
                       fill = "#ffb000",
                       #bins = numbins,
                       binwidth = 1) +
        #geom_text(aes(x = Dates, y = Count, label = Count),
        #color = "#264037",
        #family = 'libre',
        #fontface = "bold",
        #size = 18/.pt, #* text_scale,
        #vjust = -0.5) +
        scale_x_date(date_breaks = "4 days") +
        scale_y_continuous(limits = c(0, maxn), n.breaks = 7) +
        labs(x = "Dates", y = "Number of Detections",
             subtitle = "60% detection probability or higher; each bar represents 1 day",
             title = str_wrap(plottitle, 50)) +
        #theme_bw(base_size = 17) +
        theme(
          
          axis.text = element_text(family = 'libre',
                                   size = 18 * text_scale,
                                   color = "#264037",
                                   lineheight = adjusted_lineheight),
          axis.text.x = element_text(
            angle = 45,
            vjust = 1,
            hjust = 1),
          axis.title = element_text(family = 'libre',
                                    size = 20 * text_scale,
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.title = element_text(family = 'libre',
                                    size = 24 * text_scale,
                                    face = 'bold',
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.subtitle = element_text(family = 'libre',
                                       size = 19 * text_scale,
                                       color = "#264037",
                                       lineheight = adjusted_lineheight),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "#8fcdcc"),
          panel.grid.minor.y = element_line(color = "#8fcdcc"),
          panel.background = element_rect(color = "#264037",
                                          fill = "#ddebe6"),
          plot.background = element_rect(linewidth = 1,
                                         color = "#c5d1cd",
                                         fill = "#fffffa"),
          plot.margin = margin(t = 14, r = 30, b = 14, l = 14)
        )
    }
  }
}

createtimehistogram = function(site, species="overall", detection_prob="60", text_scale = 1) {
  if (species == "overall") {
    df = cleaningsites %>% 
      filter(`Site Number` == site,
             as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob))
    
    overalltimelist = list()
    overallcountlist = list()
    
    for (f in 1:nrow(df)) {
      dtlistchar = str_sub(df[f, "List of Datetimes"], 3, -3)
      dtcharlist = (str_split(dtlistchar, "\\], \\["))[[1]]
      
      timelist = list()
      countlist = list()
      
      for (i in 1:length(dtcharlist)) {
        dtcharcount = (str_split(dtcharlist[[i]], ", "))[[1]]
        
        dtchar = str_sub(dtcharcount[[1]], 2, -2)
        hr = substr(dtchar, 10, 11)
        min = substr(dtchar, 12, 13)
        time = glue(hr, ":", min, ":00")
        timelist = append(timelist, list(time))
        
        count = as.numeric(dtcharcount[[2]])
        countlist = append(countlist, list(count))
      }
      
      overalltimelist = append(overalltimelist, timelist)
      overallcountlist = append(overallcountlist, countlist)
    }
    
    dawn = c("05", "06", "07", "08", "09")
    dusk = c("19", "20", "21", "22", "23")
    
    tempdf = data.frame("Times" = as_hms(unlist(overalltimelist)),
                        "Counts" = unlist(overallcountlist)) %>%
      group_by(Times) %>% 
      summarize(Count = sum(Counts)) %>% 
      arrange(Times) %>% 
      mutate(DayPeriod = case_when(
        substr(Times, 1, 2) %in% dawn ~ "Morning",
        TRUE ~ "Evening" # if times within dusk interval
      ))
    
    tempdf$DayPeriod = factor(tempdf$DayPeriod, levels = c("Morning", "Evening"))
    
    plottitle = paste0("Bird Detections at Site ", site, " by Time of Day")
    subtt = "60% detection probability or higher; each bar represents a 1-hour time interval"
    adjusted_lineheight = 0.55 / text_scale
    
    maxn = max(tempdf$Count) + 20
    
    ggplot(tempdf) +
      geom_histogram(aes(x = Times, weight = Count),
                     color = "#264037",
                     fill = "#ffb000",
                     boundary = 0,
                     binwidth = 3600) + # 1 hour bins
      facet_wrap(~DayPeriod, scales = "free_x", drop = FALSE) +
      facetted_pos_scales(
        x = list(
          DayPeriod == "Morning" ~ scale_x_time(breaks = c(as_hms("05:00:00"), as_hms("06:00:00"),
                                                           as_hms("7:00:00"), as_hms("08:00:00"), 
                                                           as_hms("09:00:00")),
                                                labels = scales::label_time(format = "%H:%M")),
          DayPeriod == "Evening" ~ scale_x_time(breaks = c(as_hms("19:00:00"), as_hms("20:00:00"), 
                                                           as_hms("21:00:00"), as_hms("22:00:00"), 
                                                           as_hms("23:00:00")),
                                                labels = scales::label_time(format = "%H:%M"))
        )
      ) +
      scale_y_continuous(n.breaks = 7) +
      labs(x = "Times", y = "Number of Detections",
           title = str_wrap(plottitle, 50),
           subtitle = subtt) +
      #theme_bw(base_size = 17) +
      theme(
        axis.text = element_text(family = 'libre',
                                 size = 18 * text_scale,
                                 color = "#264037",
                                 lineheight = adjusted_lineheight),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1),
        axis.title = element_text(family = 'libre',
                                  size = 20 * text_scale,
                                  color = "#264037",
                                  lineheight = adjusted_lineheight),
        plot.title = element_text(family = 'libre',
                                  size = 24 * text_scale,
                                  face = 'bold',
                                  color = "#264037",
                                  lineheight = adjusted_lineheight),
        plot.subtitle = element_text(family = 'libre',
                                     size = 19 * text_scale,
                                     color = "#264037",
                                     lineheight = adjusted_lineheight),
        strip.text = element_text(family = 'libre',
                                  size = 18 * text_scale,
                                  color = "#264037"),
        strip.background = element_rect(fill = "#8fcdcc"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "#8fcdcc"),
        panel.grid.minor.y = element_line(color = "#8fcdcc"),
        panel.background = element_rect(color = "#264037",
                                        fill = "#ddebe6"),
        plot.background = element_rect(linewidth = 1,
                                       color = "#c5d1cd",
                                       fill = "#fffffa"),
        plot.margin = margin(t = 14, r = 30, b = 14, l = 14)
      )
  }
  else {
    df = cleaningsites %>% 
      filter(`Site Number` == site,
             `Common Name` == species,
             as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob))
    
    dtlistchar = str_sub(df %>% pull(`List of Datetimes`), 3, -3)
    dtcharlist = (str_split(dtlistchar, "\\], \\["))[[1]]
    
    timelist = list()
    countlist = list()
    
    for (i in 1:length(dtcharlist)) {
      dtcharcount = (str_split(dtcharlist[[i]], ", "))[[1]]
      
      dtchar = str_sub(dtcharcount[[1]], 2, -2)
      hr = substr(dtchar, 10, 11)
      min = substr(dtchar, 12, 13)
      time = glue(hr, ":", min, ":00")
      timelist = append(timelist, list(time))
      
      count = as.numeric(dtcharcount[[2]])
      countlist = append(countlist, list(count))
    }
    
    dawn = c("05", "06", "07", "08", "09")
    dusk = c("19", "20", "21", "22", "23")
    
    tempdf = data.frame("Times" = as_hms(unlist(timelist)),
                        "Counts" = unlist(countlist)) %>%
      group_by(Times) %>% 
      summarize(Count = sum(Counts)) %>% 
      arrange(Times) %>% 
      mutate(DayPeriod = case_when(
        substr(Times, 1, 2) %in% dawn ~ "Morning",
        TRUE ~ "Evening" # if times within dusk interval
      ))
    
    tempdf$DayPeriod = factor(tempdf$DayPeriod, levels = c("Morning", "Evening"))
    
    val = if (species %in% sgcnbirds_wsymbol | species == "Eastern Screech-Owl²") {str_sub(species, 1, -2)} else {species}
    plottitle = paste0(val, " Detections at Site ", site, " by Time of Day")
    subtt = "60% detection probability or higher; each bar represents a 1-hour time interval"
    adjusted_lineheight = 0.55 / text_scale
    
    maxn = max(tempdf$Count) + 2
    
    if (sum(tempdf$Count) < 8) {
      
      ggplot(tempdf) +
        geom_histogram(aes(x = Times, weight = Count),
                       color = "#264037",
                       fill = "#ffb000",
                       boundary = 0,
                       binwidth = 3600) + # 1 hour bins
        facet_wrap(~DayPeriod, scales = "free_x", drop = FALSE) +
        facetted_pos_scales(
          x = list(
            DayPeriod == "Morning" ~ scale_x_time(breaks = c(as_hms("05:00:00"), as_hms("06:00:00"),
                                                             as_hms("7:00:00"), as_hms("08:00:00"), 
                                                             as_hms("09:00:00")),
                                                  labels = scales::label_time(format = "%H:%M")),
            DayPeriod == "Evening" ~ scale_x_time(breaks = c(as_hms("19:00:00"), as_hms("20:00:00"), 
                                                             as_hms("21:00:00"), as_hms("22:00:00"), 
                                                             as_hms("23:00:00")),
                                                  labels = scales::label_time(format = "%H:%M"))
          )
        ) +
        scale_y_continuous() +
        labs(x = "Times", y = "Number of Detections",
             title = str_wrap(plottitle, 50),
             subtitle = subtt) +
        #theme_bw(base_size = 17) +
        theme(
          axis.text = element_text(family = 'libre',
                                   size = 18 * text_scale,
                                   color = "#264037",
                                   lineheight = adjusted_lineheight),
          axis.text.x = element_text(
            angle = 45,
            vjust = 1,
            hjust = 1),
          axis.title = element_text(family = 'libre',
                                    size = 20 * text_scale,
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.title = element_text(family = 'libre',
                                    size = 24 * text_scale,
                                    face = 'bold',
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.subtitle = element_text(family = 'libre',
                                       size = 19 * text_scale,
                                       color = "#264037",
                                       lineheight = adjusted_lineheight),
          strip.text = element_text(family = 'libre',
                                    size = 18 * text_scale,
                                    color = "#264037"),
          strip.background = element_rect(fill = "#8fcdcc"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "#8fcdcc"),
          panel.grid.minor.y = element_line(color = "#8fcdcc"),
          panel.background = element_rect(color = "#264037",
                                          fill = "#ddebe6"),
          plot.background = element_rect(linewidth = 1,
                                         color = "#c5d1cd",
                                         fill = "#fffffa"),
          plot.margin = margin(t = 14, r = 30, b = 14, l = 14)
        )
    }
    
    else {
      ggplot(tempdf) +
        geom_histogram(aes(x = Times, weight = Count),
                       color = "#264037",
                       fill = "#ffb000",
                       boundary = 0,
                       binwidth = 3600) + # 1 hour bins
        facet_wrap(~DayPeriod, scales = "free_x", drop = FALSE) +
        facetted_pos_scales(
          x = list(
            DayPeriod == "Morning" ~ scale_x_time(breaks = c(as_hms("05:00:00"), as_hms("06:00:00"),
                                                             as_hms("7:00:00"), as_hms("08:00:00"), 
                                                             as_hms("09:00:00")),
                                                  labels = scales::label_time(format = "%H:%M")),
            DayPeriod == "Evening" ~ scale_x_time(breaks = c(as_hms("19:00:00"), as_hms("20:00:00"), 
                                                             as_hms("21:00:00"), as_hms("22:00:00"), 
                                                             as_hms("23:00:00")),
                                                  labels = scales::label_time(format = "%H:%M"))
          )
        ) +
        scale_y_continuous(n.breaks = 5) +
        labs(x = "Times", y = "Number of Detections",
             title = str_wrap(plottitle, 50),
             subtitle = subtt) +
        #theme_bw(base_size = 17) +
        theme(
          axis.text = element_text(family = 'libre',
                                   size = 18 * text_scale,
                                   color = "#264037",
                                   lineheight = adjusted_lineheight),
          axis.text.x = element_text(
            angle = 45,
            vjust = 1,
            hjust = 1),
          axis.title = element_text(family = 'libre',
                                    size = 20 * text_scale,
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.title = element_text(family = 'libre',
                                    size = 24 * text_scale,
                                    face = 'bold',
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          plot.subtitle = element_text(family = 'libre',
                                       size = 18 * text_scale,
                                       color = "#264037",
                                       lineheight = adjusted_lineheight),
          strip.text = element_text(family = 'libre',
                                    size = 18 * text_scale,
                                    color = "#264037"),
          strip.background = element_rect(fill = "#8fcdcc"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "#8fcdcc"),
          panel.grid.minor.y = element_line(color = "#8fcdcc"),
          panel.background = element_rect(color = "#264037",
                                          fill = "#ddebe6"),
          plot.background = element_rect(linewidth = 1,
                                         color = "#c5d1cd",
                                         fill = "#fffffa"),
          plot.margin = margin(t = 14, r = 30, b = 14, l = 14)
        )
    }
  }
}

makespeciesmap = function(species, detection_prob="60") {
  
  # find which sites have detections of this species
  
  truesites = cleaningsites %>%
    filter(`Common Name` == species,
           as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob)) %>% 
    pull(`Site Number`)
  
  # use existing geogs df to create new temp df where new cols show if site has species detection or not, and if so, icon color set to white
  
  geogs_with_species = geogs %>% 
    mutate(Species_present = case_when(
      ARU %in% truesites ~ "Yes",
      TRUE ~ "No"
    ),
    Marker_color = case_when(
      Species_present == "Yes" ~ "blue", # sites where species found (public and private)
      `Public or Private` == "Public" ~ "lightgreen", # if public and species not found there
      TRUE ~ "green" # if private and species not found there
    ),
    Icon_type = case_when(
      `Biome Type` == "Forest" ~ "tree",
      TRUE ~ "leaf" # If grassland
    ))
  
  # creating dynamic icons and map based on presence/absence of species at sites
  
  species_map_icon = makeAwesomeIcon(
    icon = geogs_with_species$Icon_type,
    library = "fa",
    markerColor = geogs_with_species$Marker_color,
    iconColor = "#264037"
  )
  
  speciesmap = leaflet(options = leafletOptions(zoomSnap = 0.25,
                                                zoomControl = FALSE)) %>% 
    setView(lng = -94, lat = 45, zoom = 6.75) %>% 
    addTiles() %>% 
    addProviderTiles("OpenStreetMap.HOT") %>%  
    addAwesomeMarkers(lng = geogs_with_species$`Township Long`, 
                      lat = geogs_with_species$`Township Lat`, 
                      icon = species_map_icon,
                      label = glue("Site {geogs_with_species$ARU}"),
                      layerId = geogs_with_species$ARU,
                      group = "sites") %>%
    addControl(html = "<strong>Biome Type</strong> <br> <i class='fa fa-tree'></i> Forest <br> <i class='fa fa-leaf'></i> Grassland",
               position = "bottomright") %>% 
    addLegend(
      position = "bottomright",  # Position the legend
      colors = c("#70ac26", "#bbfa71", "#2596be"),
      labels = c("Private", "Public", "Sites with Species"),
      title = "Type of Site", # Set a title
      opacity = 1
    ) %>%
    onRender(
      "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }") %>%
    addResetMapButton()
  
  return(speciesmap)
}

filterspecies = function(species, detection_prob="60") {
  speciessites = cleaningsites %>% 
    filter(`Common Name` == species,
           as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob)) %>% 
    rename("60" = "Number of Detections, 60% Confidence",
           "65" = "Number of Detections, 65% Confidence",
           "70" = "Number of Detections, 70% Confidence",
           "75" = "Number of Detections, 75% Confidence",
           "80" = "Number of Detections, 80% Confidence",
           "85" = "Number of Detections, 85% Confidence",
           "90" = "Number of Detections, 90% Confidence",
           "95" = "Number of Detections, 95% Confidence") %>% 
    mutate(Prob_Detections = case_when(
      `Rare Detection` == "yes" ~ `Total Detections`,
      detection_prob == "60" ~ rowSums(across(c(`60`, `65`, `70`, `75`, `80`, `85`, `90`, `95`))),
      detection_prob == "70" ~ rowSums(across(c(`70`, `75`, `80`, `85`, `90`, `95`))),
      detection_prob == "80" ~ rowSums(across(c(`80`, `85`, `90`, `95`))),
      detection_prob == "90" ~ rowSums(across(c(`90`, `95`)))
    ),
    Filtered_Lowest_Conf = case_when(
      `Rare Detection` == "yes" ~ "99%",
      (detection_prob == "60") & (`60` > 0) ~ "60%",
      (detection_prob == "60") & (`65` > 0) ~ "65%",
      (detection_prob == "60") & (`70` > 0) ~ "70%",
      (detection_prob == "60") & (`75` > 0) ~ "75%",
      (detection_prob == "60") & (`80` > 0) ~ "80%",
      (detection_prob == "60") & (`85` > 0) ~ "85%",
      (detection_prob == "60") & (`90` > 0) ~ "90%",
      (detection_prob == "60") & (`95` > 0) ~ "95%",
      (detection_prob == "70") & (`70` > 0) ~ "70%",
      (detection_prob == "70") & (`75` > 0) ~ "75%",
      (detection_prob == "70") & (`80` > 0) ~ "80%",
      (detection_prob == "70") & (`85` > 0) ~ "85%",
      (detection_prob == "70") & (`90` > 0) ~ "90%",
      (detection_prob == "70") & (`95` > 0) ~ "95%",
      (detection_prob == "80") & (`80` > 0) ~ "80%",
      (detection_prob == "80") & (`85` > 0) ~ "85%",
      (detection_prob == "80") & (`90` > 0) ~ "90%",
      (detection_prob == "80") & (`95` > 0) ~ "95%",
      (detection_prob == "90") & (`90` > 0) ~ "90%",
      (detection_prob == "90") & (`95` > 0) ~ "95%",
    ),
    New_Conf_Range = case_when(
      `Filtered_Lowest_Conf` == `Highest Confidence` ~ `Highest Confidence`,
      TRUE ~ paste0(`Filtered_Lowest_Conf`, " - ", `Highest Confidence`)
    )) %>% 
    select(`Site Number`, `Scientific Name`, `Common Name`, Family, `New_Conf_Range`, 
           `Prob_Detections`, `List of Datetimes`) %>% 
    relocate(`New_Conf_Range`, .before = `Prob_Detections`)
  
  joined_df = left_join(speciessites, geogs, by = c("Site Number" = "ARU")) %>% 
    select(-`Isolated or Embedded`, -`Township Lat`, -`Township Long`, -Family, 
           -`List of Datetimes`) %>%
    relocate(`Public or Private`, .after = `Site Number`) %>% 
    relocate(`Biome Type`, .after = `Public or Private`) %>% 
    arrange(desc(`Prob_Detections`))
  return(joined_df)
}

filterbiomes = function(species, detection_prob="60", biome) {
  speciessites = cleaningsites %>% 
    filter(`Common Name` == species,
           as.numeric(substr(`Highest Confidence`, 1, 2)) >= as.numeric(detection_prob)) %>% 
    rename("60" = "Number of Detections, 60% Confidence",
           "65" = "Number of Detections, 65% Confidence",
           "70" = "Number of Detections, 70% Confidence",
           "75" = "Number of Detections, 75% Confidence",
           "80" = "Number of Detections, 80% Confidence",
           "85" = "Number of Detections, 85% Confidence",
           "90" = "Number of Detections, 90% Confidence",
           "95" = "Number of Detections, 95% Confidence") %>% 
    mutate(Prob_Detections = case_when(
      `Rare Detection` == "yes" ~ `Total Detections`,
      detection_prob == "60" ~ rowSums(across(c(`60`, `65`, `70`, `75`, `80`, `85`, `90`, `95`))),
      detection_prob == "70" ~ rowSums(across(c(`70`, `75`, `80`, `85`, `90`, `95`))),
      detection_prob == "80" ~ rowSums(across(c(`80`, `85`, `90`, `95`))),
      detection_prob == "90" ~ rowSums(across(c(`90`, `95`)))
    ),
    Filtered_Lowest_Conf = case_when(
      `Rare Detection` == "yes" ~ "99%",
      (detection_prob == "60") & (`60` > 0) ~ "60%",
      (detection_prob == "60") & (`65` > 0) ~ "65%",
      (detection_prob == "60") & (`70` > 0) ~ "70%",
      (detection_prob == "60") & (`75` > 0) ~ "75%",
      (detection_prob == "60") & (`80` > 0) ~ "80%",
      (detection_prob == "60") & (`85` > 0) ~ "85%",
      (detection_prob == "60") & (`90` > 0) ~ "90%",
      (detection_prob == "60") & (`95` > 0) ~ "95%",
      (detection_prob == "70") & (`70` > 0) ~ "70%",
      (detection_prob == "70") & (`75` > 0) ~ "75%",
      (detection_prob == "70") & (`80` > 0) ~ "80%",
      (detection_prob == "70") & (`85` > 0) ~ "85%",
      (detection_prob == "70") & (`90` > 0) ~ "90%",
      (detection_prob == "70") & (`95` > 0) ~ "95%",
      (detection_prob == "80") & (`80` > 0) ~ "80%",
      (detection_prob == "80") & (`85` > 0) ~ "85%",
      (detection_prob == "80") & (`90` > 0) ~ "90%",
      (detection_prob == "80") & (`95` > 0) ~ "95%",
      (detection_prob == "90") & (`90` > 0) ~ "90%",
      (detection_prob == "90") & (`95` > 0) ~ "95%",
    ),
    New_Conf_Range = case_when(
      `Filtered_Lowest_Conf` == `Highest Confidence` ~ `Highest Confidence`,
      TRUE ~ paste0(`Filtered_Lowest_Conf`, " - ", `Highest Confidence`)
    )) %>% 
    select(`Site Number`, `Scientific Name`, `Common Name`, Family, `New_Conf_Range`, 
           `Prob_Detections`, `List of Datetimes`) %>% 
    relocate(`New_Conf_Range`, .before = `Prob_Detections`)
  
  joined_df = left_join(speciessites, geogs, by = c("Site Number" = "ARU")) %>% 
    select(-`Isolated or Embedded`, -`Township Lat`, -`Township Long`, -Family, 
           -`List of Datetimes`) %>%
    filter(`Biome Type` == biome) %>% 
    relocate(`Public or Private`, .after = `Site Number`) %>% 
    relocate(`Biome Type`, .after = `Public or Private`) %>% 
    arrange(desc(`Prob_Detections`))
  return(joined_df)
}

createspeciesplot = function(species, detection_prob="60", text_scale = 1) {
  filtered_df = filterspecies(species, detection_prob) %>% 
    group_by(`Biome Type`) %>% 
    count()
  filtered_df$`Biome Type` = factor(filtered_df$`Biome Type`, levels = c("Forest", "Grassland")) # ensures that even if a species is not represented at one biome type, the empty biome type bar is still shown on plot
  val = if (species %in% sgcnbirds_wsymbol | species == "Eastern Screech-Owl²") {str_sub(species, 1, -2)} else {species}
  plottitle = paste0(val, " Detection Sites by Biome")
  maxn = max(filtered_df$n) + 10
  adjusted_lineheight = 0.55 / text_scale
  ggplot(filtered_df) +
    geom_col(aes(x = `Biome Type`, y = n, fill = `Biome Type`),
             color = "#264037",
             show.legend=TRUE) +
    geom_text(aes(x = `Biome Type`, y = n, label = n),
              color = "#264037",
              family = 'libre',
              fontface = "bold",
              size = 18/.pt * text_scale,
              vjust = -0.5) +
    labs(x = "Biome Type", y = "Number of Sites",
         title = str_wrap(plottitle, 40)
    ) +
    scale_x_discrete(labels = label_wrap(15), drop = FALSE) +
    scale_y_continuous(limits = c(0, maxn), breaks = seq(0, maxn, by = 20)) +
    scale_fill_manual(values = c("Forest" = "#ffb000", "Grassland" = "#fe6100")) +
    theme(legend.position = "none",
          axis.text = element_text(family = 'libre',
                                   size = 18 * text_scale,
                                   color = "#264037",
                                   lineheight = adjusted_lineheight),
          axis.title = element_text(family = 'libre',
                                    size = 20 * text_scale,
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          #legend.title = element_text(family = 'libre',
          #size = 20,
          #color = "#264037"),
          #legend.text = element_text(family = 'libre',
          #size = 16,
          #color = "#264037"),
          plot.title = element_text(family = 'libre',
                                    size = 24 * text_scale,
                                    face = 'bold',
                                    color = "#264037",
                                    lineheight = adjusted_lineheight),
          #plot.subtitle = element_text(family = 'libre',
          #size = 20 * text_scale,
          #color = "#264037",
          #lineheight = adjusted_lineheight),
          #panel.grid = element_line(color = "#ddebe6"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #legend.key = element_rect(color = "#f3f3ed",
          #size = 0.5),
          #legend.background = element_rect(fill = "#f3f3ed"),
          panel.background = element_rect(color = "#264037",
                                          fill = "#ddebe6"),
          plot.background = element_rect(linewidth = 1,
                                         color = "#c5d1cd",
                                         fill = "#fffffa"),
          plot.margin = margin(t = 14, r = 30, b = 14, l = 14)
    )
}


# Data table containers that allow for hovering in Shiny

tab1tablehover = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(""),
      th("Site Number", title = "Number assigned to site", class = 'dt-head-left'),
      th("Scientific Name", title = "Species scientific name - click the links to learn more about each species.", class = 'dt-head-left'),
      th("Common Name", title = "Species common name", class = 'dt-head-left'),
      th("Avian Group", title = "Taxonomic category to which a given species belongs.", class = 'dt-head-left'),
      th("Detection Probability Range", title = "Each detection is given a probability score reflecting how likely it is to be a real detection of that species. For example, 95% means there is a 95% chance the detection is a true positive. These probability scores are based on models we developed by manually reviewing thousands of recordings. This column shows the range of probability scores across all detections.", class = 'dt-head-left'),
      th("Total Detections", title = "The number of times this species was detected at this site above our confidence threshold. Because BirdNET analyzes audio in 3-second segments, a single bird can produce many detections, so this number reflects acoustic activity rather than the number of individual birds present.", class = 'dt-head-left')
    )
  )
))

tab2tablehover = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(""),
      th("Site Number", title = "Number assigned to site", class = 'dt-head-left'),
      th("Public or Private", title = "Ownership of site", class = 'dt-head-left'),
      th("Biome Type", title = "Primary vegetation type found at site, either 'forest' or 'grassland'", class = 'dt-head-left'),
      th("Scientific Name", title = "Species scientific name - click the links to learn more about each species.", class = 'dt-head-left'),
      th("Common Name", title = "Species common name", class = 'dt-head-left'),
      th("Detection Probability Range", title = "Each detection is given a probability score reflecting how likely it is to be a real detection of that species. For example, 95% means there is a 95% chance the detection is a true positive. These probability scores are based on models we developed by manually reviewing thousands of recordings. This column shows the range of probability scores across all detections.", class = 'dt-head-left'),
      th("Total Detections", title = "The number of times this species was detected at this site above our confidence threshold. Because BirdNET analyzes audio in 3-second segments, a single bird can produce many detections, so this number reflects acoustic activity rather than the number of individual birds present.", class = 'dt-head-left')
    )
  )
))

#### Shiny App (zoom-adjusted)

ui = fluidPage(
  #tags$script(src = "https://kit.fontawesome.com/df8147df33.js"),  # To use custom icons on map
  setBackgroundColor(color = "#f3f3ed"),
  includeCSS("www/biodiversity_styles.css"),
  useShinyjs(),
  useBusyIndicators(), # whenever something takes time to load, will be clear to user
  busyIndicatorOptions(spinner_type = "pulse", spinner_color = "#ff6b00",
                       spinner_size = "40px"),
  fav("crow"), # adding crow favicon from FontAwesome to app
  tags$head(
    tags$title("Sounds of Nature MN 2025")
  ),
  titlePanel(
    div(
      class = "header",
      img(src="Sounds_of_Nature_Logo_3.png", alt = "Sounds of Nature logo", 
          height = 175, width = 425),
      tags$div(class = "app_title", "2025 Avian Biodiversity Visualizer")
    )
    #fluidRow(
    #column(3, img(src="Sounds_of_Nature_Logo.png", height = 80, width = 240)),
    #column(9, tags$div(class = "app_title", "Sounds of Nature Minnesota 2025 Avian Biodiversity Visualizer"))
    #)
  ),
  shinybrowser::detect(),
  #"Your browser dimensions:",
  #textOutput("browser_dim"),
  br(),
  br(),
  sidebarPanel(id="sidebar",
               #img(src="Sounds_of_Nature_Logo.jpg", height = 80, width = 222),
               p(strong("Welcome to the ", em("Sounds of Nature Minnesota"), " Data Visualizer!"), "This dashboard allows users to explore the diversity of bird species detected on public and private sites across Minnesota. Our goal is to understand bird diversity across the state, particularly in areas that are understudided, like private lands. This work would not be possible without the help of our citizen science volunteers, who are helping to collect bird vocalization data using autonomous recording units (ARUs)."),
               br(),
               p(strong("Tab 1:", em("Search by Site"))),
               #p(tags$div(class = "sidebar_header", "Tab 1:<em>Search by Site</em>")),
               p("Sites were sampled during the spring migration and summer breeding periods using", em("Passive Acoustic Monitoring."), "We collected over 30,000 hours of audio data during the 2025 field season. Our analysis has yielded 7,951,449 BirdNET detections of 203 species across all sites. As of June 2026, we are completing manual validations of BirdNET detections to reduce false positives and assign accuracy probabilities to detections."),
               p(strong(em("Start")), "by zooming into your area of interest on the map. You can hover over pins to see their 5-digit site code. Alternatively, if you know your site code, you can enter it in the search bar in the top left corner of the map, below the 'Reset map view' button. Once you have found your site,", strong(em("click on a pin")), "to learn more about what birds were detected at that location. Use the selection menu below the statistics box to filter species detections by minimum detection probability.", em("Note: To protect sensitive species and landowner privacy, we have moved location coordinates to public sites within 3 miles (5 km) of sampling locations.")),
               p(strong(em("Next,")), "you will see a bar chart below the map representing the different avian groups detected at each site. Unfamiliar with any bird categories? Click on the bird icons below the plot to read about each avian group. In the plot, click on a bar (i.e., avian group) to filter the data table below to explore what species within that group were detected at a given site. Click on the scientific names in the table to learn more about each species. To view all the species detected at a site, click on the green ", strong("Show all birds at this site"), " button below the plot. Hover over column names or scroll below the data table for column descriptions."),
               p(strong(em("Finally,")), "scroll below the data table and column descriptions to view all bird detections over time at your selected site. Click on a species in the data table to see species-specific detections over time. Click the ", strong("Show all bird detections at this site"), " button below the column descriptions to reset the figures."),
               br(),
               p(strong("Tab 2:", em("Search by Species"))),
               #p(tags$div(class = "sidebar_header", "Tab 2:<em>Search by Species</em>")),
               p(strong(em("Click")), "the selection menu at the top and type in or scroll to search by species. Use the selection menu to the right to filter species detections by minimum detection probability. The map will update to highlight the sites where your species was detected (detections will appear as blue pins). The bar plot will also update to show the broad biome type(s) where the selected species was detected. See the table below for species-specific data by site, and click on a bar (i.e., biome type) in the plot above to further filter the data table by biome. Hover over column names or scroll below the data table for column descriptions."),
               width = 3,
  ),
  mainPanel(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Search by Site",
        htmlOutput("spacer1"),
        fluidRow(
          column(6, htmlOutput("siteselection")),
          column(2, hidden(actionButton(inputId = "zoomtomn", label = "Zoom to MN"))),
          column(4, hidden(actionButton(inputId = "resetmap", label = "Reset map and data")))
        ),
        br(),
        fluidRow(
          column(12, leafletOutput("map"))
        ),
        br(),
        htmlOutput("site_stats_header"),
        fluidRow(
          column(11, hidden(htmlOutput("site_statistics")))
        ),
        br(),
        fluidRow(
          column(6, hidden(selectInput("detectionprob1", "Select a Minimum Detection Probability",
                                       choices = c("60%", "70%", "80%", "90%"), multiple = FALSE,
                                       selected = "60%"))),
          column(6, hidden(htmlOutput("sitemsg")))
        ),
        br(),
        fluidRow(
          #column(1),
          column(12, tags$div(class = "plot", plotOutput("birdfamily", click = "familyclick", 
                                                         width = "840px", height = "400px")))#,
          #column(1)
        ),
        br(),
        fluidRow(
          tags$div(class = "bird_icons", 
                   hidden(imageOutput("songbirds", click = "songbirds_click")),
                   hidden(imageOutput("waterfowl", click = "waterfowl_click")),
                   hidden(imageOutput("owls", click = "owls_click")),
                   hidden(imageOutput("shorebirds", click = "shorebirds_click")),
                   hidden(imageOutput("landbirds", click = "landbirds_click"))
          )
        ),
        fluidRow(
          column(2, hidden(htmlOutput("songbirds_label"))),
          column(1),
          column(2, hidden(htmlOutput("waterfowl_label"))),
          column(2, hidden(htmlOutput("owls_label"))),
          column(2, hidden(htmlOutput("shorebirds_label"))),
          column(3, hidden(htmlOutput("landbirds_label")))
        ),
        fluidRow(
          column(3, hidden(htmlOutput("songbirds_text"))),
          column(2, hidden(htmlOutput("waterfowl_text"))),
          column(2, hidden(htmlOutput("owls_text"))),
          column(2, hidden(htmlOutput("shorebirds_text"))),
          column(3, hidden(htmlOutput("landbirds_text")))
        ),
        br(),
        br(),
        fluidRow(
          column(7, htmlOutput("familyselection")),
          column(1),
          column(4, hidden(actionButton(inputId = "showallbirds", label = "Show all birds at this site")))
        ),
        br(),
        fluidRow(
          column(12, tags$div(class = "table", DTOutput("table")))
        ),
        #br(),
        #fluidRow(
        #column(4),
        #column(4, 
        #hidden(downloadButton(outputId = "download_button1",
        #label = "Download data table as CSV"))),
        #column(4)
        #),
        br(),
        fluidRow(
          column(7, hidden(htmlOutput("columninfoheader1"))),
          column(5, hidden(actionButton(inputId = "tabledescriptions1", 
                                        label = "Show/Hide column descriptions")))
        ),
        br(),
        hidden(htmlOutput("columninfo1")),
        br(),
        br(),
        fluidRow(
          column(7, hidden(htmlOutput("hist_header"))),
          column(5, hidden(actionButton(inputId = "showallhist", 
                                        label = "Show all birds detections at this site")))
        ),
        hidden(htmlOutput("hist_text")),
        br(),
        plotOutput("datehistogram"),
        br(),
        plotOutput("timehistogram")
      ),
      tabPanel(
        title = "Search by Species",
        htmlOutput("spacer2"),
        fluidRow(
          column(1),
          column(5, selectInput("species", "Select a Species", 
                                choices = c("", unique_species$`Common Name`), multiple = FALSE)),
          column(6, selectInput("detectionprob2", "Minimum Detection Probability",
                                choices = c("60%", "70%", "80%", "90%"), multiple = FALSE,
                                selected = "60%"))
        ),
        fluidRow(
          column(2),
          column(8, htmlOutput("speciesselection")),
          column(2)
        ),
        br(),
        fluidRow(
          column(12, leafletOutput("speciesmap"))
        ),
        br(),
        br(),
        fluidRow(
          column(8, tags$div(class = "plot", plotOutput("speciesplot", click = "biomeclick",
                                                        width = "600px", height = "400px"))),
          column(4)
        ),
        br(),
        br(),
        fluidRow(
          column(7, htmlOutput("species_at_sites")),
          column(1),
          column(4, hidden(actionButton(inputId = "showallbiomes", label = "Show all sites with this species")))
        ),
        br(),
        tags$div(class = "table", DTOutput("speciestable")),
        #br(),
        #fluidRow(
        #column(4),
        #column(4, 
        #hidden(downloadButton(outputId = "download_button2",
        #label = "Download data table as CSV"))),
        #column(4)
        #),
        br(),
        fluidRow(
          column(7, hidden(htmlOutput("columninfoheader2"))),
          column(5, hidden(actionButton(inputId = "tabledescriptions2", label = "Show/Hide Column Descriptions")))
        ),
        br(),
        hidden(htmlOutput("columninfo2"))
      ),
      tabPanel(
        title = "About",
        htmlOutput("spacer3"),
        fluidRow(
          htmlOutput("about")
        ),
        br(),
        htmlOutput("go_to_github"),
        fluidRow(
          column(3),
          column(6, actionButton(inputId = "github", label = "Sounds of Nature MN GitHub Repository", 
                                 onclick = paste0("window.open('https://github.com/elena-west/sounds-of-nature-shiny', '_blank')"))),
          column(3)
        )
      )
    ),
    br(),
    br(),
    hr(),
    br(),
    htmlOutput("footer"),
    br(),
    fluidRow(
      #column(1),
      tags$img(src='uofm_logo.png', 
               alt = "University of Minnesota logo",
               height = "110px",
               align = "left"),
      tags$img(src='enrtf_logo.png', 
               alt = "Environment and Natural Resources Trust Fund logo",
               height = "110px",
               align = "left"),
      tags$img(src='mndnr_logo.png', 
               alt = "Minnesota Department of Natural Resources logo",
               height = "110px",
               align = "left"),
      tags$img(src='mncoop_logo.png', 
               alt = "Minnesota Cooperative Fish and Wildlife Research Unit logo",
               height = "110px",
               align = "left"),
      tags$img(src='audubon_logo.png', 
               alt = "Audubon logo",
               height = "110px",
               align = "left")
    ),
    br(),
    br(),
    width = 9
  )
)


server = function(input, output) {
  
  # variables holding default values to be changed for future use
  
  # tab 1
  currentsite = reactiveVal("")
  displayedsite = reactiveVal("")
  map = reactiveVal(globalmap) # default value is default map created earlier
  displayedstatsheader = reactiveVal("")
  displayedstats = reactiveVal("")
  sitemsg = reactiveVal("")
  currentprob1 = reactiveVal("60")
  birdbarplot = reactiveVal("")
  songbirds_text = reactiveVal("")
  waterfowl_text = reactiveVal("")
  owls_text = reactiveVal("")
  shorebirds_text = reactiveVal("")
  landbirds_text = reactiveVal("")
  songbirds_c = reactiveVal("hidden")
  waterfowl_c = reactiveVal("hidden")
  owls_c = reactiveVal("hidden")
  shorebirds_c = reactiveVal("hidden")
  landbirds_c = reactiveVal("hidden")
  displayedfamily = reactiveVal("")
  df = reactiveVal("")
  columninfoheader1 = reactiveVal("")
  columninfo1state = reactiveVal("shown")
  columninfo1 = reactiveVal("")
  hist_header = reactiveVal("")
  datehistogram = reactiveVal("")
  timehistogram = reactiveVal("")
  window_width = reactive(shinybrowser::get_width())
  window_height = reactive(shinybrowser::get_height())
  
  # tab 2
  currentspecies = reactiveVal("")
  currentprob2 = reactiveVal("60")
  speciesmsg = reactiveVal("")
  speciesmap = reactiveVal("")
  speciesplot = reactiveVal("")
  displayed_species_at_sites = reactiveVal("")
  speciesdf = reactiveVal("")
  columninfoheader2 = reactiveVal("")
  columninfo2state = reactiveVal("shown")
  columninfo2 = reactiveVal("")
  window_width = reactive(shinybrowser::get_width())
  window_height = reactive(shinybrowser::get_height())
  
  # when map clicked, table and plot updated to show data from specific site
  
  # tab 1
  observeEvent(input$map_marker_click, {
    
    currentsite(input$map_marker_click$id)
    
    map(makesitemap(currentsite()))
    
    displayedsite(
      paste(h4(paste0("You are now exploring birds at site ", currentsite(), ":")))
    )
    
    shinyjs::show("zoomtomn") # reveal zoom to MN button
    shinyjs::show("resetmap") # reveal reset button
    
    displayedstatsheader(
      paste(h4(paste0("Stats for site ", currentsite(), ":")), br())
    )
    
    shinyjs::show("detectionprob1") # reveal detection prob filtering
    
    currentprob1("60") # default
    totalspecies = species_stats(currentsite())
    totaldetections = prettyNum(detections_stats(currentsite()), big.mark = ",")
    currentownership = site_ownership(currentsite())
    currentbiome = site_biome(currentsite())
    
    displayedstats(
      paste(h6(tags$ul(
        tags$li(
          HTML(glue(
            "Site {currentsite()} is a <strong>{currentownership} {currentbiome}</strong> site."
          ))
        ),
        tags$li(
          HTML(glue(
            "<strong>{totalspecies}</strong> unique species and <strong>{totaldetections}</strong> total bird
            detections at site {currentsite()}."
          ))
        ), 
        tags$li(
          HTML(glue(
            "Compare to an average of <strong>{avg_species}</strong> unique species and an average of <strong>{avg_detections}</strong> total
            bird detections across all sites."
          ))
        )
      )))
    )
    shinyjs::show("site_statistics") # show site statistics
    
    df(filtersites(currentsite(), currentprob1()))
    
    birdbarplot({
      #diagonalpx = sqrt((window_width()**2)+(window_height()**2))
      scale_factor = 1440/window_width() #diagonalpx/1686 # diagonal of MacBook Pro
      #num = window_width()/1440
      #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
      createbirdplot(currentsite(), currentprob1(), text_scale = scale_factor)
    })
    
    shinyjs::show("songbirds") # reveal bird icons
    shinyjs::show("waterfowl") # reveal bird icons
    shinyjs::show("owls") # reveal bird icons
    shinyjs::show("shorebirds") # reveal bird icons
    shinyjs::show("landbirds") # reveal bird icons
    shinyjs::show("songbirds_label") # reveal bird icons
    shinyjs::show("waterfowl_label") # reveal bird icons
    shinyjs::show("owls_label") # reveal bird icons
    shinyjs::show("shorebirds_label") # reveal bird icons
    shinyjs::show("landbirds_label") # reveal bird icons
    shinyjs::hide("songbirds_text") # hide bird text
    shinyjs::hide("waterfowl_text") # hide bird text
    shinyjs::hide("owls_text") # hide bird text
    shinyjs::hide("shorebirds_text") # hide bird text
    shinyjs::hide("landbirds_text") # hide bird text
    
    displayedfamily(
      paste(h4(paste0("Showing all birds at site ", currentsite(), ":")), br())
    )
    
    columninfoheader1(
      paste(h4("How to read the data table above:"))
    )
    
    columninfo1(
      paste(
        h6(HTML(glue("<em>Audio was recorded at sites across Minnesota and processed using BirdNET,
                       a machine learning tool for identifying birds by sound. Detections were
                       filtered using statistical models to reduce false positives.</em>"))),
        br(),
        h6(HTML(glue("<strong>Avian Group:</strong> Taxonomic category to which a given species
                       belongs."))),
        h6(HTML(glue("<strong>Detection Probability Range:</strong> Each detection is given a probability score reflecting how likely it is to be a real detection of that species. For example, 95% means there is a 95% chance the detection is a true positive. These probability scores are based on models we developed by manually reviewing thousands of recordings. This column shows the range of probability scores across all detections."))),
        h6(HTML(glue("<strong>Total Detections:</strong> The number of times this species was
                       detected at this site above our confidence threshold. Because BirdNET analyzes
                       audio in 3-second segments, a single bird can produce many detections, so this
                       number reflects acoustic activity rather than the number of individual birds
                       present."))),
        br(),
        h6(HTML(glue("<strong>¹</strong> Species of Greatest Conservation Need (SGCN)"))),
        h6(HTML(glue("<strong>²</strong> Species in Need of Information (SNI)")))
      )
    )
    
    hist_header(
      paste(h4(paste0("Bird Detections at Site ", currentsite(), 
                      " by Date and Time of Day:")))
    )
    
    shinyjs::addClass(selector = "body", class = "loading-cursor") # show loading cursor
    datehistogram({
      #num = window_width()/1440
      #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
      scale_factor = 1440/window_width()
      createdatehistogram(site=currentsite(), detection_prob=currentprob1(), text_scale=scale_factor)
    })
    timehistogram({
      #num = window_width()/1440
      #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
      scale_factor = 1440/window_width()
      createtimehistogram(site=currentsite(), detection_prob=currentprob1(), text_scale=scale_factor)
    })
    shinyjs::delay(100, shinyjs::removeClass(selector = "body", class = "loading-cursor")) # back to normal cursor
    #shinyjs::show("download_button1") # show download button
    shinyjs::show("columninfoheader1") # show column info header
    shinyjs::show("columninfo1") # show column info by default
    shinyjs::show("tabledescriptions1") # show table descriptions button
    shinyjs::hide("showallbirds") # birds reset button hidden
    shinyjs::show("hist_header") # show histogram header
    shinyjs::show("hist_text") # show histogram description
  })
  
  # when zoom to MN button clicked, map view zooms in/out to show whole of MN
  
  # tab 1
  observeEvent(input$zoomtomn, {
    map(
      map() %>% 
        setView(lng = -94, lat = 45, zoom = 6.75)
    )
    shinyjs::hide("zoomtomn") # zoom to MN button hidden
  })
  
  # when reset map button clicked, map view shows whole of MN and resets map/data
  
  # tab 1
  observeEvent(input$resetmap, {
    map(globalmap)
    displayedsite(paste(h4("Select your Site Below:")))
    displayedstatsheader("")
    displayedstats("")
    birdbarplot("")
    displayedfamily("")
    df("")
    datehistogram("")
    timehistogram("")
    shinyjs::hide("site_statistics") # site statistics box hidden
    shinyjs::hide("detectionprob1") # hide detection prob filtering
    shinyjs::hide("sitemsg") # hide site error msg
    shinyjs::hide("showallbirds") # birds reset button hidden
    #shinyjs::hide("download_button1") # hide download button
    shinyjs::hide("hist_header") # hide histogram header
    shinyjs::hide("hist_text") # hide histogram description
    shinyjs::hide("showallhist") # histogram reset button hidden
    shinyjs::hide("columninfoheader1") # hide column info header
    shinyjs::hide("tabledescriptions1") # hide table descriptions button
    shinyjs::hide("columninfo1") # hide column info
    shinyjs::hide("zoomtomn") # zoom to MN button hidden
    shinyjs::hide("resetmap") # reset button hidden again
    shinyjs::hide("songbirds") # hide bird icons
    shinyjs::hide("waterfowl") # hide bird icons
    shinyjs::hide("owls") # hide bird icons
    shinyjs::hide("shorebirds") # hide bird icons
    shinyjs::hide("landbirds") # hide bird icons
    shinyjs::hide("songbirds_label") # hide bird icons
    shinyjs::hide("waterfowl_label") # hide bird icons
    shinyjs::hide("owls_label") # hide bird icons
    shinyjs::hide("shorebirds_label") # hide bird icons
    shinyjs::hide("landbirds_label") # hide bird icons
    shinyjs::hide("songbirds_text") # hide bird text
    shinyjs::hide("waterfowl_text") # hide bird text
    shinyjs::hide("owls_text") # hide bird text
    shinyjs::hide("shorebirds_text") # hide bird text
    shinyjs::hide("landbirds_text") # hide bird text
    columninfo1state("hidden")
    songbirds_c("hidden")
    waterfowl_c("hidden")
    owls_c("hidden")
    shorebirds_c("hidden")
    landbirds_c("hidden")
  })
  
  # when bar clicked on plot, table updated to show family-specific data from selected site
  
  # tab 1
  observeEvent(input$familyclick, {
    #family_clicked = unique_families$Family[which.min(abs(as.numeric(factor(unique_families$Family)) - bar_clicked))]
    
    # getting bar click coordinates to corresponds properly with avian groups regardless of plot size
    bar_clicked = input$familyclick$x
    n = nrow(unique_families)
    bar_index = round((bar_clicked - (1 / (2 * n))) / (1 / n)) + 1
    family_clicked = as.character(unique_families$Family[bar_index])
    
    df(filterfamily(currentsite(), family_clicked, currentprob1()))
    displayedfamily(
      paste(h4(paste0("Showing ", family_clicked, " at site ", currentsite(), ":")), br())
    )
    shinyjs::show("showallbirds") # reveal reset button
  })
  
  # when "show all" button clicked, table updated to show all birds at selected site (reset)
  
  # tab 1
  observeEvent(input$showallbirds, {
    df(filtersites(currentsite(), currentprob1()))
    displayedfamily(
      paste(h4(paste0("Showing all birds at site ", currentsite(), ":")), br())
    )
    
    shinyjs::hide("showallbirds") # reset button hidden again
  })
  
  # when bird icons clicked, descriptions pop up below
  
  # tab 1 
  observeEvent(input$songbirds_click, {
    if (songbirds_c() == "shown") {
      shinyjs::hide("songbirds_text") # hide bird text
      songbirds_c("hidden")
    }
    else {  # if songbirds_c() == "hidden"
      songbirds_text(
        paste(h6(HTML(glue("'Songbirds' typically refers to all birds in the order Passeriformes. Here,
               we use this term to refer to perching birds that have their own songs, such as
               finches, sparrows, and warblers. Songbirds are found in a variety of urban and natural
               habitats and can often be heard or seen early in the morning searching for nuts,
               seeds, fruit, or insects. <br> <br> <em>(Source: USFWS, Audubon Society)</em>"))))
      )
      shinyjs::show("songbirds_text") # reveal bird text
      songbirds_c("shown") 
    }
  })
  
  observeEvent(input$waterfowl_click, {
    if (waterfowl_c() == "shown") {
      shinyjs::hide("waterfowl_text") # hide bird text
      waterfowl_c("hidden")
    }
    else {  # if waterfowl_c() == "hidden"
      waterfowl_text(
        paste(h6(HTML(glue("Waterfowl are mostly web-footed, swimming birds within the order Anseriformes,
                 including ducks, geese, cranes, loons, and swans. Wading birds, like waterfowl, are
                 also dependent on aquatic habitats and are often found in wetlands, using their long
                 necks and legs to forage for fish, amphibians, and invertebrates in shallow water. <br> <br> <em>(Source: Minnesota DNR, 
                 Illinois DNR, Texas A&M)</em>"))))
      )
      shinyjs::show("waterfowl_text") # reveal bird text
      waterfowl_c("shown")
    }
  })
  
  observeEvent(input$owls_click, {
    if (owls_c() == "shown") {
      shinyjs::hide("owls_text") # hide bird text
      owls_c("hidden")
    }
    else {  # if owls_c() == "hidden"
      owls_text(
        paste(h6(HTML(glue("Owls (of the order Strigiformes) and other raptors such as hawks and 
                           falcons (Accipitriformes and Falconiformes) are known as 'birds of prey' 
                           due to their strong, grasping talons and carnivorous diets. While other 
                           raptors hunt during the day, most owls are typically active at night, 
                           relying on their sensitive hearing as they hunt for 
                           small mammals, reptiles, and even other birds. <br> <br> <em>(Source: Audubon 
                           Society, University of Missouri)</em>"))))
      )
      shinyjs::show("owls_text") # reveal bird text
      owls_c("shown")
    }
  })
  
  observeEvent(input$shorebirds_click, {
    if (shorebirds_c() == "shown") {
      shinyjs::hide("shorebirds_text") # hide bird text
      shorebirds_c("hidden")
    }
    else {  # if shorebirds_c() == "hidden"
      shorebirds_text(
        paste(h6(HTML(glue("Shorebirds are members of the order Charadriiformes and are often found in open
                 landscapes, such as shorelines, mudflats, and grasslands. Their uniquely
                 shaped beaks allow them to probe for crustaceans along beaches and dig up
                 insects beneath the soil. Most shorebirds are migratory and travel thousands of
                 miles every year. <br> <br> <em>(Source: NOAA, USFWS, Western Hemisphere Shorebird Reserve 
                  Network, Pacific Shorebird Conservation Initiative)</em>"))))
      )
      shinyjs::show("shorebirds_text") # reveal bird text
      shorebirds_c("shown")
    }
  })
  
  observeEvent(input$landbirds_click, {
    if (landbirds_c() == "shown") {
      shinyjs::hide("landbirds_text") # hide bird text
      landbirds_c("hidden")
    }
    else {  # if landbirds_c() == "hidden"
      landbirds_text(
        paste(h6("We use 'other land birds' as an umbrella term to refer to all other terrestrial
                 birds found in Minnesota, such as woodpeckers, corvids (ex: American Crow), and
                 cuckoos. Members of this group range in size from hummingbirds to
                 Wild Turkeys."))
      )
      shinyjs::show("landbirds_text") # reveal bird text
      landbirds_c("shown")
    }
  })
  
  # when "show/hide column descriptions" button clicked, column info displayed or hidden depending on current state
  
  # tab 1
  observeEvent(input$tabledescriptions1, {
    if (columninfo1state() == "shown") {
      shinyjs::hide("columninfoheader1") # hide column info header
      shinyjs::hide("columninfo1") # hide column descriptions
      columninfo1state("hidden")
    }
    else {  # if columninfo1state() == "hidden"
      shinyjs::show("columninfoheader1") # reveal column info header
      shinyjs::show("columninfo1") # reveal column descriptions
      columninfo1state("shown")
    }
  })
  
  # when row clicked in data table, date histogram for that species is shown
  
  # tab 1
  observeEvent(input$table_row_last_clicked, {
    idx = input$table_row_last_clicked
    species = df()[idx, "Common Name"] %>% pull()
    datehistogram({
      #num = window_width()/1440
      #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
      scale_factor = 1440/window_width()
      createdatehistogram(currentsite(), species, currentprob1(), scale_factor)
    })
    timehistogram({
      #num = window_width()/1440
      #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
      scale_factor = 1440/window_width()
      createtimehistogram(currentsite(), species, currentprob1(), scale_factor)
    })
    shinyjs::show("showallhist") # show histogram reset button
  })
  
  # when histogram reset button clicked, overall date/time histograms shown for that site
  
  # tab 1
  observeEvent(input$showallhist, {
    shinyjs::addClass(selector = "body", class = "loading-cursor") # show loading cursor
    datehistogram({
      #num = window_width()/1440
      #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
      scale_factor = 1440/window_width()
      createdatehistogram(site=currentsite(), detection_prob=currentprob1(), text_scale=scale_factor)
    })
    timehistogram({
      #num = window_width()/1440
      #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
      scale_factor = 1440/window_width()
      createtimehistogram(site=currentsite(), detection_prob=currentprob1(), text_scale=scale_factor)
    })
    shinyjs::delay(100, shinyjs::removeClass(selector = "body", class = "loading-cursor")) # back to normal cursor
    shinyjs::hide("showallhist") # histogram reset button hidden again
  })
  
  # when detection prob selected, bird family plot, table, and histograms updated to show sites with species at selected prob or higher
  
  # tab 1
  observeEvent(input$detectionprob1 != "", {
    req(input$detectionprob1) # ensures that if detectionprob selectinput backspaced, code won't execute and app won't crash
    
    currentprob1(substr(input$detectionprob1, 1, 2))
    df(filtersites(currentsite(), currentprob1()))
    if (nrow(df()) == 0) {
      sitemsg({
        paste(br(), h6(HTML(glue("<strong>No detections greater than or equal to {currentprob1()}% at site {currentsite()}. Select lower minimum detection probability.</strong>"))))
      })
      birdbarplot("")
      displayedfamily("")
      df("")
      datehistogram("")
      timehistogram("")
      shinyjs::hide("showallbirds") # birds reset button hidden
      #shinyjs::hide("download_button1") # hide download button
      shinyjs::hide("hist_header") # hide histogram header
      shinyjs::hide("hist_text") # hide histogram description
      shinyjs::hide("showallhist") # histogram reset button hidden
      shinyjs::hide("columninfoheader1") # hide column info header
      shinyjs::hide("tabledescriptions1") # hide table descriptions button
      shinyjs::hide("columninfo1") # hide column info
      shinyjs::hide("songbirds") # hide bird icons
      shinyjs::hide("waterfowl") # hide bird icons
      shinyjs::hide("owls") # hide bird icons
      shinyjs::hide("shorebirds") # hide bird icons
      shinyjs::hide("landbirds") # hide bird icons
      shinyjs::hide("songbirds_label") # hide bird icons
      shinyjs::hide("waterfowl_label") # hide bird icons
      shinyjs::hide("owls_label") # hide bird icons
      shinyjs::hide("shorebirds_label") # hide bird icons
      shinyjs::hide("landbirds_label") # hide bird icons
      shinyjs::hide("songbirds_text") # hide bird text
      shinyjs::hide("waterfowl_text") # hide bird text
      shinyjs::hide("owls_text") # hide bird text
      shinyjs::hide("shorebirds_text") # hide bird text
      shinyjs::hide("landbirds_text") # hide bird text
      columninfo1state("hidden")
      songbirds_c("hidden")
      waterfowl_c("hidden")
      owls_c("hidden")
      shorebirds_c("hidden")
      landbirds_c("hidden")
    } else {
      shinyjs::hide("sitemsg")
      df(filtersites(currentsite(), currentprob1()))
      birdbarplot({
        #diagonalpx = sqrt((window_width()**2)+(window_height()**2))
        scale_factor = 1440/window_width() #diagonalpx/1686 # diagonal of MacBook Pro
        #num = window_width()/1440
        #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
        createbirdplot(currentsite(), currentprob1(), text_scale = scale_factor)
      })
      
      shinyjs::show("songbirds") # reveal bird icons
      shinyjs::show("waterfowl") # reveal bird icons
      shinyjs::show("owls") # reveal bird icons
      shinyjs::show("shorebirds") # reveal bird icons
      shinyjs::show("landbirds") # reveal bird icons
      shinyjs::show("songbirds_label") # reveal bird icons
      shinyjs::show("waterfowl_label") # reveal bird icons
      shinyjs::show("owls_label") # reveal bird icons
      shinyjs::show("shorebirds_label") # reveal bird icons
      shinyjs::show("landbirds_label") # reveal bird icons
      shinyjs::hide("songbirds_text") # hide bird text
      shinyjs::hide("waterfowl_text") # hide bird text
      shinyjs::hide("owls_text") # hide bird text
      shinyjs::hide("shorebirds_text") # hide bird text
      shinyjs::hide("landbirds_text") # hide bird text
      
      displayedfamily(
        paste(h4(paste0("Showing all birds at site ", currentsite(), ":")), br())
      )
      
      columninfoheader1(
        paste(h4("How to read the data table above:"))
      )
      
      columninfo1(
        paste(
          h6(HTML(glue("<em>Audio was recorded at sites across Minnesota and processed using BirdNET,
                         a machine learning tool for identifying birds by sound. Detections were
                         filtered using statistical models to reduce false positives.</em>"))),
          br(),
          h6(HTML(glue("<strong>Avian Group:</strong> Taxonomic category to which a given species
                         belongs."))),
          h6(HTML(glue("<strong>Detection Probability Range:</strong> Each detection is given a probability score reflecting how likely it is to be a real detection of that species. For example, 95% means there is a 95% chance the detection is a true positive. These probability scores are based on models we developed by manually reviewing thousands of recordings. This column shows the range of probability scores across all detections."))),
          h6(HTML(glue("<strong>Total Detections:</strong> The number of times this species was
                         detected at this site above our confidence threshold. Because BirdNET analyzes
                         audio in 3-second segments, a single bird can produce many detections, so this
                         number reflects acoustic activity rather than the number of individual birds
                         present."))),
          br(),
          h6(HTML(glue("<strong>¹</strong> Species of Greatest Conservation Need (SGCN)"))),
          h6(HTML(glue("<strong>²</strong> Species in Need of Information (SNI)")))
        )
      )
      
      hist_header(
        paste(h4(paste0("Bird Detections at Site ", currentsite(), 
                        " by Date and Time of Day:")))
      )
      
      shinyjs::addClass(selector = "body", class = "loading-cursor") # show loading cursor
      datehistogram({
        #num = window_width()/1440
        #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
        scale_factor = 1440/window_width()
        createdatehistogram(site=currentsite(), detection_prob=currentprob1(), text_scale=scale_factor)
      })
      timehistogram({
        #num = window_width()/1440
        #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
        scale_factor = 1440/window_width()
        createtimehistogram(site=currentsite(), detection_prob=currentprob1(), text_scale=scale_factor)
      })
      shinyjs::delay(100, shinyjs::removeClass(selector = "body", class = "loading-cursor")) # back to normal cursor
      
      #shinyjs::show("download_button1") # show download button
      shinyjs::show("columninfoheader1") # show column info header
      shinyjs::show("columninfo1") # show column info by default
      shinyjs::show("tabledescriptions1") # show table descriptions button
      shinyjs::hide("showallbirds") # birds reset button hidden
      shinyjs::show("hist_header") # show histogram header
      shinyjs::show("hist_text") # show histogram description
    }
  })
  
  # when species selected, map and table updated to show sites with species
  
  # tab 2
  observeEvent(input$species != "", {
    req(input$species) # ensures that if selectinput backspaced, code won't execute and app won't crash
    currentspecies(input$species)
    speciesmap(makespeciesmap(currentspecies(), currentprob2()))
    speciesdf(filterspecies(currentspecies(), currentprob2()))
    if (nrow(speciesdf()) == 0) {
      speciesmsg({
        val = if (currentspecies() %in% sgcnbirds_wsymbol | currentspecies() == "Eastern Screech-Owl²") {str_sub(currentspecies(), 1, -2)} else {currentspecies()}
        percentsites = round((nrow(speciesdf())/121), 2)*100
        paste(br(), h6(HTML(glue("<strong>{val} detected at {percentsites}% of sites. Select lower minimum detection probability.</strong>"))))
      })
      speciesplot("")
      displayed_species_at_sites("")
      speciesdf("")
      shinyjs::hide("columninfoheader2") # hide column info header
      shinyjs::hide("columninfo2") # hide column info
    } else {
      speciesmsg({
        val = if (currentspecies() %in% sgcnbirds_wsymbol | currentspecies() == "Eastern Screech-Owl²") {str_sub(currentspecies(), 1, -2)} else {currentspecies()}
        percentsites = round((nrow(speciesdf())/121), 2)*100
        if (percentsites == 101) {percentsites = 100} else {percentsites = percentsites}
        paste(br(), h6(HTML(glue("<strong>{val} detected at ~{percentsites}% of sites</strong>"))))
      })
      speciesplot({
        #num = window_width()/1440
        #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
        scale_factor = 1440/window_width()
        createspeciesplot(currentspecies(), currentprob2(), text_scale = scale_factor)
      })
      displayed_species_at_sites({
        val = if (currentspecies() %in% sgcnbirds_wsymbol | currentspecies() == "Eastern
                  Screech-Owl²") {str_sub(currentspecies(), 1, -2)} else {currentspecies()}
        paste(
          h4(paste0("Showing all sites with ", val, ":"))
        )
      })
      
      columninfoheader2(
        paste(h4("How to read the data table above:"))
      )
      
      columninfo2(
        paste(
          h6(HTML(glue("<em>Audio was recorded at sites across Minnesota and processed using BirdNET,
                       a machine learning tool for identifying birds by sound. Detections were
                       filtered using statistical models to reduce false positives.</em>"))),
          br(),
          h6(HTML(glue("<strong>Biome Type:</strong> Primary vegetation type found at site, either
                       'forest' or 'grassland'."))),
          h6(HTML(glue("<strong>Detection Probability Range:</strong> Each detection is given a probability score reflecting how likely it is to be a real detection of that species. For example, 95% means there is a 95% chance the detection is a true positive. These probability scores are based on models we developed by manually reviewing thousands of recordings. This column shows the range of probability scores across all detections."))),
          h6(HTML(glue("<strong>Total Detections:</strong> The number of times this species was
                       detected at this site above our confidence threshold. Because BirdNET analyzes
                       audio in 3-second segments, a single bird can produce many detections, so this
                       number reflects acoustic activity rather than the number of individual birds
                       present."))),
          br(),
          h6(HTML(glue("<strong>¹</strong> Species of Greatest Conservation Need (SGCN)"))),
          h6(HTML(glue("<strong>²</strong> Species in Need of Information (SNI)")))
        )
      )
      
      #shinyjs::show("download_button2") # show download button
      shinyjs::hide("showallbiomes") # biomes reset button hidden
      shinyjs::show("columninfoheader2") # show column info header
      shinyjs::show("columninfo2") # show column info by default
    }
  })
  
  # when detection prob selected, map and table updated to show sites with species at selected prob or higher
  
  # tab 2
  observeEvent(input$detectionprob2 != "", {
    req(input$species) # ensures that if species selectinput backspaced, code won't execute and app won't crash
    req(input$detectionprob2) # ensures that if detectionprob selectinput backspaced, code won't execute and app won't crash
    
    currentprob2(substr(input$detectionprob2, 1, 2))
    speciesmap(makespeciesmap(currentspecies(), currentprob2()))
    speciesdf(filterspecies(currentspecies(), currentprob2()))
    if (nrow(speciesdf()) == 0) {
      speciesmsg({
        val = if (currentspecies() %in% sgcnbirds_wsymbol | currentspecies() == "Eastern Screech-Owl²") {str_sub(currentspecies(), 1, -2)} else {currentspecies()}
        percentsites = round((nrow(speciesdf())/121), 2)*100
        paste(br(), h6(HTML(glue("<strong>{val} detected at {percentsites}% of sites. Select lower minimum detection probability.</strong>"))))
      })
      speciesplot("")
      displayed_species_at_sites("")
      speciesdf("")
      shinyjs::hide("columninfoheader2") # hide column info header
      shinyjs::hide("columninfo2") # hide column info
    } else {
      speciesmsg({
        val = if (currentspecies() %in% sgcnbirds_wsymbol | currentspecies() == "Eastern Screech-Owl²") {str_sub(currentspecies(), 1, -2)} else {currentspecies()}
        percentsites = round((nrow(speciesdf())/121), 2)*100
        if (percentsites == 101) {percentsites = 100} else {percentsites = percentsites}
        paste(br(), h6(HTML(glue("<strong>{val} detected at ~{percentsites}% of sites</strong>"))))
      })
      speciesplot({
        #num = window_width()/1440
        #scale_factor = abs(((1-(1/num))/6.3) - (1/num))
        scale_factor = 1440/window_width()
        createspeciesplot(currentspecies(), currentprob2(), text_scale = scale_factor)
      })
      displayed_species_at_sites({
        val = if (currentspecies() %in% sgcnbirds_wsymbol | currentspecies() == "Eastern
                  Screech-Owl²") {str_sub(currentspecies(), 1, -2)} else {currentspecies()}
        paste(
          h4(paste0("Showing all sites with ", val, ":"))
        )
      })
      
      columninfoheader2(
        paste(h4("How to read the data table above:"))
      )
      
      columninfo2(
        paste(
          h6(HTML(glue("<em>Audio was recorded at sites across Minnesota and processed using BirdNET,
                       a machine learning tool for identifying birds by sound. Detections were
                       filtered using statistical models to reduce false positives.</em>"))),
          br(),
          h6(HTML(glue("<strong>Biome Type:</strong> Primary vegetation type found at site, either
                       'forest' or 'grassland'."))),
          h6(HTML(glue("<strong>Detection Probability Range:</strong> Each detection is given a probability score reflecting how likely it is to be a real detection of that species. For example, 95% means there is a 95% chance the detection is a true positive. These probability scores are based on models we developed by manually reviewing thousands of recordings. This column shows the range of probability scores across all detections."))),
          h6(HTML(glue("<strong>Total Detections:</strong> The number of times this species was
                       detected at this site above our confidence threshold. Because BirdNET analyzes
                       audio in 3-second segments, a single bird can produce many detections, so this
                       number reflects acoustic activity rather than the number of individual birds
                       present."))),
          br(),
          h6(HTML(glue("<strong>¹</strong> Species of Greatest Conservation Need (SGCN)"))),
          h6(HTML(glue("<strong>²</strong> Species in Need of Information (SNI)")))
        )
      )
      
      #shinyjs::show("download_button2") # show download button
      shinyjs::hide("showallbiomes") # biomes reset button hidden
      shinyjs::show("columninfoheader2") # show column info header
      shinyjs::show("columninfo2") # show column info by default
    }
  })
  
  # when bar clicked on plot, table updated to show biome-specific site data for selected species
  
  # tab 2
  observeEvent(input$biomeclick, {
    #biome_clicked = unique_biomes$`Biome Type`[which.min(abs(as.numeric(factor(unique_biomes$`Biome Type`)) - bar_clicked))]
    
    # getting bar click coordinates to corresponds properly with biomes regardless of plot size
    bar_clicked = input$biomeclick$x
    n = nrow(unique_biomes)
    bar_index = round((bar_clicked - (1 / (2 * n))) / (1 / n)) + 1
    biome_clicked = as.character(unique_biomes$`Biome Type`[bar_index])
    
    speciesdf(filterbiomes(currentspecies(), currentprob2(), biome_clicked))
    displayed_species_at_sites({
      val = if (currentspecies() %in% sgcnbirds_wsymbol | currentspecies() == "Eastern Screech-Owl²") {str_sub(currentspecies(), 1, -2)} else {currentspecies()}
      paste(
        h4(paste0("Showing all ", biome_clicked, " sites with ", val, ":"))
      )
    })
    
    shinyjs::show("showallbiomes") # reveal reset button
  })
  
  # when "show all" button clicked, table updated to show all sites for selected species (reset)
  
  # tab 2
  observeEvent(input$showallbiomes, {
    speciesdf(filterspecies(currentspecies(), currentprob2()))
    displayed_species_at_sites({
      val = if (currentspecies() %in% sgcnbirds_wsymbol | currentspecies() == "Eastern Screech-Owl²") {str_sub(currentspecies(), 1, -2)} else {currentspecies()}
      paste(
        h4(paste0("Showing all sites with ", val, ":"))
      )
    })
    
    shinyjs::hide("showallbiomes") # reset button hidden again
  })
  
  # when "show/hide column descriptions" button clicked, column info displayed or hidden depending on current state
  
  # tab 2
  observeEvent(input$tabledescriptions2, {
    if (columninfo2state() == "shown") {
      shinyjs::hide("columninfoheader2") # hide column info header
      shinyjs::hide("columninfo2") # hide column descriptions
      columninfo2state("hidden")
    }
    else {  # if columninfo2state() == "hidden"
      shinyjs::show("columninfoheader2") # reveal column info header
      shinyjs::show("columninfo2") # reveal column descriptions
      columninfo2state("shown")
    }
  })
  
  # outputs seen in dashboard
  
  # tab 1
  #output$browser_dim <- renderText({
  #paste0(window_width(), "x", shinybrowser::get_height())
  #})
  output$spacer1 = renderUI({HTML(paste(" ", br()))})
  output$siteselection = renderUI({
    if (displayedsite() == "") {HTML(paste(h4("Select your site below:")))} else {HTML(displayedsite())}
  })
  output$map = renderLeaflet(map())
  output$site_stats_header = renderUI({HTML(displayedstatsheader())})
  output$site_statistics = renderUI({HTML(displayedstats())})
  output$sitemsg = renderUI({HTML(sitemsg())})
  output$birdfamily = renderPlot(req(birdbarplot()), 
                                 res = 96, width = 840, height = 400)
  output$songbirds = renderImage({
    list(src = "www/songbirds.png",
         alt = "Songbirds icon",
         contentType = "image/png",
         height = "100px",
         align = "left")
  }, deleteFile = FALSE)
  output$waterfowl = renderImage({
    list(src = "www/waterfowl_and_wading_birds.png",
         alt = "Waterfowl and Wading Birds icon",
         contentType = "image/png",
         height = "100px",
         align = "left")
  }, deleteFile = FALSE)
  output$owls = renderImage({
    list(src = "www/owls_and_raptors.png",
         alt = "Owls and Raptors icon",
         contentType = "image/png",
         height = "100px",
         align = "left")
  }, deleteFile = FALSE)
  output$shorebirds = renderImage({
    list(src = "www/shorebirds.png",
         alt = "Shorebirds icon",
         contentType = "image/png",
         height = "70px",
         align = "left")
  }, deleteFile = FALSE)
  output$landbirds = renderImage({
    list(src = "www/other_land_birds.png",
         alt = "Other Land Birds icon",
         contentType = "image/png",
         height = "100px",
         align = "left")
  }, deleteFile = FALSE)
  output$songbirds_label = renderUI({HTML(
    HTML(paste0(h6("Songbirds")))
  )})
  output$waterfowl_label = renderUI({HTML(
    HTML(paste0(h6(HTML(glue("Waterfowl and<br>Wading Birds")))))
  )})
  output$owls_label = renderUI({HTML(
    HTML(paste0(h6("Owls and Other Raptors")))
  )})
  output$shorebirds_label = renderUI({HTML(
    HTML(paste0(h6("Shorebirds")))
  )})
  output$landbirds_label = renderUI({HTML(
    HTML(paste0(h6("Other Land Birds")))
  )})
  output$songbirds_text = renderUI({HTML(songbirds_text())})
  output$waterfowl_text = renderUI({HTML(waterfowl_text())})
  output$owls_text = renderUI({HTML(owls_text())})
  output$shorebirds_text = renderUI({HTML(shorebirds_text())})
  output$landbirds_text = renderUI({HTML(landbirds_text())})
  output$familyselection = renderUI({HTML(displayedfamily())})
  output$table = renderDT({
    data = df() # can't simply do req(df()) w/i datatable() anymore bc renderDT may be working faster than df() can update with the added histogram calculations
    req(is.data.frame(data))
    datatable(data, escape = FALSE,
              selection = "single",
              container = tab1tablehover,
              options = list(
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-left',
                                       targets = "_all"))
              )
    )
  })
  #output$download_button1 <- downloadHandler(
  #filename = function() {
  #glue("sounds_of_nature_site_{currentsite()}_results.csv")
  #},
  #content = function(file) {
  #write.csv(df(), file, quote = FALSE)
  #}
  #)
  output$columninfoheader1 = renderUI({HTML(columninfoheader1())})
  output$columninfo1 = renderUI({HTML(columninfo1())})
  output$hist_header = renderUI({HTML(hist_header())})
  output$hist_text = renderUI({HTML(paste(h6(HTML(glue("<em>Click on a species in the table above to see species-specific histograms</em>")))))})
  output$datehistogram = renderPlot(req(datehistogram()), 
                                    res = 96)
  output$timehistogram = renderPlot(req(timehistogram()), 
                                    res = 96)
  
  # tab 2
  output$spacer2 = renderUI({HTML(paste(" ", br()))})
  output$speciesmap = renderLeaflet(req(speciesmap()))
  output$speciesselection = renderUI({
    if (input$species == "") {HTML(paste(br(), h4("")))} else {HTML(speciesmsg())}
  })
  output$speciesplot = renderPlot(req(speciesplot()),
                                  res = 96, width = 600, height = 400)
  output$species_at_sites = renderUI({HTML(displayed_species_at_sites())})
  output$speciestable = renderDT(req(datatable(req(speciesdf()), escape = FALSE,
                                               selection = "single",
                                               container = tab2tablehover,
                                               options = list(
                                                 columnDefs = list(list(className = 'dt-left',
                                                                        targets = "_all"))))))
  #output$download_button2 <- downloadHandler(
  #filename = function() {
  #glue("sounds_of_nature_{currentspecies()}_results.csv")
  #},
  #content = function(file) {
  #write.csv(df(), file, quote = FALSE)
  #}
  #)
  output$columninfoheader2 = renderUI({HTML(columninfoheader2())})
  output$columninfo2 = renderUI({HTML(columninfo2())})
  
  # tab 3
  output$spacer3 = renderUI({HTML(paste(" ", br()))})
  output$about = renderUI({HTML(
    paste(
      h6(HTML(glue(
        "<em>Sounds of Nature MN</em> leverages citizen science, passive acoustic monitoring, and a deep artificial neural network, to examine avian biodiversity on public and private lands across Minnesota's three major biomes (Laurentian Mixed Forest, Eastern Broadleaf Forest, and Prairie Grasslands). Autonomous Recording Units (ARUs) were programmed to record continuously for 4 hours beginning at sunrise from May 15 - June 30, and for 2 hours at sunset from June 1 - June 30 (to coincide with peak migration and breeding periods, respectively, for birds in this region). Devices were deployed on private lands of volunteer study participants and on public lands, including Scientific Natural Areas (SNAs) and Wildlife Management Areas (WMAs). Over 30,000 hours of audio were recorded across 124 sites in 2025. Audio data was run through BirdNET¹, a convolutional neural network capable of rapidly identifying thousands of species from audio. To account for false positives produced by this tool, an extensive data validation process was performed for every species detected in this study, following the protocol laid out by Symes et al. (2024)². This project is made possible with support from the University of Minnesota and Minnesota’s Environment and Natural Resources Trust Fund (ENRTF). Partners include the Minnesota Department of Natural Resources, the Minnesota Cooperative Fish and Wildlife Research Unit, Audubon Upper Mississippi River, and our citizen science volunteers and collaborators."
      ))),
      br(),
      h6(tags$ol(
        tags$li(
          HTML(glue(
            "Kahl, S., Wood, C. M., Eibl, M., & Klinck, H. (2021). BirdNET: A deep learning solution for avian diversity monitoring. Ecological Informatics, 61, 101236."
          ))),
        tags$li(
          HTML(glue(
            "Symes L, Sugai LSMS, Gottesman B, Pitzrick M, Wood C, Charif, R. 2024. Acoustic analysis with BirdNET and (almost) no coding: practical instructions."
          )))
      ))
    )
  )})
  output$go_to_github = renderUI({HTML(
    HTML(paste0(h6("To view our documentation and code, visit the")))
  )})
  
  # overall
  output$footer = renderUI({
    HTML(paste(
      h6("This research is made possible with support from the University of Minnesota and Minnesota's Environment and Natural Resources Trust Fund. Partners include the Minnesota Department of Natural Resources, the Minnesota Cooperative Fish and Wildlife Research Unit, Audubon Upper Mississippi River, and our citizen science volunteers and collaborators."), h6("App last updated on June 18th, 2026."), h6(HTML(glue("<em><strong>Contact us at:</strong></em> <u>soundsofnature@umn.edu</u>")))
    ))
  })
}

shinyApp(ui, server)

#### End of Script