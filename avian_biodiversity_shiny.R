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
replacement_r <- c("Owls & Raptors")
original_shb <- c("Shorebirds and Seabirds")
replacement_shb <- c("Shorebirds & Seabirds")
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
  )
)

map_icon = makeAwesomeIcon(
  icon = "leaf",
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
    moveToLocation = FALSE,
    zoom = 10,
    autoCollapse = TRUE,
    hideMarkerOnCollapse = FALSE,
    textPlaceholder = "Search by Site Number...",
    marker = list(icon = NULL, animate = TRUE, circle = list(radius = 10, weight = 3, color
                                                             = "#fe6100", stroke = TRUE, fill = FALSE))
  ))

#### Cleaning Site Data

originalbirds = c("Cattle Egret", "Northern Goshawk", "Herring Gull", "Warbling Vireo", "Gray Jay", "House Wren", "Yellow Warbler", "Whimbrel", "Arctic Skua", "Pomarine Skua", "Long-tailed Skua", "Red-throated Diver", "Pacific Diver", "White-billed Diver", "Brent Goose", "Grey Phalarope", "Grey-crowned Rosy-Finch", "Black-throated Grey Warbler", "Red Grouse/Willow Grouse", "Common Redpoll", "Barn Owl", "Black-crowned Night-Heron")
replacementbirds = c("Western Cattle-Egret", "American Goshawk", "American Herring Gull", "Eastern Warbling Vireo", "Canada Jay", "Northern House Wren", "Northern Yellow Warbler", "Hudsonian Whimbrel", "Parasitic Jaeger", "Pomarine Jaeger", "Long-tailed Jaeger", "Red-throated Loon", "Pacific Loon", "Yellow-billed Loon", "Brant", "Red Phalarope", "Gray-crowned Rosy-Finch", "Black-throated Gray Warbler", "Willow Ptarmigan", "Redpoll", "American Barn Owl", "Black-crowned Night Heron")
for (i in 1:length(originalbirds)) {
  original_name = c(originalbirds[i])
  replacement_name = c(replacementbirds[i])
  rawsitedata$`Common Name` = replace(rawsitedata$`Common Name`, rawsitedata$`Common Name` %in% original_name, replacement_name)
}

cleaningsites = left_join(rawsitedata, birdfamilies, by = join_by(`Common Name`)) %>%
  drop_na("Total Instances") %>% 
  relocate(`Scientific Name`, .after = `Site Number`) %>%
  relocate(Family, .before = Confidence) %>% 
  rename(`Number of Detections, 50% Confidence` = `Number of Instances, 50% Confidence`,
         `Number of Detections, 55% Confidence` = `Number of Instances, 55% Confidence`,
         `Number of Detections, 60% Confidence` = `Number of Instances, 60% Confidence`,
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
  mutate(Family = factor(Family, levels = c("Songbirds", "Waterfowl & Wading Birds", "Owls & Raptors",
                                            "Shorebirds & Seabirds", "Other Land Birds")),
         `Scientific Name` = paste0("<em>", `Scientific Name`, "</em>"),
         `More Information` = paste0("<a href='", `More Information`,"' target='_blank'>", `More Information`,"</a>"),
         `Total Detections` = case_when(
           `Rare Detection` == "no" ~ rowSums(across(c(`Number of Detections, 50% Confidence`,
                                                       `Number of Detections, 55% Confidence`,
                                                       `Number of Detections, 60% Confidence`,
                                                       `Number of Detections, 65% Confidence`,
                                                       `Number of Detections, 70% Confidence`,
                                                       `Number of Detections, 75% Confidence`,
                                                       `Number of Detections, 80% Confidence`,
                                                       `Number of Detections, 85% Confidence`,
                                                       `Number of Detections, 90% Confidence`,
                                                       `Number of Detections, 95% Confidence`))),
           `Rare Detection` == "yes" ~ as.numeric(`Total Detections`)
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
           `Number of Detections, 55% Confidence` > 0 ~ "55%",
           `Number of Detections, 50% Confidence` > 0 ~ "50%",
           TRUE ~ "99%")
  )

cleanedsites = cleaningsites %>% 
  select(`Site Number`, `Scientific Name`, `Common Name`, Family, `Highest Confidence`, 
         `Total Detections`, `More Information`) %>% 
  relocate(`Highest Confidence`, .before = `Total Detections`)

unique_families = cleanedsites %>%
  select(Family) %>% 
  unique() %>% 
  mutate(Family =  factor(Family, levels = c("Songbirds", "Waterfowl & Wading Birds", "Owls & Raptors",
                                             "Shorebirds & Seabirds", "Other Land Birds"))) %>%
  arrange(Family)

unique_species = cleanedsites %>% 
  select(`Common Name`) %>% 
  unique() %>% 
  arrange(`Common Name`)

#### Summary Statistics for Shiny App

total_species = cleanedsites %>% 
  group_by(`Site Number`) %>% 
  count()

avg_species = round(mean(total_species$n))

total_detections = cleanedsites %>% 
  group_by(`Site Number`) %>% 
  summarize(`Total Site Detections` = sum(`Total Detections`))

avg_detections = round(mean(total_detections$`Total Site Detections`))

#### Functions for Shiny App (zoom-adjusted)

filtersites = function(site) {
  filtered_df = cleanedsites %>% filter(`Site Number` == site)
  return(filtered_df)
}

filterfamily = function(site, family) {
  filtered_df = cleanedsites %>% filter(`Site Number` == site,
                                        Family == family)
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

makesitemap = function(site) {
  
  # use existing geogs df to create new temp df where new col shows pin marker color depending on if site is what user has selected and if it is public or private
  
  geogs_sites = geogs %>% 
    mutate(Marker_color = case_when(
      ARU == site ~ "blue",  # User's selected site
      `Public or Private` == "Public" ~ "lightgreen",  # If public and not user's selected site
      TRUE ~ "green"  # If private and not user's selected site
    ))
  
  tlat = geogs_sites %>% 
    filter(ARU == site) %>% 
    pull(`Township Lat`)
  
  tlong = geogs_sites %>% 
    filter(ARU == site) %>% 
    pull(`Township Long`)
  
  map_icon = makeAwesomeIcon(
    icon = "leaf",
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
      moveToLocation = TRUE,
      zoom = 14,
      autoCollapse = TRUE,
      hideMarkerOnCollapse = FALSE,
      textPlaceholder = "Search by Site Number...",
      marker = list(icon = NULL, animate = TRUE, circle = list(radius = 10, weight = 3, color
                                                               = "#fe6100", stroke = TRUE, fill = FALSE))
    ))
  
  return(map)
}

createbirdplot = function(site, text_scale = 1) {
  filtered_df = filtersites(site) %>% 
    group_by(Family) %>% 
    count()
  filtered_df$Family = factor(filtered_df$Family, levels = unique_families$Family) # ensures that even if a family has no birds at a site, the empty family bar is still shown on plot
  plottitle = paste0("Number of Species Detected within Each Avian Family at Site ", site)
  maxn = max(filtered_df$n) + 10
  adjusted_lineheight = 0.55 + (0.55-(0.55*text_scale))
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
    scale_fill_manual(values = c("#ffb000", "#fe6100", "#dc267f", "#785ef0", "#648fff")) +
    labs(x = "Avian Family", y = "Species Detected",
         title = str_wrap(plottitle, 40),
         subtitle = str_wrap("Click on a bar to explore the corresponding species in the data table!", 55)) +
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
          plot.margin = margin(t = 14, r = 50, b = 14, l = 14)
    ) 
}

makespeciesmap = function(species) {
  
  # find which sites have detections of this species
  
  truesites = cleanedsites %>%
    filter(`Common Name` == species) %>% 
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
    ))
  
  # creating dynamic icons and map based on presence/absence of species at sites
  
  species_map_icon = makeAwesomeIcon(
    icon = "leaf",
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

filterspecies = function(species) {
  speciessites = cleanedsites %>% filter(`Common Name` == species)
  joined_df = left_join(speciessites, geogs, by = c("Site Number" = "ARU")) %>% 
    select(-`Isolated or Embedded`, -`Township Lat`, -`Township Long`, -Family) %>%
    relocate(`Public or Private`, .after = `Site Number`) %>% 
    relocate(`Biome Type`, .after = `Public or Private`)
  return(joined_df)
}

createspeciesplot = function(species, text_scale = 1) {
  filtered_df = filterspecies(species) %>% 
    group_by(`Biome Type`) %>% 
    count()
  filtered_df$`Biome Type` = factor(filtered_df$`Biome Type`, levels = c("Forest", "Grassland")) # ensures that even if a species is not represented at one biome type, the empty biome type bar is still shown on plot
  plottitle = paste0(species, " Detection Sites by Biome")
  maxn = max(filtered_df$n) + 10
  adjusted_lineheight = 0.55 + (0.55-(0.55*text_scale))
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
         title = str_wrap(plottitle, 30)
         ) +
    scale_x_discrete(labels = label_wrap(15), drop = FALSE) +
    scale_y_continuous(limits = c(0, maxn), breaks = seq(0, maxn, by = 20)) +
    scale_fill_manual(values = c("#dc267f", "#fe6100")) +
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
          plot.margin = margin(t = 14, r = 60, b = 14, l = 14)
    )
}


# Data table containers that allow for hovering in Shiny

tab1tablehover = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(""),
      th("Site Number", title = "Number assigned to site", class = 'dt-head-left'),
      th("Scientific Name", title = "Species scientific name", class = 'dt-head-left'),
      th("Common Name", title = "Species common name", class = 'dt-head-left'),
      th("Family", title = "Taxonomic category to which a given species belongs.", class = 'dt-head-left'),
      th("Detection Probability", title = "The likelihood that this detection is a true detection of the species, based on statistical models our team developed using hundreds of manually reviewed recordings. Higher values indicate greater certainty. For example, a value of 95% means our models estimate a 95% chance that this is a real detection.", class = 'dt-head-left'),
      th("Total Detections", title = "The number of times this species was detected at this site above our confidence threshold. Because BirdNET analyzes audio in 3-second segments, a single bird can produce many detections, so this number reflects acoustic activity rather than the number of individual birds present.", class = 'dt-head-left'),
      th("More Information", title = "Click the links below to learn more about each species", class = 'dt-head-left')
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
      th("Scientific Name", title = "Species scientific name", class = 'dt-head-left'),
      th("Common Name", title = "Species common name", class = 'dt-head-left'),
      th("Detection Probability", title = "The likelihood that this detection is a true detection of the species, based on statistical models our team developed using hundreds of manually reviewed recordings. Higher values indicate greater certainty. For example, a value of 95% means our models estimate a 95% chance that this is a real detection.", class = 'dt-head-left'),
      th("Total Detections", title = "The number of times this species was detected at this site above our confidence threshold. Because BirdNET analyzes audio in 3-second segments, a single bird can produce many detections, so this number reflects acoustic activity rather than the number of individual birds present.", class = 'dt-head-left'),
      th("More Information", title = "Click the links below to learn more about each species", class = 'dt-head-left')
    )
  )
))

#### Shiny App (zoom-adjusted)

ui = fluidPage(
  #tags$script(src = "https://kit.fontawesome.com/df8147df33.js"),  # To use custom icons on map
  setBackgroundColor(color = "#f3f3ed"),
  includeCSS("www/biodiversity_styles.css"),
  useShinyjs(),
  fav("crow"), # adding crow favicon from FontAwesome to app
  tags$head(
    tags$title("Sounds of Nature MN 2025")
  ),
  titlePanel(tags$div(class = "app_title", "Sounds of Nature Minnesota 2025 Avian Biodiversity Visualizer")),
  shinybrowser::detect(),
  #"Your browser dimensions:",
  #textOutput("browser_dim"),
  hr(),
  hr(),
  sidebarPanel(id="sidebar",
               p(strong("Welcome to the Sounds of Nature Minnesota Website!"), "This dashboard is dedicated to exploring the variety of bird species observed on private and public lands in the state using", em("Passive Acoustic Monitoring"), "from May 15th to June 30th, 2025."),
               br(),
               p(strong("Tab 1:", em("Search by Site"))),
               #p(tags$div(class = "sidebar_header", "Tab 1:<em>Search by Site</em>")),
               p(strong(em("Start")), "by zooming into your area of interest on the map. You can hover over pins to see their 5-digit site code. Alternatively, if you know your site code, you can enter it in the search bar in the top left corner of the map, below the 'reset map view' button. Once you have found your site,", strong(em("click")), "on a pin to learn more about what birds were detected at that location.", em("Note: To protect sensitive species and landowner privacy, we have moved location coordinates to public sites within 3 miles (5 km) of these sampling locations.")),
               p(strong(em("Next,")), "you will see a bar chart below the map representing the different avian families detected at each site. Click on a bar (i.e., family) to filter the data table below to explore what species within that group were detected at a given site. Scroll to the right within the table and follow the links to learn more about each species. To view all the species detected at a site, click on the green 'Show all birds at this site' button below the plot. Hover over column names or scroll below the data table for column descriptions."),
               br(),
               p(strong("Tab 2:", em("Search by Species"))),
               #p(tags$div(class = "sidebar_header", "Tab 2:<em>Search by Species</em>")),
               p(strong(em("Click")), "the selection menu at the top and type in or scroll to search by species. The map will update to highlight the sites where your species was detected (detections will appear as blue pins). The bar plot will also update to show the broad biome type(s) where the selected species was detected. See the table below for species-specific data by site. Hover over column names or scroll below the data table for column descriptions."),
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
          column(4, hidden(actionButton(inputId = "resetmap", label = "Reset Map and Data")))
        ),
        br(),
        fluidRow(
          column(12, leafletOutput("map"))
        ),
        hr(),
        htmlOutput("site_stats_header"),
        fluidRow(
          column(11, hidden(htmlOutput("site_statistics")))
        ),
        hr(),
        fluidRow(
          column(1),
          column(10, tags$div(class = "plot", plotOutput("birdfamily", click = "familyclick", 
          width = "600px", height = "400px"))),
          column(1)
        ),
        hr(),
        fluidRow(
          column(7, htmlOutput("familyselection")),
          column(1),
          column(4, hidden(actionButton(inputId = "showall", label = "Show all birds at this site"))),
        ),
        hr(),
        fluidRow(
          column(10, tags$div(class = "table", DTOutput("table", width = "800px")))
          ),
        #br(),
        #fluidRow(
          #column(4),
          #column(4, 
                 #hidden(downloadButton(outputId = "download_button1",
                       #label = "Download data table as CSV"))),
          #column(4)
        #),
        hr(),
        hidden(htmlOutput("columninfoheader1")),
        br(),
        hidden(htmlOutput("columninfo1"))
      ),
      tabPanel(
        title = "Search by Species",
        htmlOutput("spacer2"),
        fluidRow(
          column(1),
          column(5, selectInput("species", "Select a Species", choices = c("", unique_species$`Common Name`),
                                multiple = FALSE)
          ),
          column(6, htmlOutput("speciesselection"))
        ),
        fluidRow(
          column(12, leafletOutput("speciesmap"))
        ),
        hr(),
        fluidRow(
          column(8, tags$div(class = "plot", plotOutput("speciesplot", 
          width = "600px", height = "400px"))),
          column(4)
        ),
        hr(),
        htmlOutput("species_at_sites"),
        hr(),
        tags$div(class = "table", DTOutput("speciestable")),
        #br(),
        #fluidRow(
          #column(4),
          #column(4, 
                 #hidden(downloadButton(outputId = "download_button2",
                                       #label = "Download data table as CSV"))),
          #column(4)
        #),
        hr(),
        hidden(htmlOutput("columninfoheader2")),
        br(),
        hidden(htmlOutput("columninfo2"))
      )
    ),
    hr(),
    hr(),
    hr(),
    htmlOutput("footer"),
    hr(),
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
    hr(),
    hr(),
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
  birdbarplot = reactiveVal("")
  displayedfamily = reactiveVal("")
  df = reactiveVal("")
  columninfoheader1 = reactiveVal("")
  columninfo1 = reactiveVal("")
  window_width = reactive(shinybrowser::get_width())
  
  # tab 2
  currentspecies = reactiveVal("")
  speciesmsg = reactiveVal("")
  speciesmap = reactiveVal("")
  speciesplot = reactiveVal("")
  displayed_species_at_sites = reactiveVal("")
  speciesdf = reactiveVal("")
  columninfoheader2 = reactiveVal("")
  columninfo2 = reactiveVal("")
  window_width = reactive(shinybrowser::get_width())
  
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
    
    totalspecies = species_stats(currentsite())
    totaldetections = detections_stats(currentsite())
    
    displayedstats(
      paste(h6(tags$ul(
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
    
    df(filtersites(currentsite()))
    birdbarplot({
      num = window_width()/1440
      scale_factor = 1/num #((1-(1/num))/10) + (1/num)
      createbirdplot(currentsite(), text_scale = scale_factor)
    })
    displayedfamily(
      paste(h4(paste0("Showing all birds at site ", currentsite(), ":")), br())
    )
    
    columninfoheader1(
      paste(h4("How to read the data table above:"))
    )
    
    columninfo1(
      paste(
        h6(HTML(glue("<em>Audio was recorded at sites across Minnesota and processed using BirdNET, a machine learning tool for identifying birds by sound. Detections were filtered using statistical models to reduce false positives.</em>"))),
        br(),
        h6(HTML(glue("<strong>Family:</strong> Taxonomic category to which a given species belongs."))),
        h6(HTML(glue("<strong>Detection Probability:</strong> The likelihood that this detection is a true detection of the species, based on statistical models our team developed using hundreds of manually reviewed recordings. Higher values indicate greater certainty. For example, a value of 95% means our models estimate a 95% chance that this is a real detection."))),
        h6(HTML(glue("<strong>Total Detections:</strong> The number of times this species was detected at this site above our confidence threshold. Because BirdNET analyzes audio in 3-second segments, a single bird can produce many detections, so this number reflects acoustic activity rather than the number of individual birds present.")))
      )
    )
    
    #shinyjs::show("download_button1") # show download button
    shinyjs::show("columninfoheader1") # show column info header
    shinyjs::show("columninfo1") # show column info
    shinyjs::hide("showall") # birds reset button hidden
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
    shinyjs::hide("site_statistics") # site statistics box hidden
    shinyjs::hide("showall") # birds reset button hidden
    #shinyjs::hide("download_button1") # hide download button
    shinyjs::hide("columninfoheader1") # hide column info header
    shinyjs::hide("columninfo1") # hide column info
    shinyjs::hide("zoomtomn") # zoom to MN button hidden
    shinyjs::hide("resetmap") # reset button hidden again
  })
  
  # when bar clicked on plot, table updated to show family-specific data from selected site
  
  # tab 1
  observeEvent(input$familyclick, {
    bar_clicked = input$familyclick$x
    family_clicked = unique_families$Family[which.min(abs(as.numeric(factor(unique_families$Family)) - bar_clicked))]
    df(filterfamily(currentsite(), family_clicked))
    displayedfamily(
      paste(h4(paste0("Showing ", family_clicked, " at site ", currentsite(), ":")), br())
    )
    
    shinyjs::show("showall") # reveal reset button
  })
  
  # when "show all" button clicked, table updated to show all birds at selected site (reset)
  
  # tab 1
  observeEvent(input$showall, {
    df(filtersites(currentsite()))
    displayedfamily(
      paste(h4(paste0("Showing all birds at site ", currentsite(), ":")), br())
    )
    
    shinyjs::hide("showall") # reset button hidden again
  })
  
  # when species selected, map and table updated to show sites with species
  
  # tab 2
  
  observeEvent(input$species != "", {
    req(input$species) # ensures that if selectinput backspaced, code won't execute and app won't crash
    currentspecies(input$species)
    speciesmap(makespeciesmap(currentspecies()))
    speciesdf(filterspecies(currentspecies()))
    speciesmsg({
      percentsites = round((nrow(speciesdf())/121), 2)*100
      paste(br(), h6(HTML(glue("<strong>{currentspecies()} detected at ~{percentsites}% of sites</strong>"))))
    })
    speciesplot({
      num = window_width()/1440
      scale_factor = 1/num #((1-(1/num))/10) + (1/num)
      createspeciesplot(currentspecies(), text_scale = scale_factor)
    })
    displayed_species_at_sites(
      paste(br(), h4(paste0("Showing all sites with ", currentspecies(), ":")))
    )
    
    columninfoheader2(
      paste(h4("How to read the data table above:"))
    )
    
    columninfo2(
      paste(
        h6(HTML(glue("<em>Audio was recorded at sites across Minnesota and processed using BirdNET, a machine learning tool for identifying birds by sound. Detections were filtered using statistical models to reduce false positives.</em>"))),
        br(),
        h6(HTML(glue("<strong>Biome Type:</strong> Primary vegetation type found at site, either 'forest' or 'grassland'."))),
        h6(HTML(glue("<strong>Detection Probability:</strong> The likelihood that this detection is a true detection of the species, based on statistical models our team developed using hundreds of manually reviewed recordings. Higher values indicate greater certainty. For example, a value of 95% means our models estimate a 95% chance that this is a real detection."))),
        h6(HTML(glue("<strong>Total Detections:</strong> The number of times this species was detected at this site above our confidence threshold. Because BirdNET analyzes audio in 3-second segments, a single bird can produce many detections, so this number reflects acoustic activity rather than the number of individual birds present.")))
      )
    )
    #shinyjs::show("download_button2") # show download button
    shinyjs::show("columninfoheader2") # show column info header
    shinyjs::show("columninfo2") # show column info
    
  })
  
  # outputs seen in dashboard
  
  # tab 1
  output$browser_dim <- renderText({
    paste0(window_width(), "x", shinybrowser::get_height())
  })
  output$spacer1 = renderUI({HTML(paste(" ", br()))})
  output$siteselection = renderUI({
    if (displayedsite() == "") {HTML(paste(h4("Select your Site Below:")))} else {HTML(displayedsite())}
  })
  output$map = renderLeaflet(map())
  output$site_stats_header = renderUI({HTML(displayedstatsheader())})
  output$site_statistics = renderUI({HTML(displayedstats())})
  output$birdfamily = renderPlot(req(birdbarplot()), 
                                 res = 96)
  output$familyselection = renderUI({HTML(displayedfamily())})
  output$table = renderDT(datatable(req(df()), escape = FALSE, 
                                    container = tab1tablehover,
                                    options = list(
                                      autoWidth = TRUE,
                                      columnDefs = list(list(className = 'dt-left',
                                                             targets = "_all")))))
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
  
  # tab 2
  output$spacer2 = renderUI({HTML(paste(" ", br()))})
  output$speciesmap = renderLeaflet(req(speciesmap()))
  output$speciesselection = renderUI({
    if (input$species == "") {HTML(paste(br(), h4("")))} else {HTML(speciesmsg())}
  })
  output$speciesplot = renderPlot(req(speciesplot()),
                                  res = 96)
  output$species_at_sites = renderUI({HTML(displayed_species_at_sites())})
  output$speciestable = renderDT(req(datatable(req(speciesdf()), escape = FALSE,
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
  
  # overall
  output$footer = renderUI({
    HTML(paste(
      h6("This research is made possible with support from the University of Minnesota and Minnesota's Environment and Natural Resources Trust Fund. Partners include the Minnesota Department of Natural Resources, the Minnesota Cooperative Fish and Wildlife Research Unit, Audubon Upper Mississippi River, and our citizen science volunteers and collaborators."), h6("App and data last updated on Mar 11th, 2026."), h6(HTML(glue("<em><strong>Contact us at:</strong></em> <u>soundsofnature@umn.edu</u>")))
    ))
  })
}

shinyApp(ui, server)

#### End of Script