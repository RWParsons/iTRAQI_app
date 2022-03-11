library(shiny)
library(shinyWidgets)
library(leaflet)
# To get the pane options in raster layers to work, install leaflet from 
# https://github.com/rstudio/leaflet/tree/joe/feature/raster-options
# see associated PR here: https://github.com/rstudio/leaflet/pull/692
# can install directly with `remotes::install_github("rstudio/leaflet", ref="joe/feature/raster-options")`
library(leaflet.extras)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(rgdal)
# https://susanna-cramb.shinyapps.io/itraqi_app/


# layer_input <- c(
#   "SA1 acute time" = "acute_polygons_SA1_year2016",
#   "SA2 acute time" = "acute_polygons_SA2_year2016",
#   "SA1 rehab time" = "rehab_polygons_SA1_year2016",
#   "SA2 rehab time" = "rehab_polygons_SA2_year2016",
#   "Acute time" = "acute_raster",
#   "Rehab time" = "rehab_raster"
# )

layer_input <- c(
  "SA1 acute time" = "acute_polygons_SA1_year2016_simplified",
  "SA2 acute time" = "acute_polygons_SA2_year2016_simplified",
  "SA1 rehab time" = "rehab_polygons_SA1_year2016_simplified",
  "SA2 rehab time" = "rehab_polygons_SA2_year2016_simplified",
  "Acute time" = "acute_raster",
  "Rehab time" = "rehab_raster"
)

group_display <- "SA1 acute time"

ui <- navbarPage(
  "iTRAQI",
  tabPanel(
    title="Map",
    div(
      class="outer",
      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
      leafletOutput("map_async", width = "100%", height = "100%"),
      absolutePanel(
        top = 0, right = 0,
        checkboxInput("legend", "Show legend", TRUE)
      )
    )
  ),
  tabPanel(
    title="Information",
    icon=icon("info-sign",lib='glyphicon'),
    includeMarkdown("input/iTRAQI_info.md"),
    tags$br()
  ),
  tabPanel(
    title="Downloads",
    icon=icon("download-alt", lib="glyphicon"),
    tags$h3("Download links for SA1 and SA2 aggregate time to care:"),
    tags$h4("2011 SA regions:"),
    downloadBttn("download_SA1_2011", "Download (2011 SA1s)", style="pill", block=FALSE),
    downloadBttn("download_SA2_2011", "Download (2011 SA2s)", style="pill", block=FALSE),
    tags$br(),
    tags$br(),
    tags$h4("2016 SA regions:"),
    downloadBttn("download_SA1_2016", "Download (2016 SA1s)", style="pill", block=FALSE),
    downloadBttn("download_SA2_2016", "Download (2016 SA2s)", style="pill", block=FALSE),
    tags$br(),
    tags$br(),
    tags$h4("2021 SA regions:"),
    downloadBttn("download_SA1_2021", "Download (2021 SA1s)", style="pill", block=FALSE),
    downloadBttn("download_SA2_2021", "Download (2021 SA2s)", style="pill", block=FALSE)
    
  )
)

server <- function(input, output, session){
  layers_dir <- "input/layers"
  SAs_sf <- readRDS(file.path(layers_dir, "acute_polygons_SA1_year2016.rds"))
  
  df_locations <- read.csv("input/QLD_locations_with_RSQ_times_20220210.csv") %>%
    mutate(popup=paste0(
      "<b>Location: </b>", location, "<br>",
      "<b>Acute care destination: </b>", acute_care_centre, "<br>",
      "<b>Time to acute care (minutes): </b>", acute_time, "<br>",
      "<b>Rehab care destination: </b>", rehab_centre, "<br>",
      "<b>Time to rehab care (minutes): </b>", rehab_time, "<br>"
    ))
  
  rehab_centres <- c(
    "Sunshine Coast University Hospital",
    "Central West Sub-Acute Service",
    "Gympie Hospital",
    "Rockhampton Hospital",
    "Roma Hospital"
  )
  acute_centres <- c(
    "Brain Injury Rehabilitation Unit",
    "Gold Coast University Hospital",
    "Townsville University Hospital"
  )
  
  df_centres <- read.csv("input/centres.csv") 
  names(df_centres) <- c("centre_name", "address", "x", "y")
  df_centres <- df_centres %>%
    mutate(centre_name = str_trim(centre_name))%>%
    filter(centre_name %in% c(rehab_centres, acute_centres)) %>%
    mutate(
      care_type=ifelse(centre_name %in% acute_centres, "acute", "rehab"),
      popup=paste0(
        "<b>Centre name: </b>", centre_name, "<br>",
        "<b>Care type: </b>", ifelse(care_type=="acute", "Acute care", "Rehabilitation care"), "<br>",
        "<b>Address: </b>", address, "<br>"
      )
    )
  
  
  # https://medium.com/ibm-data-ai/asynchronous-loading-of-leaflet-layer-groups-afc073999e77
  rvs <- reactiveValues(to_load=NULL, map=NULL)
  
  bins <- c(0, 30, 60, 120, 180, 240, 300, 360, 900)
  palBin <- colorBin("YlOrRd", domain = 0:900, bins=bins, na.color="transparent")
  
  palNum1 <- colorNumeric(c("#FFFFCC", "#FFEDA0"), domain=0:30, na.color="transparent")
  palNum2 <- colorNumeric(c("#FFEDA0", "#FED976"), domain=30:60, na.color="transparent")
  palNum3 <- colorNumeric(c("#FED976", "#FEB24C"), domain=60:120, na.color="transparent")
  palNum4 <- colorNumeric(c("#FEB24C", "#FD8D3C"), domain=120:180, na.color="transparent")
  palNum5 <- colorNumeric(c("#FD8D3C", "#FC4E2A"), domain=180:240, na.color="transparent")
  palNum6 <- colorNumeric(c("#FC4E2A", "#E31A1C"), domain=240:300, na.color="transparent")
  palNum7 <- colorNumeric(c("#E31A1C", "#B10026"), domain=300:360, na.color="transparent")
  palNum8 <- colorNumeric(c("#B10026", "#000000"), domain=360:900, na.color="transparent")
  
  palNum <- function(x){
    case_when(
      x < 30 ~ palNum1(x),
      x < 60 ~ palNum2(x),
      x < 120 ~ palNum3(x),
      x < 180 ~ palNum4(x),
      x < 240 ~ palNum5(x),
      x < 300 ~ palNum6(x),
      x < 360 ~ palNum7(x),
      x < 900 ~ palNum8(x),
      TRUE ~ "transparent"
    )
  }
  
  centre_icons <- iconList(
    acute=makeIcon(iconUrl = "input/imgs/acute_care2.png", iconWidth = 783/18, iconHeight = 900/18),
    rehab=makeIcon(iconUrl = "input/imgs/rehab_care.png", iconWidth = 783/18, iconHeight = 783/18)
  )
  
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map_async", data = SAs_sf)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(
        opacity=1,
        position = "bottomright",
        pal = palBin, values = ~mean,
        title = "Time to care (minutes)"
      )
    }
  })
  
  output$map_async <- renderLeaflet({
    rvs$map <- 
      leaflet(options=leafletOptions(minZoom=5)) %>%
      setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5) %>%
      addSearchOSM(options=searchOptions(moveToLocation=FALSE, zoom=NULL)) %>%
      addMapPane(name = "layers", zIndex = 200) %>%
      addMapPane(name = "maplabels", zIndex = 400) %>%
      addMapPane(name = "markers", zIndex = 205) %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addProviderTiles("CartoDB.VoyagerOnlyLabels",
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("None",names(layer_input)),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(names(layer_input)) %>%
      addCircleMarkers(
        lng=df_locations$x, lat=df_locations$y, 
        radius=2, fillOpacity=0,
        popup=df_locations$popup,
        options=leafletOptions(pane="markers")
      ) %>%
      addMarkers(
        lng=df_centres$x, lat=df_centres$y, 
        icon=centre_icons[df_centres$care_type],
        popup=df_centres$popup,
        options=leafletOptions(pane="markers")
      )
    rvs$map
  })
  
  session$onFlushed(function() rvs$to_load <- TRUE)

  observeEvent(rvs$to_load,{
    req(rvs$map)
    SA2s_lookup <- read.csv("input/lookup_data/SA2s_names_lookup.csv")
    SA2s_lookup$SA2_CODE16 <- as.character(SA2s_lookup$SA2_CODE16)
    SA1s_lookup <- read.csv("input/lookup_data/SA1s_names_lookup.csv")
    SA1s_lookup$SA1_CODE16 <- as.character(SA1s_lookup$SA1_CODE16)

    group_names_to_load <- names(layer_input)
    raster_layers <- grep("raster", layer_input)
    polygon_layers <- group_names_to_load[-raster_layers]
    raster_layers <- group_names_to_load[raster_layers]

    for(group_name in polygon_layers){
      care_type <- ifelse(grepl("acute", tolower(group_name)), "acute", "rehab")
      SA_level <- as.numeric(str_extract(group_name, "(?<=SA)[0-9]"))

      new_layer <- readRDS(file.path(layers_dir, glue::glue("{layer_input[group_name]}.rds")))
      if(SA_level==2){
        new_layer <- left_join(new_layer, SA2s_lookup)
      }else if(SA_level==1){
        new_layer <- left_join(new_layer, SA1s_lookup)
      }

      new_layer <- new_layer %>%
        mutate(popup = paste0(
          paste0(
            "<b>SA2 Region: </b>", .[[3]], "<br>",
            "<b>SA", SA_level, " ID: </b>", .[[1]], "<br>",
            "<b>Time to ", care_type, " care (minutes): </b>", round(.[[2]]), "<br>"
          )
        ))
      leafletProxy("map_async") %>%
        addPolygons(
          data=new_layer,
          fillColor=~palBin(value),
          color="black",
          fillOpacity=1,
          weight=1,
          group=group_name,
          popup=new_layer$popup,
          options=leafletOptions(pane="layers")
        )
    }

    for(group_name in raster_layers){
      new_layer <- readRDS(file.path(layers_dir, glue::glue("{layer_input[group_name]}.rds")))
      leafletProxy("map_async") %>%
        addRasterImage(
          data=new_layer,
          x=raster(new_layer, layer=1),
          group=group_name,
          options=leafletOptions(pane="layers"),
          colors=palNum
        )
    }
  })
  
  output$download_SA1_2011 <- downloadHandler(
    filename="aggregated_time_to_care_SA1_year2011.csv",
    content=function(file){
      write.csv(read.csv("input/download_data/combined_data_SA1_year2011.csv"), file, row.names=FALSE)
    }
  )
  output$download_SA2_2011 <- downloadHandler(
    filename="aggregated_time_to_care_SA2_year2011.csv",
    content=function(file){
      write.csv(read.csv("input/download_data/combined_data_SA2_year2011.csv"), file, row.names=FALSE)
    }
  )
  
  output$download_SA1_2016 <- downloadHandler(
    filename="aggregated_time_to_care_SA1_year2016.csv",
    content=function(file){
      write.csv(read.csv("input/download_data/combined_data_SA1_year2016.csv"), file, row.names=FALSE)
    }
  )
  output$download_SA2_2016 <- downloadHandler(
    filename="aggregated_time_to_care_SA2_year2016.csv",
    content=function(file){
      write.csv(read.csv("input/download_data/combined_data_SA2_year2016.csv"), file, row.names=FALSE)
    }
  )
  
  output$download_SA1_2021 <- downloadHandler(
    filename="aggregated_time_to_care_SA1_year2021.csv",
    content=function(file){
      write.csv(read.csv("input/download_data/combined_data_SA1_year2021.csv"), file, row.names=FALSE)
    }
  )
  output$download_SA2_2021 <- downloadHandler(
    filename="aggregated_time_to_care_SA2_year2021.csv",
    content=function(file){
      write.csv(read.csv("input/download_data/combined_data_SA2_year2021.csv"), file, row.names=FALSE)
    }
  )
  
}

shinyApp(ui, server)
