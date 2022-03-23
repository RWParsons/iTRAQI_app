library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
# To get the pane options in raster layers to work, install leaflet from 
# https://github.com/rstudio/leaflet/tree/joe/feature/raster-options
# see associated PR here: https://github.com/rstudio/leaflet/pull/692
# can install directly with `remotes::install_github("rstudio/leaflet", ref="joe/feature/raster-options")`
library(leaflet.extras)
library(leaflegend)
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
  "Acute time" = "acute_raster",
  "Rehab time" = "rehab_raster",
  "SA1 acute time" = "acute_polygons_SA1_year2016_simplified",
  "SA2 acute time" = "acute_polygons_SA2_year2016_simplified",
  "SA1 rehab time" = "rehab_polygons_SA1_year2016_simplified",
  "SA2 rehab time" = "rehab_polygons_SA2_year2016_simplified"
)

rehab_tiers <- list(
  "Platinum" = list(
    file = "rehab_raster_platinum",
    centres = c("Brain Injury Rehabilitation Unit")
  ),
  "Gold" = list(
    file = "rehab_raster_gold",
    centres = c(
      "Townsville University Hospital",
      "Brain Injury Rehabilitation Unit"
    )
  ),
  "Future Gold" = list(
    file = "rehab_raster_future_gold",
    centres = c(
      "Sunshine Coast University Hospital",
      "Gold Coast University Hospital",
      "Townsville University Hospital",
      "Brain Injury Rehabilitation Unit"
    )
  ),
  "Silver" = list(
    file = "rehab_raster",
    centres = c(
      "Brain Injury Rehabilitation Unit",
      "Gold Coast University Hospital",
      "Townsville University Hospital",
      "Sunshine Coast University Hospital",
      "Central West Sub-Acute Service",
      "Gympie Hospital",
      "Rockhampton Hospital",
      "Roma Hospital",
      "Cairns Hospital"
    )
  )
)

tier_icons <- iconList(
  "Platinum"=makeIcon(iconUrl = "input/imgs/platinum.png", iconWidth = 549/18, iconHeight = 562/18),
  "Gold"=makeIcon(iconUrl = "input/imgs/gold_medal.png", iconWidth = 529/18, iconHeight = 625/18),
  "Future Gold"=makeIcon(iconUrl = "input/imgs/gold_medal.png", iconWidth = 529/18, iconHeight = 625/18),
  "Silver"=makeIcon(iconUrl = "input/imgs/silver_medal.png", iconWidth = 303/18, iconHeight = 518/18)
)

centre_icons <- iconList(
  acute=makeIcon(iconUrl = "input/imgs/acute_care2.png", iconWidth = 783/18, iconHeight = 900/18),
  rehab=makeIcon(iconUrl = "input/imgs/rehab_care.png", iconWidth = 783/18, iconHeight = 783/18)
)

group_display <- "SA1 acute time"

seifa_scale_to_text <- function(x){
  case_when(
    x==1 ~ "Most disadvantaged",
    x==2 ~ "Disadvantaged",
    x==3 ~ "Middle socio-economic status",
    x==4 ~ "Advantaged",
    x==5 ~ "Most advantaged",
    TRUE ~ "NA"
  )
}

download_data_dir <- "input/download_data/"

download_data_files <- list(
  SA1_2011="iTRAQI front page - ASGS 2011 SA1.xlsx",
  SA2_2011="iTRAQI front page - ASGS 2011 SA2.xlsx",
  SA1_2016="iTRAQI front page - ASGS 2016 SA1.xlsx",
  SA2_2016="iTRAQI front page - ASGS 2016 SA2.xlsx",
  SA1_2021="iTRAQI front page - ASGS 2021 SA1.xlsx",
  SA2_2021="iTRAQI front page - ASGS 2021 SA2.xlsx"
)

ui <- navbarPage(
  "iTRAQI",
  tabPanel(
    title="Main Map",
    div(
      tags$style(type = "text/css", "#map_async {height: calc(100vh - 80px) !important;}"),
      withSpinner(leafletOutput("map_async"))
    )
  ),
  tabPanel(
    title="Rehab Map",
    div(
      tags$style(type = "text/css", "#map_rehab {height: calc(100vh - 80px) !important;}"),
      withSpinner(leafletOutput("map_rehab"))
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
    "Roma Hospital",
    "Cairns Hospital"
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
  
  # based on asynchronous map loading here
  # https://medium.com/ibm-data-ai/asynchronous-loading-of-leaflet-layer-groups-afc073999e77
  # but improving it so that the trigger doesn't occur until after the basemap is up
  # https://stackoverflow.com/questions/66388965/understanding-sessiononflush-in-shiny
  rvs <- reactiveValues(to_load=NULL, map=NULL, to_load_rehab=NULL, map_rehab=NULL, map_complete=FALSE, map_rehab_complete=FALSE)
  
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
      x >= 900 ~ "#000000",
      TRUE ~ "transparent"
    )
  }
  
  palNum_hours <- function(x){
    palNum(x*60)
  }
  
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
      ) %>% 
      addLegendBin(
        opacity=1,
        position="topright",
        pal=palBin,
        values=0:900,
        title=htmltools::tagList(tags$div("Time to care (minutes)"), tags$br())
      ) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("None",names(layer_input)),
        options = layersControlOptions(collapsed = TRUE))
    rvs$map
  })
  
  output$map_rehab <- renderLeaflet({
    rvs$map_rehab <- 
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
      hideGroup(names(rehab_tiers)) %>%
      addCircleMarkers(
        lng=df_locations$x, lat=df_locations$y, 
        radius=2, fillOpacity=0,
        popup=df_locations$popup,
        options=leafletOptions(pane="markers")
      ) %>% 
      addLegendNumeric(
        pal=palNum_hours,
        position="topright",
        height=250,
        width=24,
        bins=10,
        value=c(-0.01, 0:20, 20.1),
        htmltools::tagList(tags$div("Time to care (hours)"), tags$br())
      ) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("None", names(rehab_tiers)),
        options = layersControlOptions(collapsed = TRUE))
    rvs$map_rehab
  })
  
  observeEvent(rvs$to_load_rehab, {
    req(rvs$map_rehab)
    if(is.null(isolate(rvs$map_rehab)) | isolate(rvs$map_rehab_complete))return()
    for(group_name in names(rehab_tiers)){
      print(file.path(layers_dir, glue::glue("{rehab_tiers[[group_name]]$file}.rds")))
      new_layer <- readRDS(file.path(layers_dir, glue::glue("{rehab_tiers[[group_name]]$file}.rds")))
      centres_group <- df_centres[df_centres$centre_name %in% rehab_tiers[[group_name]]$centres, ]
      leafletProxy("map_rehab") %>%
        addRasterImage(
          data=new_layer,
          x=raster(new_layer, layer=1),
          group=group_name,
          options=leafletOptions(pane="layers"),
          colors=palNum
        ) %>%
        addMarkers(
          lng=centres_group$x, lat=centres_group$y, 
          icon=tier_icons[group_name],
          popup=centres_group$popup,
          group=group_name,
          options=leafletOptions(pane="markers")
        )
    }
    if(!isolate(rvs$map_rehab_complete)) rvs$map_rehab_complete <- TRUE
  })
  
  
  f <- function(){
    if(is.null(isolate(rvs$to_load))) rvs$to_load <- 1
    if(is.null(isolate(rvs$to_load_rehab))) rvs$to_load_rehab <- 1
    
    if(!is.null(isolate(rvs$to_load)) & !isolate(rvs$map_complete) & !is.null(isolate(rvs$map))){
      rvs$to_load <- isolate(rvs$to_load) + 1
    }
    
    if(!is.null(isolate(rvs$to_load_rehab)) & !isolate(rvs$map_rehab_complete) & !is.null(isolate(rvs$map_rehab))){
      rvs$to_load_rehab <- isolate(rvs$to_load_rehab) + 1
    }
  }
  session$onFlushed(f, once=FALSE)
  
  # session$onFlushed(function() rvs$to_load <- TRUE)

  observeEvent(rvs$to_load,{
    if(is.null(isolate(rvs$map)) | isolate(rvs$map_complete))return()
    SA2s_lookup <- read.csv("input/lookup_data/SA2s_names_lookup.csv")
    SA2s_lookup$SA2_CODE16 <- as.character(SA2s_lookup$SA2_CODE16)
    SA1s_lookup <- read.csv("input/lookup_data/SA1s_names_lookup.csv")
    SA1s_lookup$SA1_CODE16 <- as.character(SA1s_lookup$SA1_CODE16)

    group_names_to_load <- names(layer_input)
    raster_layers <- grep("raster", layer_input)
    polygon_layers <- group_names_to_load[-raster_layers]
    raster_layers <- group_names_to_load[raster_layers]
    
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
            "<b>SA2 Region: </b>", .[["SA2_NAME16"]], "<br>",
            "<b>SA", SA_level, " ID: </b>", .[[1]], "<br>",
            "<b>Remoteness: </b>", .[["ra_name"]], "<br>",
            "<b>SEIFA: </b>", seifa_scale_to_text(.[["seifa_quintile"]]), "<br>",
            "<b>Time to ", care_type, " care in minutes (estimate [min - max]): </b>", "<br>", 
            "&nbsp;&nbsp;&nbsp;&nbsp; ", round(.[["value"]]), " [", round(.[["min"]]), " - ", round(.[["max"]]), "]<br>"
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
    
    if(!isolate(rvs$map_complete)) rvs$map_complete <- TRUE
  })
  
  output$download_SA1_2011 <- downloadHandler(
    filename=download_data_files$SA1_2011,
    content=function(file){
      file.copy(file.path(download_data_dir, download_data_files$SA1_2011), file)
    })
  
  output$download_SA2_2011 <- downloadHandler(
    filename=download_data_files$SA2_2011,
    content=function(file){
      file.copy(file.path(download_data_dir, download_data_files$SA2_2011), file)
    })
  
  output$download_SA1_2016 <- downloadHandler(
    filename=download_data_files$SA1_2016,
    content=function(file){
      file.copy(file.path(download_data_dir, download_data_files$SA1_2016), file)
    })
  
  output$download_SA2_2016 <- downloadHandler(
    filename=download_data_files$SA2_2016,
    content=function(file){
      file.copy(file.path(download_data_dir, download_data_files$SA2_2016), file)
    })
  
  output$download_SA1_2021 <- downloadHandler(
    filename=download_data_files$SA1_2021,
    content=function(file){
      file.copy(file.path(download_data_dir, download_data_files$SA1_2021), file)
    })
  
  output$download_SA2_2021 <- downloadHandler(
    filename=download_data_files$SA2_2021,
    content=function(file){
      file.copy(file.path(download_data_dir, download_data_files$SA2_2021), file)
    })
  
}

shinyApp(ui, server)
