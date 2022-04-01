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

source("0_utils.R")
source("1_functions.R")
source("2_pallettes.R")
source("3_load_data.R")


ui <- navbarPage(
  "iTRAQI",
  tabPanel(
    title="Main Map",
    div(
      tags$style(type = "text/css", "#map_async {height: calc(100vh - 80px) !important;}"),
      withSpinner(leafletOutput("map_async")),
      
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        radioButtons(
          inputId="layer_selection", label="Layer", 
          choices=c("None", "Acute time", "Rehab time", "SA1 Acute", "SA2 Acute", "SA1 Rehab", "SA2 Rehab"), 
          selected="None"
        ),
        h4("Filters"),
        dropdownButton(
          label="Socioeconomic status", status="default", width=dropdown_width,
          checkboxGroupInput(
            inputId="seifa", label="SEIFA", width=dropdown_width,
            choices=c(seifa_scale_to_text(1:5), NA),
            selected=c(seifa_scale_to_text(1:5), NA)
          )
        ),
        dropdownButton(
          label="Remoteness index", status="default", width=dropdown_width,
          checkboxGroupInput(
            inputId="remoteness", label="Remoteness", width=dropdown_width,
            choices=c(ra_scale_to_text(0:4)),
            selected=c(ra_scale_to_text(0:4))
          )
        )
      )
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
  
  
  
  # based on asynchronous map loading here
  # https://medium.com/ibm-data-ai/asynchronous-loading-of-leaflet-layer-groups-afc073999e77
  # but improving it so that the trigger doesn't occur until after the basemap is up
  # https://stackoverflow.com/questions/66388965/understanding-sessiononflush-in-shiny
  rvs <- reactiveValues(to_load=NULL, map=NULL, to_load_rehab=NULL, map_rehab=NULL, map_complete=FALSE, map_rehab_complete=FALSE)
  
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
      hideGroup(groupings$group_id) %>%
      addCircleMarkers(
        lng=df_locations$x, lat=df_locations$y, 
        radius=2, fillOpacity=0,
        popup=df_locations$popup,
        group="Towns",
        options=leafletOptions(pane="markers")
      ) %>%
      addMarkers(
        lng=df_centres$x[df_centres$care_type=="acute"],
        lat=df_centres$y[df_centres$care_type=="acute"],
        icon=centre_icons["acute"],
        popup=df_centres$popup[df_centres$care_type=="acute"],
        group="Acute centres",
        options=leafletOptions(pane="markers")
      ) %>% 
      addMarkers(
        lng=df_centres$x[df_centres$care_type=="rehab"],
        lat=df_centres$y[df_centres$care_type=="rehab"],
        icon=centre_icons["rehab"],
        popup=df_centres$popup[df_centres$care_type=="rehab"],
        group="Rehab centres",
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
        # baseGroups = c("None", "Acute time", "Rehab time"),
        overlayGroups = c("Towns", "Acute centres", "Rehab centres"),
        options = layersControlOptions(collapsed = TRUE))
  })
  
  observeEvent(rvs$to_load,{
    if(is.null(isolate(rvs$map)) | isolate(rvs$map_complete))return()
    
    group_names_to_load <- names(layer_input)
    raster_layers <- grep("raster", layer_input)
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
    
    for(i in groupings$group_id){
      polygons_df <- polygons[polygons$group_id==i,]
      leafletProxy("map_async") %>%
        addPolygons(
          data=polygons_df,
          fillColor=~palBin(polygons_df$value),
          color="black",
          fillOpacity=1,
          weight=1,
          group=i,
          popup=polygons_df$popup,
          options=leafletOptions(pane="layers")
        )
    }
    
    if(!isolate(rvs$map_complete)) rvs$map_complete <- TRUE
  })
  
  
  observeEvent(list(input$seifa, input$remoteness, input$layer_selection), {
    raster_ids <- c("Acute time", "Rehab time")
    all_ids <- c(groupings$group_id, raster_ids)
    sa_selected <- as.numeric(str_extract(input$layer_selection, "[0-9]{1}"))
    care_type_selected <- str_extract(tolower(input$layer_selection), "[a-z]*$")
    ra_selected <- ra_text_to_value(input$remoteness)
    seifa_selected <- seifa_text_to_value(input$seifa)

    if (input$layer_selection %in% c("None")) {
      leafletProxy("map_async") %>% hideGroup(all_ids)
    } else if (input$layer_selection %in% raster_ids){
      leafletProxy("map_async") %>% hideGroup(all_ids) %>% showGroup(input$layer_selection)
    }
    else {
      show_ids <- groupings %>%
        filter(sa==sa_selected,
               ra%in%ra_selected,
               seifa%in%seifa_selected,
               care_type==care_type_selected) %>%
        pull(group_id)
      hide_ids <- c(groupings$group_id[!groupings$group_id %in% show_ids], raster_ids)
      leafletProxy("map_async") %>%
        showGroup(show_ids) %>%
        hideGroup(hide_ids)
    }
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
        popup=df_locations$popup_rehab,
        group="Towns",
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
        overlayGroups = c("Towns"),
        options = layersControlOptions(collapsed = TRUE))
    rvs$map_rehab
  })
  
  observeEvent(rvs$to_load_rehab, {
    req(rvs$map_rehab)
    if(is.null(isolate(rvs$map_rehab)) | isolate(rvs$map_rehab_complete))return()
    for(group_name in names(rehab_tiers)){
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
}

shinyApp(ui, server)
