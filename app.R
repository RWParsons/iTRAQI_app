library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(shinyjs)

source("0_utils.R")
source("1_functions.R")
source("2_load_data.R")
source("3_palettes.R")
source("4_tour.R")
source("5_loading_displays.R")
# styles.css from https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example


ui <- 
  navbarPage(
    "iTRAQI", id="nav",
    tabPanel("Main map",
             useShinyjs(),
             div(class="outer",
                 tags$head(
                   includeCSS("styles.css"),
                   tags$script(src="script.js")
                 ),
                 leafletOutput("map", width="100%", height="100%"),
                 hidden(absolutePanel(
                   id = "plot_panel", class = "panel panel-default", fixed = TRUE,
                   draggable = FALSE, top = 'auto', left = 10, right = 'auto', bottom = 10,
                   width = 500, height = 500,
                   plotOutput(
                     "selected_SAs_plot", width = "100%", height = '100%',
                     click = "plot_click",
                     brush = brushOpts("plot_brush")
                   )
                 )),
                 absolutePanel(
                   id = "controls", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 370, left = "auto", right = 10, bottom = "auto",
                   width = 330, height = 600,
                   h4("Layer"),
                   radioButtons(
                     inputId="layer_selection", label=NULL,
                     choices=c(
                       "None", 
                       "SA1 Index", "SA2 Index", 
                       "Acute time", "SA1 Acute", "SA2 Acute",
                       "Rehab time", "SA1 Rehab", "SA2 Rehab"
                     ),
                     selected="None"
                   ),
                   h4("Markers"),
                   checkboxGroupInput(
                     "base_layers",label=NULL,
                     choices=all_base_layers,
                     selected=all_base_layers
                   ),
                   h4("Filters"),
                   dropdownButton(
                     label="Socioeconomic status", status="default", width=dropdown_width,
                     checkboxGroupInput(
                       inputId="seifa", label="SEIFA", width=dropdown_width,
                       choices=c(seifa_scale_to_text(1:5), 'NA'),
                       selected=c(seifa_scale_to_text(1:5), 'NA')
                     )
                   ),
                   htmlOutput("seifa_included"),
                   tags$br(),
                   dropdownButton(
                     label="Remoteness index", status="default", width=dropdown_width,
                     checkboxGroupInput(
                       inputId="remoteness", label="Remoteness", width=dropdown_width,
                       choices=c(ra_scale_to_text(0:4)),
                       selected=c(ra_scale_to_text(0:4))
                     )
                   ),
                   htmlOutput("remoteness_included"),
                   tags$br(),
                   dropdownButton(
                     label="iTRAQI index", status="default", width=dropdown_width,
                     checkboxGroupInput(
                       inputId="itraqi_index", label="iTRAQI Index", width=dropdown_width,
                       choices=levels(iTRAQI_bins),
                       selected=levels(iTRAQI_bins)
                     )
                   ),
                   htmlOutput("itraqi_index_included")
                 ),
                 absolutePanel(
                   id = "loadingScreen", class = "panel panel-default", 
                   fixed = TRUE, draggable = TRUE, 
                   top = 0, left = 0, right = 0, bottom = 0,
                   width = 500, height = 200,
                   HTML(loading_panel_displays[sample_display()])
                 ),
                 hidden(absolutePanel(
                   id = "itraqi_box", class = "panel panel-default", 
                   fixed = TRUE,
                   draggable = TRUE, top = 158, left = 10, right = "auto", bottom = "auto",
                   width = 330, height = 450,
                   h3("iTRAQI Index categorisation:"),
                   HTML(itraqi_categories_table)
                 )),
                 tags$div(
                   id="cite",
                   citation
                 )
             )
    ),
    tabPanel(
      "Tour",
      useShinyjs(),
      div(class="outer",
          tags$head(
            includeCSS("styles.css")
          ),
          leafletOutput("map_tour", width="100%", height="100%"),
          absolutePanel(
            id = "tour_controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top=80, left = "auto", right = 10, bottom = "auto",
            width = tours_panel_dims$width, height = tours_panel_dims$height,
            tags$br(),
            splitLayout(
              cellWidths = 230,
              uiOutput("backButtonControl"),
              uiOutput("nextButtonControl")
            ),
            uiOutput("tourText")
          ),
          tags$div(
            id="cite",
            citation
          )
      )
    ),
    tabPanel("Rehab map",
             div(class="outer",
                 tags$head(
                   includeCSS("styles.css")
                 ),
                 leafletOutput("map_rehab", width="100%", height="100%"),
                 absolutePanel(
                   id = "rehab_controls", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 370, left = "auto", right = 10, bottom = "auto",
                   width = 150, height = 280,
                   h4("Layers"),
                   tags$hr(),
                   radioButtons(
                     inputId="rehab_layer_selection", label=NULL,
                     choices=c("None", names(rehab_tiers)),
                     selected="None"
                   ),
                   tags$hr(),
                   checkboxGroupInput(
                     "rehab_markers_checkbox",label=NULL,
                     choices=c("Towns", "Centres"),
                     selected=c("Towns", "Centres")
                   )
                 ),
                 tags$div(
                   id="cite",
                   citation
                 )
             )
    ),
    tabPanel(
      title="Information",
      icon=icon("info-sign",lib='glyphicon'),
      div(class="outer", htmlOutput("info_page"))
    ),
    tabPanel(
      title="Downloads",
      icon=icon("download-alt", lib="glyphicon"),
      tags$h3("Download links for SA1 and SA2 aggregate time to care:"),
      tags$h4("2011 Australian Statistical Geography Standard (ASGS) Statistical Areas level 1 and 2 (SA1 and SA2):"),
      downloadBttn("download_SA1_2011", "Download (2011 SA1s)", style="pill", block=FALSE),
      downloadBttn("download_SA2_2011", "Download (2011 SA2s)", style="pill", block=FALSE),
      tags$br(),
      tags$br(),
      tags$h4("2016 ASGS SA1 and SA2:"),
      downloadBttn("download_SA1_2016", "Download (2016 SA1s)", style="pill", block=FALSE),
      downloadBttn("download_SA2_2016", "Download (2016 SA2s)", style="pill", block=FALSE),
      tags$br(),
      tags$br(),
      tags$h4("2021 ASGS SA1 and SA2:"),
      downloadBttn("download_SA1_2021", "Download (2021 SA1s)", style="pill", block=FALSE),
      downloadBttn("download_SA2_2021", "Download (2021 SA2s)", style="pill", block=FALSE)
      
    )
  )


server <- function(input, output, session) {
  rvs <- reactiveValues(
    to_load=NULL, map=NULL, map_complete=FALSE,
    to_load_rehab=NULL, map_rehab=NULL, map_rehab_complete=FALSE, 
    to_load_tour=NULL, map_tour=NULL, map_tour_complete=FALSE, tour_tab=1
  )

  output$nextButtonControl <- renderUI({
    if(rvs$tour_tab != n_tour_windows) actionButton("nextButton", "Loading") else NULL
  })
  
  output$backButtonControl <- renderUI({
    if(rvs$tour_tab != 1) actionButton("backButton", "Back") else NULL
  })
  
  observeEvent(input$nextButton, {
    rvs$tour_tab <- rvs$tour_tab + 1
  })
  
  observeEvent(input$backButton, {
    rvs$tour_tab <- rvs$tour_tab - 1
  })
  
  output$tourText <- renderUI({
    HTML(tour_text[[rvs$tour_tab]])
  })
  
  output$map_tour <- renderLeaflet({
    rvs$map_tour <-
      leaflet(options=leafletOptions(minZoom=5)) %>%
      setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5) %>%
      fitBounds(
        lng1=qld_bounds$lng1, lng2=qld_bounds$lng2, 
        lat1=qld_bounds$lat1, lat2=qld_bounds$lat2
      ) %>%
      addSearchOSM(options=searchOptions(moveToLocation=FALSE, zoom=NULL)) %>%
      addMapPane(name = "layers", zIndex = 200) %>%
      addMapPane(name = "maplabels", zIndex = 400) %>%
      addMapPane(name = "markers", zIndex = 206) %>%
      addMapPane(name = "acute_centres", zIndex = 205) %>%
      addMapPane(name = "rehab_centres", zIndex = 204) %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addProviderTiles("CartoDB.VoyagerOnlyLabels",
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") 
    rvs$map_tour
  })
  
  observeEvent(rvs$to_load_tour,{
    if(is.null(isolate(rvs$map_tour)) | isolate(rvs$map_tour_complete))return()
    
    leafletProxy("map_tour") %>%
      hideGroup(unique_ids) %>%
      addCircleMarkers(
        lng=df_locations$x, lat=df_locations$y,
        radius=2, fillOpacity=0,
        popup=df_locations$popup,
        group="Towns"
      ) %>%
      addPolygons(
        data=tours_polygons,
        fillColor=~palFac(aria$ra_label),
        color="black",
        fillOpacity=1,
        weight=1,
        group="tours_polygons",
        layerId=tours_polygons$CODE,
        options=leafletOptions(pane="layers")
      ) %>%
      addMarkers(
        lng=df_centres$x[df_centres$care_type=="acute"],
        lat=df_centres$y[df_centres$care_type=="acute"],
        icon=centre_icons["acute"],
        popup=df_centres$popup[df_centres$care_type=="acute"],
        group="Acute centres",
        options=leafletOptions(pane="acute_centres")
      )%>% 
      addMarkers(
        lng=df_centres$x[df_centres$care_type=="rehab"],
        lat=df_centres$y[df_centres$care_type=="rehab"],
        icon=centre_icons["rehab"],
        popup=df_centres$popup[df_centres$care_type=="rehab"],
        group="Rehab centres",
        options=leafletOptions(pane="rehab_centres")
      )
    
    group_names_to_load <- names(layer_input)
    raster_layers <- grep("raster", layer_input)
    raster_layers <- group_names_to_load[raster_layers]
    
    for(group_name in raster_layers){
      new_layer <- readRDS(file.path(layers_dir, glue::glue("{layer_input[group_name]}.rds")))
      leafletProxy("map_tour") %>%
        addRasterImage(
          data=new_layer,
          x=raster::raster(new_layer, layer=1),
          group=group_name,
          colors=palNum
        )
    }
    
    
    if(!isolate(rvs$map_tour_complete)) rvs$map_tour_complete <- TRUE
    output$nextButtonControl <- renderUI({
      if(rvs$tour_tab != n_tour_windows) actionButton("nextButton", "Next") else NULL
    })
  })
  
  observeEvent(rvs$tour_tab, {
    hide_groups <- unique_ids
    show_groups <- unique_ids
    
    tab_num <- paste0("tab", rvs$tour_tab)
    desired_groups <- tab_ids[[tab_num]]
    if(is.null(desired_groups)) desired_groups <- c()
    hide_groups <- unique_ids[!unique_ids %in% desired_groups]
    show_groups <- desired_groups
    
    desired_legend <- tab_legend_ids[[tab_num]]
    if(!is.null(desired_legend)){
      show_legend_fx <- tab_legends[[desired_legend]]
    } else{
      show_legend_fx <- function(x) {x}
    }
    
    show_hide_layers_and_legends <- function(map) {
      
      if(any(c("aria", "index") %in% show_groups)) {
        show_groups <- c(show_groups, "tours_polygons")
        if("aria" %in% show_groups){
          fill <- palFac(tours_polygons$ra_label)
        } else if("index" %in% show_groups) {
          fill <- paliTRAQI(tours_polygons$index)
        }
        
        map <- setShapeStyle(
          map = map, 
          layerId = tours_polygons$CODE, 
          fillColor = fill
        )
        
      } else {
        hide_groups <- c(hide_groups, "tours_polygons")
      }
      
      map %>%
        hideGroup(hide_groups) %>%
        showGroup(show_groups) %>%
        clearControls() %>%
        show_legend_fx()
    }
    
    if(rvs$tour_tab == 1){
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends()
    } else if(rvs$tour_tab == 2){
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends()
    } else if(rvs$tour_tab == 3){
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends() %>%
        flyTo(lng=142.93, lat=-11.15, zoom=8)
    } else if(rvs$tour_tab == 4){
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends() 
      
      # fit bounds of map to include GC and Brisbane acute care centres
      leafletProxy("map_tour") %>% 
        flyToBounds(lng1=152.78, lat1=-27.12, lng2=153.7, lat2=-28.15)
      
      # After a delay, move view to TSV hospital
      delay(6000, {
        if(rvs$tour_tab==4){ # ensure that user is still on tab 4 before executing fly to TSV
          leafletProxy("map_tour") %>% flyTo(lng=146.76, lat=-19.32, zoom=8)
        }
      })
    } else if(rvs$tour_tab == 5){
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends() %>% 
        flyToBounds(
          lng1=qld_bounds$lng1, lat1=qld_bounds$lat1,
          lng2=qld_bounds$lng2, lat2=qld_bounds$lat2
        )
    } else if(rvs$tour_tab == 6){
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends()
    } else if(rvs$tour_tab == 7){
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends()
    } else {
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends() 
    }
    
  })
  
  output$map <- renderLeaflet({
    rvs$map <- 
      leaflet(options=leafletOptions(minZoom=5)) %>%
      setMaxBounds(
        lng1 = map_bounds$lng1, lat1 = map_bounds$lat1, 
        lng2 = map_bounds$lng2, lat2 = map_bounds$lat2) %>%
      addSearchOSM(options=searchOptions(moveToLocation=FALSE, zoom=NULL)) %>%
      addMapPane(name = "layers", zIndex = 200) %>%
      addMapPane(name = "maplabels", zIndex = 400) %>%
      addMapPane(name = "markers", zIndex = 206) %>%
      addMapPane(name = "acute_centres", zIndex = 205) %>%
      addMapPane(name = "rehab_centres", zIndex = 204) %>%
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
        options=leafletOptions(pane="acute_centres")
      ) %>% 
      addMarkers(
        lng=df_centres$x[df_centres$care_type=="rehab"],
        lat=df_centres$y[df_centres$care_type=="rehab"],
        icon=centre_icons["rehab"],
        popup=df_centres$popup[df_centres$care_type=="rehab"],
        group="Rehab centres",
        options=leafletOptions(pane="rehab_centres")
      )
  })
  
  observeEvent(rvs$to_load,{
    if(is.null(isolate(rvs$map)) | isolate(rvs$map_complete))return()
    
    group_names_to_load <- names(layer_input)
    raster_layers <- grep("raster", layer_input)
    raster_layers <- group_names_to_load[raster_layers]
    
    for(group_name in raster_layers){
      new_layer <- readRDS(file.path(layers_dir, glue::glue("{layer_input[group_name]}.rds")))
      leafletProxy("map") %>%
        addRasterImage(
          data=new_layer,
          x=raster::raster(new_layer, layer=1),
          group=group_name,
          colors=palNum
        )
    }
    
    
    leafletProxy("map") %>%
      addPolygons(
        data=polygons,
        fillColor="white",
        color="black",
        fillOpacity=1,
        weight=1,
        group=polygons$group_id,
        layerId=polygons$CODE,
        options=leafletOptions(pane="layers")
      )
    
    hide("loadingScreen")
    if(!isolate(rvs$map_complete)) rvs$map_complete <- TRUE
  })
  
  observe({
    show_base_layers <- input$base_layers
    hide_base_layers <- all_base_layers[!all_base_layers %in% show_base_layers]
    
    leafletProxy("map") %>%
      showGroup(show_base_layers) %>%
      hideGroup(hide_base_layers)
  })
  
  output$seifa_included <- renderText({
    if(length(input$seifa) == 6) {
      return("<b>All included</b>")
    } else {
      paste("<b>Including:</b>", paste0(input$seifa, collapse=", "))
    }
  })
  
  output$remoteness_included <- renderText({
    if(length(input$remoteness) == 5) {
      return("<b>All included</b>")
    } else {
      paste("<b>Including:</b>", paste0(input$remoteness, collapse=", "))
    }
  })
  
  output$itraqi_index_included <- renderText({
    if(length(input$itraqi_index) == length(levels(iTRAQI_bins))) {
      return("<b>All included</b>")
    } else {
      paste("<b>Including:</b>", paste0(input$itraqi_index, collapse=", "))
    }
  })
  
  observeEvent(input$layer_selection, {
    legend_type <- 
      case_when(
        input$layer_selection=="None" ~ "none",
        str_detect(tolower(input$layer_selection), "index") ~ "index",
        TRUE ~ "time"
      )
    
    if(legend_type=="index") {
      show(id="itraqi_box")
    } else {
      hide(id="itraqi_box")
    }
    
    if(legend_type=="index") {
      leafletProxy("map") %>% 
        clearControls() %>%
        addLegendFactor(
          opacity=1,
          position="topright",
          pal=paliTRAQI,
          values=iTRAQI_bins,
          title=htmltools::tagList(tags$div("iTRAQI index"), tags$br())
        )
    } else if(legend_type=="time") {
      leafletProxy("map") %>%
        clearControls() %>%
        addLegendBin(
          opacity=1,
          position="topright",
          pal=palBin,
          values=0:900,
          title=htmltools::tagList(tags$div("Time to care (minutes)"), tags$br())
        )
    } else {
      leafletProxy("map") %>%
        clearControls()
    }
    
  })
  
  filtered_df <- reactive({
    sa_selected <- as.numeric(str_extract(input$layer_selection, "[0-9]{1}"))
    care_type_selected <- str_extract(tolower(input$layer_selection), "[a-z]*$")
    
    
    if(care_type_selected == "acute"){
      data <- mutate(polygons, selected_col=value_acute)
    } else if(care_type_selected == "rehab"){
      data <- mutate(polygons, selected_col=value_rehab)
    } else if(care_type_selected == "index"){
      data <- mutate(polygons, selected_col=index)
    } else {
      data <- polygons
    }
    
    data %>%
      filter(SA_level==sa_selected) %>%
      mutate(
        selected = ifelse(
          (
            ra %in% ra_text_to_value(input$remoteness) &
            seifa_quintile %in% seifa_text_to_value(input$seifa) &
            index %in% input$itraqi_index
          ),
          TRUE, 
          FALSE
        )) %>%
      mutate(selected_col=ifelse(selected, selected_col, NA))
      
  })
  
  observeEvent(filtered_df(), {
    if(nrow(filtered_df())==0) {
      hide(id="plot_panel")
    } else {
      show(id="plot_panel")
    }
  })
  
  plot_colour_scale <- reactive({
    care_type_selected <- str_extract(tolower(input$layer_selection), "[a-z]*$")
    na_value <- "transparent"
    
    if(care_type_selected=="index"){
      scale_colour_manual(
        values=paliTRAQI(iTRAQI_bins),
        limits=iTRAQI_bins,
        na.value=na_value
      )
    } else {
      minimum <- min(filtered_df()$selected_col, na.rm=TRUE)
      maximum <- max(filtered_df()$selected_col, na.rm=TRUE)
      col_bins <- c(minimum, bins[(bins > minimum & bins < maximum)], maximum)
      
      scale_colour_gradientn(
        colours=palNum(col_bins),
        values=scales::rescale(col_bins),
        na.value=na_value
      )
    }
  })
  
  observeEvent(list(input$plot_click, input$plot_brush,input$layer_selection), {
    clicked_point <- st_centroid(nearPoints(filtered_df(), input$plot_click)[1, ])
    brushed_points <- brushedPoints(filtered_df(), input$plot_brush)
    
    points <- st_centroid(rbind(clicked_point, brushed_points))
    
    care_type_selected <- str_extract(tolower(input$layer_selection), "[a-z]*$")
    
    if(care_type_selected=="acute") {
      popup <- clicked_point$popup_acute
    } else if(care_type_selected=="rehab") {
      popup <- clicked_point$popup_rehab
    } else {
      popup <- clicked_point$popup_index
    }
    
    revert_codes <- polygons %>%
      filter(!CODE %in% points$CODE) %>%
      pull(CODE)
    
    leafletProxy("map") %>%
      setShapeStyle(layerId = points$CODE, color="Green", weight=5) %>%
      setShapeStyle(layerId = revert_codes, color="black", weight=1)
    
    leafletProxy("map") %>%
      clearGroup(group="popup_selection") %>%
      addPopups(data=clicked_point, popup=popup, group="popup_selection")
  })
  
  output$selected_SAs_plot <- renderPlot({
    filtered_df() %>%
      as.data.frame() %>%
      ggplot(aes(value_rehab, value_acute, col=selected_col)) + 
      geom_point(alpha=0.3, size=2) +
      theme_bw() +
      labs(
        y="Acute time (minutes)",
        x="Rehab time (minutes)"
      ) +
      plot_colour_scale() +
      guides(col="none")
  })
  
  observeEvent(list(input$seifa, input$remoteness, input$itraqi_index, input$layer_selection), {
    raster_ids <- c("Acute time", "Rehab time")
    all_ids <- c(groupings$group_id, raster_ids)
    sa_selected <- as.numeric(str_extract(input$layer_selection, "[0-9]{1}"))
    care_type_selected <- str_extract(tolower(input$layer_selection), "[a-z]*$")
    ra_selected <- ra_text_to_value(input$remoteness)
    seifa_selected <- seifa_text_to_value(input$seifa)
    itraqi_index_selected <- input$itraqi_index
    
    if (input$layer_selection %in% c("None")) {
      leafletProxy("map") %>% hideGroup(all_ids)
    } else if (input$layer_selection %in% raster_ids){
      leafletProxy("map") %>% hideGroup(all_ids) %>% showGroup(input$layer_selection)
    }
    else {
      show_ids <- groupings %>%
        filter(sa==sa_selected,
               ra%in%ra_selected,
               seifa%in%seifa_selected,
               index%in%itraqi_index_selected
        ) %>%
        pull(group_id)
      hide_ids <- c(groupings$group_id[!groupings$group_id %in% show_ids], raster_ids)
      
      if(care_type_selected == "rehab") {
        fill <- palNum(polygons$value_rehab)
        label <- polygons$popup_rehab
      } else if(care_type_selected == "acute") {
        fill <- palNum(polygons$value_acute)
        label <- polygons$popup_acute
      } else {
        fill <- paliTRAQI(polygons$index)
        label <- polygons$popup_index
      }
      
      leafletProxy("map") %>%
        setShapeStyle(layerId = polygons$CODE, fillColor=fill) %>%
        setShapeLabel(layerId = polygons$CODE, label=label) %>%
        showGroup(show_ids) %>%
        hideGroup(hide_ids)
    }
  })
  
  observe({
    if(is.null(input$map_click)) return()
    
    lat <- input$map_click$lat
    lng <- input$map_click$lng
    
    if(!check_point_in_qld(lat=lat, lng=lng)) return()
    
    digits <- 2
    acute_pred <- get_nearest_pred(lat=lat, lng=lng, r_points=rasters_points$acute_raster)
    rehab_pred <- get_nearest_pred(lat=lat, lng=lng, r_points=rasters_points$rehab_raster)
    content <- glue::glue(.sep="<br/>",
                          "<b>New location</b>",
                          "Longitude: {round(lng, digits)}",
                          "Latitude: {round(lat, digits)}",
                          "iTRAQI index: {get_iTRAQI_index(acute_mins=acute_pred, rehab_mins=rehab_pred)}",
                          "Acute time prediction: {round(acute_pred, 0)} minutes",
                          "Rehab time prediction: {round(rehab_pred, 0)} minutes"
    )
    leafletProxy("map") %>%
      addMarkers(
        lng=lng, lat=lat,
        layerId="map_click_marker",
        popup=content
      ) 
    
    runjs(sprintf("setTimeout(() => open_popup('%s'), 10)", "map_click_marker"))
  })
  
  observe({
    click <- input$map_marker_click
    if(is.null(click)) return()
    
    if("map_click_marker" %in% click$id){
      leafletProxy("map") %>% removeMarker(layerId="map_click_marker")
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
      hideGroup(paste0("centres_", names(rehab_tiers))) %>%
      hideGroup(paste0("towns_", names(rehab_tiers))) %>%
      addCircleMarkers(
        lng=df_rehab_map_locations$x, lat=df_rehab_map_locations$y, 
        radius=2, fillOpacity=0,
        popup=df_rehab_map_locations$popup_none,
        group="towns_None",
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
      )
    rvs$map_rehab
  })
  
  observeEvent(list(input$rehab_layer_selection, input$rehab_markers_checkbox), {
    rehab_base_groups <- c("None", names(rehab_tiers))
    rehab_towns_groups <- paste0("towns_", rehab_base_groups)
    rehab_centre_groups <- paste0("centres_", names(rehab_tiers))
    
    hide_base_groups <- rehab_base_groups[rehab_base_groups != input$rehab_layer_selection]
    show_base_group <- input$rehab_layer_selection
    
    hide_centre_groups <- paste0("centres_", hide_base_groups)
    show_centre_group <- paste0("centres_", show_base_group)
    
    if(!"Towns" %in% input$rehab_markers_checkbox) {
      show_towns_group <- c()
      hide_towns_groups <- rehab_towns_groups
    } else {
      show_towns_group <- rehab_towns_groups[which(rehab_base_groups == show_base_group)]
      hide_towns_groups <- rehab_towns_groups[rehab_towns_groups != show_towns_group]
    }
    
    if(!"Centres" %in% input$rehab_markers_checkbox) {
      show_centre_group <- c()
      hide_centre_groups <- rehab_centre_groups
    } else {
      if(show_base_group=="None") {
        show_centre_group <- c()
      } else {
        show_centre_group <- rehab_centre_groups[which(names(rehab_tiers) == show_base_group)]
      }
      hide_centre_groups <- rehab_centre_groups[!rehab_centre_groups %in% show_centre_group]
    }
    
    show_ids <- c(show_base_group, show_centre_group, show_towns_group)
    hide_ids <- c(hide_base_groups, hide_centre_groups, hide_towns_groups)
    
    leafletProxy("map_rehab") %>%
      showGroup(show_ids) %>%
      hideGroup(hide_ids)
  })
  
  observeEvent(rvs$to_load_rehab, {
    req(rvs$map_rehab)
    if(is.null(isolate(rvs$map_rehab)) | isolate(rvs$map_rehab_complete))return()
    for(group_name in names(rehab_tiers)){
      new_layer <- readRDS(file.path(layers_dir, glue::glue("{rehab_tiers[[group_name]]$file}.rds")))
      centres_group <- df_centres[df_centres$centre_name %in% rehab_tiers[[group_name]]$centres, ]
      
      popup_col <- paste0("popup_",tolower(group_name))
      popup_col <- str_replace_all(popup_col, " ", "_")
      town_popups <- df_rehab_map_locations %>% pull(!!{popup_col})
      
      leafletProxy("map_rehab") %>%
        addRasterImage(
          data=new_layer,
          x=raster::raster(new_layer, layer=1),
          group=group_name,
          colors=palNum
        ) %>%
        addMarkers(
          lng=centres_group$x, lat=centres_group$y, 
          icon=tier_icons[group_name],
          popup=centres_group$popup,
          group=paste0("centres_", group_name),
          options=leafletOptions(pane="markers")
        ) %>%
        addCircleMarkers(
          lng=df_rehab_map_locations$x, lat=df_rehab_map_locations$y, 
          radius=2, fillOpacity=0,
          popup=town_popups,
          group=paste0("towns_", group_name),
          options=leafletOptions(pane="markers")
        )
    }
    if(!isolate(rvs$map_rehab_complete)) rvs$map_rehab_complete <- TRUE
  })
  
  f <- function(){
    if(is.null(isolate(rvs$to_load))) rvs$to_load <- 1
    if(is.null(isolate(rvs$to_load_rehab))) rvs$to_load_rehab <- 1
    if(is.null(isolate(rvs$to_load_tour))) rvs$to_load_tour <- 1
    
    if(!is.null(isolate(rvs$to_load)) & !isolate(rvs$map_complete) & !is.null(isolate(rvs$map))){
      rvs$to_load <- isolate(rvs$to_load) + 1
    }
    
    if(!is.null(isolate(rvs$to_load_rehab)) & !isolate(rvs$map_rehab_complete) & !is.null(isolate(rvs$map_rehab))){
      rvs$to_load_rehab <- isolate(rvs$to_load_rehab) + 1
    }
    
    if(!is.null(isolate(rvs$to_load_tour)) & !isolate(rvs$map_tour_complete) & !is.null(isolate(rvs$map_tour))){
      rvs$to_load_tour <- isolate(rvs$to_load_tour) + 1
    }
  }
  session$onFlushed(f, once=FALSE)
  
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
  
  output$info_page <- renderUI({
    tags$iframe(
      seamless="seamless", src="iTRAQI_info.html", 
      style='width:100vw;height:calc(100vh - 41px);'
    )
  })
}

shinyApp(ui=ui,server=server)
