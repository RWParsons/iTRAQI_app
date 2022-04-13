library(leaflet)
library(dplyr)

function(input, output, session) {
  rvs <- reactiveValues(
    to_load=NULL, map=NULL, map_complete=FALSE,
    to_load_rehab=NULL, map_rehab=NULL, map_rehab_complete=FALSE, 
    to_load_tour=NULL, map_tour=NULL, map_tour_complete=FALSE, tour_tab=1
  )
  
  output$nextButtonControl <- renderUI({
    if(rvs$tour_tab != n_tour_windows) actionButton("nextButton", "Next") else NULL
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
      addSearchOSM(options=searchOptions(moveToLocation=FALSE, zoom=NULL)) %>%
      addMapPane(name = "layers", zIndex = 200) %>%
      addMapPane(name = "maplabels", zIndex = 400) %>%
      addMapPane(name = "markers", zIndex = 205) %>%
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
        data=aria,
        fillColor=~palFac(aria$ra_label),
        color="black",
        fillOpacity=1,
        weight=1,
        group="aria",
        options=leafletOptions(pane="layers")
      )
    
    if(!isolate(rvs$map_tour_complete)) rvs$map_tour_complete <- TRUE
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
    hide_legend_id <- unique_legend_ids[!unique_legend_ids %in% desired_legend]
    if(!is.null(desired_legend)){
      show_legend_fx <- tab_legends[[desired_legend]]
    } else{
      show_legend_fx <- function(x) {x}
    }
    
    show_hide_layers_and_legends <- function(map) {
      map %>%
        hideGroup(hide_groups) %>%
        showGroup(show_groups) %>%
        show_legend_fx %>%
        removeControl(layerId=hide_legend_id)
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
        setView(lng=152, lat=-27, zoom=7)
    } else {
      leafletProxy("map_tour") %>%
        show_hide_layers_and_legends() 
    }
    
    
  })
  
  output$map <- renderLeaflet({
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
    
    for(i in groupings$group_id){
      polygons_df <- polygons[polygons$group_id==i,]
      leafletProxy("map") %>%
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
  
  observeEvent(list(input$seifa, input$remoteness, input$layer_selection), {
    raster_ids <- c("Acute time", "Rehab time")
    all_ids <- c(groupings$group_id, raster_ids)
    sa_selected <- as.numeric(str_extract(input$layer_selection, "[0-9]{1}"))
    care_type_selected <- str_extract(tolower(input$layer_selection), "[a-z]*$")
    ra_selected <- ra_text_to_value(input$remoteness)
    seifa_selected <- seifa_text_to_value(input$seifa)
    
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
               care_type==care_type_selected) %>%
        pull(group_id)
      hide_ids <- c(groupings$group_id[!groupings$group_id %in% show_ids], raster_ids)
      leafletProxy("map") %>%
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
  addResourcePath("input", "./input")
  output$info_page <- renderUI({
    tags$iframe(
      seamless="seamless", src="input/iTRAQI_info.html", 
      style='width:100vw;height:calc(100vh - 41px);'
    )
  })
}
