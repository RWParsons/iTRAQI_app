library(leaflet)
library(dplyr)

function(input, output, session) {
  rvs <- reactiveValues(to_load=NULL, map=NULL, to_load_rehab=NULL, map_rehab=NULL, map_complete=FALSE, map_rehab_complete=FALSE)
  
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
      ) %>% 
      addLayersControl(
        position = "topright",
        overlayGroups = c("Towns", "Acute centres", "Rehab centres"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Base layers</label>');
        }
    ")
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
          options=leafletOptions(pane="layers"),
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
        options = layersControlOptions(collapsed = FALSE))%>% 
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Layers</label>');
        }
    ")
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
          x=raster::raster(new_layer, layer=1),
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
