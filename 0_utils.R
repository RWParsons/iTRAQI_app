layers_dir <- "input/layers"

tours_panel_dims <- list(
  width = 330, height = 600
)

map_bounds <- list(
  lng1 = 115, 
  lat1 = -45.00, 
  lng2 = 170, 
  lat2 = -5
)

qld_bounds <- list(
  lng1 = 137.725724, 
  lat1 = -28.903687, 
  lng2 = 151.677076, 
  lat2 = -10.772608
)

layer_input <- c(
  "Acute time" = "acute_raster",
  "Rehab time" = "rehab_raster",
  "SA1 acute time" = "acute_polygons_SA1_year2016_simplified",
  "SA2 acute time" = "acute_polygons_SA2_year2016_simplified",
  "SA1 rehab time" = "rehab_polygons_SA1_year2016_simplified",
  "SA2 rehab time" = "rehab_polygons_SA2_year2016_simplified"
)

if(!"raster_points.rds" %in% list.files(layers_dir)){
  rasters_points <- list()
  
  group_names_to_load <- names(layer_input)
  raster_layers <- grep("raster", layer_input)
  raster_layers <- group_names_to_load[raster_layers]
  
  rasters_points <- list()
  for(group_name in raster_layers){
    new_layer <- readRDS(file.path(layers_dir, glue::glue("{layer_input[group_name]}.rds")))
    rasters_points <- c(rasters_points, list(raster::rasterToPoints(new_layer)))
  }
  
  names(rasters_points) <- layer_input[raster_layers]
  rasters_points <- rasters_points
  
  saveRDS(rasters_points, file=file.path(layers_dir, "raster_points.rds"))
} else {
  rasters_points <- readRDS(file.path(layers_dir, "raster_points.rds"))
}

all_base_layers <- c("Towns", "Acute centres", "Rehab centres")

tier_icons <- iconList(
  "Platinum"=makeIcon(iconUrl = "input/imgs/platinum.png", iconWidth = 549/18, iconHeight = 562/18),
  "Gold"=makeIcon(iconUrl = "input/imgs/gold_medal.png", iconWidth = 529/18, iconHeight = 625/18),
  "Future Gold"=makeIcon(iconUrl = "input/imgs/gold_medal.png", iconWidth = 529/18, iconHeight = 625/18),
  "Silver"=makeIcon(iconUrl = "input/imgs/silver_medal.png", iconWidth = 303/18, iconHeight = 518/18)
)

centre_icons <- iconList(
  acute=makeIcon(iconUrl = "input/imgs/acute_care2.png", iconWidth = 50, iconHeight = 50),
  rehab=makeIcon(iconUrl = "input/imgs/rehab_care.png", iconWidth = 40, iconHeight = 40)
)

download_data_dir <- "input/download_data/"

download_data_files <- list(
  SA1_2011="iTRAQI - ASGS 2011 SA1.xlsx",
  SA2_2011="iTRAQI - ASGS 2011 SA2.xlsx",
  SA1_2016="iTRAQI - ASGS 2016 SA1.xlsx",
  SA2_2016="iTRAQI - ASGS 2016 SA2.xlsx",
  SA1_2021="iTRAQI - ASGS 2021 SA1.xlsx",
  SA2_2021="iTRAQI - ASGS 2021 SA2.xlsx"
)


dropdown_width <- "100%"

citation <- "iTRAQI: injury Treatment & Rehabilitation Accessibility Queensland Index version 1.2"
