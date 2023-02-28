layers_dir <- "input/layers"

tours_panel_dims <- list(
  width = 330, height = 640
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

if (!"raster_points.rds" %in% list.files(layers_dir)) {
  rasters_points <- list()

  group_names_to_load <- names(layer_input)
  raster_layers <- grep("raster", layer_input)
  raster_layers <- group_names_to_load[raster_layers]

  rasters_points <- list()
  for (group_name in raster_layers) {
    new_layer <- readRDS(file.path(layers_dir, glue::glue("{layer_input[group_name]}.rds")))
    rasters_points <- c(rasters_points, list(raster::rasterToPoints(new_layer)))
  }

  names(rasters_points) <- layer_input[raster_layers]
  rasters_points <- rasters_points

  saveRDS(rasters_points, file = file.path(layers_dir, "raster_points.rds"))
} else {
  rasters_points <- readRDS(file.path(layers_dir, "raster_points.rds"))
}

all_base_layers <- c("Towns", "Acute centres", "Rehab centres", "Aeromedical bases", "QAS response locations")
default_base_layers <- c("Towns", "Acute centres", "Rehab centres")

# tier_icons <- iconList(
#   "Platinum" = makeIcon(iconUrl = "platinum.png", iconWidth = 549 / 18, iconHeight = 562 / 18),
#   "Gold" = makeIcon(iconUrl = "gold_medal.png", iconWidth = 529 / 18, iconHeight = 625 / 18),
#   "Future Gold" = makeIcon(iconUrl = "gold_medal.png", iconWidth = 529 / 18, iconHeight = 625 / 18),
#   "Silver" = makeIcon(iconUrl = "silver_medal.png", iconWidth = 303 / 18, iconHeight = 518 / 18)
# )

centre_icons <- iconList(
  acute = makeIcon(iconUrl = "acute_care.png", iconWidth = 50, iconHeight = 50),
  rehab = makeIcon(iconUrl = "rehab_care.png", iconWidth = 40, iconHeight = 40),
  rsq = makeIcon(iconUrl = "rsq.png", iconWidth = 50, iconHeight = 30),
  qas = makeIcon(iconUrl = "red-cross.png", iconWidth = 10, iconHeight = 10)
)

download_data_dir <- "input/download_data/"

download_data_files <- list(
  SA1_2011 = "iTRAQI - ASGS 2011 SA1.xlsx",
  SA2_2011 = "iTRAQI - ASGS 2011 SA2.xlsx",
  SA1_2016 = "iTRAQI - ASGS 2016 SA1.xlsx",
  SA2_2016 = "iTRAQI - ASGS 2016 SA2.xlsx",
  SA1_2021 = "iTRAQI - ASGS 2021 SA1.xlsx",
  SA2_2021 = "iTRAQI - ASGS 2021 SA2.xlsx"
)


dropdown_width <- "100%"

citation <- "iTRAQI: injury Treatment & Rehabilitation Accessibility Queensland Index version 1.3"

itraqi_categories_table <- paste(
  sep = "<br>",
  "<h4>Acute care travel time</h4><table style='width:100%'>
      <tr>
          <th style='width:30%'>Cat</th>
          <th style='width:50%'>Travel-time</th>
      </tr>
      <tr>
          <td>1</td>
          <td><1hr</td>
      </tr>
      <tr>
          <td>2</td>
          <td>1-2</td>
      </tr>
      <tr>
          <td>3</td>
          <td>2-4</td>
      </tr>
      <tr>
          <td>4</td>
          <td>4-6</td>
      </tr>
      <tr>
          <td>5</td>
          <td>6+</td>
      </tr>

  </table>",
  "<h4>Averaged Rehabilitation driving time (initial + subsequent)</h4><table style='width:100%'>
      <tr>
          <th style='width:30%'>Cat</th>
          <th style='width:50%'>Travel-time</th>
      </tr>
      <tr>
          <td>A</td>
          <td><1hr</td>
      </tr>
      <tr>
          <td>B</td>
          <td>1-2</td>
      </tr>
      <tr>
          <td>C</td>
          <td>2-4</td>
      </tr>
      <tr>
          <td>D</td>
          <td>4-6</td>
      </tr>
      <tr>
          <td>E</td>
          <td>6+</td>
      </tr>

  </table>"
)
