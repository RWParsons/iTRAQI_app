layers_dir <- "input/layers"

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
    file = "platinum_rehab",
    centres = c("Brain Injury Rehabilitation Unit")
  ),
  "Gold" = list(
    file = "gold_rehab",
    centres = c(
      "Townsville University Hospital",
      "Brain Injury Rehabilitation Unit"
    )
  ),
  "Future Gold" = list(
    file = "future_gold_rehab",
    centres = c(
      "Sunshine Coast University Hospital",
      "Gold Coast University Hospital",
      "Townsville University Hospital",
      "Brain Injury Rehabilitation Unit"
    )
  ),
  "Silver" = list(
    file = "silver_rehab",
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

all_base_layers <- c("Towns", "Acute centres", "Rehab centres")

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
