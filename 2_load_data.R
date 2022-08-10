df_locations <- read.csv("input/QLD_locations_with_RSQ_times_20220718.csv") %>%
  mutate(
    iTRAQI_index = get_iTRAQI_index(acute_mins = acute_time, rehab_mins = rehab_time),
    popup = paste0(
      "<b>Location: </b>", location, "<br>",
      "<b>iTRAQI index: </b>", iTRAQI_index, "<br>",
      "<b>Acute care destination: </b>", acute_care_centre, ifelse(is.na(acute_care_transit_location), "", paste0(" (via ", acute_care_transit_location, ")")), "<br>",
      "<b>Time to acute care (minutes): </b>", acute_time, "<br>",
      "<b>Initial rehab care destination: </b>", gold_rehab_centre, "<br>",
      "<b>Step-down rehab care destination: </b>", silver_rehab_centre, "<br>",
      "<b>Average time to rehab care (minutes): </b>", round(rehab_time), "<br>"
    ),
    popup_rehab = paste0(
      "<b>Location: </b>", location, "<br>",
      "<b>Initial rehab care destination: </b>", gold_rehab_centre, "<br>",
      "<b>Step-down rehab care destination: </b>", silver_rehab_centre, "<br>",
      "<b>Average time to rehab care (minutes): </b>", round(rehab_time), "<br>"
    )
  )

df_rehab_map_locations <- read.csv("input/all_rehab_time.csv") %>%
  mutate(across(ends_with("time"), round)) %>%
  mutate(
    popup_none = paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Initial rehab care destination: </b>", gold_rehab_centre, "<br>",
      "<b>Step-down rehab care destination: </b>", silver_rehab_centre, "<br>",
      "<b>Average time to rehab care (minutes): </b>", round((silver_time + gold_time) / 2), "<br>"
    ),
    popup_silver = paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Silver rehab care destination: </b>", silver_rehab_centre, "<br>",
      "<b>Time to silver rehab care (minutes): </b>", silver_time, "<br>"
    ),
    popup_gold = paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Gold rehab care destination: </b>", gold_rehab_centre, "<br>",
      "<b>Time to gold rehab care (minutes): </b>", gold_time, "<br>"
    ),
    popup_future_gold = paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Future gold rehab care destination: </b>", future_gold_rehab_centre, "<br>",
      "<b>Time to future gold rehab care (minutes): </b>", future_gold_time, "<br>"
    ),
    popup_platinum = paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Platinum rehab care destination: </b>", platinum_rehab_centre, "<br>",
      "<b>Time to platinum rehab care (minutes): </b>", platinum_time, "<br>"
    )
  )

rehab_tiers <- list(
  "Platinum" = list(
    file = "platinum_rehab",
    centres = unique(df_rehab_map_locations$platinum_rehab_centre)
  ),
  "Gold" = list(
    file = "gold_rehab",
    centres = unique(df_rehab_map_locations$gold_rehab_centre)
  ),
  "Future Gold" = list(
    file = "future_gold_rehab",
    centres = unique(df_rehab_map_locations$future_gold_rehab_centre)
  ),
  "Silver" = list(
    file = "silver_rehab",
    centres = unique(df_rehab_map_locations$silver_rehab_centre)
  )
)

df_centres <- read.csv("input/centres.csv")
names(df_centres) <- c("centre_name", "care_type", "address", "x", "y")
acute_centre_names <- df_centres %>%
  filter(care_type == "acute") %>%
  pull(centre_name)
df_centres <- df_centres %>%
  filter(care_type == "acute") %>%
  mutate(care_type = "rehab") %>%
  rbind(., df_centres) %>%
  mutate(
    centre_name = str_trim(centre_name),
    popup = paste0(
      "<b>Centre name: </b>", centre_name, "<br>",
      "<b>Care type: </b>", ifelse(centre_name %in% acute_centre_names, "Acute & Rehabilitation care", "Rehabilitation care"), "<br>",
      "<b>Address: </b>", address, "<br>"
    )
  )

df_rsq_locations <- read.csv("input/rsq_locations.csv") %>%
  mutate(
    type = str_to_sentence(ifelse(type == "both", "plane and helicopter", type)),
    popup = glue::glue(
      "<b>Location: </b>", "{rsq_location}<br>",
      "<b>Service: </b>", "{type}"
    )
  )
df_qas_locations <- read.csv("input/qas_locations.csv") %>%
  rename("qas_location" = 1) %>%
  mutate(popup = glue::glue("<b>Location: </b>", "{qas_location}<br>"))

polygons <-
  readRDS("input/layers/stacked_SA1_and_SA2_polygons_year2016_simplified.rds") %>%
  mutate(
    index = get_iTRAQI_index(acute_mins = value_acute, rehab_mins = value_rehab),
    rehab_time_str = str_extract(popup_rehab, "<b>Time to.*$"),
    popup_index =
      paste0(popup_acute, rehab_time_str, "<b>iTRAQI Index: </b>", index, "<br>")
  ) %>%
  filter(!is.na(value_acute))

get_iTRAQI_bins <- function() {
  unique_rehab_levels <- cut(0, breaks = iTRAQI_rehab_breaks) %>% levels()
  unique_rehab_levels <- LETTERS[1:length(unique_rehab_levels)]

  unique_acute_levels <- cut(0, breaks = iTRAQI_acute_breaks) %>% levels()
  unique_acute_levels <- 1:length(unique_acute_levels)

  grid <- expand.grid(acute = unique_acute_levels, rehab = unique_rehab_levels)
  grid$iTRAQI_index <- paste0(grid$acute, grid$rehab)
  grid <- grid[grid$iTRAQI_index %in% unique(polygons$index), ]
  as.factor(grid$iTRAQI_index)
}

iTRAQI_bins <- get_iTRAQI_bins()

groupings <- expand.grid(
  seifa = c(1:5, NA),
  ra = 0:4,
  sa = 1:2,
  index = levels(iTRAQI_bins)
)

groupings$group_id <- as.character(1:nrow(groupings))

polygons <-
  left_join(
    polygons, groupings,
    by = c(
      "ra",
      "seifa_quintile" = "seifa", "SA_level" = "sa", "index"
    )
  )

rmarkdown::render(input = "input/iTRAQI_info.md", output_file = "../www/iTRAQI_info.html")

aria <-
  polygons %>%
  filter(SA_level == 1) %>%
  mutate(ra_label = factor(ra_scale_to_text(ra), levels = ra_scale_to_text(0:4)))

index_poly <-
  polygons %>%
  filter(SA_level == 2)

tours_polygons <-
  polygons %>%
  filter(SA_level == 1) %>%
  mutate(ra_label = factor(ra_scale_to_text(ra), levels = ra_scale_to_text(0:4)))

qld_boarder <-
  # https://data.gov.au/dataset/ds-dga-2dbbec1a-99a2-4ee5-8806-53bc41d038a7/distribution/dist-dga-4c9bcadb-0361-4d79-a6f9-17b470ef9641/details?q=
  st_read("input/qld_state_polygon_shp/QLD_STATE_POLYGON_shp.shp")
