palette_list <- readRDS("input/palette_list.rds")

df_locations <- read.csv("input/QLD_locations_with_RSQ_times.csv") %>%
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
  readRDS("input/layers/stacked_SA1_and_SA2_polygons.rds") %>%
  mutate(
    index = get_iTRAQI_index(acute_mins = value_acute, rehab_mins = value_rehab),
    rehab_time_str = str_extract(popup_rehab, "<b>Time to.*$"),
    popup_index =
      paste0(popup_acute, rehab_time_str, "<b>iTRAQI Index: </b>", index, "<br>")
  ) %>%
  filter(!is.na(value_acute))

iTRAQI_bins <- palette_list$bins_index

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
