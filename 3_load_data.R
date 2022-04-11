df_locations <- read.csv("input/QLD_locations_with_RSQ_times_20220411.csv") %>%
  mutate(
    popup=paste0(
      "<b>Location: </b>", location, "<br>",
      "<b>Acute care destination: </b>", acute_care_centre, "<br>",
      "<b>Time to acute care (minutes): </b>", acute_time, "<br>",
      "<b>Initial rehab care destination: </b>", gold_rehab_centre, "<br>",
      "<b>Step-down rehab care destination: </b>", silver_rehab_centre, "<br>",
      "<b>Average time to rehab care (minutes): </b>", round(rehab_time), "<br>"
    ),
    popup_rehab=paste0(
      "<b>Location: </b>", location, "<br>",
      "<b>Initial rehab care destination: </b>", gold_rehab_centre, "<br>",
      "<b>Step-down rehab care destination: </b>", silver_rehab_centre, "<br>",
      "<b>Average time to rehab care (minutes): </b>", round(rehab_time), "<br>"
    )
  )


df_rehab_map_locations <- read.csv("input/all_rehab_time.csv") %>%
  mutate(across(ends_with("time"), round)) %>%
  mutate(
    popup_none=paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Initial rehab care destination: </b>", gold_rehab_centre, "<br>",
      "<b>Step-down rehab care destination: </b>", silver_rehab_centre, "<br>",
      "<b>Average time to rehab care (minutes): </b>", round((silver_time + gold_time)/2), "<br>"
    ),
    popup_silver=paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Silver rehab care destination: </b>", silver_rehab_centre, "<br>",
      "<b>Time to silver rehab care (minutes): </b>", silver_time, "<br>"
    ),
    popup_gold=paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Gold rehab care destination: </b>", gold_rehab_centre, "<br>",
      "<b>Time to gold rehab care (minutes): </b>", gold_time, "<br>"
    ),
    popup_future_gold=paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Future gold rehab care destination: </b>", future_gold_rehab_centre, "<br>",
      "<b>Time to future gold rehab care (minutes): </b>", future_gold_time, "<br>"
    ),
    popup_platinum=paste0(
      "<b>Location: </b>", town_name, "<br>",
      "<b>Platinum rehab care destination: </b>", platinum_rehab_centre, "<br>",
      "<b>Time to platinum rehab care (minutes): </b>", platinum_time, "<br>"
    )
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

groupings <- expand.grid(
  seifa=c(1:5, NA),
  ra=0:4,
  sa=1:2,
  care_type=c("acute", "rehab")
)
groupings$group_id <- as.character(1:nrow(groupings))

polygons <- 
  readRDS("input/layers/vertical_stacked_SA1_and_SA2_polygons_year2016_simplified.rds") %>% 
  left_join(., groupings, by=c("ra", "seifa_quintile"="seifa", "SA_level"="sa", "care_type"))

rmarkdown::render("input/iTRAQI_info.md")

aria <- 
  polygons %>%
  filter(SA_level==1, care_type=="acute") %>%
  mutate(ra_label=factor(ra_scale_to_text(ra), levels=ra_scale_to_text(0:4)))
