df_locations <- read.csv("input/QLD_locations_with_RSQ_times_20220210.csv") %>%
  mutate(
    popup=paste0(
      "<b>Location: </b>", location, "<br>",
      "<b>Acute care destination: </b>", acute_care_centre, "<br>",
      "<b>Time to acute care (minutes): </b>", acute_time, "<br>",
      "<b>Rehab care destination: </b>", rehab_centre, "<br>",
      "<b>Time to rehab care (minutes): </b>", rehab_time, "<br>"
    ),
    popup_rehab=paste0(
      "<b>Location: </b>", location, "<br>",
      "<b>Silver rehab care destination: </b>", rehab_centre, "<br>",
      "<b>Time to silver rehab care (minutes): </b>", rehab_time, "<br>"
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
