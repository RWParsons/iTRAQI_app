# this script isn't sourced by the app. It is run only occasionally so that the
# inputs to the app can be updated from the analyses project.

download_list <- list(
  palette = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/palette_list.rds",
    dest = "input/palette_list.rds"
  ),
  raster_points = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/raster_points.rds",
    dest = "input/layers/raster_points.rds"
  ),
  stacked_polygons = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/stacked_SA1_and_SA2_polygons.rds",
    dest = "input/layers/stacked_SA1_and_SA2_polygons.rds"
  ),
  qld_locations = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/QLD_locations_with_RSQ_times.csv",
    dest = "input/QLD_locations_with_RSQ_times.csv"
  ),
  qas_locations = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/data/inputs-for-visualisations/qas_locations.csv",
    dest = "input/qas_locations.csv"
  ),
  rsq_locations = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/data/inputs-for-visualisations/rsq_locations.csv",
    dest = "input/rsq_locations.csv"
  ),
  dl_2011_sa1 = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/download-data/iTRAQI%20-%20ASGS%202011%20SA1.xlsx",
    dest = "input/download_data/iTRAQI - ASGS 2011 SA1.xlsx"
  ),
  dl_2011_sa2 = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/download-data/iTRAQI%20-%20ASGS%202011%20SA2.xlsx",
    dest = "input/download_data/iTRAQI - ASGS 2011 SA2.xlsx"
  ),
  dl_2016_sa1 = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/download-data/iTRAQI%20-%20ASGS%202016%20SA1.xlsx",
    dest = "input/download_data/iTRAQI - ASGS 2016 SA1.xlsx"
  ),
  dl_2016_sa2 = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/download-data/iTRAQI%20-%20ASGS%202016%20SA2.xlsx",
    dest = "input/download_data/iTRAQI - ASGS 2016 SA2.xlsx"
  ),
  dl_2021_sa1 = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/download-data/iTRAQI%20-%20ASGS%202021%20SA1.xlsx",
    dest = "input/download_data/iTRAQI - ASGS 2021 SA1.xlsx"
  ),
  dl_2021_sa2 = list(
    gh = "https://raw.githubusercontent.com/RWParsons/iTRAQI-analyses/main/output/download-data/iTRAQI%20-%20ASGS%202021%20SA2.xlsx",
    dest = "input/download_data/iTRAQI - ASGS 2021 SA2.xlsx"
  )
)


for (obj_name in names(download_list)) {
  download.file(
    url = download_list[[obj_name]]$gh,
    destfile = download_list[[obj_name]]$dest
  )
}
