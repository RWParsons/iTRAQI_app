separator <- "<br>"
img_width <- tours_panel_dims$width * 0.9

tour_01 <- paste(
  sep = separator,
  "<h3>Welcome to iTRAQI: injury Treatment & Rehabilitation Accessibility Queensland Index</h3>",
  "This pilot study uses moderate-to-severe traumatic brain injury (TBI) to map and rank access to acute treatment and rehabilitation units.",
  "Take this self-paced tour to explore and understand iTRAQI.",
  glue::glue('<br><img src="tour-1-tbi-image.jpg" alt="tbi-image" style="width:{img_width}px;">')
)


tour_02 <- paste(
  sep = separator,
  "<h3>Accessibility Indices</h3>",
  "The most common measure of remoteness used in Australia is ARIA+ (Accessibility and Remoteness Index of Australia) and variants. ARIA+ groups are shown on this map. While most of Queensland’s land area is remote or very remote, these do not specifically consider access to health care.",
  "For many injury types, timely access to treatment is a matter of life and death. For more severe injuries, such as TBI, access to rehabilitation is vital to regain function and improve quality of life."
)

tour_03 <- paste(
  sep = separator,
  "<h3>Queensland</h3>",
  "Since emergency services and hospitals are organised at the State level, our focus is on Queensland. Covering 1.7 million square kilometres, including very remote islands in the Torres Strait, moving seriously injured patients to the right hospital for time-sensitive emergency care is a challenge. In Queensland, we use helicopters, planes and road ambulances to transport patients quickly, with bases scattered throughout the State (see map).",
  glue::glue(
    '<div class="container">',
    '<img src="tour-3-plane.jfif" alt="plane-image" align="left" style="width:{(img_width-15)*(4/9)}px;">',
    '<img src="tour-3-ambulance.png" alt="ambulance-image" style="width:{(img_width-15)*(5/9)}px;">',
    "</div>"
  ),
  '<img src="rsq.png" width="50"/>         : Aeromedical bases (n=13)',
  '<img src="red-cross.png" width="50"/>   : Queensland Ambulance (n=302)'
)

tour_04 <- paste(
  sep = separator,
  "<h3>TBI Treatment</h3>",
  "Four hospitals have the specialised staff, equipment, and infrastructure to treat adults with moderate-to-severe TBI in Queensland. Only one of these is outside the South-East corner of the State. Accessing appropriate emergency care for TBI can therefore involve long distances.",
  glue::glue('<br><img src="tour-4-ambulance.png" alt="road-image" style="width:{img_width}px;">')
)

tour_05 <- paste(
  sep = separator,
  "<h3>TBI rehabilitation</h3>",
  "While the Brain Injury Rehabilitation Unit is housed at the Princess Alexandra Hospital (PAH), initial rehabilitation will usually commence at/near the hospital providing the specialised acute care. Patients may then be transferred to another appropriate in-patient facility closer to home to continue their rehabilitation. The green hospital markers <img src='rehab_care.png' width='25'/> show public hospital in-patient rehabilitation units where moderate-severe TBI patients usually undergo rehabilitation."
)

tour_06 <- paste(
  sep = separator,
  "<h3>Building iTRAQI &#8211; travel time to acute care</h3>",
  "The Queensland Ambulance Service and Retrieval Services Queensland provided idealised patient retrieval pathways and flight time details from 441 locations <img src='town_symbol.png' width='25'/> to the most appropriate TBI acute care destination <img src='acute_care.png' width='25'/>. Travel times were based on the most appropriate transport route using air and/or road, under ideal but realistic conditions."
)

tour_07 <- paste(
  sep = separator,
  "<h3>Building iTRAQI &#8211; visualising access to acute care</h3>",
  "These travel times were interpolated using ordinary kriging to provide coverage for all of Queensland.",
  
  "<br>CLICK on a location <img src='town_symbol.png' width='25'/> to reveal acute care travel time details.",
  "CLICK on a TBI acute care destination <img src='acute_care.png' width='25'/> for hospital details. "
)

tour_08 <- paste(
  sep = separator,
  "<h3>Building iTRAQI &#8211; visualising access to rehabilitation</h3>",
  "Patients commence their rehabilitation care at the centre where they received acute care (initial rehabilitation unit) and then may be transferred to a centre closer to where they live (subsequent rehabilitation unit).",
  "Rehabilitation time incorporates the driving time from each locality <img src='town_symbol.png' width='25'/> to the acute care centre where they received their initial care (Townsville/South-East QLD) <img src='acute_care.png' width='25'/> and the closest in-patient rehabilitation <img src='rehab_care.png' width='25'/> facility was calculated using road networks and off-peak driving conditions in ArcGIS Online. The average of the travel time to the initial and subsequent rehabilitation units (can be the same centre if the initial centre is closest) was used to represent the rehabilitation travel time for each locality. These travel times were then interpolated to provide coverage for all of Queensland.",
  
  "<br>CLICK on a rehabilitation destination for facility details."
)

tour_09 <- paste(
  sep = separator,
  "<h3>Building iTRAQI – aggregation into categories</h3>",
  "Travel time to acute and rehabilitation care was categorised and combined to form iTRAQI. iTRAQI can be displayed by small areas, using Statistical Areas (SA) level 1 (shown here) or Level 2.",
  itraqi_categories_table
)

info_icon <- "<i class=' glyphicon glyphicon-info-sign' role='presentation' aria-label=' icon'></i>"
downloads_icon <- "<i class=' glyphicon glyphicon-download-alt' role='presentation' aria-label=' icon'></i>"

tour_10 <- paste(
  sep = separator,
  "<h3>Using iTRAQI website</h3>",
  glue::glue("Explore and discover how your location impacts access to time-critical injury care.
  <ul>
    <li>Filter by socio-economic status and remoteness when looking at areas 'Main map' tab.</li>
    <li>Find out how to cite the website and contact options '{info_icon} Information' tab.</li>
    <li>Get the data to use iTRAQI (or components) with other datasets from the '{downloads_icon} Downloads' tab.</li>
  </ul>")
)

tour_text <- list(
  tour_01,
  tour_02,
  tour_03,
  tour_04,
  tour_05,
  tour_06,
  tour_07,
  tour_08,
  tour_09,
  tour_10
)

n_tour_windows <- length(tour_text)

tab_ids <- list(
  "tab1" = c(),
  "tab2" = c("aria", "ariaLegend"),
  "tab3" = c("Towns", "Aeromedical bases", "QAS response locations"),
  "tab4" = c("Acute centres"),
  "tab5" = c("Rehab centres"),
  "tab6" = c("Acute centres", "Towns"),
  "tab7" = c("Acute centres", "Towns", "Acute time"),
  "tab8" = c("Rehab centres", "Towns", "Rehab time"),
  "tab9" = c("Towns", "index")
)

tab_legend_ids <- list(
  "tab1" = c(),
  "tab2" = c("ariaLegend"),
  "tab3" = c(),
  "tab4" = c(),
  "tab5" = c(),
  "tab6" = c(),
  "tab7" = c("timeLegend"),
  "tab8" = c("timeLegend"),
  "tab9" = c("indexLegend")
)

legend_position <- "topleft"
tab_legends <- list(
  "ariaLegend" = function(x) {
    addLegendFactor(
      x,
      position = legend_position,
      pal = palFac,
      values = unique(aria$ra_label),
      layerId = "ariaLegend"
    )
  },
  "indexLegend" = function(x) {
    addLegendFactor(
      x,
      opacity = 1,
      position = legend_position,
      pal = paliTRAQI,
      values = iTRAQI_bins,
      layerId = "indexLegend",
      title = htmltools::tagList(tags$div("iTRAQI index"), tags$br())
    )
  },
  "timeLegend" = function(x) {
    addLegendBin(
      x,
      opacity = 1,
      position = legend_position,
      pal = palBin,
      values = 0:900,
      layerId = "timeLegend",
      title = htmltools::tagList(tags$div("Time to care (minutes)"), tags$br())
    )
  }
)


unique_legend_ids <- unique(unlist(tab_legend_ids))
unique_ids <- unique(unlist(tab_ids))
