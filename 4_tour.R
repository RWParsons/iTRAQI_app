separator <- "<br>"
img_width <- tours_panel_dims$width*0.9

tour_01 <- paste(
  sep=separator,
  "<h3>Welcome to iTRAQI: injury Treatment & Rehabilitation Accessibility Queensland Index</h3>",
  "This pilot study uses moderate-to-severe traumatic brain injury (TBI) to map and rank access to acute treatment and rehabilitation centres.",
  "Take this self-paced tour to explore and understand iTRAQI.",
  glue::glue('<br><img src="input/imgs/tour_images/tour-1-tbi-image.jpg" alt="tbi-image" style="width:{img_width}px;">')
)


tour_02 <- paste(
  sep=separator,
  "<h3>Accessibility Indices</h3>",
  "The most common measure of remoteness used in Australia is ARIA+ (Accessibility and Remoteness Index of Australia) and variants. While most of Queensland’s land area is remote or very remote, these do not specifically consider access to health care.",
  "For many injuries timely access to treatment is a matter of life and death. For more serious injuries, access to rehabilitation is vital to regain function and improve quality of life. "
)

tour_03 <- paste(
  sep=separator,
  "<h3>Queensland</h3>",
  "Since emergency and hospitals are State-based, our focus is on Queensland. Covering 1.7 million square kilometres, including very remote Torres Strait islands, moving patients efficiently for time-sensitive emergency care is a challenge.",
  glue::glue('<br><img src="input/imgs/tour_images/tour-3-plane.jfif" alt="plane-image" style="width:{img_width}px;">'),
  glue::glue('<br><img src="input/imgs/tour_images/tour-3-ambulance.webp" alt="ambulance-image" style="width:{img_width}px;">')
)

tour_04 <- paste(
  sep=separator,
  "<h3>TBI Treatment</h3>",
  "Only 4 adult hospitals have the facilities to treat moderate-to-severe TBI in Queensland. Only one is outside the South-East corner."
)

tour_05 <- paste(
  sep=separator,
  "<h3>TBI rehabilitation</h3>",
  "While PAH provides the highest level rehabilitation, initial rehabilitation will usually be given at Townsville or PAH. Following progress, the patient may be transferred to an in-patient facility closer to home that is less specialised. The Rehab map shows the driving time to different rehabilitation levels.",
  "(Platinum=PAH; Gold=Townsville/PAH, Silver=less specialised)"
)

tour_06 <- paste(
  sep=separator,
  "<h3>Building iTRAQI &#8211; acute care</h3>",
  "First, Queensland Ambulance Service and Retrieval Services Queensland provided details for the travel time from over 400 locations to the most appropriate TBI acute treatment centre. This was based on the most appropriate transfers using air and/or road, under ideal but realistic conditions."
)

tour_07 <- paste(
  sep=separator,
  "<h3>Building iTRAQI &#8211; acute care</h3>",
  "These travel times were interpolated using ordinary kriging to cover all of Queensland."
)

tour_08 <- paste(
  sep=separator,
  "<h3>Building iTRAQI &#8211; rehabilitation</h3>",
  "Driving time was calculated from each of the 441 localities using road networks and off-peak driving conditions in ArcGIS Online to each of the [xxx] rehabilitation facilities, and interpolated to cover all of Queensland. An average of the sum of gold (Townsville/Brisbane) and silver (less specialised facilities)."
)

tour_09 <- paste(
  sep=separator,
  "<h3>iTRAQI</h3>",
  "Acute and rehabilitation travel time was categorised to form iTRAQI by small areas (statistical areas level 1 and 2):",
  "<h4>Acute care travel time</h4><table style='width:100%'>
      <tr>
          <th style='width:30%'>Cat</th>
          <th style='width:50%'>Travel-time</th>
      </tr>
      <tr>
          <td>0</td>
          <td><1hr</td>
      </tr>
      <tr>
          <td>1</td>
          <td>1-2</td>
      </tr>
      <tr>
          <td>2</td>
          <td>2-3</td>
      </tr>
      <tr>
          <td>3</td>
          <td>3-4</td>
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
  "<h4>Averaged Rehabilitation driving time (gold + silver)</h4><table style='width:100%'>
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

info_icon <- "<i class=' glyphicon glyphicon-info-sign' role='presentation' aria-label=' icon'></i>"
downloads_icon <- "<i class=' glyphicon glyphicon-download-alt' role='presentation' aria-label=' icon'></i>"

tour_10 <- paste(
  sep=separator,
  "<h3>Using iTRAQI website</h3>",
  glue::glue("Explore and discover the impact of location in Queensland for injured patients.
  <ul>
    <li>Filter by SES and remoteness when looking at areas 'Main map' tab.</li>
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
  "tab3" = c("Towns"),
  "tab4" = c("Acute centres"),
  "tab5" = c("Rehab centres"),
  "tab6" = c("Acute centres", "Towns"),
  "tab7" = c("Acute centres", "Towns", "Acute time"),
  "tab8" = c("Rehab centres", "Towns", "Rehab time")
)

tab_legend_ids <- list(
  "tab1" = c(),
  "tab2" = c("ariaLegend"),
  "tab3" = c(),
  "tab4" = c(),
  "tab5" = c(),
  "tab6" = c(),
  "tab7" = c("timeLegend"),
  "tab8" = c("timeLegend")
)

legend_position <- "topleft"
tab_legends <- list(
  "ariaLegend" = function(x) {
    addLegendFactor(
      x,
      position=legend_position,
      pal=palFac,
      values=unique(aria$ra_label),
      layerId="ariaLegend"
    )},
  "indexLegend" = function(x) {
    addLegendFactor(
      x,
      opacity=1,
      position=legend_position,
      pal=paliTRAQI,
      values=iTRAQI_bins,
      layerId="indexLegend",
      title=htmltools::tagList(tags$div("iTRAQI index"), tags$br())
    )
  },
  "timeLegend" = function(x) {
    addLegendBin(
      x,
      opacity=1,
      position=legend_position,
      pal=palBin,
      values=0:900,
      layerId="timeLegend",
      title=htmltools::tagList(tags$div("Time to care (minutes)"), tags$br())
    )
  }
)


unique_legend_ids <- unique(unlist(tab_legend_ids))
unique_ids <- unique(unlist(tab_ids))
