seperator <- "<br>"
loader_img_width <- 500
loading_panel_displays <- c(
  paste(
    sep=seperator,
    "<h2>Humans are the Only Animals That Enjoy Spicy Foods (factoid)</h2>",
    glue::glue('<img src="tour-1-tbi-image.jpg" alt="tbi-image" style="width:{loader_img_width}px;">')
  ),
  paste(
    sep=seperator,
    "<h2>There were 1,024,242 Triple Zero calls in the 2020-21 FY</h2>",
    glue::glue('<img src="tour-3-ambulance.webp" alt="ambulance-image" style="width:{loader_img_width}px;">')
  ),
  paste(
    sep=seperator,
    '<h2>"kriging - it\'s a dance move right?!?" (Someone, 2022)</h2>',
    glue::glue('<img src="kriging.gif" alt="kriging-gif" style="width:{loader_img_width}px;">')
  ),
  paste(
    sep=seperator,
    '<h2>Retrieval Services Queensland (RSQ) get 67 referrals every day</h2>',
    glue::glue('<img src="rsq-helicopter.jpg" alt="rsq-helicopter" style="width:{loader_img_width}px;">')
  ),
  paste(
    sep=seperator,
    '<h2>Falls are the leading cause of the traumatic brain injury in children and older adults</h2>',
    glue::glue('<img src="fall.gif" alt="fall" style="width:{loader_img_width}px;">')
  ),
  paste(
    sep=seperator,
    '<h2>Queensland Ambulance Service attended to 233 incidents per 1,000 population in the 2021-2020 FY</h2>',
    glue::glue('<img src="ambulance.jfif" alt="ambulance" style="width:{loader_img_width}px;">')
  ),
  paste(
    sep=seperator,
    '<h2>The iTRAQI index combines travel to both acute and rehab care but you can also see these travel times separately by selecting a specific layer on the control panel</h2>',
    glue::glue('<img src="iTRAQI-hex.png" alt="iTRAQI-hex" style="width:{loader_img_width/3}px;">')
  )
  
  
)

sample_display <- function() {
  sample(1:length(loading_panel_displays), size=1)
}
