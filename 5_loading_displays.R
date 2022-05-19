seperator <- "<br>"
loader_img_width <- 500
loading_panel_displays <- c(
  paste(
    sep=seperator,
    "<h2>Humans are the Only Animals That Enjoy Spicy Foods (factoid)</h2>",
    glue::glue('<img src="input/imgs/tour_images/tour-1-tbi-image.jpg" alt="tbi-image" style="width:{loader_img_width}px;">')
  ),
  paste(
    sep=seperator,
    "<h2>There were 1,024,242 Triple Zero calls in the 2020-21 FY</h2>",
    glue::glue('<img src="input/imgs/tour_images/tour-3-ambulance.webp" alt="ambulance-image" style="width:{loader_img_width}px;">')
  ),
  paste(
    sep=seperator,
    '<h2>"kriging - it\'s a dance move right?!?" (Someone, 2022)</h2>',
    glue::glue('<img src="input/imgs/kriging.gif" alt="kriging-gif" style="width:{loader_img_width}px;">')
  )
)
