dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  # https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) {
      paste0("width: ", validateCssUnit(width), ";")
    },
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status, " dropdown-toggle"),
    type = "button",
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});"
    )
  )
}

seifa_scale_to_text <- function(x) {
  case_when(
    x == 1 ~ "Most disadvantaged",
    x == 2 ~ "Disadvantaged",
    x == 3 ~ "Middle socio-economic status",
    x == 4 ~ "Advantaged",
    x == 5 ~ "Most advantaged",
    is.na(x) ~ "NA"
  )
}

seifa_text_to_value <- function(x) {
  case_when(
    x == "Most disadvantaged" ~ 1,
    x == "Disadvantaged" ~ 2,
    x == "Middle socio-economic status" ~ 3,
    x == "Advantaged" ~ 4,
    x == "Most advantaged" ~ 5,
  )
}

ra_scale_to_text <- function(x) {
  case_when(
    x == 0 ~ "Major Cities of Australia",
    x == 1 ~ "Inner Regional Australia",
    x == 2 ~ "Outer Regional Australia",
    x == 3 ~ "Remote Australia",
    x == 4 ~ "Very Remote Australia",
    TRUE ~ "NA"
  )
}

ra_text_to_value <- function(x) {
  case_when(
    x == "Major Cities of Australia" ~ 0,
    x == "Inner Regional Australia" ~ 1,
    x == "Outer Regional Australia" ~ 2,
    x == "Remote Australia" ~ 3,
    x == "Very Remote Australia" ~ 4,
  )
}

get_nearest_pred <- function(lat, lng, r_points) {
  r_points %>%
    as.data.frame() %>%
    mutate(
      x_diff = abs(x - lng),
      y_diff = abs(y - lat),
      xy_diff = x_diff + y_diff
    ) %>%
    arrange(xy_diff) %>%
    pull(var1.pred) %>%
    first()
}

iTRAQI_acute_breaks <- c(-Inf, 1, 2, 4, 6, Inf)
iTRAQI_rehab_breaks <- c(-Inf, 1, 2, 4, 6, Inf)

get_iTRAQI_index <- function(acute_mins, rehab_mins) {
  acute_cat <- cut(acute_mins / 60, breaks = iTRAQI_acute_breaks)
  rehab_cat <- cut(rehab_mins / 60, breaks = iTRAQI_rehab_breaks)

  acute_label <- as.numeric(acute_cat)
  rehab_label <- LETTERS[rehab_cat]

  paste0(acute_label, rehab_label)
}


check_point_in_qld <- function(lat, lng) {
  point <- st_point(c(lng, lat))
  intersects <- st_intersects(point, qld_boarder)
  nrow(as.data.frame(intersects)) > 0
}

setShapeStyle <- function(map, data = getMapData(map), layerId,
                          stroke = NULL, color = NULL,
                          weight = NULL, opacity = NULL,
                          fill = NULL, fillColor = NULL,
                          fillOpacity = NULL, dashArray = NULL,
                          smoothFactor = NULL, noClip = NULL,
                          options = NULL) {
  options <- c(
    list(layerId = layerId),
    options,
    filterNULL(list(
      stroke = stroke, color = color,
      weight = weight, opacity = opacity,
      fill = fill, fillColor = fillColor,
      fillOpacity = fillOpacity, dashArray = dashArray,
      smoothFactor = smoothFactor, noClip = noClip
    ))
  )
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))

  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  # print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style)
}

setShapeLabel <- function(map, data = getMapData(map), layerId,
                          label = NULL,
                          options = NULL) {
  cat("in setShapeLabel", "\n")
  options <- c(
    list(layerId = layerId),
    options,
    filterNULL(list(label = label))
  )
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))

  layerId <- options[[1]]
  label <- options[-1] # drop layer column

  # typo fixed in this line
  leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
}
