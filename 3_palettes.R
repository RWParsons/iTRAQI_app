bins_mins <- palette_list$bins_mins
palBin <- palette_list$palBin
palBin_hours <- palette_list$palBin_hours
palNum <- palette_list$palNum
palNum_hours <- palette_list$palNum_hours
paliTRAQI <- palette_list$paliTRAQI

palFac <- colorFactor("Greens", levels = ra_scale_to_text(0:4), ordered = TRUE, reverse = TRUE)

make_categories_table <- function(acute_breaks, rehab_breaks) {
  acute_table <- make_cat_and_label_table(
    labels = breaks_to_labels(palette_list$iTRAQI_acute_breaks),
    header_text = "Acute care travel time",
    cat_type = "number"
  )

  rehab_table <- make_cat_and_label_table(
    labels = breaks_to_labels(palette_list$iTRAQI_rehab_breaks),
    header_text = "Averaged Rehabilitation driving time (initial + subsequent)",
    cat_type = "letter"
  )

  paste0("<br>", acute_table, rehab_table)
}

make_cat_and_label_table <- function(labels, header_text, cat_type = c("number", "letter")) {
  cat_type <- match.arg(cat_type)
  rows <- tibble(labels = labels) |>
    mutate(idx = row_number()) |>
    (\(d) {
      if (cat_type == "letter") {
        d$idx <- LETTERS[d$idx]
      }
      d
    })() |>
    mutate(
      across(everything(), \(x) paste0("<td>", x, "</td>")),
      tr = paste0("<tr>", idx, labels, "</tr>")
    ) |>
    pull(tr)

  header <- "
      <tr>
          <th style='width:30%'>Cat</th>
          <th style='width:50%'>Travel-time (hours)</th>
      </tr>
    "
  header_label <- paste0(
    "<h4>",
    header_text,
    "</h4>"
  )

  glue::glue(
    "{header_label}",
    "<table style='width:100%'>",
    header,
    paste0(rows, collapse = ""),
    "</table>"
  )
}

breaks_to_labels <- function(x) {
  cut(1, breaks = x) |>
    levels() |>
    str_split(",") |>
    map(~ str_extract(.x, "[0-9]+")) |>
    map(\(.x) {
      if (is.na(.x[1])) {
        c <- paste0("<", .x[2])
      } else if (is.na(.x[2])) {
        c <- paste0(">", .x[1])
      } else {
        c <- paste0(.x[1], " – ", .x[2])
      }
      c
    }) |>
    unlist()
}

itraqi_categories_table <- make_categories_table(
  acute_breaks = palette_list$iTRAQI_acute_breaks,
  rehab_breaks = palette_list$iTRAQI_rehab_breaks
)
