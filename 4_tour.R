tour_text <- list(
  "<h3>tour-1-title text</h3><br>This tour will show you around the iTRAQI map and highlight some of the main findings. Use the 'Next' and 'Back' buttons to go progress through the tour.",
  
  "<h3>tour-2-title text</h3><br>For many injuries time to treatment plays a large role on survival and recovery. Quick access to [[correct]] healthcare facilities can provide patients with a better [[chance]] of a full recovery. But some injuries also require extensive rehabilitation and access to these facilities can be hindered by location. Current methods in determining accessibility and remoteness use [[indexes]] like ARIA+ which use broad generalisations and does not specifically consider access to health care. ",
  
  "<h3>tour-3-title text</h3><br>It also has a population distribution throughout a number of large major cities as well as big rural and remote population base ranging from the Simpson desert on the western border to the isolated islands of the Torres Strait.",
  
  "<h3>tour-4-title text</h3><br>text 4",
  
  "<h3>tour-5-title text</h3><br>text 5"
)

n_tour_windows <- length(tour_text)

tab_ids <- list(
  "tab1" = c(),
  "tab2" = c("aria", "ariaLegend"),
  "tab3" = c("Towns"),
  "tab4" = c(),
  "tab5" = c(),
  "tab6" = c()
)

tab_legend_ids <- list(
  "tab1" = c(),
  "tab2" = c("ariaLegend"),
  "tab3" = c(),
  "tab4" = c(),
  "tab5" = c(),
  "tab6" = c()
)

tab_legends <- list(
  "ariaLegend" = function(x) {addLegendFactor(
    x,
    pal=palFac,
    values=unique(aria$ra_label),
    layerId="ariaLegend"
  )}
)

unique_legend_ids <- unique(unlist(tab_legend_ids))
unique_ids <- unique(unlist(tab_ids))
