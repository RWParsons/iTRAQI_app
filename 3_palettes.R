bins <- c(0, 30, 60, 120, 180, 240, 300, 360, 900, 1200)

palBin <- colorBin("YlOrRd", domain = min(bins):max(bins), bins=bins, na.color="transparent")

palNum1 <- colorNumeric(c(palBin(bins[1]), palBin(bins[2])), domain=0:30, na.color="transparent")
palNum2 <- colorNumeric(c(palBin(bins[2]), palBin(bins[3])), domain=30:60, na.color="transparent")
palNum3 <- colorNumeric(c(palBin(bins[3]), palBin(bins[4])), domain=60:120, na.color="transparent")
palNum4 <- colorNumeric(c(palBin(bins[4]), palBin(bins[5])), domain=120:180, na.color="transparent")
palNum5 <- colorNumeric(c(palBin(bins[5]), palBin(bins[6])), domain=180:240, na.color="transparent")
palNum6 <- colorNumeric(c(palBin(bins[6]), palBin(bins[7])), domain=240:300, na.color="transparent")
palNum7 <- colorNumeric(c(palBin(bins[7]), palBin(bins[8])), domain=300:360, na.color="transparent")
palNum8 <- colorNumeric(c(palBin(bins[8]), palBin(bins[9])), domain=360:900, na.color="transparent")
palNum9 <- colorNumeric(c(palBin(bins[9]), "#000000"), domain=900:1200, na.color="transparent")

palNum <- function(x){
  suppressWarnings(case_when(
    x < 30  ~ palNum1(x),
    x < 60  ~ palNum2(x),
    x < 120 ~ palNum3(x),
    x < 180 ~ palNum4(x),
    x < 240 ~ palNum5(x),
    x < 300 ~ palNum6(x),
    x < 360 ~ palNum7(x),
    x < 900 ~ palNum8(x),
    x <1200 ~ palNum9(x),
    x >=1200~ "#000000",
    TRUE ~ "transparent"
  ))
}

palNum_hours <- function(x){
  palNum(x*60)
}

palFac <- colorFactor("Greens", levels=ra_scale_to_text(0:4), ordered = TRUE, reverse=TRUE)

# select colours from http://derekogle.com/NCGraphing/resources/colors
iTRAQI_colour_ramp <- c("lightblue","mediumpurple1", "pink","red","maroon")
index_palette <- read.csv("input/index_palette.csv")

paliTRAQI <- colorFactor(
  # https://stackoverflow.com/questions/44269655/ggplot-rcolorbrewer-extend-and-apply-to-factor-data
  # colorRampPalette(iTRAQI_colour_ramp)(length(iTRAQI_bins)), 
  index_palette$hex2,
  levels=levels(iTRAQI_bins),
  ordered=FALSE
)

