bins <- c(0, 30, 60, 120, 180, 240, 300, 360, 900)
palBin <- colorBin("YlOrRd", domain = 0:900, bins=bins, na.color="transparent")

palNum1 <- colorNumeric(c("#FFFFCC", "#FFEDA0"), domain=0:30, na.color="transparent")
palNum2 <- colorNumeric(c("#FFEDA0", "#FED976"), domain=30:60, na.color="transparent")
palNum3 <- colorNumeric(c("#FED976", "#FEB24C"), domain=60:120, na.color="transparent")
palNum4 <- colorNumeric(c("#FEB24C", "#FD8D3C"), domain=120:180, na.color="transparent")
palNum5 <- colorNumeric(c("#FD8D3C", "#FC4E2A"), domain=180:240, na.color="transparent")
palNum6 <- colorNumeric(c("#FC4E2A", "#E31A1C"), domain=240:300, na.color="transparent")
palNum7 <- colorNumeric(c("#E31A1C", "#B10026"), domain=300:360, na.color="transparent")
palNum8 <- colorNumeric(c("#B10026", "#000000"), domain=360:900, na.color="transparent")

palNum <- function(x){
  case_when(
    x < 30 ~ palNum1(x),
    x < 60 ~ palNum2(x),
    x < 120 ~ palNum3(x),
    x < 180 ~ palNum4(x),
    x < 240 ~ palNum5(x),
    x < 300 ~ palNum6(x),
    x < 360 ~ palNum7(x),
    x < 900 ~ palNum8(x),
    x >= 900 ~ "#000000",
    TRUE ~ "transparent"
  )
}

palNum_hours <- function(x){
  palNum(x*60)
}