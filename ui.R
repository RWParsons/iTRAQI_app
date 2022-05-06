library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(shinyjs)
# https://susanna-cramb.shinyapps.io/itraqi_app/

source("0_utils.R")
source("1_functions.R")
source("2_load_data.R")
source("3_palettes.R")
source("4_tour.R")
# styles.css from https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example

navbarPage(
  "iTRAQI", id="nav",
  tabPanel(
    "Tour",
    useShinyjs(),
    div(class="outer",
        tags$head(
          includeCSS("styles.css")
        ),
        leafletOutput("map_tour", width="100%", height="100%"),
        absolutePanel(
          id = "tour_controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top=80, left = "auto", right = 10, bottom = "auto",
          width = tours_panel_dims$width, height = tours_panel_dims$height,
          tags$br(),
          splitLayout(
            cellWidths = 230,
            uiOutput("backButtonControl"),
            uiOutput("nextButtonControl")
          ),
          uiOutput("tourText")
        ),
        tags$div(
          id="cite",
          citation
        )
    )
  ),
  tabPanel("Main map",
    div(class="outer",
      tags$head(
        includeCSS("styles.css")
      ),
      leafletOutput("map", width="100%", height="100%"),
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 370, left = "auto", right = 10, bottom = "auto",
        width = 330, height = 600,
        h4("Layer"),
        radioButtons(
          inputId="layer_selection", label=NULL,
          choices=c("None", "Acute time", "Rehab time", "SA1 Index", "SA2 Index", "SA1 Acute", "SA2 Acute", "SA1 Rehab", "SA2 Rehab"),
          selected="None"
        ),
        h4("Markers"),
        checkboxGroupInput(
          "base_layers",label=NULL,
          choices=all_base_layers,
          selected=all_base_layers
        ),
        h4("Filters"),
        dropdownButton(
          label="Socioeconomic status", status="default", width=dropdown_width,
          checkboxGroupInput(
            inputId="seifa", label="SEIFA", width=dropdown_width,
            choices=c(seifa_scale_to_text(1:5), 'NA'),
            selected=c(seifa_scale_to_text(1:5), 'NA')
          )
        ),
        htmlOutput("seifa_included"),
        tags$br(),
        dropdownButton(
          label="Remoteness index", status="default", width=dropdown_width,
          checkboxGroupInput(
            inputId="remoteness", label="Remoteness", width=dropdown_width,
            choices=c(ra_scale_to_text(0:4)),
            selected=c(ra_scale_to_text(0:4))
          )
        ),
        htmlOutput("remoteness_included"),
        tags$br(),
        dropdownButton(
          label="iTRAQI index", status="default", width=dropdown_width,
          checkboxGroupInput(
            inputId="itraqi_index", label="iTRAQI Index", width=dropdown_width,
            choices=levels(iTRAQI_bins),
            selected=levels(iTRAQI_bins)
          )
        ),
        htmlOutput("itraqi_index_included")
      ),
      tags$div(
        id="cite",
        citation
      )
    )
  ),
  tabPanel("Rehab map",
    div(class="outer",
        tags$head(
          includeCSS("styles.css")
        ),
        leafletOutput("map_rehab", width="100%", height="100%"),
        absolutePanel(
          id = "rehab_controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 370, left = "auto", right = 10, bottom = "auto",
          width = 150, height = 280,
          h4("Layers"),
          tags$hr(),
          radioButtons(
            inputId="rehab_layer_selection", label=NULL,
            choices=c("None", names(rehab_tiers)),
            selected="None"
          ),
          tags$hr(),
          checkboxGroupInput(
            "rehab_markers_checkbox",label=NULL,
            choices=c("Towns", "Centres"),
            selected=c("Towns", "Centres")
          )
        ),
        tags$div(
          id="cite",
          citation
        )
    )
  ),
  
  tabPanel(
    title="Information",
    icon=icon("info-sign",lib='glyphicon'),
    div(class="outer", htmlOutput("info_page"))
  ),
  tabPanel(
    title="Downloads",
    icon=icon("download-alt", lib="glyphicon"),
    tags$h3("Download links for SA1 and SA2 aggregate time to care:"),
    tags$h4("2011 Australian Statistical Geography Standard (ASGS) Statistical Areas level 1 and 2 (SA1 and SA2):"),
    downloadBttn("download_SA1_2011", "Download (2011 SA1s)", style="pill", block=FALSE),
    downloadBttn("download_SA2_2011", "Download (2011 SA2s)", style="pill", block=FALSE),
    tags$br(),
    tags$br(),
    tags$h4("2016 ASGS SA1 and SA2:"),
    downloadBttn("download_SA1_2016", "Download (2016 SA1s)", style="pill", block=FALSE),
    downloadBttn("download_SA2_2016", "Download (2016 SA2s)", style="pill", block=FALSE),
    tags$br(),
    tags$br(),
    tags$h4("2021 ASGS SA1 and SA2:"),
    downloadBttn("download_SA1_2021", "Download (2021 SA1s)", style="pill", block=FALSE),
    downloadBttn("download_SA2_2021", "Download (2021 SA2s)", style="pill", block=FALSE)
    
  )
)
