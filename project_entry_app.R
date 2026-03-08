library(tidyverse)
library(shiny)
library(DT)
library(shinyWidgets)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(shinyvalidate)
library(glue)
library(sf)
library(here)

# read in layers that will be nice for reference on the map
# when entering data

# define the geopackage so it doesn't have to be typed out
# for every layer read

habitat.gpkg <- "data-raw/idfg_habitat_mapping.gpkg"

drainages.sf <- st_read(habitat.gpkg, layer = "drainages")

lakes.sf <- st_read(habitat.gpkg, layer = "lakes")

states.sf <- st_read(habitat.gpkg, layer = "states")

idaho.sf <- states.sf %>%
  filter(name == "Idaho")

regions.sf <- st_read(habitat.gpkg, layer = "idfg_regions")

projects.sf <- st_read(habitat.gpkg, layer = "projects")

idaho_huc8.sf <- readRDS("data-raw/huc8")

# make base leaflet map

leaflet_base <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Roads") %>%
  setView(lng = -114.27979, lat = 45.02695, zoom = 6) %>%
  addMouseCoordinates() %>%
  addResetMapButton() |>
  addLayersControl(
    baseGroups = c("Topographic", "Imagery", "Roads"),
    overlayGroups = c("States"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  )

# make UI

ui <- fluidPage(
  titlePanel("New Habitat Project"),
  sidebarLayout(
    sidebarPanel(
      textInput("project_name", "Project name"),
      textInput("project_id", "IDFG Project Tracking Number"),
      radioButtons(
        "coord_mode",
        "Coordinate entry method",
        choices = c(
          "Click on map" = "map",
          "Manual entry" = "manual"
        ),
        selected = "map"
      ),
      conditionalPanel(
        condition = "input.coord_mode == 'manual'",
        numericInput("manual_lat", "Latitude", value = NA, min = -90, max = 90, step = 0.000001),
        numericInput("manual_lng", "Longitude", value = NA, min = -180, max = 180, step = 0.000001),
        actionButton("use_manual_coords", "Use manual coordinates")
      ),
      tags$hr(),
      strong("Selected coordinates"),
      verbatimTextOutput("coord_text"),
      tags$br(),
      actionButton("clear_point", "Clear point"),
      actionButton("submit_project", "Create project", class = "btn-primary"),
      tags$hr(),
      verbatimTextOutput("status_text")
    ),
    mainPanel(
      leafletOutput("project_map", height = 650)
    )
  )
)

server <- function(input, output, session) {
  selected_point <- reactiveVal(NULL)


  output$project_map <- renderLeaflet({
    leaflet_base
  })


  observeEvent(input$project_map_click, {
    req(input$coord_mode == "map")

    click <- input$project_map_click

    selected_point(
      tibble(
        latitude = click$lat,
        longitude = click$lng
      )
    )
  })

  observeEvent(input$use_manual_coords, {
    req(input$coord_mode == "manual")
    req(!is.na(input$manual_lat), !is.na(input$manual_lng))

    validate(
      need(input$manual_lat >= -90 && input$manual_lat <= 90, "Latitude must be between -90 and 90."),
      need(input$manual_lng >= -180 && input$manual_lng <= 180, "Longitude must be between -180 and 180.")
    )

    selected_point(
      tibble(
        latitude = input$manual_lat,
        longitude = input$manual_lng
      )
    )
  })

  observeEvent(input$clear_point, {
    selected_point(NULL)
  })

  selected_huc8 <- reactive({
    req(selected_point())

    pt <- selected_point() |>
      st_as_sf(
        coords = c("longitude", "latitude"),
        crs = st_crs(idaho_huc8.sf)
      )

    pt.join <- idaho_huc8.sf |>
      filter(lengths(st_intersects(geometry, pt)) > 0)

    req(nrow(pt.join) > 0)

    pt.join$huc8[1]
  })

  local_flowlines <- reactive({
    req(selected_huc8())

    path <- file.path("data-raw/flowlines_huc8", paste0(selected_huc8(), ".parquet"))

    req(file.exists(path))

    read_parquet(path) |>
      st_as_sf()
  })

  observe({
    leafletProxy("project_map") |> clearMarkers()

    pt <- selected_point()

    if (!is.null(pt)) {
      leafletProxy("project_map") |>
        clearGroup("local_flowlines") |>
        addPolylines(
          data = local_flowlines(),
          group = "local_flowlines",
          label = ~gnis_name
        ) |>
        addMarkers(
          lng = pt$longitude,
          lat = pt$latitude,
          popup = glue(
            "<b>{input$project_name %||% 'New Project'}</b><br/>
             Lat: {round(pt$latitude, 6)}<br/>
             Lon: {round(pt$longitude, 6)}"
          )
        )
    }
  })

  output$coord_text <- renderText({
    pt <- selected_point()

    if (is.null(pt)) {
      return("No point selected yet.")
    }

    glue(
      "Latitude: {round(pt$latitude, 6)}\nLongitude: {round(pt$longitude, 6)}"
    )
  })

  observeEvent(input$submit_project, {
    pt <- selected_point()

    validate(
      need(nzchar(trimws(input$project_name)), "Enter a project name."),
      need(!is.null(pt), "Select project coordinates.")
    )

    new_project <- tibble(
      project_name = trimws(input$project_name),
      project_id = trimws(input$project_id),
      latitude = pt$latitude,
      longitude = pt$longitude,
      created_at = Sys.time()
    )

    # Replace this with your real DB write
    print(new_project)

    output$status_text <- renderText({
      glue(
        "Project ready to write:\n
         Name: {new_project$project_name}\n
         Project ID: {new_project$project_id}\n
         Latitude: {round(new_project$latitude, 6)}\n
         Longitude: {round(new_project$longitude, 6)}"
      )
    })
  })
}

# helper for NULL-safe text
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") y else x
}

shinyApp(ui, server)
