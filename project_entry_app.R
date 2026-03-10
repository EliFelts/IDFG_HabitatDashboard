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
library(scales)
library(bslib)
library(arrow)

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

# make a vector of species that may be benefitted

species_vector <- c(
  "Brown Trout", "Bull Trout", "Chinook salmon",
  "Mountain Whitefish", "Rainbow Trout",
  "Westslope Cutthroat Trout",
  "Yellowstone Cutthroat Trout"
)

# make UI
library(shiny)
library(bslib)
library(leaflet)
library(shinyWidgets)
ui <- page_sidebar(
  title = "New Habitat Project",
  sidebar = sidebar(
    width = 900,
    open = "open",
    div(
      style = "height: calc(100vh - 80px); overflow-y: auto; padding-right: 10px;",
      h4("Project Information"),
      layout_columns(
        col_widths = c(6, 6),
        textInput("project_name", "Project name"),
        textInput("project_id", "IDFG Project Tracking Number"),
        textInput(
          "project_objective",
          "Project Objectives"
        ),
        selectInput(
          "project_agency",
          "Managing agency/organization",
          choices = c("IDFG", "DOGE"),
          selected = "IDFG"
        ),
        autonumericInput(
          "amt_awarded",
          "Amount Awarded",
          value = NULL,
          digitGroupSeparator = ",",
          currencySymbol = "$",
          currencySymbolPlacement = "p"
        ),
        airDatepickerInput(
          "project_startdate",
          "Project Start Date",
          value = NULL,
          clearButton = TRUE
        )
      ),
      textAreaInput(
        "project_description",
        "Project description",
        rows = 8,
        width = "100%"
      ),
      pickerInput(
        "idfg_staff",
        "IDFG Staff associated with project",
        choices = c(
          "Robert Hand",
          "Brian Knoth"
        ),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE
        ),
        width = "100%"
      ),
      tags$hr(),
      h4("Location"),
      layout_columns(
        col_widths = c(6, 6),
        radioButtons(
          "coord_mode",
          "Coordinate entry method",
          choices = c(
            "Click on map" = "map",
            "Manual entry" = "manual"
          ),
          selected = "map"
        ),
        radioButtons(
          "map_mode",
          "Map mode",
          choices = c(
            "Set project point" = "point",
            "Select primary stream" = "stream"
          ),
          selected = "point"
        )
      ),
      conditionalPanel(
        condition = "input.coord_mode == 'manual'",
        layout_columns(
          col_widths = c(6, 6),
          numericInput(
            "manual_lat",
            "Latitude",
            value = NA,
            min = -90,
            max = 90,
            step = 0.000001
          ),
          numericInput(
            "manual_lng",
            "Longitude",
            value = NA,
            min = -180,
            max = 180,
            step = 0.000001
          )
        ),
        actionButton("use_manual_coords", "Use manual coordinates")
      ),
      strong("Selected coordinates"),
      verbatimTextOutput("coord_text"),
      tags$br(),
      actionButton("clear_point", "Clear point"),
      tags$hr(),
      h4("Biological Benefits"),
      layout_columns(
        col_widths = c(6, 6),
        pickerInput(
          "primary_species_benefitted",
          "Primary Species Benefitted",
          choices = species_vector,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE
          ),
          width = "100%"
        ),
        pickerInput(
          "secondary_species_benefitted",
          "Secondary Species Benefitted",
          choices = species_vector,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE
          ),
          width = "100%"
        ),
        pickerInput(
          "lifestages_benefitted",
          "Life stage(s) benefitted",
          choices = c(
            "Spawning", "Rearing",
            "Migration", "Overwintering"
          ),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE
          ),
          width = "100%"
        ),
        pickerInput(
          "habtypes_improved",
          "Habitat type(s) improved",
          choices = c(
            "Mainstem River", "Tributary",
            "Floodplain", "Wetland",
            "Riparian", "Spring/groundwater"
          ),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE
          ),
          width = "100%"
        )
      ),
      tags$hr(),
      h4("Land Ownership"),
      pickerInput(
        "ownership",
        "Land Ownership",
        choices = c(
          "Private", "State",
          "Federal", "Tribal"
        ),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE
        ),
        width = "100%"
      ),
      tags$hr(),
      actionButton("submit_project", "Create project", class = "btn-primary"),
      tags$hr(),
      verbatimTextOutput("status_text")
    )
  ),
  card(
    full_screen = TRUE,
    height = "calc(100vh - 80px)",
    card_body(
      padding = 0,
      leafletOutput("project_map", height = "100%")
    )
  )
)

server <- function(input, output, session) {
  selected_point <- reactiveVal(NULL)


  output$project_map <- renderLeaflet({
    leaflet_base
  })


  observeEvent(input$project_map_click, {
    req(input$map_mode == "point")
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
    req(input$map_mode == "point")
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
          layerId = ~comid,
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

  selected_flowline_id <- reactiveVal(NULL)

  observeEvent(input$project_map_shape_click, {
    req(input$map_mode == "stream")
    click <- input$project_map_shape_click
    req(click$id)

    selected_flowline_id(click$id)
  })

  selected_flowline <- reactive({
    req(local_flowlines(), selected_flowline_id())

    local_flowlines() |>
      filter(comid == selected_flowline_id())
  })

  observe({
    req(selected_flowline())

    leafletProxy("project_map") |>
      clearGroup("selected_flowline") |>
      addPolylines(
        data = selected_flowline(),
        group = "selected_flowline",
        label = ~ str_c(gnis_name),
        weight = 5,
        opacity = 1,
        color = "red"
      )
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
