library(tidyverse)
library(rnaturalearth)
library(sf)
library(nhdplusTools)
library(arrow)
library(sfarrow)
library(tigris)

options(tigris_use_cache = TRUE)

id_counties <- counties(state = "ID", class = "sf")

saveRDS(
  id_counties,
  "data-raw/idaho_counties"
)

states.sf <- ne_states(returnclass = "sf")

idaho.sf <- states.sf |>
  filter(name == "Idaho")

idaho_huc8.sf <- get_huc(
  AOI = idaho.sf,
  type = "huc08"
) |>
  mutate(huc6 = substr(huc8, 1, 6))

saveRDS(
  idaho_huc8.sf,
  "data-raw/huc8"
)

idaho_huc6.sf <- get_huc(
  AOI = idaho.sf,
  type = "huc06"
)

saveRDS(
  idaho_huc6.sf,
  "data-raw/huc6"
)


id_bbox <- st_bbox(idaho.sf)

idaho_flowlines.sf <- subset_nhdplus(
  bbox = id_bbox,
  nhdplus_data = "download",
  flowline_only = T,
  output_file = tempfile(fileext = ".gpkg"),
  return_data = T
)

id_flowlines.sf <- idaho_flowlines.sf$NHDFlowline_Network

flowlines_huc8 <- id_flowlines.sf |>
  st_transform(crs = st_crs(idaho_huc8.sf)) |>
  st_join(idaho_huc8.sf) |>
  select(comid,
    gnis_id = gnis_id.x,
    gnis_name,
    stream_order = streamorde,
    huc8
  ) |>
  filter(!is.na(huc8))

# write to parquet
flowlines_huc8 |>
  group_split(huc8) |>
  walk(function(x) {
    this_huc8 <- unique(x$huc8)

    st_write_parquet(
      x,
      glue::glue("data-raw/flowlines_huc8/{this_huc8}.parquet"),
      compression = "snappy"
    )
  })

test <- read_parquet("data-raw/flowlines_huc8/16010102.parquet") |>
  st_as_sf()

# try this with the IDFG stream layer instead

idfg_streams.sf <- st_read("data-raw/Hydrography_Public_3451083698128016512.geojson") %>%
  st_transform(crs = 4326) %>%
  st_zm()

leaflet_base |>
  addPolylines(data = idfg_streams.sf)

idfg_flowlines_huc8 <- idfg_streams.sf |>
  st_transform(crs = st_crs(idaho_huc8.sf)) |>
  st_join(idaho_huc8.sf) |>
  select(
    LLID, NAME, Drainage,
    huc8
  )

# write to parquet
idfg_flowlines_huc8 |>
  group_split(huc8) |>
  walk(function(x) {
    this_huc8 <- unique(x$huc8)

    st_write_parquet(
      x,
      glue::glue("data-raw/idfg_flowlines_huc8/{this_huc8}.parquet"),
      compression = "snappy"
    )
  })
