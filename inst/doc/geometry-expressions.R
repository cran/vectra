## ----setup, include = FALSE---------------------------------------------------
has_sf <- requireNamespace("sf", quietly = TRUE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = has_sf
)

## ----libs, message = FALSE----------------------------------------------------
library(vectra)
library(sf)

## ----data---------------------------------------------------------------------
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

f <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  NAME     = nc$NAME,
  BIR74    = nc$BIR74,
  geometry = st_as_binary(st_geometry(nc), hex = TRUE)
), f)

tbl(f)

## ----measures-----------------------------------------------------------------
tbl(f) |>
  mutate(area   = st_area(geometry),
         perim  = st_perimeter(geometry),
         nverts = st_npoints(geometry)) |>
  select(NAME, area, perim, nverts) |>
  collect() |>
  head()

## ----coords-------------------------------------------------------------------
tbl(f) |>
  mutate(centroid = st_centroid(geometry),
         cx = st_x(centroid),
         cy = st_y(centroid)) |>
  select(NAME, cx, cy) |>
  collect() |>
  head()

## ----predicate-filter---------------------------------------------------------
aoi <- st_as_sfc(st_bbox(c(xmin = -80, ymin = 35, xmax = -78, ymax = 36.5)),
                 crs = st_crs(nc))

tbl(f) |>
  filter(st_intersects(geometry, aoi)) |>
  collect() |>
  nrow()

## ----predicate-mutate---------------------------------------------------------
tbl(f) |>
  mutate(near_raleigh = st_intersects(geometry, aoi)) |>
  filter(near_raleigh) |>
  select(NAME) |>
  collect() |>
  head()

## ----distance-----------------------------------------------------------------
raleigh <- st_sfc(st_point(c(-78.64, 35.78)), crs = st_crs(nc))

tbl(f) |>
  mutate(centroid   = st_centroid(geometry),
         d_raleigh  = st_distance(centroid, raleigh)) |>
  select(NAME, d_raleigh) |>
  arrange(d_raleigh) |>
  collect() |>
  head()

## ----summarise----------------------------------------------------------------
tbl(f) |>
  mutate(area = st_area(geometry)) |>
  summarise(total_area = sum(area), counties = n()) |>
  collect()

## ----transforms---------------------------------------------------------------
hulls <- tbl(f) |>
  mutate(geometry = st_convex_hull(geometry)) |>
  select(NAME, geometry) |>
  collect_sf(crs = st_crs(nc))

hulls

## ----buffer-------------------------------------------------------------------
tbl(f) |>
  mutate(geometry = st_buffer(geometry, 0.1)) |>
  select(NAME, geometry) |>
  collect_sf(crs = st_crs(nc)) |>
  st_area() |>
  head()

## ----na-----------------------------------------------------------------------
g <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = 1:4,
  geometry = c(st_as_binary(st_geometry(nc)[1:3], hex = TRUE), NA)
), g)

tbl(g) |>
  mutate(area = st_area(geometry)) |>
  collect()

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(f, g))

