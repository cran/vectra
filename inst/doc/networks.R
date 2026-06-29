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

## ----build--------------------------------------------------------------------
mk <- function(x1, y1, x2, y2)
  st_linestring(rbind(c(x1, y1), c(x2, y2)))

streets <- st_sfc(
  mk(0, 0, 1, 0), mk(1, 0, 2, 0), mk(2, 0, 3, 0),   # bottom row
  mk(0, 1, 1, 1), mk(1, 1, 2, 1), mk(2, 1, 3, 1),   # middle row
  mk(0, 2, 1, 2), mk(1, 2, 2, 2), mk(2, 2, 3, 2),   # top row
  mk(0, 0, 0, 1), mk(0, 1, 0, 2),                   # left verticals
  mk(1, 0, 1, 1), mk(1, 1, 1, 2),
  mk(2, 0, 2, 1), mk(2, 1, 2, 2),
  mk(3, 0, 3, 1), mk(3, 1, 3, 2))                   # right verticals

net <- spatial_network(streets)
net

## ----route-cost---------------------------------------------------------------
f <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = 1L, x = 0, y = 0), f)

dest <- st_sfc(st_point(c(3, 2)))

tbl(f) |>
  spatial_route(net, to = dest, coords = c("x", "y"), geometry = FALSE) |>
  collect()

## ----route-geom---------------------------------------------------------------
tbl(f) |>
  spatial_route(net, to = dest, coords = c("x", "y")) |>
  collect_sf()

## ----od-matrix----------------------------------------------------------------
g <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = 1:2, x = c(0, 3), y = c(0, 0)), g)

dests <- st_sf(
  name = c("top_left", "top_right"),
  geometry = st_sfc(st_point(c(0, 2)), st_point(c(3, 2))))

tbl(g) |>
  spatial_route(net, to = dests, to_id = "name", geometry = FALSE,
                coords = c("x", "y")) |>
  collect()

## ----service-nodes------------------------------------------------------------
tbl(f) |>
  spatial_service_area(net, cost = c(1, 2), output = "nodes",
                       coords = c("x", "y")) |>
  collect_sf()

## ----service-polygon----------------------------------------------------------
tbl(f) |>
  spatial_service_area(net, cost = 2, output = "polygon",
                       coords = c("x", "y")) |>
  collect_sf() |>
  st_area()

## ----weighted-----------------------------------------------------------------
streets_df <- st_sf(
  cost = runif(length(streets), 1, 3),
  geometry = streets)

wnet <- spatial_network(streets_df, weight = "cost")

tbl(f) |>
  spatial_route(wnet, to = dest, coords = c("x", "y"), geometry = FALSE) |>
  collect()

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(f, g))

