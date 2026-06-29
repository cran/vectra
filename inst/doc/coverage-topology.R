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

## ----polygonize---------------------------------------------------------------
grid <- st_sfc(
  st_linestring(rbind(c(0, 0), c(2, 0))),
  st_linestring(rbind(c(0, 1), c(2, 1))),
  st_linestring(rbind(c(0, 2), c(2, 2))),
  st_linestring(rbind(c(0, 0), c(0, 2))),
  st_linestring(rbind(c(1, 0), c(1, 2))),
  st_linestring(rbind(c(2, 0), c(2, 2))))

f <- tempfile(fileext = ".vtr")
write_vtr(data.frame(geometry = st_as_binary(grid, hex = TRUE)), f)

tbl(f) |> spatial_polygonize() |> collect_sf()

## ----line-merge---------------------------------------------------------------
seg <- st_sfc(
  st_linestring(rbind(c(0, 0), c(1, 0))),
  st_linestring(rbind(c(1, 0), c(2, 0))),
  st_linestring(rbind(c(2, 0), c(3, 0))))

g <- tempfile(fileext = ".vtr")
write_vtr(data.frame(geometry = st_as_binary(seg, hex = TRUE)), g)

tbl(g) |> spatial_line_merge() |> collect_sf()

## ----snap-grid----------------------------------------------------------------
p <- st_polygon(list(rbind(c(0.04, 0.03), c(1.02, 0.01),
                           c(0.98, 1.03), c(0.01, 0.97), c(0.04, 0.03))))
h <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = 1L, geometry = st_as_binary(st_sfc(p), hex = TRUE)), h)

tbl(h) |> spatial_snap_grid(0.1) |> collect_sf()

## ----snap---------------------------------------------------------------------
ref  <- st_sfc(st_linestring(rbind(c(0, 0), c(10, 0))))
line <- st_linestring(rbind(c(0, 0.2), c(5, 0.1), c(10, 0.2)))

sn <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = 1L, geometry = st_as_binary(st_sfc(line), hex = TRUE)), sn)

tbl(sn) |> spatial_snap(ref, tolerance = 0.5) |> collect_sf()

## ----eliminate----------------------------------------------------------------
big    <- st_polygon(list(rbind(
  c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))))
sliver <- st_polygon(list(rbind(
  c(10, 0), c(10.3, 0), c(10.3, 10), c(10, 10), c(10, 0))))

e <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = c("keep", "sliver"),
  geometry = st_as_binary(st_sfc(big, sliver), hex = TRUE)), e)

tbl(e) |> spatial_eliminate(max_area = 5) |> collect_sf()

## ----simplify-----------------------------------------------------------------
p1 <- st_polygon(list(rbind(
  c(0, 0), c(1, 0), c(1, 0.5), c(1, 1), c(0, 1), c(0, 0))))
p2 <- st_polygon(list(rbind(
  c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0.5), c(1, 0))))

s <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = c("a", "b"),
  geometry = st_as_binary(st_sfc(p1, p2), hex = TRUE)), s)

tbl(s) |> spatial_simplify(tolerance = 0.6) |> collect_sf()

## ----explode------------------------------------------------------------------
mp <- st_multipolygon(list(
  list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))),
  list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2)))))

m <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = 1L, geometry = st_as_binary(st_sfc(mp), hex = TRUE)), m)

tbl(m) |> spatial_explode(part = "part_id") |> collect_sf()

## ----topology-----------------------------------------------------------------
q1 <- st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
q2 <- st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))

tp <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = c("a", "b"),
  geometry = st_as_binary(st_sfc(q1, q2), hex = TRUE)), tp)

tbl(tp) |> spatial_topology(id = "id") |> collect()

## ----centerline---------------------------------------------------------------
strip <- st_polygon(list(rbind(
  c(0, 0), c(10, 0), c(10, 2), c(0, 2), c(0, 0))))

cl <- tempfile(fileext = ".vtr")
write_vtr(data.frame(geometry = st_as_binary(st_sfc(strip), hex = TRUE)), cl)

tbl(cl) |> spatial_centerline(density = 0.25, prune = 0.5) |> collect_sf()

## ----construct-hull-----------------------------------------------------------
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc$band <- nc$SID74 > 5

n <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  band = nc$band,
  geometry = st_as_binary(st_geometry(nc), hex = TRUE)), n)

tbl(n) |>
  spatial_construct("convex_hull", by = "band", crs = st_crs(nc)) |>
  collect_sf()

## ----construct-voronoi--------------------------------------------------------
xy <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0.4, 0.6))
cloud <- st_sfc(lapply(seq_len(nrow(xy)), function(i) st_point(xy[i, ])))

v <- tempfile(fileext = ".vtr")
write_vtr(data.frame(geometry = st_as_binary(cloud, hex = TRUE)), v)

tbl(v) |> spatial_construct("voronoi") |> collect_sf()

## ----locate-------------------------------------------------------------------
roads <- st_sf(road = c("main", "side"), geometry = st_sfc(
  st_linestring(rbind(c(0, 0), c(10, 0))),
  st_linestring(rbind(c(0, 5), c(0, 15)))))

pts <- data.frame(id = 1:2, x = c(3, 1), y = c(1, 9))
l <- tempfile(fileext = ".vtr")
write_vtr(pts, l)

tbl(l) |>
  spatial_locate(roads, coords = c("x", "y"), y_id = "road") |>
  collect()

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(f, g, h, sn, e, s, m, tp, cl, n, v, l))

