## ----setup, include = FALSE---------------------------------------------------
has_sf <- requireNamespace("sf", quietly = TRUE) &&
  requireNamespace("s2", quietly = TRUE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = has_sf,
  fig.width = 7,
  fig.height = 4.2,
  out.width = "100%",
  dpi = 96
)

## ----libs, message = FALSE----------------------------------------------------
library(vectra)
library(sf)

## ----load-nc------------------------------------------------------------------
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc <- st_transform(nc, 32119)        # NAD83 / North Carolina, metres
crs_nc <- st_crs(nc)
nrow(nc)

## ----write-poly---------------------------------------------------------------
f_poly <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  NAME  = nc$NAME,
  BIR74 = nc$BIR74,
  SID74 = nc$SID74,
  geometry = st_as_binary(st_geometry(nc), hex = TRUE)
), f_poly)

## ----map-buffer---------------------------------------------------------------
cent <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  NAME = nc$NAME,
  geometry = st_as_binary(st_centroid(st_geometry(nc)), hex = TRUE)
), cent)

buffered <- tbl(cent) |>
  spatial_map(~ st_buffer(.x, 15000), crs = crs_nc)
buffered

## ----map-buffer-plot----------------------------------------------------------
b_sf <- collect(buffered)
plot(st_geometry(nc), border = "grey80", col = NA,
     main = "County centroids buffered by 15 km")
plot(st_geometry(b_sf), border = "#3366cc", col = "#3366cc22", add = TRUE)

## ----map-area-----------------------------------------------------------------
areas <- tbl(f_poly) |>
  spatial_map(~ data.frame(NAME = .x$NAME,
                           area_km2 = as.numeric(st_area(.x)) / 1e6),
              crs = crs_nc)
head(collect(areas))

## ----smooth-------------------------------------------------------------------
zig <- st_linestring(rbind(c(0, 0), c(1, 1), c(2, 0), c(3, 1), c(4, 0)))
f_zig <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = 1L, geometry = st_as_binary(st_sfc(zig), hex = TRUE)), f_zig)

tbl(f_zig) |> spatial_smooth(iterations = 3) |> collect()
unlink(f_zig)

## ----sample-points------------------------------------------------------------
set.seed(1)
pts <- st_coordinates(st_sample(st_union(nc), 500))
fp <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = seq_len(nrow(pts)), x = pts[, 1], y = pts[, 2]), fp)

## ----filter-region------------------------------------------------------------
region <- nc[nc$NAME %in% c("Ashe", "Alleghany", "Surry", "Wilkes", "Watauga"),
             "NAME"]

keep_xy <- tbl(fp) |>
  spatial_filter(region, coords = c("x", "y"), crs = crs_nc) |>
  collect()
nrow(keep_xy)

## ----filter-plot--------------------------------------------------------------
plot(st_geometry(nc), border = "grey85", col = NA, main = "Select by location")
plot(st_geometry(region), border = "#cc3344", col = "#cc334411", add = TRUE)
points(pts, pch = 16, cex = 0.5, col = "grey70")
points(keep_xy$x, keep_xy$y, pch = 16, cex = 0.6, col = "#cc3344")

## ----filter-distance----------------------------------------------------------
near <- tbl(fp) |>
  spatial_filter(region, predicate = st_is_within_distance,
                 coords = c("x", "y"), crs = crs_nc, dist = 30000)
nrow(collect(near))

## ----clip---------------------------------------------------------------------
mask_region <- st_union(st_geometry(region))

clipped <- tbl(f_poly) |> spatial_clip(mask_region, crs = crs_nc)
c_sf <- collect(clipped)
nrow(c_sf)

## ----clip-plot----------------------------------------------------------------
plot(st_geometry(nc), border = "grey85", col = NA,
     main = "Counties clipped to the region")
plot(st_geometry(c_sf), border = "#2a9d5c", col = "#2a9d5c33", add = TRUE)

## ----split--------------------------------------------------------------------
square <- st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0))))
blade  <- st_sfc(st_linestring(rbind(c(2, -1), c(2, 5))))

f_sq <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = 1L, geometry = st_as_binary(st_sfc(square), hex = TRUE)), f_sq)

tbl(f_sq) |> spatial_split(blade) |> collect()
unlink(f_sq)

## ----join-tag-----------------------------------------------------------------
tagged <- tbl(fp) |>
  spatial_join(nc["NAME"], coords = c("x", "y"), crs = crs_nc)
tdf <- collect(tagged)
head(tdf[, c("id", "NAME")])

## ----join-plot----------------------------------------------------------------
tagged_sf <- st_as_sf(tdf, coords = c("x", "y"), crs = crs_nc)
plot(st_geometry(nc), border = "grey85", col = NA,
     main = "Points tagged with their county")
plot(tagged_sf["NAME"], pch = 16, cex = 0.5, add = TRUE)

## ----join-count---------------------------------------------------------------
tbl(fp) |>
  spatial_join(nc["NAME"], coords = c("x", "y"), crs = crs_nc,
               keep_geom = FALSE) |>
  count(NAME) |>
  collect() |>
  head()

## ----join-nearest-------------------------------------------------------------
ncent <- st_sf(NAME = nc$NAME, geometry = st_centroid(st_geometry(nc)))
nearest <- tbl(fp) |>
  spatial_join(ncent, join = st_nearest_feature,
               coords = c("x", "y"), crs = crs_nc)
nrow(collect(nearest))

## ----join-partition-----------------------------------------------------------
g_poly <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  NAME = nc$NAME,
  geometry = st_as_binary(st_geometry(nc), hex = TRUE)
), g_poly)

tagged2 <- tbl(fp) |>
  spatial_join(tbl(g_poly), coords = c("x", "y"), crs = crs_nc,
               partition = grid(80000))
t2 <- collect(tagged2)
sum(!is.na(t2$NAME))

## ----join-equality------------------------------------------------------------
streamed <- collect(
  tbl(fp) |> spatial_join(nc["NAME"], coords = c("x", "y"), crs = crs_nc))

resident <- st_join(
  st_as_sf(collect(tbl(fp)), coords = c("x", "y"), crs = crs_nc, remove = FALSE),
  nc["NAME"], join = st_intersects)

all.equal(streamed$NAME[order(streamed$id)],
          resident$NAME[order(resident$id)])

## ----filter-scale-------------------------------------------------------------
set.seed(42)
bb <- st_bbox(nc)
n_big <- 2e5
big <- data.frame(id = seq_len(n_big),
                  x = runif(n_big, bb["xmin"], bb["xmax"]),
                  y = runif(n_big, bb["ymin"], bb["ymax"]))
fbig <- tempfile(fileext = ".vtr")
write_vtr(big, fbig)

kept <- tbl(fbig) |>
  spatial_filter(region, coords = c("x", "y"), crs = crs_nc) |>
  collect()
nrow(kept)

## ----cleanup-scale, include = FALSE-------------------------------------------
unlink(fbig)

## ----knn----------------------------------------------------------------------
towns <- suppressWarnings(st_centroid(st_geometry(nc)))[1:5]
towns <- st_sf(town = nc$NAME[1:5], geometry = towns)

set.seed(1)
pts <- suppressWarnings(st_coordinates(st_sample(nc, 100)))
f_pts <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = seq_len(nrow(pts)), x = pts[, 1], y = pts[, 2]), f_pts)

tbl(f_pts) |>
  spatial_knn(towns, k = 2, coords = c("x", "y"), crs = crs_nc, y_id = "town") |>
  collect() |> head()
unlink(f_pts)

## ----dissolve-----------------------------------------------------------------
nc$band <- ifelse(nc$SID74 > 5, "high", "low")
fb <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  band = nc$band, BIR74 = nc$BIR74,
  geometry = st_as_binary(st_geometry(nc), hex = TRUE)
), fb)

merged <- tbl(fb) |>
  spatial_dissolve(by = "band", crs = crs_nc,
                   .fun = list(births = function(d) sum(d$BIR74)))
m_sf <- collect(merged)
m_sf

## ----dissolve-plot------------------------------------------------------------
plot(m_sf["band"], main = "Counties dissolved into two SIDS bands")

## ----overlay------------------------------------------------------------------
sq <- function(a, b) st_polygon(list(rbind(
  c(a, 0), c(b, 0), c(b, 1), c(a, 1), c(a, 0))))
polys <- st_sf(year = c(1990L, 2010L, 2000L),
               geometry = st_sfc(sq(0, 2), sq(1, 3), sq(1.5, 3.5)))

pieces <- collect(spatial_overlay(polys))
nrow(pieces)
length(unique(pieces$piece_id))

## ----overlay-resolve----------------------------------------------------------
first <- spatial_overlay(polys) |>
  group_by(piece_id) |>
  slice_min(year, n = 1, with_ties = FALSE) |>
  collect()
nrow(first)

plot(first["year"], main = "Overlay pieces, earliest year wins")

## ----overlay-two--------------------------------------------------------------
zones <- st_sf(zone = c("A", "B"),
               geometry = st_sfc(sq(0, 1.5), sq(1.5, 3)))

inter <- spatial_overlay(polys, zones, how = "intersection") |> collect()
inter

## ----roundtrip----------------------------------------------------------------
out <- tempfile(fileext = ".vtr")
tbl(fp) |>
  spatial_filter(region, coords = c("x", "y"), crs = crs_nc) |>
  write_vtr(out)
nrow(collect(tbl(out)))

## ----gx-data------------------------------------------------------------------
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

f <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  NAME     = nc$NAME,
  BIR74    = nc$BIR74,
  geometry = st_as_binary(st_geometry(nc), hex = TRUE)
), f)

tbl(f)

## ----gx-measures--------------------------------------------------------------
tbl(f) |>
  mutate(area   = st_area(geometry),
         perim  = st_perimeter(geometry),
         nverts = st_npoints(geometry)) |>
  select(NAME, area, perim, nverts) |>
  collect() |>
  head()

## ----gx-coords----------------------------------------------------------------
tbl(f) |>
  mutate(centroid = st_centroid(geometry),
         cx = st_x(centroid),
         cy = st_y(centroid)) |>
  select(NAME, cx, cy) |>
  collect() |>
  head()

## ----gx-predicate-filter------------------------------------------------------
aoi <- st_as_sfc(st_bbox(c(xmin = -80, ymin = 35, xmax = -78, ymax = 36.5)),
                 crs = st_crs(nc))

tbl(f) |>
  filter(st_intersects(geometry, aoi)) |>
  collect() |>
  nrow()

## ----gx-predicate-mutate------------------------------------------------------
tbl(f) |>
  mutate(near_raleigh = st_intersects(geometry, aoi)) |>
  filter(near_raleigh) |>
  select(NAME) |>
  collect() |>
  head()

## ----gx-distance--------------------------------------------------------------
raleigh <- st_sfc(st_point(c(-78.64, 35.78)), crs = st_crs(nc))

tbl(f) |>
  mutate(centroid   = st_centroid(geometry),
         d_raleigh  = st_distance(centroid, raleigh)) |>
  select(NAME, d_raleigh) |>
  arrange(d_raleigh) |>
  collect() |>
  head()

## ----gx-summarise-------------------------------------------------------------
tbl(f) |>
  mutate(area = st_area(geometry)) |>
  summarise(total_area = sum(area), counties = n()) |>
  collect()

## ----gx-transforms------------------------------------------------------------
hulls <- tbl(f) |>
  mutate(geometry = st_convex_hull(geometry)) |>
  select(NAME, geometry) |>
  collect(geom = "geometry", crs = st_crs(nc))

hulls

## ----gx-buffer----------------------------------------------------------------
tbl(f) |>
  mutate(geometry = st_buffer(geometry, 0.1)) |>
  select(NAME, geometry) |>
  collect(geom = "geometry", crs = st_crs(nc)) |>
  st_area() |>
  head()

## ----gx-na--------------------------------------------------------------------
g <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = 1:4,
  geometry = c(st_as_binary(st_geometry(nc)[1:3], hex = TRUE), NA)
), g)

tbl(g) |>
  mutate(area = st_area(geometry)) |>
  collect()

## ----gx-cleanup, include = FALSE----------------------------------------------
unlink(c(f, g))

## ----ct-polygonize------------------------------------------------------------
grid <- st_sfc(
  st_linestring(rbind(c(0, 0), c(2, 0))),
  st_linestring(rbind(c(0, 1), c(2, 1))),
  st_linestring(rbind(c(0, 2), c(2, 2))),
  st_linestring(rbind(c(0, 0), c(0, 2))),
  st_linestring(rbind(c(1, 0), c(1, 2))),
  st_linestring(rbind(c(2, 0), c(2, 2))))

f <- tempfile(fileext = ".vtr")
write_vtr(data.frame(geometry = st_as_binary(grid, hex = TRUE)), f)

tbl(f) |> spatial_polygonize() |> collect()

## ----ct-line-merge------------------------------------------------------------
seg <- st_sfc(
  st_linestring(rbind(c(0, 0), c(1, 0))),
  st_linestring(rbind(c(1, 0), c(2, 0))),
  st_linestring(rbind(c(2, 0), c(3, 0))))

g <- tempfile(fileext = ".vtr")
write_vtr(data.frame(geometry = st_as_binary(seg, hex = TRUE)), g)

tbl(g) |> spatial_line_merge() |> collect()

## ----ct-snap-grid-------------------------------------------------------------
p <- st_polygon(list(rbind(c(0.04, 0.03), c(1.02, 0.01),
                           c(0.98, 1.03), c(0.01, 0.97), c(0.04, 0.03))))
h <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = 1L, geometry = st_as_binary(st_sfc(p), hex = TRUE)), h)

tbl(h) |> spatial_snap_grid(0.1) |> collect()

## ----ct-snap------------------------------------------------------------------
ref  <- st_sfc(st_linestring(rbind(c(0, 0), c(10, 0))))
line <- st_linestring(rbind(c(0, 0.2), c(5, 0.1), c(10, 0.2)))

sn <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = 1L, geometry = st_as_binary(st_sfc(line), hex = TRUE)), sn)

tbl(sn) |> spatial_snap(ref, tolerance = 0.5) |> collect()

## ----ct-eliminate-------------------------------------------------------------
big    <- st_polygon(list(rbind(
  c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))))
sliver <- st_polygon(list(rbind(
  c(10, 0), c(10.3, 0), c(10.3, 10), c(10, 10), c(10, 0))))

e <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = c("keep", "sliver"),
  geometry = st_as_binary(st_sfc(big, sliver), hex = TRUE)), e)

tbl(e) |> spatial_eliminate(max_area = 5) |> collect()

## ----ct-simplify--------------------------------------------------------------
p1 <- st_polygon(list(rbind(
  c(0, 0), c(1, 0), c(1, 0.5), c(1, 1), c(0, 1), c(0, 0))))
p2 <- st_polygon(list(rbind(
  c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0.5), c(1, 0))))

s <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = c("a", "b"),
  geometry = st_as_binary(st_sfc(p1, p2), hex = TRUE)), s)

tbl(s) |> spatial_simplify(tolerance = 0.6) |> collect()

## ----ct-explode---------------------------------------------------------------
mp <- st_multipolygon(list(
  list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))),
  list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2)))))

m <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = 1L, geometry = st_as_binary(st_sfc(mp), hex = TRUE)), m)

tbl(m) |> spatial_explode(part = "part_id") |> collect()

## ----ct-topology--------------------------------------------------------------
q1 <- st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
q2 <- st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))

tp <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  id = c("a", "b"),
  geometry = st_as_binary(st_sfc(q1, q2), hex = TRUE)), tp)

tbl(tp) |> spatial_topology(id = "id") |> collect()

## ----ct-centerline------------------------------------------------------------
strip <- st_polygon(list(rbind(
  c(0, 0), c(10, 0), c(10, 2), c(0, 2), c(0, 0))))

cl <- tempfile(fileext = ".vtr")
write_vtr(data.frame(geometry = st_as_binary(st_sfc(strip), hex = TRUE)), cl)

tbl(cl) |> spatial_centerline(density = 0.25, prune = 0.5) |> collect()

## ----ct-construct-hull--------------------------------------------------------
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc$band <- nc$SID74 > 5

n <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  band = nc$band,
  geometry = st_as_binary(st_geometry(nc), hex = TRUE)), n)

tbl(n) |>
  spatial_construct("convex_hull", by = "band", crs = st_crs(nc)) |>
  collect()

## ----ct-construct-voronoi-----------------------------------------------------
xy <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0.4, 0.6))
cloud <- st_sfc(lapply(seq_len(nrow(xy)), function(i) st_point(xy[i, ])))

v <- tempfile(fileext = ".vtr")
write_vtr(data.frame(geometry = st_as_binary(cloud, hex = TRUE)), v)

tbl(v) |> spatial_construct("voronoi") |> collect()

## ----ct-locate----------------------------------------------------------------
roads <- st_sf(road = c("main", "side"), geometry = st_sfc(
  st_linestring(rbind(c(0, 0), c(10, 0))),
  st_linestring(rbind(c(0, 5), c(0, 15)))))

pts <- data.frame(id = 1:2, x = c(3, 1), y = c(1, 9))
l <- tempfile(fileext = ".vtr")
write_vtr(pts, l)

tbl(l) |>
  spatial_locate(roads, coords = c("x", "y"), y_id = "road") |>
  collect()

## ----ct-cleanup, include = FALSE----------------------------------------------
unlink(c(f, g, h, sn, e, s, m, tp, cl, n, v, l))

## ----nw-build-----------------------------------------------------------------
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

## ----nw-route-cost------------------------------------------------------------
f <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = 1L, x = 0, y = 0), f)

dest <- st_sfc(st_point(c(3, 2)))

tbl(f) |>
  spatial_route(net, to = dest, coords = c("x", "y"), geometry = FALSE) |>
  collect()

## ----nw-route-geom------------------------------------------------------------
tbl(f) |>
  spatial_route(net, to = dest, coords = c("x", "y")) |>
  collect()

## ----nw-od-matrix-------------------------------------------------------------
g <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = 1:2, x = c(0, 3), y = c(0, 0)), g)

dests <- st_sf(
  name = c("top_left", "top_right"),
  geometry = st_sfc(st_point(c(0, 2)), st_point(c(3, 2))))

tbl(g) |>
  spatial_route(net, to = dests, to_id = "name", geometry = FALSE,
                coords = c("x", "y")) |>
  collect()

## ----nw-service-nodes---------------------------------------------------------
tbl(f) |>
  spatial_service_area(net, cost = c(1, 2), output = "nodes",
                       coords = c("x", "y")) |>
  collect()

## ----nw-service-polygon-------------------------------------------------------
tbl(f) |>
  spatial_service_area(net, cost = 2, output = "polygon",
                       coords = c("x", "y")) |>
  collect() |>
  st_area()

## ----nw-weighted--------------------------------------------------------------
streets_df <- st_sf(
  cost = runif(length(streets), 1, 3),
  geometry = streets)

wnet <- spatial_network(streets_df, weight = "cost")

tbl(f) |>
  spatial_route(wnet, to = dest, coords = c("x", "y"), geometry = FALSE) |>
  collect()

## ----nw-cleanup, include = FALSE----------------------------------------------
unlink(c(f, g))

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(f_poly, cent, fp, g_poly, fb, out))

