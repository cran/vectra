## ----setup, include = FALSE---------------------------------------------------
has_sf <- requireNamespace("sf", quietly = TRUE)
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
b_sf <- collect_sf(buffered)
plot(st_geometry(nc), border = "grey80", col = NA,
     main = "County centroids buffered by 15 km")
plot(st_geometry(b_sf), border = "#3366cc", col = "#3366cc22", add = TRUE)

## ----map-area-----------------------------------------------------------------
areas <- tbl(f_poly) |>
  spatial_map(~ data.frame(NAME = .x$NAME,
                           area_km2 = as.numeric(st_area(.x)) / 1e6),
              crs = crs_nc)
head(collect(areas))

## ----sample-points------------------------------------------------------------
set.seed(1)
pts <- st_coordinates(st_sample(st_union(nc), 500))
fp <- tempfile(fileext = ".vtr")
write_vtr(data.frame(id = seq_len(nrow(pts)), x = pts[, 1], y = pts[, 2]), fp)

## ----filter-region------------------------------------------------------------
region <- nc[nc$NAME %in% c("Ashe", "Alleghany", "Surry", "Wilkes", "Watauga"),
             "NAME"]

inside <- tbl(fp) |>
  spatial_filter(region, coords = c("x", "y"), crs = crs_nc)
nrow(collect(inside))

## ----filter-plot--------------------------------------------------------------
keep_xy <- collect(inside)
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
c_sf <- collect_sf(clipped)
nrow(c_sf)

## ----clip-plot----------------------------------------------------------------
plot(st_geometry(nc), border = "grey85", col = NA,
     main = "Counties clipped to the region")
plot(st_geometry(c_sf), border = "#2a9d5c", col = "#2a9d5c33", add = TRUE)

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
m_sf <- collect_sf(merged)
m_sf

## ----dissolve-plot------------------------------------------------------------
plot(m_sf["band"], main = "Counties dissolved into two SIDS bands")

## ----overlay------------------------------------------------------------------
sq <- function(a, b) st_polygon(list(rbind(
  c(a, 0), c(b, 0), c(b, 1), c(a, 1), c(a, 0))))
polys <- st_sf(year = c(1990L, 2010L, 2000L),
               geometry = st_sfc(sq(0, 2), sq(1, 3), sq(1.5, 3.5)))

pieces <- collect_sf(spatial_overlay(polys))
nrow(pieces)
length(unique(pieces$piece_id))

## ----overlay-resolve----------------------------------------------------------
first <- spatial_overlay(polys) |>
  group_by(piece_id) |>
  slice_min(year, n = 1, with_ties = FALSE) |>
  collect_sf()
nrow(first)

plot(first["year"], main = "Overlay pieces, earliest year wins")

## ----roundtrip----------------------------------------------------------------
out <- tempfile(fileext = ".vtr")
tbl(fp) |>
  spatial_filter(region, coords = c("x", "y"), crs = crs_nc) |>
  write_vtr(out)
nrow(collect(tbl(out)))

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(f_poly, cent, fp, g_poly, fb, out))

