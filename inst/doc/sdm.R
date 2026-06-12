## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(1)
has_biglm <- requireNamespace("biglm", quietly = TRUE)
library(vectra)

## ----reproject, eval = FALSE--------------------------------------------------
# # Occurrences recorded in lon/lat (EPSG:4326), raster in a projected CRS.
# target <- tiff_crs("predictors.tif")$epsg          # e.g. 3035 (LAEA Europe)
# 
# pts <- sf::st_as_sf(occ, coords = c("lon", "lat"), crs = 4326)
# pts <- sf::st_transform(pts, target)
# xy  <- sf::st_coordinates(pts)
# 
# env <- tiff_extract_points("predictors.tif", x = xy[, 1], y = xy[, 2])

## ----raster-------------------------------------------------------------------
nx <- 80; ny <- 50
xmin <- 5; xmax <- 17; ymin <- 45; ymax <- 55
xres <- (xmax - xmin) / nx
yres <- (ymax - ymin) / ny

g <- expand.grid(col = 0:(nx - 1), row = 0:(ny - 1))
g$x <- xmin + (g$col + 0.5) * xres          # pixel centres
g$y <- ymax - (g$row + 0.5) * yres          # row 1 is the northern edge
g$band1 <- 14 - 0.6 * (g$y - 50) + 0.2 * (g$x - 11) + rnorm(nrow(g), 0, 0.3)
g$band2 <- 750 + 25 * (g$y - 50) - 8 * (g$x - 11) + rnorm(nrow(g), 0, 10)

tif <- tempfile(fileext = ".tif")
write_tiff(g[, c("x", "y", "band1", "band2")], tif, crs = 4326L)

tiff_crs(tif)

## ----extract------------------------------------------------------------------
N <- 1500
sites <- data.frame(
  x = runif(N, xmin + 0.3, xmax - 0.3),
  y = runif(N, ymin + 0.3, ymax - 0.3)
)

env <- tiff_extract_points(tif, sites)
head(env)

## ----outcome------------------------------------------------------------------
occ <- data.frame(
  bio1  = as.numeric(scale(env$band1)),   # standardized once, then stored
  bio12 = as.numeric(scale(env$band2))
)
eta <- -0.2 + 1.3 * occ$bio1 - 0.9 * occ$bio12
occ$presence <- rbinom(N, 1, plogis(eta))
table(occ$presence)

## ----glm----------------------------------------------------------------------
fit <- glm(presence ~ bio1 + bio12, data = occ, family = binomial())
round(coef(fit), 3)

## ----write-vtr----------------------------------------------------------------
vtr <- tempfile(fileext = ".vtr")
write_vtr(occ, vtr)

## ----collect-chunked----------------------------------------------------------
acc <- collect_chunked(
  tbl(vtr),
  function(acc, chunk) {
    p <- chunk$presence == 1
    acc$n      <- acc$n      + nrow(chunk)
    acc$n_pres <- acc$n_pres + sum(p)
    acc$sum1   <- acc$sum1   + tapply(chunk$bio1, p, sum)[c("FALSE", "TRUE")]
    acc
  },
  .init = list(n = 0L, n_pres = 0L, sum1 = c("FALSE" = 0, "TRUE" = 0))
)
acc$n_pres / acc$n                      # prevalence

## ----bigglm, eval = has_biglm-------------------------------------------------
src <- function() tbl(vtr) |> select(presence, bio1, bio12)

fit_stream <- biglm::bigglm(
  presence ~ bio1 + bio12,
  data = chunk_feeder(src),
  family = binomial()
)
round(coef(fit_stream), 3)

## ----bigglm-note, echo = FALSE, results = "asis", eval = !has_biglm-----------
# cat("> The out-of-core GLM example needs the **biglm** package, which is not",
#     "installed, so it was not run here. Install it with",
#     "`install.packages(\"biglm\")` to reproduce the streamed fit.\n")

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(tif, vtr))

