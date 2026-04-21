## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data-setup---------------------------------------------------------------
library(vectra)

# Fact table: field observations
obs_path <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  obs_id  = 1:12,
  sp_id   = c(1, 2, 3, 1, 2, 4, 3, 1, 5, 2, 3, 1),
  site_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
  count   = c(5, 12, 3, 8, 15, 2, 7, 20, 1, 9, 4, 11),
  dbh_cm  = c(35, 22, 48, 31, 19, 55, 42, 28, 12, 25, 39, 33)
), obs_path)

# Dimension: species
sp_path <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  sp_id       = 1:4,
  name        = c("Quercus robur", "Fagus sylvatica",
                   "Pinus sylvestris", "Abies alba"),
  family      = c("Fagaceae", "Fagaceae", "Pinaceae", "Pinaceae"),
  red_list    = c("LC", "LC", "LC", "NT"),
  shade_tol   = c(0.4, 0.8, 0.2, 0.7),
  max_height  = c(40, 45, 35, 55),
  stringsAsFactors = FALSE
), sp_path)

# Dimension: sites
site_path <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  site_id   = 1:4,
  site_name = c("Wienerwald A", "Wienerwald B",
                "Donau-Auen", "Neusiedlersee"),
  habitat   = c("Deciduous", "Deciduous", "Riparian", "Steppe"),
  elev_m    = c(450, 520, 155, 120),
  annual_precip_mm = c(750, 780, 550, 600),
  stringsAsFactors = FALSE
), site_path)

## ----links--------------------------------------------------------------------
sp_link   <- link("sp_id", tbl(sp_path))
site_link <- link("site_id", tbl(site_path))

## ----schema-------------------------------------------------------------------
s <- vtr_schema(
  fact = tbl(obs_path),
  sp   = sp_link,
  site = site_link
)
s

## ----lookup-basic-------------------------------------------------------------
lookup(s, count, sp$name, site$habitat, .report = FALSE) |> collect()

## ----lookup-one-dim-----------------------------------------------------------
lookup(s, count, dbh_cm, site$habitat, site$elev_m, .report = FALSE) |>
  collect()

## ----report-------------------------------------------------------------------
result <- lookup(s, count, sp$name) |> collect()

## ----report-na----------------------------------------------------------------
result

## ----report-ok----------------------------------------------------------------
lookup(s, count, site$habitat) |> collect()

## ----report-off---------------------------------------------------------------
lookup(s, count, sp$name, .report = FALSE) |> collect()

## ----named-keys---------------------------------------------------------------
# Dimension with a different key name
sp2_path <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  species_code = 1:4,
  latin_name   = c("Quercus robur", "Fagus sylvatica",
                    "Pinus sylvestris", "Abies alba"),
  stringsAsFactors = FALSE
), sp2_path)

s2 <- vtr_schema(
  fact = tbl(obs_path),
  sp   = link(c("sp_id" = "species_code"), tbl(sp2_path))
)

lookup(s2, count, sp$latin_name, .report = FALSE) |> collect()

## ----join-inner---------------------------------------------------------------
# Only observations with known species
lookup(s, count, sp$name, .join = "inner", .report = FALSE) |> collect()

## ----join-inner-report--------------------------------------------------------
lookup(s, count, sp$name, .join = "inner") |> collect()

## ----reuse--------------------------------------------------------------------
# Analysis 1: species composition by habitat
a1 <- lookup(s, sp$name, site$habitat, .report = FALSE) |> collect()

# Analysis 2: stem diameter by elevation
a2 <- lookup(s, dbh_cm, site$elev_m, .report = FALSE) |> collect()

# Analysis 3: conservation status across sites
a3 <- lookup(s, count, sp$red_list, site$site_name, .report = FALSE) |>
  collect()

## ----reuse-show---------------------------------------------------------------
a1
a2
a3

## ----pattern-filter-----------------------------------------------------------
s_large <- vtr_schema(
  fact = tbl(obs_path) |> filter(count >= 5),
  sp   = link("sp_id", tbl(sp_path)),
  site = link("site_id", tbl(site_path))
)
lookup(s_large, count, sp$name, site$habitat, .report = FALSE) |> collect()

## ----pattern-agg--------------------------------------------------------------
lookup(s, count, sp$family, .report = FALSE) |>
  group_by(family) |>
  summarise(total = sum(count), n_obs = n()) |>
  collect()

## ----pattern-agg2-------------------------------------------------------------
lookup(s, count, site$habitat, .report = FALSE) |>
  group_by(habitat) |>
  summarise(mean_count = mean(count), max_count = max(count)) |>
  collect()

## ----pattern-cross------------------------------------------------------------
lookup(s, count, sp$family, site$habitat, .report = FALSE) |>
  group_by(family, habitat) |>
  summarise(total = sum(count)) |>
  collect()

## ----pattern-write------------------------------------------------------------
out_path <- tempfile(fileext = ".vtr")
lookup(s, count, sp$name, site$habitat, .report = FALSE) |>
  write_vtr(out_path)

tbl(out_path) |> collect()

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(obs_path, sp_path, site_path, sp2_path, out_path))

