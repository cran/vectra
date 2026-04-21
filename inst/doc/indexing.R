## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----create-data--------------------------------------------------------------
library(vectra)

set.seed(42)
n <- 50000

sites <- paste0("site_", sprintf("%03d", 1:200))
species <- paste0("sp_", sprintf("%03d", 1:80))

eco <- data.frame(
  site     = sample(sites, n, replace = TRUE),
  year     = sample(2000:2023, n, replace = TRUE),
  species  = sample(species, n, replace = TRUE),
  value    = round(runif(n, 0, 100), 2),
  quality  = sample(c("good", "moderate", "poor"), n, replace = TRUE),
  stringsAsFactors = FALSE
)

## ----write-vtr----------------------------------------------------------------
f <- tempfile(fileext = ".vtr")
write_vtr(eco, f, batch_size = 5000)

## ----zonemap-explain----------------------------------------------------------
tbl(f) |>
  filter(value > 95) |>
  explain()

## ----zonemap-sorted-----------------------------------------------------------
eco_sorted <- eco[order(eco$value), ]
f_sorted <- tempfile(fileext = ".vtr")
write_vtr(eco_sorted, f_sorted, batch_size = 5000)

## ----zonemap-sorted-explain---------------------------------------------------
tbl(f_sorted) |>
  filter(value > 95) |>
  explain()

## ----index-no-index-----------------------------------------------------------
tbl(f) |>
  filter(site == "site_042") |>
  explain()

## ----create-index-------------------------------------------------------------
create_index(f, "site")
has_index(f, "site")

## ----index-with-index---------------------------------------------------------
tbl(f) |>
  filter(site == "site_042") |>
  explain()

## ----index-timing-------------------------------------------------------------
# Without index -- drop the existing one first
unlink(paste0(f, ".site.vtri"))

t_no_idx <- system.time({
  for (i in 1:50) {
    tbl(f) |> filter(site == "site_042") |> collect()
  }
})

# With index
create_index(f, "site")

t_idx <- system.time({
  for (i in 1:50) {
    tbl(f) |> filter(site == "site_042") |> collect()
  }
})

cat("Without index:", t_no_idx["elapsed"], "s\n")
cat("With index:   ", t_idx["elapsed"], "s\n")

## ----composite-create---------------------------------------------------------
create_index(f, c("site", "year"))
has_index(f, c("site", "year"))

## ----composite-explain--------------------------------------------------------
tbl(f) |>
  filter(site == "site_042", year == 2015) |>
  explain()

## ----ci-index-----------------------------------------------------------------
# Create a dataset with mixed-case species
eco_mixed <- eco
eco_mixed$species[1:100] <- toupper(eco_mixed$species[1:100])
f_mixed <- tempfile(fileext = ".vtr")
write_vtr(eco_mixed, f_mixed, batch_size = 5000)

create_index(f_mixed, "species", ci = TRUE)

## ----in-explain---------------------------------------------------------------
target_sites <- c("site_010", "site_042", "site_100", "site_150")

tbl(f) |>
  filter(site %in% target_sites) |>
  explain()

## ----in-timing----------------------------------------------------------------
t_in_no_idx <- system.time({
  for (i in 1:50) {
    tbl(f) |>
      filter(site %in% c("site_010", "site_042")) |>
      collect()
  }
})

cat("With index, %in% filter:", t_in_no_idx["elapsed"], "s\n")

## ----col-pruning--------------------------------------------------------------
tbl(f) |>
  filter(value > 90) |>
  select(site, value) |>
  explain()

## ----col-pruning-wide---------------------------------------------------------
wide <- data.frame(
  id = seq_len(1000),
  matrix(rnorm(1000 * 20), ncol = 20,
         dimnames = list(NULL, paste0("v", 1:20)))
)
f_wide <- tempfile(fileext = ".vtr")
write_vtr(wide, f_wide)

tbl(f_wide) |>
  select(id, v1, v2) |>
  explain()

## ----pushdown-basic-----------------------------------------------------------
tbl(f) |>
  filter(value > 50) |>
  explain()

## ----pushdown-blocked---------------------------------------------------------
tbl(f) |>
  mutate(scaled = value / 100) |>
  filter(scaled > 0.9) |>
  explain()

## ----pushdown-rewrite---------------------------------------------------------
tbl(f) |>
  filter(value > 90) |>
  mutate(scaled = value / 100) |>
  explain()

## ----explain-complex----------------------------------------------------------
tbl(f) |>
  filter(site == "site_042", value > 50) |>
  select(site, year, value) |>
  mutate(decade = year - (year %% 10)) |>
  explain()

## ----explain-agg--------------------------------------------------------------
tbl(f) |>
  filter(year >= 2020) |>
  group_by(site) |>
  summarise(avg = mean(value), n = n()) |>
  explain()

## ----materialize-block--------------------------------------------------------
# Build a small reference table
ref <- data.frame(
  species = species,
  common   = paste("Common name for", species),
  family   = sample(paste0("fam_", 1:10), 80, replace = TRUE),
  stringsAsFactors = FALSE
)
f_ref <- tempfile(fileext = ".vtr")
write_vtr(ref, f_ref)

blk <- materialize(tbl(f_ref))
blk

## ----block-lookup-------------------------------------------------------------
hits <- block_lookup(blk, "species", c("sp_010", "sp_042", "sp_001"))
hits

## ----block-fuzzy--------------------------------------------------------------
# Deliberately misspelled queries
fuzzy_hits <- block_fuzzy_lookup(
  blk, "species",
  keys = c("sp_10", "sp_42"),
  method = "dl",
  max_dist = 0.3
)
fuzzy_hits

## ----block-fuzzy-blocking-----------------------------------------------------
# Add a genus column for blocking
ref2 <- ref
ref2$genus <- substr(ref2$species, 1, 5)
f_ref2 <- tempfile(fileext = ".vtr")
write_vtr(ref2, f_ref2)

blk2 <- materialize(tbl(f_ref2))

fuzzy_blocked <- block_fuzzy_lookup(
  blk2, "species",
  keys       = c("sp_10", "sp_42"),
  method     = "dl",
  max_dist   = 0.3,
  block_col  = "genus",
  block_keys = c("sp_01", "sp_04")
)
fuzzy_blocked

## ----sort-write---------------------------------------------------------------
eco_by_site <- eco[order(eco$site), ]
f_by_site <- tempfile(fileext = ".vtr")
write_vtr(eco_by_site, f_by_site, batch_size = 5000)

tbl(f_by_site) |>
  filter(site == "site_042") |>
  explain()

## ----index-rebuild------------------------------------------------------------
append_vtr(eco[1:100, ], f)

# Old index is now stale -- recreate it
create_index(f, "site")

## ----compose-all--------------------------------------------------------------
tbl(f) |>
  filter(site == "site_042", value > 80) |>
  select(site, year, value) |>
  explain()

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(
  f, paste0(f, ".site.vtri"), paste0(f, ".site_year.vtri"),
  f_sorted, f_mixed, paste0(f_mixed, ".species.vtri"),
  f_wide, f_ref, f_ref2, f_by_site
))

