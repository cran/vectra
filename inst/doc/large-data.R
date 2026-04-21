## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----synthetic-data-----------------------------------------------------------
library(vectra)

set.seed(42)

n <- 50000
species_pool <- c(
  "Quercus robur", "Fagus sylvatica", "Pinus sylvestris",
  "Betula pendula", "Acer pseudoplatanus", "Fraxinus excelsior",
  "Picea abies", "Tilia cordata", "Carpinus betulus",
  "Alnus glutinosa", "Sorbus aucuparia", "Ulmus glabra"
)
sites <- paste0("SITE_", sprintf("%03d", 1:50))
years <- 2015:2024

obs <- data.frame(
  obs_id    = seq_len(n),
  site      = sample(sites, n, replace = TRUE),
  species   = sample(species_pool, n, replace = TRUE),
  year      = sample(years, n, replace = TRUE),
  abundance = rpois(n, lambda = 15),
  cover_pct = round(runif(n, 0.1, 95.0), 1),
  quality   = sample(c("good", "moderate", "poor"), n,
                     replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  stringsAsFactors = FALSE
)

## ----write-sources------------------------------------------------------------
csv_path <- tempfile(fileext = ".csv")
write.csv(obs, csv_path, row.names = FALSE)

vtr_path <- tempfile(fileext = ".vtr")
write_vtr(obs, vtr_path)

## ----streaming-basic----------------------------------------------------------
clean_path <- tempfile(fileext = ".vtr")

tbl_csv(csv_path) |>
  filter(quality == "good") |>
  mutate(log_abundance = log(abundance + 1)) |>
  write_vtr(clean_path)

## ----verify-streaming---------------------------------------------------------
tbl(clean_path) |>
  select(obs_id, site, species, abundance, log_abundance) |>
  slice_head(n = 5) |>
  collect()

## ----csv-to-sqlite------------------------------------------------------------
db_path <- tempfile(fileext = ".sqlite")

tbl_csv(csv_path) |>
  filter(year >= 2020) |>
  write_sqlite(db_path, "recent_obs")

## ----vtr-to-csv---------------------------------------------------------------
export_csv <- tempfile(fileext = ".csv")

tbl(vtr_path) |>
  filter(species == "Fagus sylvatica") |>
  select(obs_id, site, year, abundance) |>
  write_csv(export_csv)

## ----collect-agg--------------------------------------------------------------
tbl(vtr_path) |>
  group_by(species, year) |>
  summarise(
    n_obs     = n(),
    mean_abun = mean(abundance),
    mean_cov  = mean(cover_pct)
  ) |>
  arrange(species, year) |>
  collect() |>
  head(10)

## ----batch-size-write---------------------------------------------------------
small_groups <- tempfile(fileext = ".vtr")
large_groups <- tempfile(fileext = ".vtr")

tbl(vtr_path) |>
  arrange(year) |>
  write_vtr(small_groups, batch_size = 5000)

tbl(vtr_path) |>
  arrange(year) |>
  write_vtr(large_groups, batch_size = 50000)

## ----batch-size-compare-------------------------------------------------------
tbl(small_groups) |>
  filter(year == 2023) |>
  explain()

## ----csv-batch-size-----------------------------------------------------------
tbl_csv(csv_path, batch_size = 10000) |>
  filter(quality == "good") |>
  slice_head(n = 3) |>
  collect()

## ----append-init--------------------------------------------------------------
archive <- tempfile(fileext = ".vtr")
first_year <- obs[obs$year == 2015, ]
write_vtr(first_year, archive)

nrow(tbl(archive) |> collect())

## ----append-year--------------------------------------------------------------
year_2016 <- obs[obs$year == 2016, ]
append_vtr(year_2016, archive)

nrow(tbl(archive) |> collect())

## ----append-streaming---------------------------------------------------------
csv_2017 <- tempfile(fileext = ".csv")
write.csv(obs[obs$year == 2017, ], csv_2017, row.names = FALSE)

tbl_csv(csv_2017) |> append_vtr(archive)

nrow(tbl(archive) |> collect())

## ----compact------------------------------------------------------------------
compacted <- tempfile(fileext = ".vtr")
tbl(archive) |> write_vtr(compacted, batch_size = 50000)

## ----append-schema------------------------------------------------------------
new_data_node <- tbl_csv(csv_2017) |>
  mutate(observer = NA_character_)

## ----delete-basic-------------------------------------------------------------
del_demo <- tempfile(fileext = ".vtr")
write_vtr(obs[1:100, ], del_demo)

# Delete rows 0, 1, and 99 (first two and last)
delete_vtr(del_demo, c(0, 1, 99))

tbl(del_demo) |> collect() |> nrow()

## ----delete-cumulative--------------------------------------------------------
delete_vtr(del_demo, c(10, 11, 12))

tbl(del_demo) |> collect() |> nrow()

## ----delete-undo--------------------------------------------------------------
unlink(paste0(del_demo, ".del"))
tbl(del_demo) |> collect() |> nrow()

## ----delete-compact-----------------------------------------------------------
delete_vtr(del_demo, 0:49)
compacted_del <- tempfile(fileext = ".vtr")
tbl(del_demo) |> write_vtr(compacted_del)

nrow(tbl(compacted_del) |> collect())

## ----diff-setup---------------------------------------------------------------
snap_v1 <- tempfile(fileext = ".vtr")
snap_v2 <- tempfile(fileext = ".vtr")

# Version 1: observations 1-100
write_vtr(obs[1:100, ], snap_v1)

# Version 2: observations 51-150 (rows 1-50 removed, 101-150 added)
write_vtr(obs[51:150, ], snap_v2)

## ----diff-compute-------------------------------------------------------------
changes <- diff_vtr(snap_v1, snap_v2, "obs_id")

# Keys that disappeared
head(changes$deleted)
length(changes$deleted)

## ----diff-added---------------------------------------------------------------
collect(changes$added) |> head()

## ----diff-append--------------------------------------------------------------
archive_diff <- tempfile(fileext = ".vtr")
write_vtr(obs[1:100, ], archive_diff)

changes$added |> append_vtr(archive_diff)

nrow(tbl(archive_diff) |> collect())

## ----external-sort------------------------------------------------------------
sorted_path <- tempfile(fileext = ".vtr")

tbl(vtr_path) |>
  arrange(species, desc(abundance)) |>
  write_vtr(sorted_path)

tbl(sorted_path) |>
  select(species, abundance, site) |>
  slice_head(n = 8) |>
  collect()

## ----join-setup---------------------------------------------------------------
# Small reference table: site metadata
site_meta <- data.frame(
  site      = sites,
  region    = sample(c("North", "South", "East", "West"),
                     length(sites), replace = TRUE),
  elevation = round(runif(length(sites), 100, 2500)),
  stringsAsFactors = FALSE
)
site_path <- tempfile(fileext = ".vtr")
write_vtr(site_meta, site_path)

## ----join-streaming-----------------------------------------------------------
enriched <- tempfile(fileext = ".vtr")

tbl(vtr_path) |>
  left_join(tbl(site_path), by = "site") |>
  write_vtr(enriched)

tbl(enriched) |>
  select(obs_id, site, region, elevation, species) |>
  slice_head(n = 5) |>
  collect()

## ----join-filter-right--------------------------------------------------------
joined_path <- tempfile(fileext = ".vtr")

tbl(vtr_path) |>
  filter(year >= 2022) |>
  inner_join(
    tbl(site_path) |> filter(region == "North"),
    by = "site"
  ) |>
  write_vtr(joined_path)

nrow(tbl(joined_path) |> collect())

## ----join-pre-agg-------------------------------------------------------------
# Right side: per-site-year summary from a second dataset
summary_path <- tempfile(fileext = ".vtr")

tbl(vtr_path) |>
  group_by(site, year) |>
  summarise(site_year_avg = mean(abundance)) |>
  write_vtr(summary_path)

# Join the summary back to the detail table
tbl(vtr_path) |>
  left_join(tbl(summary_path), by = c("site", "year")) |>
  select(obs_id, site, year, abundance, site_year_avg) |>
  slice_head(n = 5) |>
  collect()

## ----multifile-setup----------------------------------------------------------
monthly_paths <- character(6)
for (m in 1:6) {
  monthly_paths[m] <- tempfile(fileext = ".vtr")
  idx <- which(obs$year == 2024 & ((obs$obs_id %% 6) + 1) == m)
  month_data <- obs[idx[seq_len(min(200, length(idx)))], ]
  write_vtr(month_data, monthly_paths[m])
}

## ----multifile-combine--------------------------------------------------------
nodes <- lapply(monthly_paths, tbl)
combined <- do.call(bind_rows, nodes)

combined |>
  group_by(species) |>
  summarise(
    total_obs  = n(),
    mean_abun  = mean(abundance)
  ) |>
  arrange(desc(total_obs)) |>
  slice_head(n = 5) |>
  collect()

## ----multifile-consolidate----------------------------------------------------
consolidated <- tempfile(fileext = ".vtr")
nodes2 <- lapply(monthly_paths, tbl)
do.call(bind_rows, nodes2) |> write_vtr(consolidated)

nrow(tbl(consolidated) |> collect())

## ----multifile-append---------------------------------------------------------
running_archive <- tempfile(fileext = ".vtr")
initial_nodes <- lapply(monthly_paths[1:3], tbl)
do.call(bind_rows, initial_nodes) |> write_vtr(running_archive)

# Next month's files arrive
new_nodes <- lapply(monthly_paths[4:6], tbl)
do.call(bind_rows, new_nodes) |> append_vtr(running_archive)

nrow(tbl(running_archive) |> collect())

## ----etl-basic----------------------------------------------------------------
vtr_archive <- tempfile(fileext = ".vtr")
tbl_csv(csv_path) |> write_vtr(vtr_archive)

## ----etl-clean----------------------------------------------------------------
clean_archive <- tempfile(fileext = ".vtr")

tbl_csv(csv_path) |>
  filter(quality != "poor") |>
  mutate(
    abundance_log = log(abundance + 1),
    cover_frac    = cover_pct / 100
  ) |>
  select(-quality) |>
  arrange(site, year) |>
  write_vtr(clean_archive, batch_size = 10000)

## ----etl-sqlite---------------------------------------------------------------
sqlite_export <- tempfile(fileext = ".sqlite")

tbl(clean_archive) |>
  filter(year >= 2020) |>
  write_sqlite(sqlite_export, "observations")

## ----etl-summary-csv----------------------------------------------------------
summary_csv <- tempfile(fileext = ".csv")

tbl(clean_archive) |>
  group_by(site, year) |>
  summarise(
    n_species = n_distinct(species),
    total_abundance = sum(abundance)
  ) |>
  write_csv(summary_csv)

read.csv(summary_csv) |> head()

## ----etl-multi----------------------------------------------------------------
csv_north <- tempfile(fileext = ".csv")
csv_south <- tempfile(fileext = ".csv")
write.csv(obs[1:25000, ], csv_north, row.names = FALSE)
write.csv(obs[25001:50000, ], csv_south, row.names = FALSE)

merged_vtr <- tempfile(fileext = ".vtr")

bind_rows(
  tbl_csv(csv_north),
  tbl_csv(csv_south)
) |>
  filter(abundance > 0) |>
  write_vtr(merged_vtr, batch_size = 25000)

nrow(tbl(merged_vtr) |> collect())

## ----memory-estimate----------------------------------------------------------
# Example pipeline:
# tbl(huge.vtr) |>
#   filter(year == 2023) |>        -> streaming, ~5 MB per batch
#   left_join(sites, by = "site")  -> build side = 50 sites, ~1 KB
#   group_by(species) |>           -> 12 groups, ~1 KB
#   summarise(total = sum(abun))   -> 12 accumulators, ~1 KB
#   arrange(desc(total))           -> 12 rows, in-memory
#
# Total peak: ~5 MB (one batch) + negligible join/agg overhead

# Compare to:
# tbl(huge.vtr) |>
#   arrange(species)               -> external sort, up to 1 GB
#   left_join(big_ref, by = "id")  -> build side = big_ref size
#
# Total peak: 1 GB (sort) + big_ref size

## ----memory-pipeline----------------------------------------------------------
final_path <- tempfile(fileext = ".vtr")

tbl(vtr_path) |>
  filter(year >= 2020, quality != "poor") |>
  left_join(tbl(site_path), by = "site") |>
  group_by(region, species) |>
  summarise(
    n_obs     = n(),
    mean_cov  = mean(cover_pct)
  ) |>
  arrange(region, desc(n_obs)) |>
  write_vtr(final_path)

tbl(final_path) |> collect() |> head(10)

## ----cleanup------------------------------------------------------------------
all_files <- c(
  csv_path, vtr_path, clean_path, db_path, export_csv,
  del_demo, paste0(del_demo, ".del"),
  snap_v1, snap_v2,
  sorted_path, site_path, enriched, joined_path,
  summary_path, monthly_paths, consolidated, running_archive,
  vtr_archive, clean_archive, sqlite_export, summary_csv,
  csv_north, csv_south, merged_vtr,
  small_groups, large_groups,
  archive, csv_2017, archive_diff, compacted, compacted_del,
  final_path
)
unlink(all_files[file.exists(all_files)])

