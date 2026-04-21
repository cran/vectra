## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic-data---------------------------------------------------------------
library(vectra)

trees <- data.frame(
  species  = c("Quercus robur  ", "  fagus sylvatica", "ACER platanoides",
               "Betula pendula", "  Pinus SYLVESTRIS  "),
  site     = c("plot_A01", "plot_B12", "plot_A01", "plot_C03", "plot_B12"),
  observer = c("J. Smith", "A. Mueller", "J. Smith",
               "B. Novak", "A. Mueller"),
  dbh_cm   = c(45.2, 38.1, 22.7, 31.0, 55.3),
  stringsAsFactors = FALSE
)

f <- tempfile(fileext = ".vtr")
write_vtr(trees, f)

## ----trimws-------------------------------------------------------------------
tbl(f) |>
  mutate(species_clean = trimws(species)) |>
  select(species, species_clean) |>
  collect()

## ----case---------------------------------------------------------------------
tbl(f) |>
  mutate(species_lower = tolower(trimws(species)),
         species_upper = toupper(trimws(species))) |>
  select(species_lower, species_upper) |>
  collect()

## ----nchar--------------------------------------------------------------------
tbl(f) |>
  mutate(name_len = nchar(trimws(species))) |>
  select(species, name_len) |>
  collect()

## ----substr-------------------------------------------------------------------
tbl(f) |>
  mutate(plot_letter = substr(site, 6, 6),
         plot_number = substr(site, 7, 8)) |>
  select(site, plot_letter, plot_number) |>
  collect()

## ----starts-ends--------------------------------------------------------------
tbl(f) |>
  filter(startsWith(site, "plot_A")) |>
  select(species, site) |>
  collect()

## ----endswith-----------------------------------------------------------------
tbl(f) |>
  mutate(is_mueller = endsWith(observer, "Mueller")) |>
  select(observer, is_mueller) |>
  collect()

## ----paste0-------------------------------------------------------------------
tbl(f) |>
  mutate(record_id = paste0(site, "_", trimws(species))) |>
  select(record_id) |>
  collect()

## ----paste-sep----------------------------------------------------------------
tbl(f) |>
  mutate(label = paste(observer, site, sep = " @ ")) |>
  select(label) |>
  collect()

## ----paste-multi--------------------------------------------------------------
tbl(f) |>
  mutate(full_label = paste(observer, site,
                            trimws(species), sep = " | ")) |>
  select(full_label) |>
  collect()

## ----grepl-fixed--------------------------------------------------------------
tbl(f) |>
  filter(grepl("Smith", observer, fixed = TRUE)) |>
  collect()

## ----gsub-fixed---------------------------------------------------------------
messy_sites <- data.frame(
  site = c("plot_A01", "plot-B12", "plot_A01", "plot-C03", "plot_B12"),
  stringsAsFactors = FALSE
)
f2 <- tempfile(fileext = ".vtr")
write_vtr(messy_sites, f2)

tbl(f2) |>
  mutate(site_clean = gsub("-", "_", site, fixed = TRUE)) |>
  collect()

## ----sub-fixed----------------------------------------------------------------
notes <- data.frame(
  note = c("tree dead, bark loose, dead branches",
           "alive, healthy canopy",
           "dead standing, no bark"),
  stringsAsFactors = FALSE
)
f3 <- tempfile(fileext = ".vtr")
write_vtr(notes, f3)

tbl(f3) |>
  mutate(note_edited = sub("dead", "DEAD", note, fixed = TRUE)) |>
  select(note, note_edited) |>
  collect()

## ----grepl-regex--------------------------------------------------------------
species_list <- data.frame(
  name = c("Quercus robur", "fagus sylvatica", "ACER PLATANOIDES",
           "Betula pendula", "Pinus  sylvestris", "Tilia cordata"),
  stringsAsFactors = FALSE
)
f4 <- tempfile(fileext = ".vtr")
write_vtr(species_list, f4)

tbl(f4) |>
  mutate(valid_format = grepl("^[A-Z][a-z]+ [a-z]+$",
                              name, fixed = FALSE)) |>
  collect()

## ----gsub-regex---------------------------------------------------------------
tbl(f4) |>
  mutate(short_name = gsub("^([A-Za-z])[a-z]+ ([a-z]+)$",
                           "\\1. \\2",
                           name, fixed = FALSE)) |>
  select(name, short_name) |>
  collect()

## ----sub-regex----------------------------------------------------------------
records <- data.frame(
  code = c("ID:001-Quercus", "ID:042-Fagus",
           "ID:007-Betula", "ID:113-Pinus"),
  stringsAsFactors = FALSE
)
f5 <- tempfile(fileext = ".vtr")
write_vtr(records, f5)

tbl(f5) |>
  mutate(genus = sub("^ID:[0-9]+-", "", code, fixed = FALSE)) |>
  select(code, genus) |>
  collect()

## ----str-extract--------------------------------------------------------------
sites <- data.frame(
  site_code = c("Forest_Plot_042", "Meadow_Transect_007",
                "Forest_Plot_113", "Wetland_Quad_019"),
  stringsAsFactors = FALSE
)
f6 <- tempfile(fileext = ".vtr")
write_vtr(sites, f6)

tbl(f6) |>
  mutate(plot_num = str_extract(site_code, "([0-9]+)")) |>
  select(site_code, plot_num) |>
  collect()

## ----str-extract-habitat------------------------------------------------------
tbl(f6) |>
  mutate(habitat = str_extract(site_code, "^([A-Za-z]+)_")) |>
  select(site_code, habitat) |>
  collect()

## ----levenshtein-data---------------------------------------------------------
typos <- data.frame(
  field_name = c("Qurecus robur", "Fagus silvatica",
                 "Acer platanodes", "Betula pendula",
                 "Pinus sylvestrs"),
  ref_name   = c("Quercus robur", "Fagus sylvatica",
                 "Acer platanoides", "Betula pendula",
                 "Pinus sylvestris"),
  stringsAsFactors = FALSE
)
f7 <- tempfile(fileext = ".vtr")
write_vtr(typos, f7)

tbl(f7) |>
  mutate(lev = levenshtein(field_name, ref_name),
         lev_norm = levenshtein_norm(field_name, ref_name)) |>
  collect()

## ----dl-dist------------------------------------------------------------------
tbl(f7) |>
  mutate(dl = dl_dist(field_name, ref_name),
         dl_norm = dl_dist_norm(field_name, ref_name)) |>
  collect()

## ----jaro-winkler-------------------------------------------------------------
tbl(f7) |>
  mutate(jw = jaro_winkler(field_name, ref_name)) |>
  collect()

## ----filter-fuzzy-------------------------------------------------------------
field_data <- data.frame(
  species = c("Qurecus robur", "Quercus robor", "Fagus sylvatica",
              "Quercus robur", "Quercis rubur", "Betula pendula"),
  plot    = c("A1", "A2", "B1", "A3", "B2", "C1"),
  stringsAsFactors = FALSE
)
f8 <- tempfile(fileext = ".vtr")
write_vtr(field_data, f8)

tbl(f8) |>
  mutate(dist = dl_dist_norm(species, "Quercus robur")) |>
  filter(dist < 0.15) |>
  collect()

## ----rank-fuzzy---------------------------------------------------------------
tbl(f8) |>
  mutate(sim = jaro_winkler(species, "Quercus robur")) |>
  arrange(desc(sim)) |>
  collect()

## ----fuzzy-join-basic---------------------------------------------------------
ref <- data.frame(
  canonical = c("Quercus robur", "Fagus sylvatica",
                "Acer platanoides", "Betula pendula",
                "Pinus sylvestris", "Tilia cordata"),
  family    = c("Fagaceae", "Fagaceae", "Sapindaceae",
                "Betulaceae", "Pinaceae", "Malvaceae"),
  stringsAsFactors = FALSE
)
f_ref <- tempfile(fileext = ".vtr")
write_vtr(ref, f_ref)

messy <- data.frame(
  field_species = c("Qurecus robur", "Fagus silvatica",
                    "Acer platanodes", "Betla pendula",
                    "Pinis sylvestris"),
  count         = c(12L, 7L, 3L, 15L, 9L),
  stringsAsFactors = FALSE
)
f_messy <- tempfile(fileext = ".vtr")
write_vtr(messy, f_messy)

fuzzy_join(
  tbl(f_messy), tbl(f_ref),
  by = c("field_species" = "canonical"),
  method = "dl",
  max_dist = 0.25
) |> collect()

## ----fuzzy-join-jw------------------------------------------------------------
fuzzy_join(
  tbl(f_messy), tbl(f_ref),
  by = c("field_species" = "canonical"),
  method = "jw",
  max_dist = 0.15
) |> collect()

## ----fuzzy-join-blocked-------------------------------------------------------
ref_blocked <- data.frame(
  genus     = c("Quercus", "Fagus", "Acer",
                "Betula", "Pinus", "Tilia"),
  canonical = c("Quercus robur", "Fagus sylvatica",
                "Acer platanoides", "Betula pendula",
                "Pinus sylvestris", "Tilia cordata"),
  family    = c("Fagaceae", "Fagaceae", "Sapindaceae",
                "Betulaceae", "Pinaceae", "Malvaceae"),
  stringsAsFactors = FALSE
)
f_ref2 <- tempfile(fileext = ".vtr")
write_vtr(ref_blocked, f_ref2)

messy_blocked <- data.frame(
  genus_field   = c("Quercus", "Fagus", "Acer",
                    "Betula", "Pinus"),
  field_species = c("Qurecus robur", "Fagus silvatica",
                    "Acer platanodes", "Betla pendula",
                    "Pinis sylvestris"),
  count         = c(12L, 7L, 3L, 15L, 9L),
  stringsAsFactors = FALSE
)
f_messy2 <- tempfile(fileext = ".vtr")
write_vtr(messy_blocked, f_messy2)

fuzzy_join(
  tbl(f_messy2), tbl(f_ref2),
  by = c("field_species" = "canonical"),
  method = "dl",
  max_dist = 0.25,
  block_by = c("genus_field" = "genus")
) |> collect()

## ----fuzzy-join-threads, eval = FALSE-----------------------------------------
# fuzzy_join(
#   tbl(f_messy), tbl(f_ref),
#   by = c("field_species" = "canonical"),
#   method = "dl",
#   max_dist = 0.25,
#   n_threads = 8L
# ) |> collect()

## ----block-lookup-------------------------------------------------------------
blk <- tbl(f_ref) |>
  select(canonical, family) |>
  materialize()

block_lookup(blk, "canonical",
             c("Quercus robur", "Betula pendula"))

## ----block-lookup-ci----------------------------------------------------------
block_lookup(blk, "canonical",
             c("quercus robur", "BETULA PENDULA"),
             ci = TRUE)

## ----block-fuzzy--------------------------------------------------------------
blk2 <- tbl(f_ref2) |>
  select(genus, canonical, family) |>
  materialize()

block_fuzzy_lookup(
  blk2, "canonical",
  keys = c("Qurecus robur", "Fagus silvatica"),
  method = "dl",
  max_dist = 0.2,
  block_col = "genus",
  block_keys = c("Quercus", "Fagus"),
  n_threads = 2L
)

## ----cleaning-pattern---------------------------------------------------------
raw_names <- data.frame(
  species = c("  Quercus ROBUR ", "fagus sylvatica.",
              "Acer platanoides (L.)", "BETULA   pendula"),
  stringsAsFactors = FALSE
)
f9 <- tempfile(fileext = ".vtr")
write_vtr(raw_names, f9)

tbl(f9) |>
  mutate(clean = tolower(trimws(species))) |>
  mutate(clean = gsub(".", "", clean, fixed = TRUE)) |>
  mutate(clean = gsub(" +", " ", clean, fixed = FALSE)) |>
  select(species, clean) |>
  collect()

## ----layered-matching---------------------------------------------------------
ref_clean <- data.frame(
  canonical = c("quercus robur", "fagus sylvatica",
                "acer platanoides", "betula pendula"),
  status    = c("accepted", "accepted", "accepted", "accepted"),
  stringsAsFactors = FALSE
)
f_refc <- tempfile(fileext = ".vtr")
write_vtr(ref_clean, f_refc)

cleaned <- tbl(f9) |>
  mutate(clean = tolower(trimws(species))) |>
  mutate(clean = gsub(".", "", clean, fixed = TRUE)) |>
  mutate(clean = gsub(" +", " ", clean, fixed = FALSE))

# Step 1: exact join on cleaned names
exact <- left_join(
  cleaned, tbl(f_refc),
  by = c("clean" = "canonical")
) |> collect()

exact

