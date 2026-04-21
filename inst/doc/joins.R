## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example-data-------------------------------------------------------------
library(vectra)

obs_path <- tempfile(fileext = ".vtr")
sites_path <- tempfile(fileext = ".vtr")
ref_path <- tempfile(fileext = ".vtr")

observations <- data.frame(
  site_id = c(1, 1, 2, 2, 3, 3, 4, 4),
  species = c("Quercus robur", "Pinus sylvestris",
              "Quercus robur", "Fagus sylvatica",
              "Pinus sylvestris", "Betula pendula",
              "Fagus sylvatica", "Acer pseudoplatanus"),
  date = c("2024-06-01", "2024-06-01", "2024-06-15",
           "2024-06-15", "2024-07-01", "2024-07-01",
           "2024-07-10", "2024-07-10"),
  count = c(12, 5, 8, 3, 7, 14, 6, 2),
  stringsAsFactors = FALSE
)

sites <- data.frame(
  site_id = c(1, 2, 3, 5),
  latitude = c(48.21, 47.07, 46.62, 48.85),
  longitude = c(16.37, 15.44, 14.31, 13.02),
  habitat = c("forest", "wetland", "grassland", "forest"),
  stringsAsFactors = FALSE
)

reference <- data.frame(
  species_name = c("Quercus robur", "Pinus sylvestris",
                   "Fagus sylvatica", "Betula pendula",
                   "Alnus glutinosa"),
  family = c("Fagaceae", "Pinaceae", "Fagaceae",
             "Betulaceae", "Betulaceae"),
  conservation_status = c("LC", "LC", "LC", "LC", "LC"),
  stringsAsFactors = FALSE
)

write_vtr(observations, obs_path)
write_vtr(sites, sites_path)
write_vtr(reference, ref_path)

## ----by-same-name-------------------------------------------------------------
left_join(tbl(obs_path), tbl(sites_path), by = "site_id") |>
  collect()

## ----by-named-----------------------------------------------------------------
inner_join(
  tbl(obs_path),
  tbl(ref_path),
  by = c("species" = "species_name")
) |> collect()

## ----by-natural---------------------------------------------------------------
left_join(tbl(obs_path), tbl(sites_path)) |>
  collect()

## ----multi-key-setup----------------------------------------------------------
survey1_path <- tempfile(fileext = ".vtr")
survey2_path <- tempfile(fileext = ".vtr")

write_vtr(data.frame(
  site_id = c(1, 1, 2, 2),
  year = c(2023, 2024, 2023, 2024),
  richness = c(5, 7, 3, 4)
), survey1_path)

write_vtr(data.frame(
  site_id = c(1, 2, 2),
  year = c(2024, 2023, 2024),
  temperature = c(18.2, 16.1, 17.5)
), survey2_path)

inner_join(
  tbl(survey1_path), tbl(survey2_path),
  by = c("site_id", "year")
) |> collect()

## ----left-join----------------------------------------------------------------
enriched <- left_join(
  tbl(obs_path), tbl(sites_path), by = "site_id"
) |> collect()
enriched

## ----left-join-suffix---------------------------------------------------------
extra_path <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  site_id = c(1, 2, 3),
  count = c(100, 200, 300)
), extra_path)

left_join(
  tbl(obs_path), tbl(extra_path),
  by = "site_id", suffix = c("_obs", "_site")
) |> collect()

## ----inner-join---------------------------------------------------------------
inner_join(
  tbl(obs_path), tbl(sites_path), by = "site_id"
) |> collect()

## ----right-join---------------------------------------------------------------
right_join(
  tbl(obs_path), tbl(sites_path), by = "site_id"
) |> collect()

## ----full-join----------------------------------------------------------------
full_join(
  tbl(obs_path), tbl(sites_path), by = "site_id"
) |> collect()

## ----semi-join-setup----------------------------------------------------------
targets_path <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  species = c("Quercus robur", "Betula pendula"),
  stringsAsFactors = FALSE
), targets_path)

## ----semi-join----------------------------------------------------------------
semi_join(
  tbl(obs_path), tbl(targets_path), by = "species"
) |> collect()

## ----anti-join----------------------------------------------------------------
anti_join(
  tbl(obs_path),
  tbl(ref_path),
  by = c("species" = "species_name")
) |> collect()

## ----anti-join-reverse--------------------------------------------------------
anti_join(
  tbl(ref_path),
  tbl(obs_path),
  by = c("species_name" = "species")
) |> collect()

## ----cross-join---------------------------------------------------------------
years_path <- tempfile(fileext = ".vtr")
write_vtr(data.frame(year = c(2022, 2023, 2024)), years_path)

grid <- cross_join(tbl(sites_path), tbl(years_path))
grid

## ----fuzzy-setup--------------------------------------------------------------
messy_path <- tempfile(fileext = ".vtr")
clean_path <- tempfile(fileext = ".vtr")

write_vtr(data.frame(
  obs_id = c(1, 2, 3, 4, 5),
  name = c("Quercus robar", "Pinus sylvestris L.",
           "Fagus silvatica", "Betula pendla",
           "Acer pseudoplatanus"),
  stringsAsFactors = FALSE
), messy_path)

write_vtr(data.frame(
  species_name = c("Quercus robur", "Pinus sylvestris",
                   "Fagus sylvatica", "Betula pendula",
                   "Acer pseudoplatanus"),
  family = c("Fagaceae", "Pinaceae", "Fagaceae",
             "Betulaceae", "Sapindaceae"),
  stringsAsFactors = FALSE
), clean_path)

## ----fuzzy-join---------------------------------------------------------------
fuzzy_join(
  tbl(messy_path),
  tbl(clean_path),
  by = c("name" = "species_name"),
  method = "dl",
  max_dist = 0.2
) |> collect()

## ----fuzzy-block-setup--------------------------------------------------------
messy2_path <- tempfile(fileext = ".vtr")
clean2_path <- tempfile(fileext = ".vtr")

write_vtr(data.frame(
  genus = c("Quercus", "Pinus", "Fagus", "Betula"),
  name = c("Quercus robar", "Pinus sylvestris L.",
           "Fagus silvatica", "Betula pendla"),
  stringsAsFactors = FALSE
), messy2_path)

write_vtr(data.frame(
  genus = c("Quercus", "Pinus", "Fagus",
            "Betula", "Alnus"),
  species_name = c("Quercus robur", "Pinus sylvestris",
                   "Fagus sylvatica", "Betula pendula",
                   "Alnus glutinosa"),
  stringsAsFactors = FALSE
), clean2_path)

## ----fuzzy-block--------------------------------------------------------------
fuzzy_join(
  tbl(messy2_path),
  tbl(clean2_path),
  by = c("name" = "species_name"),
  method = "dl",
  max_dist = 0.25,
  block_by = c("genus" = "genus")
) |> collect()

## ----multi-key-data-----------------------------------------------------------
counts_path <- tempfile(fileext = ".vtr")
effort_path <- tempfile(fileext = ".vtr")

write_vtr(data.frame(
  site_id = c(1, 1, 2, 2, 3),
  year = c(2023, 2024, 2023, 2024, 2024),
  n_species = c(12, 15, 8, 10, 20)
), counts_path)

write_vtr(data.frame(
  site_id = c(1, 1, 2, 3, 3),
  year = c(2023, 2024, 2024, 2023, 2024),
  hours = c(4.5, 5.0, 3.0, 6.0, 4.0)
), effort_path)

## ----multi-key-join-----------------------------------------------------------
left_join(
  tbl(counts_path), tbl(effort_path),
  by = c("site_id", "year")
) |> collect()

## ----multi-key-named----------------------------------------------------------
effort2_path <- tempfile(fileext = ".vtr")
write_vtr(data.frame(
  loc = c(1, 1, 2, 3, 3),
  survey_year = c(2023, 2024, 2024, 2023, 2024),
  hours = c(4.5, 5.0, 3.0, 6.0, 4.0)
), effort2_path)

left_join(
  tbl(counts_path), tbl(effort2_path),
  by = c("site_id" = "loc", "year" = "survey_year")
) |> collect()

## ----coercion-----------------------------------------------------------------
int_path <- tempfile(fileext = ".vtr")
dbl_path <- tempfile(fileext = ".vtr")

write_vtr(data.frame(id = c(1L, 2L, 3L), x = c(10, 20, 30)), int_path)
write_vtr(data.frame(id = c(1.0, 2.0, 4.0), y = c(100, 200, 400)), dbl_path)

inner_join(tbl(int_path), tbl(dbl_path), by = "id") |> collect()

## ----na-keys------------------------------------------------------------------
na_left <- tempfile(fileext = ".vtr")
na_right <- tempfile(fileext = ".vtr")

write_vtr(data.frame(
  id = c(1, NA, 3), x = c(10, 20, 30)
), na_left)
write_vtr(data.frame(
  id = c(1, NA, 4), y = c(100, 200, 400)
), na_right)

left_join(tbl(na_left), tbl(na_right), by = "id") |> collect()

## ----guidance-enrich----------------------------------------------------------
# Attach habitat type to every observation
left_join(tbl(obs_path), tbl(sites_path), by = "site_id") |>
  select(site_id, species, habitat) |>
  collect()

## ----guidance-preprocess------------------------------------------------------
# Preprocessing before an exact join
tbl(obs_path) |>
  mutate(species_clean = trimws(tolower(species))) |>
  select(site_id, species_clean, count) |>
  collect()

## ----cleanup, include = FALSE-------------------------------------------------
unlink(c(obs_path, sites_path, ref_path, targets_path,
         extra_path, survey1_path, survey2_path,
         years_path, messy_path, clean_path,
         messy2_path, clean2_path,
         counts_path, effort_path, effort2_path,
         int_path, dbl_path, na_left, na_right))

