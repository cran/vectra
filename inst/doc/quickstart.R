## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----write-read---------------------------------------------------------------
library(vectra)

f <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f)

node <- tbl(f)
node

## ----collect------------------------------------------------------------------
tbl(f) |> collect() |> head()

## ----write-batch-size---------------------------------------------------------
f_batched <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f_batched, batch_size = 10)
tbl(f_batched) |> collect() |> nrow()

## ----filter-and---------------------------------------------------------------
tbl(f) |>
  filter(cyl == 6, mpg > 19) |>
  select(mpg, cyl, hp, wt) |>
  collect()

## ----filter-or----------------------------------------------------------------
tbl(f) |>
  filter(cyl == 4 | cyl == 8) |>
  select(mpg, cyl) |>
  collect() |>
  head()

## ----filter-in----------------------------------------------------------------
tbl(f) |>
  filter(cyl %in% c(4, 6)) |>
  select(mpg, cyl) |>
  collect() |>
  head()

## ----select-helpers-----------------------------------------------------------
tbl(f) |>
  select(starts_with("d"), mpg) |>
  collect() |>
  head()

## ----select-negate------------------------------------------------------------
tbl(f) |>
  select(-am, -vs, -gear, -carb) |>
  collect() |>
  head()

## ----explain-filter-----------------------------------------------------------
tbl(f) |>
  filter(cyl > 4) |>
  select(mpg, cyl, hp) |>
  explain()

## ----mutate-arith-------------------------------------------------------------
tbl(f) |>
  mutate(kpl = mpg * 0.425144, hp_per_wt = hp / wt) |>
  select(mpg, kpl, hp, wt, hp_per_wt) |>
  collect() |>
  head()

## ----mutate-math--------------------------------------------------------------
tbl(f) |>
  mutate(
    log_hp = log(hp),
    hp_floor = floor(hp / 10) * 10,
    bounded = pmin(pmax(mpg, 15), 25)
  ) |>
  select(hp, log_hp, hp_floor, mpg, bounded) |>
  collect() |>
  head()

## ----transmute----------------------------------------------------------------
tbl(f) |>
  transmute(
    efficiency = mpg / wt,
    power_ratio = hp / disp
  ) |>
  collect() |>
  head()

## ----mutate-cast--------------------------------------------------------------
tbl(f) |>
  mutate(cyl_str = as.character(cyl)) |>
  select(cyl, cyl_str) |>
  collect() |>
  head(3)

## ----mutate-control-----------------------------------------------------------
tbl(f) |>
  mutate(
    size = case_when(
      cyl == 4 ~ "small",
      cyl == 6 ~ "medium",
      cyl == 8 ~ "large"
    ),
    mpg_class = if_else(mpg > 20, "high", "low"),
    in_range = between(hp, 100, 200)
  ) |>
  select(cyl, size, mpg, mpg_class, hp, in_range) |>
  collect() |>
  head()

## ----mutate-coalesce----------------------------------------------------------
df_na <- data.frame(
  a = c(NA, 2, NA, 4),
  b = c(10, NA, NA, 40),
  stringsAsFactors = FALSE
)
f_na <- tempfile(fileext = ".vtr")
write_vtr(df_na, f_na)

tbl(f_na) |>
  mutate(filled = coalesce(a, b, 0)) |>
  collect()

## ----string-data--------------------------------------------------------------
people <- data.frame(
  name = c("  Alice  ", "Bob", "Charlie Brown", "Diana"),
  city = c("Amsterdam", "Berlin", "Chicago", "Dublin"),
  email = c("alice@example.com", "bob@test.org",
            "charlie.b@work.net", "diana@example.com"),
  stringsAsFactors = FALSE
)
fs <- tempfile(fileext = ".vtr")
write_vtr(people, fs)

## ----string-basic-------------------------------------------------------------
tbl(fs) |>
  mutate(
    name_trimmed = trimws(name),
    name_len = nchar(trimws(name)),
    city_prefix = substr(city, 1, 3)
  ) |>
  select(name_trimmed, name_len, city_prefix) |>
  collect()

## ----string-case--------------------------------------------------------------
tbl(fs) |>
  mutate(
    city_upper = toupper(city),
    is_example = endsWith(email, "example.com"),
    starts_a = startsWith(city, "A")
  ) |>
  select(city_upper, email, is_example, starts_a) |>
  collect()

## ----string-grepl-------------------------------------------------------------
tbl(fs) |>
  mutate(has_at = grepl("@example", email)) |>
  select(email, has_at) |>
  collect()

## ----string-gsub--------------------------------------------------------------
tbl(fs) |>
  mutate(domain = gsub(".*@", "", email, fixed = FALSE)) |>
  select(email, domain) |>
  collect()

## ----string-extract-----------------------------------------------------------
tbl(fs) |>
  mutate(user = str_extract(email, "^[^@]+")) |>
  select(email, user) |>
  collect()

## ----string-paste-------------------------------------------------------------
tbl(fs) |>
  mutate(
    greeting = paste0("Hello, ", trimws(name), "!"),
    label = paste(trimws(name), city, sep = " - ")
  ) |>
  select(greeting, label) |>
  collect()

## ----summarise-basic----------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(
    count = n(),
    avg_mpg = mean(mpg),
    total_hp = sum(hp),
    best_mpg = max(mpg)
  ) |>
  collect()

## ----summarise-advanced-------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(
    mpg_sd = sd(mpg),
    mpg_var = var(mpg),
    first_hp = first(hp),
    last_hp = last(hp)
  ) |>
  collect()

## ----summarise-median---------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(
    med_mpg = median(mpg),
    unique_gears = n_distinct(gear)
  ) |>
  collect()

## ----count--------------------------------------------------------------------
tbl(f) |>
  count(cyl, sort = TRUE) |>
  collect()

## ----tally--------------------------------------------------------------------
tbl(f) |>
  group_by(gear) |>
  tally() |>
  collect()

## ----across-summarise---------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(across(c(mpg, hp, wt), mean)) |>
  collect()

## ----across-multi-------------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(across(
    c(mpg, hp),
    list(avg = mean, total = sum),
    .names = "{.col}_{.fn}"
  )) |>
  collect()

## ----ungroup------------------------------------------------------------------
tbl(f) |>
  group_by(cyl, gear) |>
  summarise(n = n(), .groups = "keep") |>
  ungroup() |>
  arrange(desc(n)) |>
  collect()

## ----arrange------------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  arrange(cyl, desc(mpg)) |>
  collect() |>
  head(8)

## ----slice-head---------------------------------------------------------------
tbl(f) |>
  slice_head(n = 5) |>
  collect()

## ----slice-min----------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  slice_min(order_by = mpg, n = 3) |>
  collect()

## ----slice-no-ties------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl) |>
  slice_min(order_by = cyl, n = 3, with_ties = FALSE) |>
  collect()

## ----slice-max----------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  slice_max(order_by = hp, n = 4, with_ties = FALSE) |>
  collect()

## ----join-setup---------------------------------------------------------------
cyl_info <- data.frame(
  cyl = c(4, 6, 8),
  engine_type = c("inline", "v-type", "v-type"),
  stringsAsFactors = FALSE
)
f_cyl <- tempfile(fileext = ".vtr")
write_vtr(cyl_info, f_cyl)

## ----left-join----------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  left_join(tbl(f_cyl), by = "cyl") |>
  collect() |>
  head()

## ----semi-anti----------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl) |>
  anti_join(
    tbl(f_cyl) |> filter(engine_type == "v-type"),
    by = "cyl"
  ) |>
  collect() |>
  head()

## ----join-named---------------------------------------------------------------
ratings <- data.frame(
  cylinders = c(4, 6, 8),
  rating = c("A", "B", "C"),
  stringsAsFactors = FALSE
)
f_rat <- tempfile(fileext = ".vtr")
write_vtr(ratings, f_rat)

tbl(f) |>
  select(mpg, cyl) |>
  inner_join(tbl(f_rat), by = c("cyl" = "cylinders")) |>
  collect() |>
  head()

## ----fuzzy-join---------------------------------------------------------------
ref_species <- data.frame(
  canonical = c("Quercus robur", "Quercus petraea",
                 "Fagus sylvatica"),
  code = c("QR", "QP", "FS"),
  stringsAsFactors = FALSE
)
query_species <- data.frame(
  name = c("Quercus robur", "Qurecus petraea",
           "Fagus sylvatca"),
  stringsAsFactors = FALSE
)
f_ref <- tempfile(fileext = ".vtr")
f_query <- tempfile(fileext = ".vtr")
write_vtr(ref_species, f_ref)
write_vtr(query_species, f_query)

tbl(f_query) |>
  fuzzy_join(
    tbl(f_ref),
    by = c("name" = "canonical"),
    method = "dl",
    max_dist = 0.15
  ) |>
  collect()

## ----window-rank--------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  slice_head(n = 8) |>
  mutate(
    rn = row_number(),
    mpg_rank = rank(mpg),
    mpg_dense = dense_rank(mpg)
  ) |>
  collect()

## ----window-lag-lead----------------------------------------------------------
tbl(f) |>
  select(mpg, hp) |>
  slice_head(n = 6) |>
  mutate(
    prev_mpg = lag(mpg),
    next_mpg = lead(mpg),
    prev2_hp = lag(hp, n = 2, default = 0)
  ) |>
  collect()

## ----window-cum---------------------------------------------------------------
tbl(f) |>
  select(mpg, hp) |>
  slice_head(n = 6) |>
  mutate(
    running_hp = cumsum(hp),
    running_avg = cummean(mpg),
    running_min = cummin(mpg)
  ) |>
  collect()

## ----window-grouped-----------------------------------------------------------
tbl(f) |>
  select(mpg, cyl) |>
  group_by(cyl) |>
  mutate(rn = row_number(), pct = percent_rank(mpg)) |>
  slice_head(n = 10) |>
  collect()

## ----date-data----------------------------------------------------------------
events <- data.frame(
  event_date = as.Date(c("2020-03-15", "2020-07-01",
                          "2021-01-15", "2021-06-30")),
  event_time = as.POSIXct(c("2020-03-15 09:30:00",
                             "2020-07-01 14:00:00",
                             "2021-01-15 08:15:00",
                             "2021-06-30 17:45:00"),
                           tz = "UTC"),
  value = c(10, 20, 30, 40)
)
fd <- tempfile(fileext = ".vtr")
write_vtr(events, fd)

## ----date-extract-------------------------------------------------------------
tbl(fd) |>
  mutate(
    yr = year(event_date),
    mo = month(event_date),
    dy = day(event_date)
  ) |>
  group_by(yr) |>
  summarise(total = sum(value)) |>
  collect()

## ----time-extract-------------------------------------------------------------
tbl(fd) |>
  mutate(
    hr = hour(event_time),
    mn = minute(event_time)
  ) |>
  select(event_time, hr, mn) |>
  collect()

## ----date-filter--------------------------------------------------------------
tbl(fd) |>
  filter(event_date >= as.Date("2021-01-01")) |>
  collect()

## ----date-arith---------------------------------------------------------------
tbl(fd) |>
  mutate(plus_30 = event_date + 30) |>
  select(event_date, plus_30) |>
  collect()

## ----similarity-data----------------------------------------------------------
species <- data.frame(
  name = c("Quercus robur", "Quercus rubra",
           "Fagus sylvatica", "Acer platanoides",
           "Quercus petraea"),
  stringsAsFactors = FALSE
)
fs2 <- tempfile(fileext = ".vtr")
write_vtr(species, fs2)

## ----similarity-metrics-------------------------------------------------------
tbl(fs2) |>
  mutate(
    lev = levenshtein(name, "Quercus robur"),
    dl = dl_dist(name, "Quercus robur"),
    jw = jaro_winkler(name, "Quercus robur")
  ) |>
  filter(lev <= 5) |>
  arrange(lev) |>
  collect()

## ----similarity-norm----------------------------------------------------------
tbl(fs2) |>
  mutate(
    lev_norm = levenshtein_norm(name, "Quercus robur"),
    dl_norm = dl_dist_norm(name, "Quercus robur")
  ) |>
  collect()

## ----dl-transposition---------------------------------------------------------
tbl(fs2) |>
  mutate(
    lev = levenshtein(name, "Qurecus robur"),
    dl = dl_dist(name, "Qurecus robur")
  ) |>
  collect()

## ----resolve------------------------------------------------------------------
taxa <- data.frame(
  id        = c(1L, 2L, 3L, 4L),
  name      = c("Fagaceae", "Quercus", "Q. robur", "Q. petraea"),
  parent_id = c(NA, 1L, 2L, 2L),
  stringsAsFactors = FALSE
)
ft <- tempfile(fileext = ".vtr")
write_vtr(taxa, ft)

tbl(ft) |>
  mutate(parent_name = resolve(parent_id, id, name)) |>
  collect()

## ----propagate----------------------------------------------------------------
tbl(ft) |>
  mutate(family = propagate(
    parent_id, id,
    if_else(is.na(parent_id), name, NA_character_)
  )) |>
  collect()

## ----csv-roundtrip------------------------------------------------------------
csv_in <- tempfile(fileext = ".csv")
write.csv(mtcars, csv_in, row.names = FALSE)

tbl_csv(csv_in) |>
  filter(cyl == 6) |>
  select(mpg, cyl, hp) |>
  collect()

## ----sqlite-roundtrip---------------------------------------------------------
db <- tempfile(fileext = ".sqlite")
f_src <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f_src)
tbl(f_src) |> write_sqlite(db, "cars")

tbl_sqlite(db, "cars") |>
  filter(mpg > 25) |>
  collect()

## ----format-conversion--------------------------------------------------------
csv_file <- tempfile(fileext = ".csv")
vtr_file <- tempfile(fileext = ".vtr")
csv_out <- tempfile(fileext = ".csv")

write.csv(mtcars, csv_file, row.names = FALSE)
tbl_csv(csv_file) |> write_vtr(vtr_file)

tbl(vtr_file) |>
  filter(cyl == 6) |>
  write_csv(csv_out)

read.csv(csv_out) |> head()

## ----index-create-------------------------------------------------------------
f_idx <- tempfile(fileext = ".vtr")
write_vtr(
  data.frame(id = letters, val = 1:26, stringsAsFactors = FALSE),
  f_idx,
  batch_size = 5
)

has_index(f_idx, "id")  # FALSE
create_index(f_idx, "id")
has_index(f_idx, "id")  # TRUE

## ----index-query--------------------------------------------------------------
tbl(f_idx) |>
  filter(id == "m") |>
  collect()

## ----index-composite----------------------------------------------------------
f_comp <- tempfile(fileext = ".vtr")
write_vtr(
  data.frame(
    region = rep(c("north", "south"), each = 13),
    id = letters,
    val = 1:26,
    stringsAsFactors = FALSE
  ),
  f_comp,
  batch_size = 5
)
create_index(f_comp, c("region", "id"))

tbl(f_comp) |>
  filter(region == "north", id == "c") |>
  collect()

## ----append-------------------------------------------------------------------
fa <- tempfile(fileext = ".vtr")
write_vtr(mtcars[1:16, ], fa)
append_vtr(mtcars[17:32, ], fa)
tbl(fa) |> collect() |> nrow()

## ----delete-------------------------------------------------------------------
delete_vtr(fa, c(0, 1, 2))  # 0-based row indices
tbl(fa) |> collect() |> nrow()
unlink(c(fa, paste0(fa, ".del")))

## ----diff---------------------------------------------------------------------
fd1 <- tempfile(fileext = ".vtr")
fd2 <- tempfile(fileext = ".vtr")
old <- data.frame(id = 1:5, val = letters[1:5],
                  stringsAsFactors = FALSE)
new <- data.frame(id = c(3L, 4L, 5L, 6L, 7L),
                  val = c("C", "d", "e", "f", "g"),
                  stringsAsFactors = FALSE)
write_vtr(old, fd1)
write_vtr(new, fd2)

d <- diff_vtr(fd1, fd2, "id")
d$deleted
collect(d$added)
unlink(c(fd1, fd2))

## ----block-materialize--------------------------------------------------------
blk_data <- data.frame(
  taxonID = c("T1", "T2", "T3", "T4", "T5"),
  name = c("Quercus robur", "Pinus sylvestris",
           "Fagus sylvatica", "Acer campestre",
           "Betula pendula"),
  stringsAsFactors = FALSE
)
f_blk <- tempfile(fileext = ".vtr")
write_vtr(blk_data, f_blk)

blk <- materialize(tbl(f_blk))
blk

## ----block-lookup-------------------------------------------------------------
block_lookup(blk, "name", c("Quercus robur", "Betula pendula"))

## ----block-fuzzy--------------------------------------------------------------
block_fuzzy_lookup(
  blk, "name",
  c("Qurecus robur", "Pinus silvestris"),
  method = "dl",
  max_dist = 0.2
)

## ----explain-full-------------------------------------------------------------
tbl(f) |>
  filter(cyl > 4) |>
  select(mpg, cyl, hp) |>
  arrange(desc(mpg)) |>
  explain()

## ----glimpse------------------------------------------------------------------
tbl(f) |> glimpse()

## ----cleanup------------------------------------------------------------------
unlink(c(f, f_batched, f_na, fs, fs2, f_cyl, f_rat, f_ref, f_query, fd,
         ft, csv_in, csv_out, csv_file, vtr_file, db, f_src, f_idx,
         paste0(f_idx, ".id.vtri"), f_comp,
         paste0(f_comp, ".region_id.vtri"), f_blk))

