## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----intro-demo---------------------------------------------------------------
library(vectra)

# Write mtcars to .vtr, then read it back lazily
f <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f)
node <- tbl(f)
node

## ----vtr-roundtrip------------------------------------------------------------
f <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f)

tbl(f) |>
  filter(cyl == 6) |>
  select(mpg, cyl, hp) |>
  collect()

## ----vtr-batch-size-----------------------------------------------------------
f <- tempfile(fileext = ".vtr")
csv <- tempfile(fileext = ".csv")
write.csv(mtcars, csv, row.names = FALSE)

# Convert CSV to .vtr with 10-row row groups
tbl_csv(csv) |> write_vtr(f, batch_size = 10)

# The file now has multiple row groups
tbl(f) |> collect() |> nrow()

## ----csv-read-----------------------------------------------------------------
csv <- tempfile(fileext = ".csv")
write.csv(mtcars, csv, row.names = FALSE)

tbl_csv(csv) |>
  filter(hp > 200) |>
  select(mpg, hp, wt) |>
  collect()

## ----csv-write----------------------------------------------------------------
f <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f)

out_csv <- tempfile(fileext = ".csv")
tbl(f) |>
  filter(cyl == 4) |>
  write_csv(out_csv)

# Verify the output
read.csv(out_csv) |> head()

## ----sqlite-read--------------------------------------------------------------
db <- tempfile(fileext = ".sqlite")
write_sqlite(mtcars, db, "cars")

tbl_sqlite(db, "cars") |>
  filter(mpg > 25) |>
  select(mpg, cyl, wt) |>
  collect()

## ----sqlite-write-------------------------------------------------------------
f <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f)

db <- tempfile(fileext = ".sqlite")
tbl(f) |>
  filter(cyl == 8) |>
  write_sqlite(db, "v8_cars")

# Read it back through vectra
tbl_sqlite(db, "v8_cars") |> collect()

## ----convert-csv-vtr----------------------------------------------------------
csv <- tempfile(fileext = ".csv")
write.csv(mtcars, csv, row.names = FALSE)

vtr <- tempfile(fileext = ".vtr")
tbl_csv(csv) |> write_vtr(vtr)

tbl(vtr) |> collect() |> head()

## ----convert-filtered---------------------------------------------------------
csv <- tempfile(fileext = ".csv")
write.csv(mtcars, csv, row.names = FALSE)

vtr <- tempfile(fileext = ".vtr")
tbl_csv(csv) |>
  filter(mpg > 20) |>
  mutate(kpl = mpg * 0.425144) |>
  write_vtr(vtr)

tbl(vtr) |> collect()

## ----etl-pipeline-------------------------------------------------------------
# CSV -> filter + transform -> SQLite
csv <- tempfile(fileext = ".csv")
write.csv(mtcars, csv, row.names = FALSE)

db <- tempfile(fileext = ".sqlite")
tbl_csv(csv) |>
  filter(cyl >= 6) |>
  select(mpg, cyl, hp, wt) |>
  mutate(power_weight = hp / wt) |>
  write_sqlite(db, "powerful_cars")

# SQLite -> VTR
vtr <- tempfile(fileext = ".vtr")
tbl_sqlite(db, "powerful_cars") |> write_vtr(vtr)

tbl(vtr) |> collect()

## ----join-across-formats------------------------------------------------------
f1 <- tempfile(fileext = ".vtr")
f2 <- tempfile(fileext = ".csv")

cars_main <- mtcars[, c("mpg", "cyl", "hp")]
cars_extra <- data.frame(cyl = c(4, 6, 8), label = c("small", "mid", "big"))

write_vtr(cars_main, f1)
write.csv(cars_extra, f2, row.names = FALSE)

tbl(f1) |>
  left_join(tbl_csv(f2), by = "cyl") |>
  collect() |>
  head()

## ----batch-size-effect--------------------------------------------------------
csv <- tempfile(fileext = ".csv")
big <- data.frame(
  id = seq_len(1000),
  value = rnorm(1000)
)
write.csv(big, csv, row.names = FALSE)

# Small row groups: more granular zone maps
f_small <- tempfile(fileext = ".vtr")
tbl_csv(csv) |> write_vtr(f_small, batch_size = 100)

# Default: single row group for 1000 rows
f_default <- tempfile(fileext = ".vtr")
tbl_csv(csv) |> write_vtr(f_default)

cat("Small batches:", file.size(f_small), "bytes\n")
cat("Default:      ", file.size(f_default), "bytes\n")

