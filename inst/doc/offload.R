## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(1)
has_biglm <- requireNamespace("biglm", quietly = TRUE)
library(vectra)

## ----fold-setup---------------------------------------------------------------
f <- tempfile(fileext = ".vtr")
df <- data.frame(g = rep(c("a", "b", "c"), length.out = 6000),
                 wt = rnorm(6000, 3, 1), hp = rnorm(6000, 150, 40))
df$mpg <- 30 - 5 * df$wt - 0.05 * df$hp + rnorm(6000, 0, 1)
write_vtr(df, f)

## ----fold-count---------------------------------------------------------------
collect_chunked(tbl(f), function(acc, chunk) acc + nrow(chunk), .init = 0L)

## ----fold-ols-----------------------------------------------------------------
acc <- collect_chunked(
  tbl(f) |> select(mpg, wt, hp),
  function(acc, chunk) {
    X <- cbind(1, chunk$wt, chunk$hp); y <- chunk$mpg
    list(XtX = acc$XtX + crossprod(X), Xty = acc$Xty + crossprod(X, y))
  },
  .init = list(XtX = matrix(0, 3, 3), Xty = matrix(0, 3, 1))
)
drop(solve(acc$XtX, acc$Xty))
coef(lm(mpg ~ wt + hp, data = df))

## ----offload-identity---------------------------------------------------------
s <- offload(tbl(f) |> select(mpg, wt, hp))

identical(collect(s), collect(tbl(f) |> select(mpg, wt, hp)))
s

## ----offload-bigglm, eval = has_biglm-----------------------------------------
s <- offload(tbl(f) |> select(mpg, wt, hp))
fit <- biglm::bigglm(mpg ~ wt + hp, data = chunk_feeder(s),
                     family = gaussian())
coef(fit)

## ----offload-bigglm-note, echo = FALSE, results = "asis", eval = !has_biglm----
# cat("> The out-of-core GLM example needs the **biglm** package, which is not",
#     "installed, so it was not run here.\n")

## ----arrange------------------------------------------------------------------
tbl(f) |> arrange(desc(mpg)) |> head(3) |> collect()

## ----partition----------------------------------------------------------------
p <- offload(tbl(f), by = "g")
p

## ----partition-fits-----------------------------------------------------------
fits <- group_map(p, function(d, g) coef(lm(mpg ~ wt + hp, data = d)))
fits

## ----partition-modify---------------------------------------------------------
group_modify(p, function(d, g)
  data.frame(n = nrow(d), mean_mpg = mean(d$mpg)))

## ----partition-reduce---------------------------------------------------------
accumulate <- function(acc, chunk) {
  X <- cbind(1, chunk$wt, chunk$hp); y <- chunk$mpg
  list(XtX = acc$XtX + crossprod(X), Xty = acc$Xty + crossprod(X, y))
}
combine <- function(a, b) list(XtX = a$XtX + b$XtX, Xty = a$Xty + b$Xty)

acc <- collect_chunked(
  offload(tbl(f) |> select(g, mpg, wt, hp), by = "g"),
  accumulate,
  .init = list(XtX = matrix(0, 3, 3), Xty = matrix(0, 3, 1)),
  combine = combine, commutative = TRUE
)
drop(solve(acc$XtX, acc$Xty))
coef(lm(mpg ~ wt + hp, data = df))

## ----cleanup, include = FALSE-------------------------------------------------
unlink(f)

