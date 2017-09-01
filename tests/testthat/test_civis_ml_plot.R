library(civis)
context("civis_ml_plot")

model_list    <- readRDS("data/civis_ml_models.rds")
plotable_mods <- model_list[1:10]
err_mod       <- model_list[11:12]
is_classif    <- sapply(plotable_mods, function(m) is(m, "civis_ml_classifier"))

test_that("decile plot for classification is produced", {
  ps <- lapply(plotable_mods[is_classif], function(m) plot(m))
  expect_true(all(sapply(ps, is, "ggplot")))

  plot_has_facets <- function(p) !is.null(p$facet)
  expect_true(all(sapply(ps, plot_has_facets)))

  plot_has_bars <- function(p) is(purrr::pluck(p, "layers", 1, "geom"), "GeomBar")
  expect_true(all(sapply(ps, plot_has_bars)))

  plot_has_x_decile <- function(p) {
    all(p$data[[as.character(p$mapping$x)]] %in% 1:10)
  }
  expect_true(all(sapply(ps, plot_has_x_decile)))
})

ps <- lapply(plotable_mods[!is_classif], function(m) plot(m))

test_that("multi output plot throws error for hist and plot", {
  msg <- "Plotting data not available."
  e <- tryCatch(plot(err_mod[[1]]), error = function(e) e)
  expect_equal(e$message, msg)

  e <- tryCatch(plot(err_mod[[2]]), error = function(e) e)
  expect_equal(e$message, msg)

  msg <- "Histogram data not available."
  e <- tryCatch(hist(err_mod[[1]]), error = function(e) e)
  expect_equal(e$message, msg)

  e <- tryCatch(hist(err_mod[[2]]), error = function(e) e)
  expect_equal(e$message, msg)
})

test_that("y_yhat plot for reg is produced", {
  expect_true(all(sapply(ps, is, "ggplot")))

  plot_has_bins <- function(p) is(purrr::pluck(p, "layers", 1, "geom"), "GeomTile")
  expect_true(all(sapply(ps, plot_has_bins)))

  plot_vals_correct <- function(p, m) {
    var_name <- as.character(p$mapping$fill)
    all.equal(c(m$metrics$metrics$y_yhat_plot$values),
              p$data[[var_name]])
  }
  ms <- plotable_mods[!is_classif]
  expect_true(all(mapply(plot_vals_correct, ps, ms), fill = T))
})

test_that("hist is produced", {
  hs <- lapply(plotable_mods, function(m) hist(m))

  expect_true(all(sapply(hs, is, "ggplot")))

  plot_has_bars <- function(p) is(purrr::pluck(p, "layers", 1, "geom"), "GeomBar")
  expect_true(all(sapply(hs, plot_has_bars)))
})


