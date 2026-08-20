library(ggplot2)

seg <- data.frame(x = 0, y = 0, xend = 1, yend = 1)
circ <- data.frame(x = 0, y = 0, diameter = 1)
segmapping <- aes(x = x, y = y, xend = xend, yend = yend)

test_that("xkcdline is deprecated", {
  expect_warning(xkcdline(segmapping, seg), "deprecated")
})

# xkcdline() returns one element per input row, each a list of layers.
layers_of <- function(out) unlist(out, recursive = TRUE)

test_that("segments still produce usable layers", {
  out <- suppressWarnings(xkcdline(segmapping, seg))
  expect_type(out, "list")
  expect_length(out, nrow(seg))
  expect_true(all(vapply(layers_of(out), inherits, logical(1), "Layer")))
})

test_that("circles still produce usable layers", {
  out <- suppressWarnings(
    xkcdline(aes(x = x, y = y, diameter = diameter), circ,
             typexkcdline = "circunference")
  )
  expect_true(all(vapply(layers_of(out), inherits, logical(1), "Layer")))
})

test_that("an unknown line type is rejected", {
  expect_error(
    suppressWarnings(xkcdline(segmapping, seg, typexkcdline = "triangle")),
    "segment or circle"
  )
})

test_that("mask adds a second layer per segment", {
  masked <- suppressWarnings(xkcdline(segmapping, seg, mask = TRUE))
  bare <- suppressWarnings(xkcdline(segmapping, seg, mask = FALSE))
  expect_length(masked[[1]], 2)
  expect_length(bare[[1]], 1)
})

test_that("a legacy plot still builds", {
  p <- ggplot() + suppressWarnings(xkcdline(segmapping, seg))
  expect_no_error(ggplot_build(p))
})

test_that("mask_linewidth is at least 1 and doubles thereafter", {
  expect_equal(xkcd:::mask_linewidth(0.1), 1)
  expect_equal(xkcd:::mask_linewidth(2), 4)
})
