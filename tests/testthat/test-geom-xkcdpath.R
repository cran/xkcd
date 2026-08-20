library(ggplot2)

seg <- data.frame(x = 0, y = 0, xend = 1, yend = 1)
circ <- data.frame(x = 0, y = 0, diameter = 1)

test_that("geom_xkcdpath returns a layer", {
  expect_s3_class(geom_xkcdpath(aes(x = x, y = y, xend = xend, yend = yend),
                                data = seg),
                  "Layer")
})

test_that("a segment plot builds", {
  p <- ggplot() +
    geom_xkcdpath(aes(x = x, y = y, xend = xend, yend = yend), data = seg)
  expect_no_error(ggplot_build(p))
})

test_that("a circle plot builds", {
  p <- ggplot() +
    geom_xkcdpath(aes(x = x, y = y, diameter = diameter), data = circ)
  expect_no_error(ggplot_build(p))
})

test_that("ratioxy is a real aesthetic with a default", {
  expect_true("ratioxy" %in% names(GeomXkcdPath$default_aes))
  built <- ggplot_build(
    ggplot() + geom_xkcdpath(aes(x = x, y = y, diameter = diameter), data = circ)
  )
  expect_equal(built$data[[1]]$ratioxy[1], 1)
})

test_that("ratioxy passed as a layer parameter reaches the geom", {
  # Regression: previously ratioxy fell into ... and was silently dropped,
  # so circles rendered elliptical.
  narrow <- ggplot_build(ggplot() +
    geom_xkcdpath(aes(x = x, y = y, diameter = diameter), data = circ,
                  ratioxy = 1, seed = 3))
  wide <- ggplot_build(ggplot() +
    geom_xkcdpath(aes(x = x, y = y, diameter = diameter), data = circ,
                  ratioxy = 5, seed = 3))
  expect_equal(narrow$data[[1]]$ratioxy[1], 1)
  expect_equal(wide$data[[1]]$ratioxy[1], 5)
})

test_that("mask toggles the number of grobs drawn", {
  build_grob <- function(mask) {
    p <- ggplot() +
      geom_xkcdpath(aes(x = x, y = y, xend = xend, yend = yend), data = seg,
                    mask = mask, seed = 1)
    layer_grob(p)[[1]]
  }
  expect_length(build_grob(TRUE)$children, 2)
  expect_length(build_grob(FALSE)$children, 1)
})

test_that("wobble = 0 draws a straight segment", {
  # The path is expanded in draw_panel(), so inspect the grob rather than
  # ggplot_build()$data (which still holds the single unexpanded input row).
  straight <- ggplot() +
    geom_xkcdpath(aes(x = x, y = y, xend = xend, yend = yend), data = seg,
                  wobble = 0, mask = FALSE)
  wobbly <- ggplot() +
    geom_xkcdpath(aes(x = x, y = y, xend = xend, yend = yend), data = seg,
                  wobble = 1, mask = FALSE, seed = 1)
  npoints <- function(p) length(layer_grob(p)[[1]]$children[[1]]$x)
  expect_equal(npoints(straight), 2)
  expect_gt(npoints(wobbly), 2)
})

test_that("a seeded layer builds identically twice", {
  mk <- function() ggplot_build(ggplot() +
    geom_xkcdpath(aes(x = x, y = y, xend = xend, yend = yend), data = seg,
                  seed = 77))$data[[1]]
  expect_identical(mk(), mk())
})

test_that("empty data does not error", {
  empty <- seg[0, ]
  p <- ggplot() +
    geom_xkcdpath(aes(x = x, y = y, xend = xend, yend = yend), data = empty)
  expect_no_error(ggplot_build(p))
})
