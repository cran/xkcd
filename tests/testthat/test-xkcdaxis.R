library(ggplot2)

xrange <- c(0, 10)
yrange <- c(100, 200)

test_that("xkcdaxis returns axis layers, a coord and a theme", {
  out <- suppressWarnings(xkcdaxis(xrange, yrange))
  expect_type(out, "list")
  expect_length(out, 3)
  expect_s3_class(out[[2]], "CoordCartesian")
  expect_s3_class(out[[3]], "theme")
})

test_that("a plot with xkcd axes builds", {
  p <- ggplot() +
    geom_point(aes(x = mpg, y = wt), data = mtcars) +
    suppressWarnings(xkcdaxis(range(mtcars$mpg), range(mtcars$wt)))
  expect_no_error(ggplot_build(p))
})

test_that("both ranges are required", {
  expect_error(xkcdaxis(NULL, yrange), "xrange, yrange")
  expect_error(xkcdaxis(xrange, NULL), "xrange, yrange")
})

test_that("the coord limits extend past the data range", {
  out <- suppressWarnings(xkcdaxis(xrange, yrange))
  coord <- out[[2]]
  # Jitter is range/50, and the limits pad by 1.5 * that on each side
  expect_lt(coord$limits$x[1], xrange[1])
  expect_gt(coord$limits$x[2], xrange[2])
  expect_lt(coord$limits$y[1], yrange[1])
  expect_gt(coord$limits$y[2], yrange[2])
})

test_that("jitter scales with the size of the range", {
  small <- suppressWarnings(xkcdaxis(c(0, 1), c(0, 1)))
  large <- suppressWarnings(xkcdaxis(c(0, 1000), c(0, 1000)))
  pad <- function(o) diff(o[[2]]$limits$x) - 1
  expect_gt(pad(large), pad(small))
})

test_that("extra arguments reach the axis layers", {
  out <- suppressWarnings(xkcdaxis(xrange, yrange, linewidth = 2))
  axes <- out[[1]]
  expect_true(all(vapply(axes, inherits, logical(1), "Layer")))
})
