library(ggplot2)

manpose <- data.frame(
  x = 1, y = 1, scale = 0.5, ratioxy = 1,
  angleofspine = -pi / 2,
  anglerighthumerus = -pi / 6, anglelefthumerus = pi + pi / 6,
  anglerightradius = 0, angleleftradius = pi,
  anglerightleg = 3 * pi / 2 - pi / 12, angleleftleg = 3 * pi / 2 + pi / 12,
  angleofneck = 3 * pi / 2
)

mapping <- aes(x = x, y = y, scale = scale, ratioxy = ratioxy,
               angleofspine = angleofspine,
               anglerighthumerus = anglerighthumerus,
               anglelefthumerus = anglelefthumerus,
               anglerightradius = anglerightradius,
               angleleftradius = angleleftradius,
               anglerightleg = anglerightleg,
               angleleftleg = angleleftleg,
               angleofneck = angleofneck)

test_that("xkcdman emits two layers, not one per bone", {
  layers <- xkcdman(mapping, manpose)
  expect_length(layers, 2)
  expect_s3_class(layers[[1]], "Layer")
  expect_s3_class(layers[[2]], "Layer")
})

test_that("the bone layer holds seven segments per figure", {
  layers <- xkcdman(mapping, manpose)
  expect_equal(nrow(layers[[1]]$data), 7)
  expect_equal(nrow(layers[[2]]$data), 1)
})

test_that("a stick figure plot builds", {
  p <- ggplot() + xkcdman(mapping, manpose) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
  expect_no_error(ggplot_build(p))
})

test_that("several figures at once are supported", {
  two <- rbind(manpose, manpose)
  two$x <- c(1, 2)
  layers <- xkcdman(mapping, two)
  expect_equal(nrow(layers[[1]]$data), 14)
  expect_equal(nrow(layers[[2]]$data), 2)
})

test_that("xkcdman no longer routes through the deprecated xkcdline", {
  expect_no_warning(xkcdman(mapping, manpose))
})

test_that("a seeded figure builds identically twice", {
  mk <- function() {
    p <- ggplot() + xkcdman(mapping, manpose, seed = 4) +
      coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
    ggplot_build(p)$data
  }
  expect_identical(mk(), mk())
})
