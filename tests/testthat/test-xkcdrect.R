library(ggplot2)

rects <- data.frame(year = 2007:2011,
                    number = c(56470, 56998, 59686, 61783, 64251),
                    grp = c("a", "a", "b", "b", "b"))

basic_aes <- aes(xmin = year - 0.2, xmax = year + 0.2,
                 ymin = number - 500, ymax = number + 500)

test_that("xkcdrect returns a single layer", {
  layer <- xkcdrect(basic_aes, data = rects)
  expect_s3_class(layer, "Layer")
})

test_that("a rectangle plot builds", {
  p <- ggplot() + xkcdrect(basic_aes, data = rects)
  expect_no_error(ggplot_build(p))
})

test_that("a missing required aesthetic is reported", {
  p <- ggplot() +
    xkcdrect(aes(xmin = year - 0.2, xmax = year + 0.2, ymin = number - 500),
             data = rects)
  expect_error(ggplot_build(p), "ymax")
})

test_that("aesthetics are inherited from the plot", {
  # Regression: the old implementation evaluated its own mapping and ignored
  # plot-level aes() entirely.
  p <- ggplot(rects, basic_aes) + xkcdrect()
  expect_no_error(ggplot_build(p))
})

test_that("faceting on a variable works", {
  # Regression: faceting used to break because the mapping was evaluated up front.
  p <- ggplot(rects, basic_aes) + xkcdrect() + facet_wrap(~grp)
  built <- ggplot_build(p)
  expect_equal(length(unique(built$data[[1]]$PANEL)), 2)
})

test_that("the .data pronoun works", {
  # Regression: .data used to fail inside the hand-rolled eval_tidy() calls.
  p <- ggplot() +
    xkcdrect(aes(xmin = .data$year - 0.2, xmax = .data$year + 0.2,
                 ymin = .data$number - 500, ymax = .data$number + 500),
             data = rects)
  expect_no_error(ggplot_build(p))
})

test_that("fill, colour and linewidth reach the built layer", {
  built <- ggplot_build(ggplot() +
    xkcdrect(basic_aes, data = rects,
             fill = "pink", colour = "navy", linewidth = 1.2))
  d <- built$data[[1]]
  expect_equal(unique(d$fill), "pink")
  expect_equal(unique(d$colour), "navy")
  expect_equal(unique(d$linewidth), 1.2)
})

test_that("fill can be mapped to a variable", {
  built <- ggplot_build(ggplot(rects) +
    xkcdrect(aes(xmin = year - 0.2, xmax = year + 0.2,
                 ymin = number - 500, ymax = number + 500, fill = grp)))
  expect_equal(length(unique(built$data[[1]]$fill)), 2)
})

test_that("the drawn grob holds a fill and a border", {
  p <- ggplot() + xkcdrect(basic_aes, data = rects, seed = 1)
  g <- layer_grob(p)[[1]]
  expect_length(g$children, 2)
})

test_that("deprecated arguments still work but warn", {
  expect_warning(xkcdrect(basic_aes, data = rects, fillcolour = "pink"),
                 "fillcolour")
  expect_warning(xkcdrect(basic_aes, data = rects, bordercolour = "navy"),
                 "bordercolour")
  expect_warning(xkcdrect(basic_aes, data = rects, borderlinewidth = 2),
                 "borderlinewidth")
})

test_that("a deprecated argument maps onto the standard aesthetic", {
  layer <- suppressWarnings(
    xkcdrect(basic_aes, data = rects, fillcolour = "pink")
  )
  built <- ggplot_build(ggplot() + layer)
  expect_equal(unique(built$data[[1]]$fill), "pink")
})

test_that("a seeded rectangle builds identically twice", {
  mk <- function() {
    p <- ggplot() + xkcdrect(basic_aes, data = rects, seed = 3)
    layer_grob(p)[[1]]$children[[2]]$x
  }
  expect_identical(mk(), mk())
})

test_that("wobble = 0 draws straight borders", {
  p <- ggplot() + xkcdrect(basic_aes, data = rects, wobble = 0)
  # Four straight edges of two points each, per rectangle
  expect_length(layer_grob(p)[[1]]$children[[2]]$x, nrow(rects) * 4 * 2)
})
