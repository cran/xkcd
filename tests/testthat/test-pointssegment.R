test_that("endpoints are preserved exactly", {
  d <- xkcd:::pointssegment(x = 0, y = 0, xend = 10, yend = 5,
                            xjitteramount = 0.5, yjitteramount = 0.5)
  expect_equal(d$x[1], 0)
  expect_equal(d$y[1], 0)
  expect_equal(d$x[nrow(d)], 10)
  expect_equal(d$y[nrow(d)], 5)
})

test_that("no jitter collapses to a two-point straight segment", {
  d <- xkcd:::pointssegment(x = 0, y = 0, xend = 10, yend = 5,
                            xjitteramount = 0, yjitteramount = 0)
  expect_equal(nrow(d), 2)
  expect_equal(d$x, c(0, 10))
  expect_equal(d$y, c(0, 5))
})

test_that("npoints below 2 is an error", {
  expect_error(
    xkcd:::pointssegment(x = 0, y = 0, xend = 1, yend = 1, npoints = 1),
    "npoints must be greater than 1"
  )
})

test_that("vertical segments are handled without dividing by zero", {
  d <- xkcd:::pointssegment(x = 2, y = 0, xend = 2, yend = 8,
                            yjitteramount = 0.1)
  expect_true(all(is.finite(d$x)))
  expect_true(all(is.finite(d$y)))
  expect_equal(d$y[1], 0)
  expect_equal(d$y[nrow(d)], 8)
})
