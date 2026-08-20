test_that("a circle returns a bezier-smoothed loop", {
  d <- xkcd:::pointscircunference(x = 0, y = 0, diameter = 2, seed = 1)
  expect_true(nrow(d) > 16)
  expect_named(d, c("x", "y"))
  expect_true(all(is.finite(d$x)))
  expect_true(all(is.finite(d$y)))
})

test_that("the circle is centred on x and y", {
  d <- xkcd:::pointscircunference(x = 5, y = -3, diameter = 2, seed = 42)
  expect_equal(mean(range(d$x)), 5, tolerance = 0.3)
  expect_equal(mean(range(d$y)), -3, tolerance = 0.3)
})

test_that("ratioxy stretches x but leaves y alone", {
  round_one <- xkcd:::pointscircunference(x = 0, y = 0, diameter = 2,
                                          ratioxy = 1, seed = 7)
  wide <- xkcd:::pointscircunference(x = 0, y = 0, diameter = 2,
                                     ratioxy = 4, seed = 7)
  expect_gt(diff(range(wide$x)), diff(range(round_one$x)) * 3)
  expect_equal(diff(range(wide$y)), diff(range(round_one$y)), tolerance = 1e-8)
})
