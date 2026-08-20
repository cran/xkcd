test_that("the same seed gives identical segments", {
  a <- xkcd:::pointssegment(x = 0, y = 0, xend = 1, yend = 1,
                            xjitteramount = 0.1, yjitteramount = 0.1, seed = 99)
  b <- xkcd:::pointssegment(x = 0, y = 0, xend = 1, yend = 1,
                            xjitteramount = 0.1, yjitteramount = 0.1, seed = 99)
  expect_identical(a, b)
})

test_that("different seeds give different segments", {
  a <- xkcd:::pointssegment(x = 0, y = 0, xend = 1, yend = 1,
                            xjitteramount = 0.1, yjitteramount = 0.1, seed = 1)
  b <- xkcd:::pointssegment(x = 0, y = 0, xend = 1, yend = 1,
                            xjitteramount = 0.1, yjitteramount = 0.1, seed = 2)
  expect_false(isTRUE(all.equal(a, b)))
})

test_that("the same seed gives identical circles", {
  a <- xkcd:::pointscircunference(x = 0, y = 0, diameter = 1, seed = 5)
  b <- xkcd:::pointscircunference(x = 0, y = 0, diameter = 1, seed = 5)
  expect_identical(a, b)
})

test_that("seeding does not disturb the global RNG", {
  set.seed(2024)
  before <- .Random.seed
  xkcd:::pointssegment(x = 0, y = 0, xend = 1, yend = 1,
                       xjitteramount = 0.1, yjitteramount = 0.1, seed = 123)
  xkcd:::pointscircunference(x = 0, y = 0, diameter = 1, seed = 123)
  expect_identical(.Random.seed, before)
})

test_that("without a seed the RNG advances as usual", {
  set.seed(2024)
  before <- .Random.seed
  xkcd:::pointssegment(x = 0, y = 0, xend = 1, yend = 1,
                       xjitteramount = 0.1, yjitteramount = 0.1)
  expect_false(identical(.Random.seed, before))
})
