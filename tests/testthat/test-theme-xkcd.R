library(ggplot2)

# theme_xkcd() caches the font lookup for the session; clear it between tests so
# each one exercises a known state.
reset_font_cache <- function() {
  rm(list = ls(envir = xkcd:::xkcd_font_cache), envir = xkcd:::xkcd_font_cache)
}

test_that("the font is detected through systemfonts alone", {
  # Regression: theme_xkcd() used to ask only extrafont, whose database is empty
  # unless font_import() was run, so axis text silently lost the xkcd font on
  # machines where the font was installed and working.
  skip_if_not_installed("systemfonts")
  reset_font_cache()
  local_mocked_bindings(fonts = function(...) character(0), .package = "extrafont")
  local_mocked_bindings(font_info = function(...) data.frame(family = "xkcd"),
                        .package = "systemfonts")
  th <- expect_no_warning(theme_xkcd())
  expect_s3_class(th, "theme")
  expect_equal(th$text$family, "xkcd")
  reset_font_cache()
})

test_that("the font is detected through extrafont alone", {
  skip_if_not_installed("systemfonts")
  reset_font_cache()
  local_mocked_bindings(fonts = function(...) "xkcd", .package = "extrafont")
  local_mocked_bindings(font_info = function(...) data.frame(family = "Helvetica"),
                        .package = "systemfonts")
  th <- expect_no_warning(theme_xkcd())
  expect_equal(th$text$family, "xkcd")
  reset_font_cache()
})

test_that("a warning is raised when neither source has the font", {
  skip_if_not_installed("systemfonts")
  reset_font_cache()
  local_mocked_bindings(fonts = function(...) character(0), .package = "extrafont")
  local_mocked_bindings(font_info = function(...) data.frame(family = "Helvetica"),
                        .package = "systemfonts")
  expect_warning(th <- theme_xkcd(), "was not found")
  expect_s3_class(th, "theme")
  expect_null(th$text$family)
  reset_font_cache()
})

test_that("the font lookup is cached across calls", {
  skip_if_not_installed("systemfonts")
  reset_font_cache()
  calls <- 0
  local_mocked_bindings(
    font_info = function(...) { calls <<- calls + 1; data.frame(family = "xkcd") },
    .package = "systemfonts"
  )
  theme_xkcd(); theme_xkcd(); theme_xkcd()
  expect_equal(calls, 1)
  reset_font_cache()
})

test_that("theme_xkcd returns a usable theme on this machine", {
  reset_font_cache()
  th <- suppressWarnings(theme_xkcd())
  expect_s3_class(th, "theme")
  expect_equal(th$text$size, 16)
  reset_font_cache()
})
