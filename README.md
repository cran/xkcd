# xkcd


![](https://toledoem.github.io/img/xkcd_logo.png)

An R package to create hand-drawn (xkcd-style) plots and elements for ggplot2.

This repository contains the source for the `xkcd` package (development version). Originally from https://r-forge.r-project.org/projects/xkcd/ which is deprecated and now maintained independently.

## Install

Install the current development version from GitHub:

```r
# using remotes
remotes::install_github("ToledoEM/xkcd")

# or using devtools
devtools::install_github("ToledoEM/xkcd")
```

## Quick Start

```r
library(xkcd)
library(ggplot2)

df <- data.frame(x = 1:10, y = cumsum(runif(10, -0.5, 0.8)))

ggplot(df, aes(x = x, y = y)) +
  geom_xkcdpath(linewidth = 1, colour = "black") +
  theme_xkcd()
```

## Key Notes

- The package uses `Hmisc::bezier()` internally for smoothing paths.
- Uses `linewidth` (ggplot2 >= 3.4.0) for line thickness; older code using `size` is supported where possible.
- Requires xkcd fonts to be installed; see **Fonts** section below.

## Fonts

To use xkcd fonts in your plots, you need to install and register them with R's graphics system.

### Install xkcd Fonts

If xkcd fonts are not already installed on your system:

```r
library(extrafont)

# Download and install the font
download.file(
  "https://toledoem.github.io/img/xkcd.ttf",
  dest = "xkcd.ttf", mode = "wb"
)
font_import(pattern = "[X/x]kcd", prompt = FALSE)
```

### Quick font-check (fundamental)

Run this small example to verify the `xkcd` font is available and to produce a quick check plot. This should be run locally after installing the font and registering it with `extrafont`.

```r
# Font availability check and example plot
library(extrafont)
library(ggplot2)

if ('xkcd' %in% extrafont::fonts()) {
  p <- ggplot() + geom_point(aes(x = mpg, y = wt), data = mtcars) +
    theme(text = element_text(size = 16, family = "xkcd"))
} else {
  warning("xkcd fonts are not installed; using default font for plot.")
  p <- ggplot() + geom_point(aes(x = mpg, y = wt), data = mtcars)
}

print(p)

# Optionally save a small PNG to `vignettes/` for documentation purposes
try({
  ggsave(filename = file.path("vignettes", "font_check.png"), plot = p, width = 6, height = 4)
}, silent = TRUE)
```
### Load Fonts for Plotting

Before plotting, register fonts with your graphics device:

```r
library(xkcd)
library(extrafont)

# Load fonts for your output device
extrafont::loadfonts(device = "win", quiet = TRUE)  # or "pdf" or "postscript"

# Then create your plot
ggplot(...) + theme_xkcd()
```

### Automatic Font Loading (opt-in)

To automatically load fonts when the package is attached (opt-in):

```r
# Set before loading the package
options(xkcd.auto_load_fonts = TRUE)
library(xkcd)
```

This is opt-in to avoid surprising side-effects during package attach.

## Example Images

Below are three examples from the vignette:

![Font availability check (font_check)](vignettes/font_check.png)

![Mother's Day example (mommy_plot)](vignettes/mommy_plot.png)

![Caritas volunteers example (caritas_plot)](vignettes/caritas_plot.png)

## Development

Set up a development workflow with:

**Note:** A TeX distribution (for example, MacTeX or TinyTeX) is required to compile vignettes or to build the PDF manual during development. If LaTeX is not available you can either skip the manual with `--no-manual` when checking or install TinyTeX from R:

```r
# from R: install tinytex and the TinyTeX distribution
install.packages("tinytex")
tinytex::install_tinytex()
```

```r
# Regenerate documentation from roxygen comments
devtools::document()

# Run package checks (skip PDF manual if LaTeX not installed)
devtools::check(args = "--no-manual")

# Build vignettes
devtools::build_vignettes()

# Install from local source
devtools::install_local()
```

To render the vignette directly:

```r
rmarkdown::render("vignettes/xkcd-intro.Rmd")
```

## Dependencies

The package requires:

- **ggplot2** — Graphics framework
- **Hmisc** — Bezier curve interpolation
- **grid** — Low-level graphics primitives
- **extrafont** (optional) — Font management

Install dependencies with:

```r
install.packages(c("ggplot2", "Hmisc", "grid", "extrafont"))
```

## Contributing

Contributions, bug reports, and pull requests are welcome. Please open an issue with a description and minimal reproducible example if relevant.

## License

This package is released under the **MIT License**. See the [LICENSE](LICENSE) file for details.


