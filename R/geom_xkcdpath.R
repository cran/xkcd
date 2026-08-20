## GeomXkcdPath: ggproto implementation
## This Geom expands each input row into a jittered/bezier-smoothed path
## (segment or circle) and draws it using ggplot2's GeomPath. It provides
## proper integration with ggplot2 aesthetics and uses 'linewidth' for
## line thickness.

#' GeomXkcdPath: fuzzy path/circle geom (XKCD style)
#'
#' A ggplot2 geom that draws jittered, smoothed paths or fuzzy circles. It
#' expects aesthetics like `x`, `y`, and either `xend`/`yend` (for segments) or
#' `diameter` (for circles). Additional aesthetics (colour, alpha,
#' linewidth, linetype) are respected.
#'
#' @param mapping Aesthetic mapping.
#' @param data Data frame.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment.
#' @param ... Other arguments passed on to layer().
#' @param xjitteramount Horizontal jitter amount for segments, in data units.
#' @param yjitteramount Vertical jitter amount for segments, in data units.
#' @param wobble Scalar multiplier applied to both jitter amounts. `1` leaves the
#'   jitter amounts unchanged; `0` draws straight lines.
#' @param seed Optional integer. When supplied the wobble is reproducible: the same
#'   `seed` always yields the same path. Each row of `data` is offset from `seed` so
#'   rows still differ from one another. The global RNG state is left untouched.
#' @param mask Logical; if TRUE draws a thicker white mask path under the main path.
#' @param show.legend Show legend.
#' @param inherit.aes Whether to inherit aesthetics from the plot.
#' @export
#' @importFrom grid gList grobTree gpar
geom_xkcdpath <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ..., xjitteramount = 0.01,
                          yjitteramount = 0.01, wobble = 1, seed = NULL,
                          mask = TRUE, show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXkcdPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xjitteramount = xjitteramount,
                  yjitteramount = yjitteramount,
                  wobble = wobble,
                  seed = seed,
                  mask = mask,
                  ...)
  )
}

GeomXkcdPath <- ggplot2::ggproto(
  "GeomXkcdPath",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.8, linetype = 1, alpha = 1,
                             xend = NA, yend = NA, diameter = NA, ratioxy = 1),
  extra_params = c("xjitteramount", "yjitteramount", "wobble", "seed", "mask", "na.rm"),
  # NOTE: no `...` in this signature. ggplot2's Geom$parameters() falls back to
  # draw_group()'s formals whenever draw_panel() accepts dots, which would leave
  # this geom with no recognised parameters and silently ignore every one of them.
  draw_panel = function(self, data, panel_params, coord, xjitteramount = 0.01,
                        yjitteramount = 0.01, wobble = 1, seed = NULL,
                        mask = TRUE, na.rm = FALSE) {
    if (nrow(data) == 0) return(grid::nullGrob())

    # `wobble` scales both jitter amounts
    xjitteramount <- xjitteramount * wobble
    yjitteramount <- yjitteramount * wobble

    # Row-invariant, so computed once rather than once per row
    aesth_cols <- setdiff(names(data),
                          c("x", "y", "xend", "yend", "diameter", "PANEL", "group"))

    # Expand each row into path points
    paths <- vector("list", nrow(data))
    for (i in seq_len(nrow(data))) {
      row <- data[i, , drop = FALSE]
      # Offset per row so rows differ from each other but stay reproducible
      rowseed <- if (is.null(seed)) NULL else seed + i

      if (!is.null(row$diameter) && !is.na(row$diameter)) {
        df <- pointscircunference(x = row$x, y = row$y, diameter = row$diameter,
                                  ratioxy = row$ratioxy, seed = rowseed)
      } else if (!is.null(row$xend) && !is.na(row$xend) && !is.null(row$yend) && !is.na(row$yend)) {
        df <- pointssegment(x = row$x, y = row$y, xend = row$xend, yend = row$yend,
                            xjitteramount = xjitteramount, yjitteramount = yjitteramount,
                            seed = rowseed)
      } else {
        df <- data.frame(x = row$x, y = row$y)
      }

      # Copy aesthetics from the original row into the expanded path
      for (nm in aesth_cols) df[[nm]] <- row[[nm]]
      df$group <- i
      paths[[i]] <- df
    }

    pathdata <- do.call(rbind, paths)
    if (nrow(pathdata) == 0) return(grid::nullGrob())

    # Columns consumed by this geom that GeomPath does not understand
    pathdata$ratioxy <- NULL
    pathdata$diameter <- NULL

    # Draw mask (white thicker path) if requested
    grobs <- list()
    if (isTRUE(mask)) {
      maskdata <- pathdata
      # ensure linewidth present
      if (!"linewidth" %in% names(maskdata)) maskdata$linewidth <- 1
      maskdata$linewidth <- mask_linewidth(maskdata$linewidth)
      maskdata$colour <- "white"
      grob_mask <- ggplot2::GeomPath$draw_panel(maskdata, panel_params, coord)
      grobs[[length(grobs) + 1]] <- grob_mask
    }

    # Main path
    grob_main <- ggplot2::GeomPath$draw_panel(pathdata, panel_params, coord)
    grobs[[length(grobs) + 1]] <- grob_main

    grid::grobTree(do.call(grid::gList, grobs))
  },

  draw_key = ggplot2::draw_key_path
)

# Declare globals used in aes to avoid R CMD check NOTES
if (getRversion() >= "2.15.1") utils::globalVariables(c("x", "y", "group", "xend", "yend", "diameter"))
