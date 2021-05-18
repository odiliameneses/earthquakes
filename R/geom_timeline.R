#' GeomTimeline
#'
#' Geom code
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = "x",
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = ggplot2::aes(
    y = 0,
    colour = "black",
    shape = 19,
    size = 1.5,
    stroke = 0.5,
    alpha = 0.5,
    fill = NA
  ),
  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_params)
    grid::gList(
      grid::pointsGrob(
        coords$x,
        coords$y,
        pch = coords$shape,
        gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          # Stroke is added around the outside of the point
          fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
          lwd = coords$stroke * .stroke / 2
        )
      ),
      grid::segmentsGrob(
        x0 = min(coords$x),
        y0 = coords$y,
        x1 = max(coords$x),
        y1 = coords$y,
        gp = grid::gpar(col = "black", lwd = 1)
      )
    )
  }
)


#' Earthquakes timeline
#'
#' Use for plotting a time line of earthquakes ranging from xmin to xmaxdates with a point for each earthquake.
#'
#' @param mapping Set of aesthetic mappings
#' @param data The dataset
#' @param stat stat layer
#' @param position Position adjustment
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend legend to display
#' @param inherit.aes false: default aesthetics
#' @param ... Other arguments passed on to [layer()].
#'
#' @details Use \code{earthquakes} dataframe from this package or download new dataset from NOAA site.
#'
#' @return time line plot of selected earthquakes.
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'a<-ggplot(earthquakes,aes(date,location_name,col=total_deaths,size=mag))+
#'geom_timeline()
#' }
geom_timeline <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimeline,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

