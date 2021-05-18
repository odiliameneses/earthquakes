#' GeomTimelinelabel
#'
#'
#' @export
#'
GeomTimelinelabel <-
  ggplot2::ggproto(
    "GeomTimelinelabel",
    ggplot2::Geom,
    required_aes = c("x", "label"),
    default_aes = ggplot2::aes(
      y = 0,
      n_max = 0,
      y_length = 1
    ),
    draw_key = ggplot2::draw_key_point,

    draw_panel = function(data, panel_params, coord) {
      if (data$n_max[1] > 0) {
        if (data$y[1] == 0) {
          data <- data %>%
            dplyr::arrange(desc(size)) %>%
            dplyr::slice(1:data$n_max[1])
        }
        else {
          data <- data %>%
            dplyr::arrange(desc(size)) %>%
            dplyr::group_by(y) %>%
            dplyr::slice(1:data$n_max[1])
        }
      }
      if (!data$y[1] == 0) {
        data$y_length <- dim(table(data$y))
      }

      coords <-
        coord$transform(data, panel_params)
      grid::gList(
        grid::segmentsGrob(
          x0 = coords$x,
          y0 = coords$y,
          x1 = coords$x,
          y1 = (.2 / coords$y_length) + coords$y,
          gp = grid::gpar(col = "black", lwd = .5)
        ),
        grid::textGrob(
          label = coords$label,
          x = coords$x,
          y = (.2 / coords$y_length) + coords$y ,
          just = "left",
          rot = 45
        )
      )
    }
  )




#' Annotations to the earthquake data
#'
#' This geom adds a vertical line to each data point with a text annotation (e.g. the location of the earthquake) attached to each line.
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
#' @return a geom
#' @export
#'@examples
#' \donttest{
#' library(ggplot2)
#'a<-ggplot(earthquakes,aes(date,location_name,col=total_deaths,size=mag))+
#'geom_timelinelabel()
#' }
geom_timelinelabel <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimelinelabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

