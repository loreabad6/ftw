#' Plot the clipped Fietstelweek data
#'
#' Explore the clipped Fietstelweek data using the library tmap with its \code{plot} mode.
#'
#' @name plot_ftw
#'
#' @param x object of class \code{ftw_atmp} or \code{ftw_tmp}
#' @param ... ignores
#' @param type the method allows for two type of plots. A basic one which plots the
#'             \code{network} (default) and a facet plot of type \code{trips per day} that allows
#'             to plot the edges according to intensity or speed for the different days of the week.
#' @param network_coledges if type is \code{network}, the parameter sets the color of the edges.
#' @param network_colnodes if type is \code{network}, the parameter sets the color of the nodes.
#' @param trips_coledges if type \code{trips per day}, the parameter sets the color of the edges based on \code{intensity} (default) or \code{speed}.
#' @param trips_ncol if type \code{trips per day}, the parameter sets the number of columns to show on the facet plot.
#' @param palette if type \code{trips per day}, sets the palette to color edges
#' @method plot ftw_atmp
#' @export
plot.ftw_atmp <- function(
  x,
  ...,
  type = "network",
  network_coledges = "grey50",
  network_colnodes = "cyan",
  trips_coledges = "intensity",
  trips_ncol = 3,
  palette = "plasma"
) {
  tmap::tmap_mode("plot")
  if (type == "network"){
    map <- tmap::tm_shape(x$data$edges) +
      tmap::tm_lines(col = network_coledges, lwd = 1.5) +
      tmap::tm_shape(x$data$nodes) +
      tmap::tm_dots(col = network_colnodes, size = 0.02, shape = 20)
    map
  }
  else if (type == "trips per day"){
    if (trips_coledges == "intensity") trips_coledges = "INTENSITEI"
    else if (trips_coledges == "speed" & x$year == 2015) trips_coledges = "SNELHEID"
    else if (trips_coledges == "speed" & x$year == 2016) trips_coledges = "SPEED"
    map <- tmap::tm_shape(x$data$edges_routes) +
      tmap::tm_lines(col = trips_coledges, palette = palette) +
      tmap::tm_facets(by="weekdag", ncol = trips_ncol)
    map
  }
}

#' @name plot_ftw
#' @method plot ftw_tmp
#' @return For object of class \code{ftw_atmp} plots according to the type.
#'         For object of class \code{ftw_tmp} returns a network plot to compare between 2015 (left) and 2016 (right)
#' @export
plot.ftw_tmp <- function(
  x,
  ...,
  network_coledges = "grey50",
  network_colnodes = "orange"
){
  tmap::tmap_mode("plot")
  map1 <- tmap::tm_shape(x$data1$edges) +
    tmap::tm_lines(col = network_coledges, lwd = 1.5) +
    tmap::tm_shape(x$data1$nodes) +
    tmap::tm_dots(col = network_colnodes, size = 0.02, shape = 20)
  map2 <- tmap::tm_shape(x$data2$edges) +
    tmap::tm_lines(col = network_coledges, lwd = 1.5) +
    tmap::tm_shape(x$data2$nodes) +
    tmap::tm_dots(col = network_colnodes, size = 0.02, shape = 20)
 tmap::tmap_arrange(map1,map2)
}
