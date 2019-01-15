#' Explore the clipped Fietstelweek data
#'
#' Explore the clipped Fietstelweek data using the library tmap with its \code{view} mode.
#'
#' @name explore
#' @param obj an object of class \code{ftw_atmp} or \code{ftw_tmp}
#' @param type the basic information within the data that the user wants to explore.
#'             Can be set to \code{network} (default), \code{edges}, \code{nodes}, \code{trips}.
#' @param ... ignored
#' @param network_coledges if type is \code{network}, the parameter sets the color of the edges.
#' @param network_colnodes if type is \code{network}, the parameter sets the color of the nodes.
#' @param edges_coledges if type is \code{edges}, the parameter sets the color of the edges based on \code{intensity} (default) or \code{speed}.
#' @param nodes_colnodes if type is \code{nodes}, the parameter sets the color of the nodes based on \code{waiting time} (default) or \code{count}.
#' @param trips_coledges if type is \code{trips}, the parameter sets the color of the edges based on \code{intensity} (default) or \code{speed}.
#' @param trips_weekday if type is \code{trips}, the parameter subsets the data according to the day of the week. Monday is equivalent to 0 and Sunday to 6. The default shows weekdays \code{c(0,1,2,3,4)}.
#' @param trips_hour if type is \code{trips}, the parameter subsets the data according to the hour of the day. The default shows the whole day data.
#' @param palette sets the palette to color edges and nodes for types \code{edges}, \code{nodes}, and \code{trips}.
#' @export
explore <- function(
  obj,
  type,
  ...,
  network_coledges,
  network_colnodes,
  edges_coledges,
  nodes_colnodes,
  trips_coledges,
  trips_weekday,
  trips_hour,
  palette
) UseMethod("explore")

#' @name explore
#' @method explore ftw_atmp
#' @export
explore.ftw_atmp <- function(
  obj,
  type = "network",
  ...,
  network_coledges = "#ff8d00",
  network_colnodes = "#800080",
  edges_coledges = "intensity",
  nodes_colnodes = "waiting time",
  trips_coledges = "intensity",
  trips_weekday = c(0,1,2,3,4),
  trips_hour = NULL,
  palette = "plasma"
){
 tmap::tmap_mode("view")
 if (type == "network"){
   map <- tmap::tmap_leaflet(
     tmap::tm_view(
     basemaps = c(
       "Esri.WorldGrayCanvas",
       "CartoDB.DarkMatter",
       "OpenStreetMap.Mapnik"
     )
   ) +
     tmap::tm_shape(obj$data$edges) +
     tmap::tm_lines(col = network_coledges, lwd = 1.5) +
     tmap::tm_shape(obj$data$nodes) +
     tmap::tm_dots(col = network_colnodes, size = 0.01, shape = 20)
   )

   map
 }
 else if (type == "edges"){
   if (edges_coledges == "intensity") edges_coledges = "INTENSITEI"
   else if (edges_coledges == "speed" & obj$year == 2015) edges_coledges = "SNELHEID"
   else if (edges_coledges == "speed" & obj$year == 2016) edges_coledges = "SPEED"
   map <- tmap::tmap_leaflet(
     tmap::tm_view(basemaps = c(
       "CartoDB.DarkMatter",
       "Esri.WorldGrayCanvas",
       "OpenStreetMap.Mapnik"
       )
     ) +
       tmap::tm_shape(obj$data$edges) +
       tmap::tm_lines(
         col = edges_coledges,
         lwd = 1.5,
         palette = palette,
         contrast = c(0.24, 1)
       )
   )
   map
 }

  else if (type == "nodes"){
    if (nodes_colnodes == "waiting time") nodes_colnodes = "TIJD"
    else if (nodes_colnodes == "count") nodes_colnodes = "AANTAL"
    map <- tmap::tmap_leaflet(
      tmap::tm_view(basemaps = c(
      "CartoDB.DarkMatter",
      "Esri.WorldGrayCanvas",
      "OpenStreetMap.Mapnik"
    )
    ) +
      tmap::tm_shape(obj$data$nodes) +
      tmap::tm_dots(
        col = nodes_colnodes,
        shape = 20,
        palette = palette,
        contrast = c(0.24, 1)
      )
    )
    map
  }

  else if (type == "trips"){
    # Create NULL variables for undefined global variables
    uur <- NULL
    weekdag <- NULL

    if (trips_coledges == "intensity") trips_coledges = "INTENSITEI"
    else if (trips_coledges == "speed" & obj$year == 2015) trips_coledges = "SNELHEID"
    else if (trips_coledges == "speed" & obj$year == 2016) trips_coledges = "SPEED"
    map <- tmap::tmap_leaflet(
      tmap::tm_view(basemaps = c(
      "CartoDB.DarkMatter",
      "Esri.WorldGrayCanvas",
      "OpenStreetMap.Mapnik"
    )
    ) +
      tmap::tm_shape(
        if(is.null(trips_hour)) suppressWarnings(subset(obj$data$edges_routes, weekdag==trips_weekday))
        else suppressWarnings(subset(obj$data$edges_routes, weekdag==trips_weekday & uur==trips_hour))
        ) +
      tmap::tm_lines(
        col = trips_coledges,
        lwd = 2,
        palette = palette,
        contrast = c(0.24, 1)
      )
    )
    map
  }
}

#' @name explore
#' @method explore ftw_tmp
#' @export
explore.ftw_tmp <- function(
  obj,
  type = "network",
  ...,
  network_coledges = "#ff8d00",
  network_colnodes = "#800080",
  edges_coledges = "intensity",
  nodes_colnodes = "waiting time",
  trips_coledges = "intensity",
  trips_weekday = c(0,1,2,3,4),
  trips_hour = NULL,
  palette = "plasma"
){
  tmap::tmap_mode("view")
  if (type == "network"){
    map1 <- tmap::tm_view(
      basemaps = c(
        "Esri.WorldGrayCanvas",
        "CartoDB.DarkMatter",
        "OpenStreetMap.Mapnik"
      )
    ) +
      tmap::tm_shape(obj$data1$edges) +
      tmap::tm_lines(col = network_coledges, lwd = 1.5) +
      tmap::tm_shape(obj$data1$nodes) +
      tmap::tm_dots(col = network_colnodes, size = 0.01, shape = 20)

    map2 <- tmap::tm_view(
      basemaps = c(
        "Esri.WorldGrayCanvas",
        "CartoDB.DarkMatter",
        "OpenStreetMap.Mapnik"
      )
    ) +
      tmap::tm_shape(obj$data2$edges) +
      tmap::tm_lines(col = network_coledges, lwd = 1.5) +
      tmap::tm_shape(obj$data2$nodes) +
      tmap::tm_dots(col = network_colnodes, size = 0.01, shape = 20)
    tmap::tmap_arrange(map1,map2,sync = TRUE)
  }
  else if (type == "edges"){
    if (edges_coledges == "intensity") edges_coledges = "INTENSITEI"
    else if (edges_coledges == "speed" & obj$year == 2015) edges_coledges = "SNELHEID"
    else if (edges_coledges == "speed" & obj$year == 2016) edges_coledges = "SPEED"
    map1 <-tmap::tm_view(basemaps = c(
      "CartoDB.DarkMatter",
      "Esri.WorldGrayCanvas",
      "OpenStreetMap.Mapnik"
    )
    ) +
      tmap::tm_shape(obj$data1$edges) +
      tmap::tm_lines(
        col = edges_coledges,
        lwd = 1.5,
        palette = palette,
        contrast = c(0.24, 1)
    )
    map2 <- tmap::tm_view(basemaps = c(
      "CartoDB.DarkMatter",
      "Esri.WorldGrayCanvas",
      "OpenStreetMap.Mapnik"
    )
    ) +
      tmap::tm_shape(obj$data2$edges) +
      tmap::tm_lines(
        col = edges_coledges,
        lwd = 1.5,
        palette = palette,
        contrast = c(0.24, 1)
    )
    tmap::tmap_arrange(map1,map2,sync = TRUE)
  }

  else if (type == "nodes"){
    if (nodes_colnodes == "waiting time") nodes_colnodes = "TIJD"
    else if (nodes_colnodes == "count") nodes_colnodes = "AANTAL"
    map1 <- tmap::tm_view(basemaps = c(
      "CartoDB.DarkMatter",
      "Esri.WorldGrayCanvas",
      "OpenStreetMap.Mapnik"
    )
    ) +
      tmap::tm_shape(obj$data1$nodes) +
      tmap::tm_dots(
        col = nodes_colnodes,
        shape = 20,
        palette = palette,
        contrast = c(0.24, 1)
        )
    map2 <- tmap::tm_view(basemaps = c(
      "CartoDB.DarkMatter",
      "Esri.WorldGrayCanvas",
      "OpenStreetMap.Mapnik"
    )
    ) +
      tmap::tm_shape(obj$data2$nodes) +
      tmap::tm_dots(
        col = nodes_colnodes,
        shape = 20,
        palette = palette,
        contrast = c(0.24, 1)
    )
    tmap::tmap_arrange(map1,map2,sync = TRUE)
  }

  else if (type == "trips"){
    # Create NULL variables for undefined global variables
    uur <- NULL
    weekdag <- NULL

    if (trips_coledges == "intensity") trips_coledges = "INTENSITEI"
    else if (trips_coledges == "speed" & obj$year == 2015) trips_coledges = "SNELHEID"
    else if (trips_coledges == "speed" & obj$year == 2016) trips_coledges = "SPEED"
    map1 <- tmap::tm_view(basemaps = c(
      "CartoDB.DarkMatter",
      "Esri.WorldGrayCanvas",
      "OpenStreetMap.Mapnik"
    )
    ) +
      tmap::tm_shape(
        if(is.null(trips_hour)) suppressWarnings(subset(obj$data1$edges_routes, weekdag==trips_weekday))
        else suppressWarnings(subset(obj$data1$edges_routes, weekdag==trips_weekday & uur==trips_hour))
      ) +
      tmap::tm_lines(
        col = trips_coledges,
        lwd = 2,
        palette = palette,
        contrast = c(0.24, 1)
    )

    map2 <- tmap::tm_view(basemaps = c(
      "CartoDB.DarkMatter",
      "Esri.WorldGrayCanvas",
      "OpenStreetMap.Mapnik"
    )
    ) +
      tmap::tm_shape(
        if(is.null(trips_hour)) suppressWarnings(subset(obj$data2$edges_routes, weekdag==trips_weekday))
        else suppressWarnings(subset(obj$data2$edges_routes, weekdag==trips_weekday & uur==trips_hour))
      ) +
      tmap::tm_lines(
        col = trips_coledges,
        lwd = 2,
        palette = palette,
        contrast = c(0.24, 1)
      )
    tmap::tmap_arrange(map1,map2,sync = TRUE)
  }
}
