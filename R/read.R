#' Read clipped Fietstelweek data resulting of \code{clip_ftw}
#'
#' Reads the resulting files created by the \link[ftw]{clip_ftw} function and
#' loads them into an object of class \code{ftw_atmp} when loading data only for
#' one year, or into an object of class \code{ftw_tmp} when loading data for both years.
#'
#' @param directory filepath to the directory where the Fietstelweek data is stored.
#'                  Same filepath as the one used for the \link[ftw]{clip_ftw} function.
#' @param year desired year of data, can either be \code{2015} or \code{2016}, which will return
#'             an object of class \code{ftw_atmp} for "atemporal" analysis; or both \code{c(2015,2016)},
#'             which will return an object of class \code{ftw_tmp} for "temporal" analysis
#'             (comparison between years).
#' @param study_area name of the municipality or province the data was clipped to. The parameter is case sensitive
#'                   and prone to fail with spelling mistakes.
#' @name read_ftw
#' @return Returns either and object of class \code{ftw_atmp} for "atemporal" analysis when year is
#'         \code{2015} or \code{2016}, or into an object of class \code{ftw_tmp} for "temporal" analysis
#'         (comparison between years) when year is \code{c(2015,2016)}.
#' @export read_ftw

read_ftw <- function(directory, year, study_area)
{
  if(length(year)==1){
    path <- file.path(directory,year,study_area)
    if(file.exists(path)){
      ftw = list(
        study_area = study_area,
        year = year,
        data = list(
          edges = sf::st_read(file.path(path,"edges.gpkg"), quiet = TRUE),
          nodes = sf::st_read(file.path(path,"nodes.gpkg"), quiet = TRUE),
          routes = suppressWarnings(sf::st_read(file.path(path,"routes.csv"), quiet = TRUE)),
          edges_routes = merge(sf::st_read(file.path(path,"edges.gpkg"), quiet = TRUE),
                               suppressWarnings(sf::st_read(file.path(path,"routes.csv"), quiet = TRUE)),
                               by.x="LINKNUMMER",by.y="linknummer")

        )
      )
      class(ftw) <- c("ftw_atmp")
      ftw
    }
    else {stop("Check that the study area name matches the folder name, and that you have data available for the desired year")}
  }
  else if (length(year)==2){
    path1 <- file.path(directory,year[1],study_area)
    path2 <- file.path(directory,year[2],study_area)
    if(file.exists(path1)&&file.exists(path2)){
      ftw = list(
        study_area = study_area,
        year1 = year[1],
        data1 = list(
          edges = sf::st_read(file.path(path1,"edges.gpkg"), quiet = TRUE),
          nodes = sf::st_read(file.path(path1,"nodes.gpkg"), quiet = TRUE),
          routes = suppressWarnings(sf::st_read(file.path(path1,"routes.csv"), quiet = TRUE)),
          edges_routes = merge(sf::st_read(file.path(path1,"edges.gpkg"), quiet = TRUE),
                               suppressWarnings(sf::st_read(file.path(path1,"routes.csv"), quiet = TRUE)),
                               by.x="LINKNUMMER",by.y="linknummer")
        ),
        year2 = year[2],
        data2 = list(
          edges = sf::st_read(file.path(path2,"edges.gpkg"), quiet = TRUE),
          nodes = sf::st_read(file.path(path2,"nodes.gpkg"), quiet = TRUE),
          routes = suppressWarnings(sf::st_read(file.path(path2,"routes.csv"), quiet = TRUE)),
          edges_routes = merge(sf::st_read(file.path(path2,"edges.gpkg"), quiet = TRUE),
                               suppressWarnings(sf::st_read(file.path(path2,"routes.csv"), quiet = TRUE)),
                               by.x="LINKNUMMER",by.y="linknummer")
        )
      )
      class(ftw) <- c("ftw_tmp")
      ftw
    }
    else {stop("Check that the study area name matches the folder name, and that you have data available for the desired year")}
  }
}

#' @method print ftw_atmp
#' @name read_ftw
#' @param x object of class \code{ftw_atmp} or \code{ftw_tmp}
#' @param ... ignored
#' @export
print.ftw_atmp <- function(x,...) {
  x$data$edges$id <- NULL;  x$data$edges$code <- NULL
  x$data$nodes$id <- NULL;  x$data$nodes$code <- NULL
  x$data$routes$field_1 <- NULL
  cat("You are now looking at the Fietstelweek data for ", x$study_area, " for the year ", x$year,".",sep = "")
  cat("\n\n")
  cat("Your data has:")
  cat("\n\n")
  cat("\t",nrow(x$data$edges), " edges, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data$edges)))
  cat("\n\n")
  cat("\t", nrow(x$data$nodes), " nodes, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data$nodes)))
  cat("\n\n")
  cat("\t and ", length(unique(x$data$routes$routeid)), " unique trips, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data$routes)))
  cat("\n\n")
  invisible(x)
}

#' @method print ftw_tmp
#' @name read_ftw
#' @export
print.ftw_tmp <- function(x,...) {
  x$data1$edges$id <- NULL; x$data1$edges$code <- NULL
  x$data1$nodes$id <- NULL; x$data1$nodes$code <- NULL
  x$data2$edges$id <- NULL; x$data2$edges$code <- NULL
  x$data2$nodes$id <- NULL; x$data2$nodes$code <- NULL
  x$data1$routes$field_1 <- NULL
  x$data2$routes$field_1 <- NULL
  cat("You are now looking at the Fietstelweek data for ", x$study_area, " for the years ", x$year1," and ", x$year2, ".", sep = "")
  cat("\n\n")
  cat("Your", x$year1, "data has:")
  cat("\n\n")
  cat("\t",nrow(x$data1$edges), " edges, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data1$edges)))
  cat("\n\n")
  cat("\t",nrow(x$data1$nodes), " nodes, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data1$nodes)))
  cat("\n\n")
  cat("\t and ", length(unique(x$data1$routes$routeid)), " unique trips, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data1$routes)))
  cat("\n\n")
  cat("Your", x$year2, "data has:")
  cat("\n\n")
  cat("\t",nrow(x$data2$edges), " edges, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data2$edges)))
  cat("\n\n")
  cat("\t",nrow(x$data2$nodes), " nodes, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data2$nodes)))
  cat("\n\n")
  cat("\t and ", length(unique(x$data2$routes$routeid)), " unique trips, for which the first six records are:",sep = "")
  cat("\n\n")
  print(utils::head(data.frame(x$data2$routes)))
  cat("\n\n")
  invisible(x)
}

#' @method summary ftw_atmp
#' @name read_ftw
#' @param object object of class \code{ftw_atmp} or \code{ftw_tmp}
#' @export
summary.ftw_atmp <- function(object,...){
  edges <- nrow(object$data$edges)
  nodes <- nrow(object$data$nodes)
  trips <- length(unique(object$data$routes$routeid))
  df <- data.frame(edges,nodes,trips)
  rownames(df) <- paste(object$study_area,object$year)
  df
}

#' @method summary ftw_tmp
#' @name read_ftw
#' @return The summay method returns the total number of edges, nodes and trips of the data by year.
#' @export
summary.ftw_tmp <- function(object,...){
  edges <- c(nrow(object$data1$edges),nrow(object$data2$edges))
  nodes <- c(nrow(object$data1$nodes), nrow(object$data2$nodes))
  trips <- c(length(unique(object$data1$routes$routeid)),length(unique(object$data2$routes$routeid)))
  df <- data.frame(edges,nodes,trips)
  rownames(df) <- c(object$year1,object$year2)
  df
}
