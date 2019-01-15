#' Download function
#'
#' Download function for internal use.
#'
#' @param directory filepath to the directory where the Fietstelweek data will be stored.
#' @param year desired year of analysis, can either be \code{2015}, \code{2016}, or both \code{c(2015,2016)}.
#' @param rm.zip logical value to indicate if the zip files downloaded should be erased from the system (\code{TRUE})
#'               which is the default, or kept (\code{FALSE})

download_ftw <- function(directory, year, rm.zip){
  # Access bikeprint http site
  # Get URLs for Fietstelweek data for 2015 or 2016, depending on input variable
  ## URL to download data on this order: [1]links, [2]knopen, [3]routes, from bikeprint.nl
  h <- RCurl::basicTextGatherer()
  bp <- RCurl::getURL("http://www.bikeprint.nl/fietstelweek/",header= TRUE, headerfunction = h$update,
                      httpheader = c(Accept="text/html", Test=1), verbose = FALSE)
  bp <- unlist(strsplit(bp,"\r\n"))[35]
  bp <- unlist(strsplit(bp,"<p><a"))[c(2,3,5,7,8,10)]
  bp <- unlist(strsplit(bp, "'"))[c(5,8,2,14,17,11)]

  # Create variables depending of year
  if (length(year) == 1){
    if(year == 2015|2016){
      if(year == 2015){
        urls <- bp[1:3]
      }
      else if (year == 2016){
        urls <- bp[4:6]
      }
    }


    # Create a temporal directory to store the downloaded data
    cd_dir <- file.path(directory,year,"complete_data")
    dir.create(cd_dir, showWarnings = FALSE, recursive = TRUE)

    # Create vectors with destination files and exit directories to download and unzip
    files <- c(
      file.path(cd_dir,"links.zip"),
      file.path(cd_dir,"knopen.zip"),
      file.path(cd_dir,"routes.zip")
    )

    exdir <- c(
      file.path(cd_dir,"links"),
      file.path(cd_dir,"knopen"),
      file.path(cd_dir,"routes")
    )

  }

  else if (length(year) > 1 && year == 2015|2016){

    urls <- bp

    # Create a temporal directory to store the downloaded data
    dir15 <- file.path(directory,"2015","complete_data")
    dir16 <- file.path(directory,"2016","complete_data")
    dir.create(dir15, showWarnings = FALSE, recursive = TRUE)
    dir.create(dir16, showWarnings = FALSE, recursive = TRUE)

    # Create vectors with destination files and exit directories to download and unzip
    files <- c(
      file.path(dir15,"links.zip"),
      file.path(dir15,"knopen.zip"),
      file.path(dir15,"routes.zip"),
      file.path(dir16,"links.zip"),
      file.path(dir16,"knopen.zip"),
      file.path(dir16,"routes.zip")
    )

    exdir <- c(
      file.path(dir15,"links"),
      file.path(dir15,"knopen"),
      file.path(dir15,"routes"),
      file.path(dir16,"links"),
      file.path(dir16,"knopen"),
      file.path(dir16,"routes")
    )
  }
  else{
    stop("The Fietstelweek data currently has data available only for 2015 or 2016.
         Please choose one of these years.")
  }

  # Download and unzip the data into the temporal directory created

  mapply(
    function(x,y) utils::download.file(x, y, method = "curl", quiet = TRUE),
    urls,
    files
  )

  if(FALSE %in% (file.size(files)>45)){
    stop("There is an error with the download server.
           Please try downloading the data manually from: http://www.bikeprint.nl/fietstelweek/")
  }

  else{
    mapply(
      function(x,y) utils::unzip(x, exdir = y),
      files,
      exdir
    )

    if (rm.zip == TRUE) file.remove(files)
  }
}

#' Clip Fietstelweek data to a study area of interest
#'
#' Clips the Fietstelweek data on a system directory to a study area within the
#' Netherlands, either a municipality or a province for 2015 and/or 2016 and saves it as
#' three files: edges.gpkg, nodes.gpkg, routes.csv.
#' It can be used after applying the \link[ftw]{downloader_ftw} function, or providing
#' the user's self-downloaded data.
#'
#' @param directory filepath to the directory where the Fietstelweek data is and will be stored.
#' @param year desired year of analysis, can either be \code{2015}, \code{2016}, or both \code{c(2015,2016)}.
#' @param study_area name of the municipality or province to clip the data to. The parameter is case sensitive
#'                   and prone to fail with spelling mistakes. To have a full list of the municipalities and
#'                   provinces names allowed, please type on the console \code{View(municipalities)} or
#'                   \code{View(provinces)}.
#' @param municipality logical value. Indicates if the study area is a municipality (\code{TRUE}) which is the
#'                     default, or a province (\code{FALSE}).
#' @param ... ignored
#' @param edges_file_2015 self downloaded filepath for the edges file for 2015. Its format should be compatible
#'                        with \link[sf]{st_read}.
#' @param nodes_file_2015 self downloaded filepath for the nodes file for 2015. Its format should be compatible
#'                        with \link[sf]{st_read}.
#' @param routes_file_2015 self downloaded filepath for the routes file for 2015. Its format should be compatible
#'                        with \link[data.table]{fread}.
#' @param edges_file_2016 self downloaded filepath for the edges file for 2016. Its format should be compatible
#'                        with \link[sf]{st_read}.
#' @param nodes_file_2016 self downloaded filepath for the nodes file for 2016. Its format should be compatible
#'                        with \link[sf]{st_read}.
#' @param routes_file_2016 self downloaded filepath for the routes file for 2016. Its format should be compatible
#'                        with \link[data.table]{fread}.
#' @return Prints the directory where the data has been saved.
#' @export

clip_ftw <- function(
  directory,
  year,
  study_area,
  municipality = TRUE,
  ...,
  edges_file_2015 = NULL,
  nodes_file_2015 = NULL,
  routes_file_2015 = NULL,
  edges_file_2016 = NULL,
  nodes_file_2016 = NULL,
  routes_file_2016 = NULL
)
{
  # Create NULL variables for undefined global variables
  gemeentena <- NULL
  provincien <- NULL
  linknummer <- NULL
  KNOOPNUMME <- NULL

  # Check for correct inputs

  if (length(study_area)>1) stop("Please provide only one study area, either a Municipality or a Province")
  if (length(year) > 2) stop("The Fietstelweek data currently has data available only for 2015 or 2016. Please choose one of these years, or both, but not more")
  if (length(year) == 2 && (!(sapply(year[1], `%in%`, c(2015,2016))) | !(sapply(year[2], `%in%`, c(2015,2016))))){
    stop("The Fietstelweek data currently has data available only for 2015 or 2016. Please choose one of these years.")
  }
  if (length(year) == 1 && (!(year %in% c(2015,2016)))) stop("The Fietstelweek data currently has data available only for 2015 or 2016.
         Please choose one of these years.")

  if (municipality == TRUE && (study_area %in% mun$gemeentena) == FALSE)
  {
    stop("Municipality name not valid. Check spelling and Match case.
         Type View(municipalities) to get a list of the valid names.")
  }
  else if (municipality == FALSE && (study_area %in% prv$provincien) == FALSE)
  {
    stop("Province name not valid. Check spelling and Match case.
         Type View(provinces) to get a list of the valid names.")
  }
  else {
    # Read data into R and clip it to study area
    # Consider different approaches for the year in use

    if (2015 %in% year){
      # Read data into R as sf objects and data.frame
      if (is.null(edges_file_2015) && is.null(nodes_file_2015) && is.null(routes_file_2015)){
        links <- sf::read_sf(file.path(directory,"2015","complete_data","links","home/goudappel/links-met-snelheden-900913.shp"))
        knopen <- sf::read_sf(file.path(directory,"2015","complete_data","knopen","home/goudappel/knopen-900913.shp"))
        routes <- data.table::fread(file.path(directory,"2015","complete_data","routes","routes.csv"))
      }
      else {
        links <- sf::read_sf(edges_file_2015)
        knopen <- sf::read_sf(nodes_file_2015)
        routes <- sf::read_sf(routes_file_2015)
      }

      ## Project to admin data CRS

      links <- sf::st_transform(links, sf::st_crs(mun))
      knopen <- sf::st_transform(knopen, sf::st_crs(mun))

      # Establish the study area, which can either be a municipality or a province, or a set of one of them
      if (municipality == TRUE){
        outline <- subset(mun, gemeentena == study_area)
      }
      else {
        outline <- subset(prv, provincien == study_area)
      }

      clip_links <- suppressWarnings(sf::st_intersection(links, outline))
      nummer <- clip_links$LINKNUMMER
      clip_knopen <- suppressWarnings(sf::st_intersection(knopen, outline))
      clip_routes <- dplyr::filter(routes, linknummer %in% nummer)

      clip_links <- sf::st_transform(clip_links, crs = 3857)
      clip_knopen <- sf::st_transform(clip_knopen, crs = 3857)

      ## Creating new directory to store the downloaded data
      sa_dir15 <- file.path(directory,"2015",study_area)
      dir.create(sa_dir15, showWarnings = FALSE, recursive = TRUE)

      # Write sf objects as geopackage and data.frame as .csv
      sf::write_sf(clip_knopen, file.path(sa_dir15,"nodes.gpkg"))
      sf::write_sf(clip_links, file.path(sa_dir15,"edges.gpkg"))
      utils::write.csv(clip_routes, file.path(sa_dir15,"routes.csv"))
    }

    if (2016 %in% year){
      # Read data into R as sf objects and data.frame
      if (is.null(edges_file_2016) && is.null(nodes_file_2016) && is.null(routes_file_2016)){
        links <- sf::read_sf(file.path(directory,"2016","complete_data","links","netwerk-2016-900913.shp"))
        knopen <- sf::read_sf(file.path(directory,"2016","complete_data","knopen","knopen-2016-900913.shp"))
        routes <- data.table::fread(file.path(directory,"2016","complete_data","routes","routes2016.csv"))
      }
      else{
        links <- sf::read_sf(edges_file_2016)
        knopen <- sf::read_sf(nodes_file_2016)
        routes <- sf::read_sf(routes_file_2016)
      }

      links <- sf::st_transform(links, sf::st_crs(mun))
      knopen <- sf::st_transform(knopen, sf::st_crs(mun))

      # Establish the study area, which can either be a municipality or a province, or a set of one of them
      if (municipality == TRUE){
        outline <- subset(mun, gemeentena == study_area)
      }
      else {
        outline <- subset(prv, provincien == study_area)
      }

      clip_links <- suppressWarnings(sf::st_intersection(links, outline))
      source <- clip_links$SOURCE
      target <- clip_links$TARGET
      clip_knopen <- dplyr::filter(knopen, KNOOPNUMME %in% source | KNOOPNUMME %in% target)
      clip_knopen <- suppressWarnings(sf::st_intersection(clip_knopen, outline))
      clip_links <- dplyr::filter(
        clip_links,
        SOURCE %in% clip_knopen$KNOOPNUMME
        &
        TARGET %in% clip_knopen$KNOOPNUMME
      )
      nummer <- clip_links$LINKNUMMER
      clip_routes <- dplyr::filter(routes, linknummer %in% nummer)

      clip_links <- sf::st_transform(clip_links, crs = 3857)
      clip_knopen <- sf::st_transform(clip_knopen, crs = 3857)

      ## Creating new directory to store the downloaded data
      sa_dir16 <- file.path(directory,"2016",study_area)
      dir.create(sa_dir16, showWarnings = FALSE, recursive = TRUE)

      # Write sf objects as geopackage and data.frame as .csv
      sf::write_sf(clip_knopen, file.path(sa_dir16,"nodes.gpkg"))
      sf::write_sf(clip_links, file.path(sa_dir16,"edges.gpkg"))
      utils::write.csv(clip_routes, file.path(sa_dir16,"routes.csv"))
    }
  }
}

#' Download the complete Fietstelweek data
#'
#' Downloads the complete Fietstelweek data to a system directory for 2015 and/or 2016.
#'
#' @param directory filepath to the directory where the Fietstelweek data will be stored.
#' @param year desired year of analysis, can either be \code{2015}, \code{2016}, or both \code{c(2015,2016)}.
#' @param rm.zip logical value to indicate if the zip files downloaded should be erased from the system (\code{TRUE})
#'               which is the default, or kept (\code{FALSE})
#' @return Prints the directory where the data has been saved.
#' @note The function depends on the Bikeprint server availability, and may sometimes fail to download the data.
#'       The function will return an error if this is the case and will ask the user to download the data manually.
#' @export
downloader_ftw <- function(
  directory,
  year,
  rm.zip = TRUE
)
{
  download_ftw(directory, year, rm.zip)

  cat("Your files have been downloaded to:", directory)
}

#' Download and clip Fietstelweek data to a study area of interest
#'
#' The function is a combination of \link[ftw]{downloader_ftw} and \link[ftw]{clip_ftw} functions.
#' It is specially made for the users who are only interested on a particular study area, and do not intend to use
#' the rest of the Dutch territory on their analysis.
#'
#' @param directory filepath to the directory where the clipped Fietstelweek data will be stored.
#' @param year desired year of analysis, can either be \code{2015}, \code{2016}, or both \code{c(2015,2016)}.
#' @param study_area name of the municipality or province to clip the data to. The parameter is case sensitive
#'                   and prone to fail with spelling mistakes. To have a full list of the municipalities and
#'                   provinces names allowed, please type on the console \code{View(municipalities)} or
#'                   \code{View(provinces)}.
#' @param municipality logical value. Indicates if the study area is a municipality (\code{TRUE}) which is the
#'                     default, or a province (\code{FALSE}).
#' @param rm.src logical value. Indicates if the complete Fietstelweek data should be removed from the system
#'               (\code{TRUE}) which is the default, or kept (\code{FALSE}).
#' @return Prints the directory where the data has been saved.
#' @note This function might take a while to run. It depends on the Bikeprint server availability, and may sometimes fail to download the data.
#'       The function will return an error if this is the case and will ask the user to download the data manually.
#' @export
downloader_clip_ftw <- function(
  directory,
  year,
  study_area,
  municipality = TRUE,
  rm.src = TRUE
)
{

  # Check for correct inputs
  if (municipality == TRUE && (study_area %in% mun$gemeentena) == FALSE)
  {
    stop("Municipality name not valid. Check spelling and Match case.
         Type View(municipalities) to get a list of the valid names.")
  }
  else if (municipality == FALSE && (study_area %in% prv$provincien) == FALSE)
  {
    stop("Municipality name not valid. Check spelling and Match case.
         Type View(provinces) to get a list of the valid names.")
  }
  else {

    source('E:/GeoTech/2nd/1. Spatial Data Science with R/Spatial Data Science with R/utils.R', echo=FALSE)

    download_ftw(directory, year, rm.zip = FALSE)

    clip_ftw(directory, year, study_area, municipality)

    ## Deleting the big data from System
    if (rm.src == TRUE) {
      path1 <- file.path(directory,"2015","complete_data")
      path2 <- file.path(directory,"2016","complete_data")
      if (file.exists(path1)) unlink(path1, recursive = TRUE)
      if (file.exists(path2)) unlink(path2, recursive = TRUE)
    }

    ## Showing the user where their files are
    sa_dir15 <- file.path(directory,"2015",study_area)
    sa_dir16 <- file.path(directory,"2016",study_area)
    cat("Your files have been created on:",
        if(dir.exists(sa_dir15)) sa_dir15,
        if(dir.exists(sa_dir16)) sa_dir16)
  }
}
