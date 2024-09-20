#' @title Compute Solar Radiation from NetCDF Data using Angstrom-Prescott Model
#'
#' @description
#' The \code{radNC} function computes daily solar radiation for each pixel in a SpatRaster object based on sunshine duration data from a NetCDF file. It uses the Angstrom-Prescott model implemented in the \code{sol_rad} function.
#'
#' @param ssd SpatRaster object. A multi-layer raster containing sunshine duration data (in hours) for each pixel, with one layer per day.
#' @param show_progress Logical. Whether to display a progress bar during the computation. Defaults to \code{TRUE}.
#' @param na_neg Logical. If \code{TRUE}, negative sunshine duration values are set to NA; if \code{FALSE}, they are set to 0. Defaults to \code{TRUE}.
#'
#' @return A SpatRaster object where each layer contains the computed solar radiation (in MJ/m²/day) for each corresponding day.
#'
#' @details
#' This function extracts the sunshine duration from each pixel in the \code{ssd} SpatRaster, and computes solar radiation for each pixel and each layer (day) using the Angstrom-Prescott model. The model requires sunshine duration, latitude, and the corresponding date as inputs. If any required attributes are missing, a warning or error is issued.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' filename <- system.file("extdata", "ssd.nc", package = "sola")
#' ssd <- rast(filename)
#' result_raster <- radNC(ssd)
#' plot(result_raster)
#' }
#'
#' @export
radNC <- function(ssd, show_progress = TRUE, na_neg = TRUE) {
  # Check if there is a time attribute
  if (is.null(terra::time(ssd))) {
    stop("The input SpatRaster object does not have a valid time attribute. Please ensure the input is a valid NetCDF file with time layers.")
  }

  # Extract the longitude and latitude of all pixels
  lons <- terra::xFromCell(ssd, 1:ncell(ssd))
  lats <- terra::yFromCell(ssd, 1:ncell(ssd))

  # Check for valid longitude and latitude attributes
  if (is.null(lons) || is.null(lats)) {
    stop("The input SpatRaster object does not have valid longitude and latitude information.")
  }

  # Extract all time attributes
  dates <- terra::time(ssd)

  # Check if the time attribute is empty
  if (any(is.na(dates))) {
    warning("Some layers in the SpatRaster object do not have valid time information.")
  }

  # Define the calculation function
  calc_solar_radiation <- function(ssd_layer, date) {
    ssd_values <- terra::values(ssd_layer)

    # Handle negative values based on the na_neg parameter
    if (na_neg) {
      ssd_values[ssd_values < 0] <- NA
    } else {
      ssd_values[ssd_values < 0] <- 0
    }

    solar_radiation <- calc_Rs(lat = lats, date = rep(date, length(lats)), ssd = ssd_values)
    return(solar_radiation)
  }

  # Initialize a raster to store the calculation results
  solar_radiation_raster <- terra::rast(ssd, nlyr = terra::nlyr(ssd))

  # Start time measurement
  start_time <- Sys.time()

  # Use a standard for loop, with a progress bar
  if (show_progress) {
    pb <- txtProgressBar(min = 0, max = terra::nlyr(ssd), style = 3)
  }

  for (i in 1:terra::nlyr(ssd)) {
    # Calculate solar radiation for each layer
    solar_radiation_raster[[i]] <- terra::setValues(solar_radiation_raster[[i]], calc_solar_radiation(ssd[[i]], dates[i]))

    # Update the progress bar
    if (show_progress) {
      setTxtProgressBar(pb, i)

      # Estimate remaining time
      elapsed_time <- Sys.time() - start_time
      estimated_time <- (elapsed_time / i) * (terra::nlyr(ssd) - i)
      cat(sprintf("\rEstimated remaining time: %s", format(estimated_time, digits = 2)))
    }
  }

  # Close the progress bar
  if (show_progress) {
    close(pb)
  }

  # Calculate total elapsed time
  total_time <- Sys.time() - start_time

  # Display total time if progress is shown
  if (show_progress) {
    cat(sprintf("\nTotal computation time: %s\n", format(total_time, digits = 2)))
  }

  # Return the raster with the calculation results
  return(solar_radiation_raster)
}
