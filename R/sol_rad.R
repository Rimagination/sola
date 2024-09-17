#' Estimate Solar Radiation using the Angstrom-Prescott model
#'
#' @description
#' \code{sol_rad} estimates daily solar radiation on a horizontal surface based on sunshine duration using the Angstrom-Prescott model.
#' If extraterrestrial radiation (\code{ext_rad}) or maximum possible sunshine duration (\code{N}) is not provided, they are calculated within the function.
#'
#' @param lat Numeric. Latitude in degrees. Use positive values for north, negative for south.
#' @param date Date or character. A Date object or a date-like string that can be coerced to Date.
#' @param ssd Numeric. Sunshine duration in hours.
#' @param N Numeric. Numeric value for maximum possible sunshine duration (day length) in hours. If not provided, it is calculated.
#' @param ext_rad Optional. Numeric value for extraterrestrial radiation in MJ/m²/day. If not provided, it is calculated.
#' @param method Character. Calculation method: "standard" (Default), "parallel", or "gpu".
#' @param B Numeric. Angstrom-Prescott coefficient B (default is derived if not provided).
#'
#' @return Numeric. Estimated solar radiation in MJ/m²/day.
#'
#' @export
#'
#' @examples
#' sol_rad(lat = 35.0, date = as.Date("2023-03-15"), ssd = 8)
#' sol_rad(lat = -15.0, date = "2024-06-21", ssd = 10, method = "parallel")
#' sol_rad(lat = c(35,20), date = c("2023-03-15","2022-02-15"), ssd = c(8,5))
#'
#' # Create a test vector with 1000 elements
#' set.seed(123)
#' test_lat <- runif(10000, -90, 90)
#' test_date <- as.Date("2023-01-01") + sample(1:365, 10000, replace = TRUE)
#' test_ssd <- runif(10000, 0, 24)
#' # Compare computation times
#' system.time({sol_rad(lat = test_lat, date = test_date, ssd = test_ssd, method = "standard")})
#' system.time({  sol_rad(lat = test_lat, date = test_date, ssd = test_ssd, method = "parallel")})

#'
sol_rad <- function(lat, date, ssd, N = NULL, ext_rad = NULL, method = "standard", A = NA, B = NA) {
  # Ensure inputs are vectors of the same length
  len <- max(length(lat), length(date), length(ssd))
  lat <- rep(lat, length.out = len)
  date <- rep(date, length.out = len)
  ssd <- rep(ssd, length.out = len)

  # Validate inputs
  if (any(!is.numeric(lat))) stop("Latitude must be numeric.")
  if (any(lat < -90 | lat > 90)) stop("Latitude must be between -90 and 90 degrees.")
  if (any(!is.numeric(ssd) | ssd < 0 | ssd > 24)) stop("Sunshine duration must be a number between 0 and 24.")

  # Convert date if necessary
  date <- tryCatch(as.Date(date), error = function(e) stop("Invalid date input."))

  # Calculate extraterrestrial radiation if not provided
  if (is.null(ext_rad)) {
    ext_rad <- mapply(ext_rad, lat = lat, date = date)
  }

  # Calculate day length (N) if not provided
  if (is.null(N)) {
    N <- mapply(day_length, lat = lat, date = date)
  }

  # Default coefficients if not provided
  if (is.na(A)) A <- 0.25
  if (is.na(B)) B <- 0.50

  # Function to calculate radiation using Angstrom-Prescott equation
  calc_rad <- function(A, B, ssd, N, ext_rad) {
    (A + B * (ssd / N)) * ext_rad
  }

  # Use different methods based on the "method" parameter
  if (method == "gpu") {
    if (!requireNamespace("GPUmatrix", quietly = TRUE)) stop("GPUmatrix package is required for GPU computation.")

    # Convert matrices to gpu.matrix
    ssd_gpu <- GPUmatrix::gpu.matrix(ssd, nrow = length(ssd), ncol = 1)
    N_gpu <- GPUmatrix::gpu.matrix(N, nrow = length(N), ncol = 1)
    ext_rad_gpu <- GPUmatrix::gpu.matrix(ext_rad, nrow = length(ext_rad), ncol = 1)

    # GPU-based computation
    rad_estimate <- calc_rad(A, B, ssd_gpu, N_gpu, ext_rad_gpu)

  } else if (method == "parallel") {
    if (!requireNamespace("future.apply", quietly = TRUE)) stop("future.apply package is required for parallel computation.")

    # Set up parallel plan
    future::plan(future::multisession)

    # Parallel calculation of radiation estimates
    rad_estimate <- future.apply::future_mapply(function(lat, date, ssd, N, ext_rad) {
      calc_rad(A, B, ssd, N, ext_rad)
    }, lat, date, ssd, N, ext_rad)

  } else {
    # Standard sequential calculation using mapply to handle vectors
    rad_estimate <- mapply(function(lat, date, ssd, N, ext_rad) {
      calc_rad(A, B, ssd, N, ext_rad)
    }, lat, date, ssd, N, ext_rad)
  }

  # Return results
  return(rad_estimate)
}
