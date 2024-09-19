#' @title Compute Solar Radiation from NetCDF Data using Angstrom-Prescott Model
#'
#' @description
#' The \code{radNC} function computes daily solar radiation for each pixel in a SpatRaster object based on sunshine duration data from a NetCDF file. It uses the Angstrom-Prescott model implemented in the \code{sol_rad} function.
#'
#' @param ssd SpatRaster object. A multi-layer raster containing sunshine duration data (in hours) for each pixel, with one layer per day.
#' @param show_progress Logical. Whether to display a progress bar during the computation. Defaults to \code{TRUE}.
#'
#' @return A SpatRaster object where each layer contains the computed solar radiation (in MJ/m²/day) for each corresponding day.
#'
#' @details
#' This function extracts the sunshine duration from each pixel in the \code{ssd} SpatRaster, and computes solar radiation for each pixel and each layer (day) using the Angstrom-Prescott model. The model requires sunshine duration, latitude, and the corresponding date as inputs. If any required attributes are missing, a warning is issued.
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
radNC <- function(ssd, show_progress = TRUE) {
  # 提取所有像元的经纬度，使用 terra 包中的函数
  lons <- terra::xFromCell(ssd, 1:ncell(ssd))
  lats <- terra::yFromCell(ssd, 1:ncell(ssd))

  # 提取所有时间属性
  dates <- terra::time(ssd)

  # 检查是否有属性缺失
  if (any(is.na(lons)) || any(is.na(lats)) || any(is.na(dates))) {
    warning("Some attributes are missing (e.g., latitude, longitude, or time). The computation may be inaccurate.")
  }

  # 定义计算函数，适应栅格数据
  calc_solar_radiation <- function(ssd_layer, date) {
    # 提取日照时长数据为数值型
    ssd_values <- terra::values(ssd_layer)

    # 将每个像元的日照时长转换为太阳辐射
    solar_radiation <- sol_rad(lat = lats, date = rep(date, length(lats)), ssd = ssd_values)
    return(solar_radiation)
  }

  # 初始化存储计算结果的栅格
  solar_radiation_raster <- terra::rast(ssd, nlyr = terra::nlyr(ssd))

  # 使用标准的 for 循环，配合 pbapply 的进度条
  if (show_progress) {
    pb <- txtProgressBar(min = 0, max = terra::nlyr(ssd), style = 3)
  }

  for (i in 1:terra::nlyr(ssd)) {
    # 计算每一层（即每一天）的太阳辐射
    solar_radiation_raster[[i]] <- terra::setValues(solar_radiation_raster[[i]], calc_solar_radiation(ssd[[i]], dates[i]))

    # 更新进度条
    if (show_progress) {
      setTxtProgressBar(pb, i)
    }
  }

  # 关闭进度条
  if (show_progress) {
    close(pb)
  }

  # 返回计算结果的栅格
  return(solar_radiation_raster)
}
