#' Prepare environmental raster layers
#'
#' Crop, mask, and resample environmental layers to Japan boundary.
#'
#' @param path Folder containing .tif rasters.
#' @return SpatRaster environmental stack
#' @export
prepare_env <- function(path){
  library(terra)
  library(sf)
  library(rnaturalearth)

  japan_sf   <- ne_countries(country = "Japan", returnclass = "sf")
  japan_vect <- terra::vect(japan_sf)

  files <- list.files(path, "\\.tif$", full.names = TRUE)
  if(length(files) == 0) stop("No .tif files found in path.")

  r0 <- terra::rast(files[1]) |> crop(japan_vect) |> mask(japan_vect)

  ras_list <- list(r0)

  for(i in 2:length(files)){
    r <- terra::rast(files[i]) |> crop(japan_vect) |> mask(japan_vect)
    r <- resample(r, r0)
    ras_list[[i]] <- r
  }

  env <- terra::rast(ras_list)
  return(env)
}
