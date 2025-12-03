#' Clean occurrence data for SSDM
#'
#' Remove duplicates, round coordinates, and filter species with <20 records.
#'
#' @param df Dataframe with species, longitude, latitude.
#' @return Cleaned dataframe.
#' @export
clean_occ <- function(df){
  df |>
    dplyr::mutate(
      lon_r = round(longitude, 4),
      lat_r = round(latitude, 4)
    ) |>
    dplyr::distinct(species, lon_r, lat_r, .keep_all = TRUE) |>
    dplyr::group_by(species) |>
    dplyr::filter(n() >= 20) |>
    dplyr::ungroup() |>
    dplyr::select(species, longitude, latitude)
}
