#' Run SSDM for multiple species
#'
#' Wrapper for SSDM::stack_modelling.
#'
#' @param occ Occurrence dataframe (species, longitude, latitude)
#' @param env Environmental SpatRaster
#' @return SSDM model object
#' @export
run_ssdm <- function(occ, env){
  library(SSDM)

  SSDM::stack_modelling(
    algorithms   = c("GLM", "RF"),
    Occurrences  = as.data.frame(occ),
    Env          = env,
    Xcol         = "longitude",
    Ycol         = "latitude",
    Spcol        = "species",
    rep          = 1,
    cores        = 4,
    verbose      = FALSE
  )
}
