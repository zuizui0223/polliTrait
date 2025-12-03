#' Fit SPDE-INLA model for floral orientation
#'
#' @param df Dataframe with columns:
#'   x, y (lon/lat),
#'   bee, butter, hover (pollinator richness),
#'   orient_bin (0/1 orientation response)
#'
#' @return INLA model object
#' @export
run_inla_orientation <- function(df){
  library(INLA)
  library(dplyr)
  library(sf)

  # scale covariates
  df <- df |>
    mutate(
      z_bee     = as.numeric(scale(bee)),
      z_butter  = as.numeric(scale(butter)),
      z_hover   = as.numeric(scale(hover))
    )

  # project coordinates
  pts <- st_as_sf(df, coords = c("x","y"), crs = 4326)
  pts <- st_transform(pts, 3857)
  coords <- st_coordinates(pts)

  # mesh
  mesh <- inla.mesh.2d(
    loc      = coords,
    max.edge = c(50e3, 200e3),
    cutoff   = 20e3
  )

  spde <- inla.spde2.matern(mesh)
  idx  <- inla.spde.make.index("spatial", spde$n.spde)
  A    <- inla.spde.make.A(mesh, loc = coords)

  stack <- inla.stack(
    data    = list(y = df$orient_bin),
    A       = list(A, 1),
    effects = list(
      spatial = idx,
      data.frame(
        Intercept = 1,
        z_bee     = df$z_bee,
        z_butter  = df$z_butter,
        z_hover   = df$z_hover
      )
    )
  )

  formula <- y ~ 1 + z_bee + z_butter + z_hover + f(spatial, model = spde)

  fit <- inla(
    formula,
    family = "binomial",
    data = inla.stack.data(stack),
    control.predictor = list(
      A = inla.stack.A(stack),
      compute = TRUE
    )
  )

  return(fit)
}
