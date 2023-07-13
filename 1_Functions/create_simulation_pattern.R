#' create_simulation_pattern
#' 
#' @details 
#' Algorithm to create simulation pattern with 4 species with different species-habitat associations.
#' 
#' \cr Species 1: Positive associations (Poisson process)
#' \cr Species 2: Positive associations (Thomas process)
#' \cr Species 3: Negative associations (Poisson process)
#' \cr Species 4: Negative associations (Thomas process)
#' 
#' @param raster terra:rast() object of the raster package with habitats
#' @param number_points Number of points for each species (association_strength=0)
#' @param association_strength Strength of species-habitat association
#' 
#' @return ppp object of the spatstat package with simulated species

#' @export
create_simulation_pattern <- function(raster, number_points = 100, association_strength = 0.3){
  
  extent_raster <- terra::ext(raster)
  
  owin_overall <- spatstat.geom::owin(xrange = c(extent_raster[1], extent_raster[2]), 
                                      yrange = c(extent_raster[3], extent_raster[4]))
  
  habitats_poly <- terra::as.polygons(raster, trunc = TRUE, dissolve = TRUE, values = TRUE) |> 
    sf::st_as_sf() |>
    dplyr::group_by(layer) |> 
    dplyr::summarise()
  
  # Species 1: Positive associations (Poisson)
  habitat_1 <- sample(x = seq(min(terra::values(raster)):max(terra::values(raster))), size = 1)
  
  species_1 <- create_simulation_species(habitats_poly = habitats_poly, owin_overall = owin_overall,
                                         type = "positive", process = "Poisson",
                                         habitat = habitat_1, number_points = number_points, 
                                         association_strength = association_strength,
                                         species_code = 1, verbose = FALSE)
  
  # Species 2: Positive associations (Thomas process)
  habitat_2 <- sample(x = seq(min(terra::values(raster)):max(terra::values(raster))), size = 1)
  
  species_2 <- create_simulation_species(habitats_poly = habitats_poly,
                                         owin_overall = owin_overall,
                                         type = "positive", process = "Thomas",
                                         habitat = habitat_2, 
                                         number_points = number_points,
                                         association_strength = association_strength,
                                         species_code = 2, verbose = FALSE)
  
  # Species 3: Negative associations (Poisson)
  habitat_3 <- sample(x = seq(min(terra::values(raster)):max(terra::values(raster))), size = 1)
  
  species_3 <- create_simulation_species(habitats_poly = habitats_poly,
                                         owin_overall = owin_overall, 
                                         type = "negative", process = "Poisson",
                                         habitat = habitat_3, 
                                         number_points = number_points, 
                                         association_strength = association_strength,
                                         species_code = 3, verbose = FALSE)
  
  # Species 4: Negative associations habitat 4 (Thomas process)
  habitat_4 <- sample(x = seq(min(terra::values(raster)):max(terra::values(raster))), size = 1)
  
  species_4 <- create_simulation_species(habitats_poly = habitats_poly, 
                                         owin_overall = owin_overall,
                                         type = "negative", process = "Thomas",
                                         habitat = habitat_4,
                                         number_points = number_points, 
                                         association_strength = association_strength,
                                         species_code = 4, verbose = FALSE)
  
  simulation_pattern <- spatstat.geom::superimpose(species_1, species_2, species_3, species_4,
                                                   W = owin_overall)
  
  return(simulation_pattern)
}
