#' create_simulation_species
#'
#' @details 
#' Algorithm to create simulation species with choosen characteristics
#' 
#' @param habitats_poly sf object with habitats
#' @param habitat Habitat to which species is associated
#' @param owin_overall owin object with whole observation window
#' @param type 'positive' or 'negative' associations
#' @param process Process type to chose. Either 'Poisson' or 'Thomas'
#' @param association_strength Strength of species-habitat association
#' @param number_points Number of points for each species (association_strength = 0)
#' @param species_code Species code to number species
#' @param verbose Print advanced error message
#'
#' @return ppp object of the spatstat package with simulated species

#' @export
create_simulation_species <- function(habitats_poly, habitat, owin_overall, 
                                      type, process, association_strength = 0.3,
                                      number_points = 100, species_code = 0, verbose = TRUE) {
  
  scale <- mean(diff(owin_overall$yrange), diff(owin_overall$xrange)) / 25
  
  owin_pattern <- sf::as_Spatial(habitats_poly[habitats_poly$layer == habitat, ]) |> 
    maptools::as.owin.SpatialPolygons()
  
  p_retain <- 1 - association_strength
  
  if (process == "Poisson") {
    
    pattern_a <- spatstat.random::runifpoint(n = number_points, win = owin_overall)
    
    if (type == "positive") {
      
      pattern_b <- spatstat.random::runifpoint(n = floor(pattern_a$n * association_strength), 
                                               win = owin_pattern)
      
      pattern <- spatstat.geom::superimpose.ppp(pattern_a, pattern_b, W = owin_overall)
      
      species <- rep(paste0("poisson_positive_", habitat), pattern$n)
      
    } else if (type == "negative") {
      
      pattern_b <- pattern_a[!spatstat.geom::inside.owin(x = pattern_a, w = owin_pattern)]
      
      pattern_c <- spatstat.random::rthin(X = pattern_a[spatstat.geom::inside.owin(x = pattern_a, w = owin_pattern)], 
                                          P = p_retain)
      
      pattern <- spatstat.geom::superimpose(pattern_b, pattern_c, W = owin_overall)
      
      species <-  rep(paste0("poisson_negative_", habitat), pattern$n)
      
    } else {
      
      stop("Please select either 'positive', 'negative' or 'neutral' as type")
      
    }
    
  } else if (process == "Thomas") {
    
    pattern_a <- spatstat.random::rThomas(kappa = (number_points / spatstat.geom::area.owin(owin_overall)) / 5, 
                                          scale = scale, mu = 5, win = owin_overall)
    
    # create lambda within habitat only
    lambda <- density(pattern_a) |> 
      terra::rast() |> 
      terra::mask(mask = habitats_poly[habitats_poly$layer == habitat, ]) |> 
      terra::as.data.frame(xy = TRUE) |> 
      spatstat.geom::as.im()
      
    if (type == "positive") {
      
      # add points with higher probability in clusters
      pattern_b <- spatstat.random::rpoint(n = floor(pattern_a$n * association_strength),
                                           f = lambda, win = owin_pattern)

      # pattern_b <- spatstat.random::runifpoint(n = floor(pattern_a$n * association_strength), 
      #                                          win = owin_pattern)
      
      pattern <- spatstat.geom::superimpose.ppp(pattern_a, pattern_b, W = owin_overall)
      
      species <- rep(paste0("thomas_positive_", habitat), pattern$n)
      
    } else if (type == "negative") {
      
      pattern_b <- pattern_a[!spatstat.geom::inside.owin(x = pattern_a, w = owin_pattern)]
      
      pattern_c <- spatstat.random::rthin(X = pattern_a[spatstat.geom::inside.owin(x = pattern_a, w = owin_pattern)], 
                                          P = p_retain)
      
      pattern <- spatstat.geom::superimpose(pattern_b, pattern_c, W = owin_overall)
      
      species <- rep(paste0("thomas_negative_", habitat), pattern$n)
      
    } else {
      
      stop("Please select either 'positive', 'negative' or 'neutral' as type")
      
    }
    
  } else {
    
    stop("Please select either 'Poisson', 'Thomas' as process")
    
  }
    
  marks_pattern <- data.frame(species = species, species_code = species_code, 
                              habitat = rep(habitat, pattern$n))
  
  spatstat.geom::marks(pattern) <- marks_pattern
  
  return(pattern)

}
