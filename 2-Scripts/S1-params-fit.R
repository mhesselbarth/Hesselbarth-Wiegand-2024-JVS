##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1-Functions/setup.R")
source("1-Functions/detect-habitat-associations.R")

simulation_experiment_list <- readRDS("3-Data/main-sim-experiment.rds")

#### Loop through clustered patterns #### 

param_fitted_df <- purrr::map_dfr(seq_along(simulation_experiment_list), function(i) {
  
  # get current pattern
  simulation_pattern <- simulation_experiment_list[[i]]$pattern

  # get simulation parameters
  fract_dim <- simulation_experiment_list[[i]]$parameter[["fract_dim"]]
  
  association_strength <- simulation_experiment_list[[i]]$parameter[["association_strength"]]
  
  # only pattern containing species 2
  species_2 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2)
  
  # randomize pattern using point process fitting
  random_species_2 <- shar::fit_point_process(species_2, process = "cluster", 
                                              return_para = TRUE, verbose = FALSE)
  
  # only pattern containing species 4
  species_4 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 4)
  
  # randomize pattern using point process fitting
  random_species_4 <- shar::fit_point_process(species_4, process = "cluster", 
                                              return_para = TRUE, verbose = FALSE)
  
  data.frame(fract_dim = fract_dim, association_strength = association_strength, 
             species = c(unique(species_2$marks$species_code), unique(species_4$marks$species_code)), 
             number_parents = c(random_species_2$param[["number_parents"]], 
                                random_species_4$param[["number_parents"]]), 
             number_points  = c(random_species_2$param[["number_points"]], 
                                random_species_4$param[["number_points"]]),
             cluster_area  = c(random_species_2$param[["cluster_area"]], 
                               random_species_4$param[["cluster_area"]]))
  
})

#### Summarize results ####

param_sum_df <- dplyr::group_by(param_fitted_df, fract_dim, association_strength, species) |> 
  dplyr::summarise(number_parents = round(mean(number_parents), 2), 
                   number_points = round(mean (number_points), 2),
                   cluster_area = round(mean(cluster_area), 2), .groups = "drop") |> 
  dplyr::mutate(fract_dim = dplyr::case_when(fract_dim == 0.5 ~ "Low", 
                                             fract_dim == 1.65 ~ "High"),
                fract_dim = factor(fract_dim, levels = c("Low", "High")),
                species = dplyr::case_when(species == 2 ~ "Positive", 
                                           species == 4 ~ "Negative"),
                species = factor(species, levels = c("Positive", "Negative"))) |> 
  dplyr::arrange(fract_dim, species, association_strength)

#### Write results ####

write.table(x = param_sum_df, file = "3-Data/S1-params-fit.txt", sep = ",", row.names = FALSE)
