##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1_Functions/setup.R")
source("1_Functions/detect_habitat_associations.R")

simulation_experiment_list <- paste0("3_Data/simulation_experiment_list_", iterations, ".rds") |> 
  readRDS()

#### Define HPC function ####

foo_hpc <- function(input) {
  
  # get simulation data
  simulation_habitat <- terra::rast(input$habitat)
  
  simulation_pattern <- input$pattern
  
  # get simulation parameters
  fract_dim <- input$parameter["fract_dim"]
  
  n_random <- input$parameter["n_random"]
  
  association_strength <- input$parameter["association_strength"]
  
  # name of species include type of association
  names_species <- as.character(unique(simulation_pattern$marks$species))
  
  # results of species-habitat associations
  
  # species 1
  
  # only pattern containing species 1
  species_1 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 1)
  
  # randomize pattern using point process fitting
  random_species_1 <- shar::fit_point_process(species_1, process = "poisson", n_random = n_random,
                                              verbose = FALSE)
  
  # get habitat associations
  associations_species_1 <- shar::results_habitat_association(pattern = random_species_1, raster = simulation_habitat,
                                                              verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
                                                     species_type = names_species[1])
  
  # species 2
  
  # only pattern containing species 2
  species_2 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2)
  
  # randomize pattern using point process fitting
  random_species_2 <- shar::fit_point_process(species_2, process = "cluster", n_random = n_random,
                                              verbose = FALSE)
  
  # get habitat associations
  associations_species_2 <- shar::results_habitat_association(pattern = random_species_2, raster = simulation_habitat,
                                                              verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                     species_type = names_species[2])
  
  # species 3
  
  # only pattern containing species 3
  species_3 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 3)
  
  # randomize pattern using point process fitting
  random_species_3 <- shar::fit_point_process(species_3, process = "poisson", n_random = n_random,
                                              verbose = FALSE)
  
  # get habitat associations
  associations_species_3 <- shar::results_habitat_association(pattern = random_species_3, raster = simulation_habitat,
                                                              verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_3 <- detect_habitat_associations(input = associations_species_3, 
                                                     species_type = names_species[3])
  
  # species 4
  
  # only pattern containing species 4
  species_4 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 4)
  
  # randomize pattern using point process fitting
  random_species_4 <- shar::fit_point_process(species_4, process = "cluster", n_random = n_random,
                                              verbose = FALSE)
  
  # get habitat associations
  associations_species_4 <- shar::results_habitat_association(pattern = random_species_4, raster = simulation_habitat,
                                                              verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
                                                     species_type = names_species[4])
  
  # combine results of current association strength to one data frame
  dplyr::bind_rows("1" = detection_species_1, "2" = detection_species_2, 
                   "3" = detection_species_3, "4" = detection_species_4, .id = "species") |>  
    dplyr::mutate(fract_dim = fract_dim, n_random = n_random, association_strength = association_strength, 
                  .before = species)
  
}

#### Submit HPC ####

globals <- c("detect_habitat_associations") # helper functions

sbatch_gamma <- rslurm::slurm_map(x = simulation_experiment_list, f = foo_hpc,
                                  global_objects = globals, jobname = paste0("gamma_test_", iterations),
                                  nodes = length(simulation_experiment_list), cpus_per_node = 1, 
                                  slurm_options = list("partition" = "medium",
                                                       "time" = "01:00:00", 
                                                       "mem-per-cpu" = "1G"),
                                  pkgs = c("dplyr", "shar", "spatstat", "stringr", "terra"),
                                  rscript_path = rscript_path, submit = FALSE)

#### Collect results #### 

suppoRt::rslurm_missing(x = sbatch_gamma)

gamma_test <- rslurm::get_slurm_out(sbatch_gamma, outtype = "table")

suppoRt::save_rds(object = gamma_test, filename = paste0("gamma_test_", iterations, ".rds"),
                  path = "3_Data/", overwrite = FALSE)

rslurm::cleanup_files(sbatch_gamma)
