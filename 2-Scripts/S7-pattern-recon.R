##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1-Functions/setup.R")
source("1-Functions/detect-habitat-associations.R")

simulation_experiment_list <- readRDS("3-Data/S7-sim-experiment.rds")

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
  
  # randomize pattern using pattern reconstruction
  random_species_1 <- shar::reconstruct_pattern(pattern = species_1, n_random = n_random,
                                                max_runs = max_runs, method = "homo", 
                                                no_change = no_change, verbose = FALSE)
  
  # get habitat associations
  associations_species_1 <- shar::results_habitat_association(pattern = random_species_1, raster = simulation_habitat,
                                                              verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
                                                     species_type = names_species[1])
  
  # species 2
  
  # only pattern containing species 2
  species_2 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2)
  
  # randomize pattern using pattern reconstruction
  random_species_2 <- shar::reconstruct_pattern(pattern = species_2, n_random = n_random,
                                                max_runs = max_runs, method = "cluster",
                                                no_change = no_change, verbose = FALSE)
  
  # get habitat associations
  associations_species_2 <- shar::results_habitat_association(pattern = random_species_2, raster = simulation_habitat,
                                                              verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                     species_type = names_species[2])
  
  # species 3
  
  # only pattern containing species 3
  species_3 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 3)
  
  # randomize pattern using pattern reconstruction
  random_species_3 <- shar::reconstruct_pattern(pattern = species_3, n_random = n_random,
                                                max_runs = max_runs, method = "homo", 
                                                no_change = no_change, verbose = FALSE)
  
  # get habitat associations
  associations_species_3 <- shar::results_habitat_association(pattern = random_species_3, raster = simulation_habitat,
                                                              verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_3 <- detect_habitat_associations(input = associations_species_3, 
                                                     species_type = names_species[3])
  
  # species 4
  
  # only pattern containing species 4
  species_4 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 4)
  
  # randomize pattern using pattern reconstruction
  random_species_4 <- shar::reconstruct_pattern(pattern = species_4, n_random = n_random,
                                                max_runs = max_runs, method = "cluster",
                                                no_change = no_change, verbose = FALSE)
  
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

globals <- c("max_runs", "no_change", # reconstruct_pattern
             "detect_habitat_associations") # helper functions

sbatch_recon <- rslurm::slurm_map(x = simulation_experiment_list, f = foo_hpc,
                                  global_objects = globals, jobname = "pattern_recon_grad",
                                  nodes = length(simulation_experiment_list), cpus_per_node = 1, 
                                  slurm_options = list("partition" = "medium",
                                                       "time" = "48:00:00", 
                                                       "mem-per-cpu" = "1G"),
                                  pkgs = c("dplyr", "shar", "spatstat.geom", "stringr", "terra"),
                                  rscript_path = rscript_path, submit = FALSE)

#### Collect results #### 

suppoRt::rslurm_missing(x = sbatch_recon)

pattern_recon <- rslurm::get_slurm_out(sbatch_recon, outtype = "table")

suppoRt::save_rds(object = pattern_recon, filename = "S7-pattern-recon.rds",
                  path = "3-Data/", overwrite = FALSE)

rslurm::cleanup_files(sbatch_recon)
