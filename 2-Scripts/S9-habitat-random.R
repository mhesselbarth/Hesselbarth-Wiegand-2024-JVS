##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1-Functions/setup.R")
source("1-Functions/detect-habitat-associations.R")

simulation_experiment_list <- readRDS("3-Data/S9-sim-experiment.rds")

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
  
  # randomize habitats using randomization algorithm
  random_habitats <- shar::randomize_raster(raster = simulation_habitat,
                                            n_random = n_random, verbose = FALSE)
  
  # results of species-habitat associations
  
  # species 2
  
  associations_species_2 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2) |> 
    shar::results_habitat_association(pattern = _, raster = random_habitats, verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                     species_type = names_species[1])
  
  # species 4
  
  associations_species_4 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 4) |> 
    shar::results_habitat_association(pattern = _, raster = random_habitats, verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
                                                     species_type = names_species[2])
  
  # combine results of current association strength to one data frame
  dplyr::bind_rows("2" = detection_species_2, "4" = detection_species_4, .id = "species") |> 
    dplyr::mutate(fract_dim = fract_dim, n_random = n_random, association_strength = association_strength, 
                  .before = species)

}

#### Submit HPC ####

globals <- c("detect_habitat_associations") # helper functions

sbatch_habitat <- rslurm::slurm_map(x = simulation_experiment_list, f = foo_hpc,
                                    global_objects = globals, jobname = "habitat_random_cluster",
                                    nodes = length(simulation_experiment_list), cpus_per_node = 1, 
                                    slurm_options = list("partition" = "medium",
                                                         "time" = "03:00:00",
                                                         "mem-per-cpu" = "1G"),
                                    pkgs = c("dplyr", "shar", "spatstat.geom", "stringr", "terra"),
                                    rscript_path = rscript_path, submit = FALSE)

#### Collect results #### 

suppoRt::rslurm_missing(x = sbatch_habitat)

habitat_random <- rslurm::get_slurm_out(sbatch_habitat, outtype = "table")

suppoRt::save_rds(object = habitat_random, filename = "S9-habitat-random.rds",
                  path = "3-Data/", overwrite = FALSE)

rslurm::cleanup_files(sbatch_habitat)
