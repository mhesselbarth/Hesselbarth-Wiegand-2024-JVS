##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

# Purpose of script: Simulation study of habitat associations using gamma test

source("1_Functions/setup.R")

source("1_Functions/detect_habitat_associations.R")

df_experiment <-  readRDS("3_Data/df_experiment.rds")

simulation_habitat_list <- readRDS("3_Data/simulation_habitat_list.rds")

simulation_pattern_list <- readRDS("3_Data/simulation_pattern_list.rds")

#### Define HPC function ####

foo_hpc <- function(fract_dim, association_strength, n_random, row_counter) {
  
  # get simulation data
  simulation_habitat <- terra::rast(simulation_habitat_list[[row_counter]])
  
  simulation_pattern <- simulation_pattern_list[[row_counter]]
  
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
                                                              verbose = TRUE)
  
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
                                                              verbose = TRUE)
  
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
                                                              verbose = TRUE)
  
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
                                                              verbose = TRUE)
  
  # count correct/false detections of species-habitat associations
  detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
                                                     species_type = names_species[4])
  
  # combine results of current association strength to one data frame
  dplyr::bind_rows("1" = detection_species_1, "2" = detection_species_2, 
                   "3" = detection_species_3, "4" = detection_species_4, .id = "species") %>% 
    dplyr::mutate(association_strength = association_strength, fract_dim = fract_dim,
                  n_random = n_random, row_counter =  row_counter, .before = correct)
  
}

#### Submit HPC ####

globals <- c("simulation_habitat_list", "simulation_pattern_list", # sim data
             "detect_habitat_associations") # helper functions

sbatch_gamma <- rslurm::slurm_apply(f = foo_hpc, params = df_experiment, 
                                    global_objects = globals, jobname = "gamma_test",
                                    nodes = nrow(df_experiment), cpus_per_node = 1, 
                                    slurm_options = list("partition" = "medium",
                                                         "time" = "06:00:00"),
                                    pkgs = c("dplyr", "shar", "spatstat.geom", "stringr", "terra"),
                                    rscript_path = rscript_path, submit = FALSE)

#### Collect results #### 

suppoRt::rslurm_missing(x = sbatch_gamma)

gamma_test <- rslurm::get_slurm_out(sbatch_gamma, outtype = "table")

suppoRt::save_rds(object = gamma_test, filename = "gamma_test.rds",
                  path = "3_Data/", overwrite = FALSE)

rslurm::cleanup_files(sbatch_gamma)
