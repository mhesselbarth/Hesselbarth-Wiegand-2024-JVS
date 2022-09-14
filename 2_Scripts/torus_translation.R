##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

# Purpose of script: Simulation study of habitat associations using torus translation

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
  
  # randomize habitats using randomization algorithm
  torus_trans <- shar::translate_raster(raster = simulation_habitat, verbose = FALSE)
  
  # sample n_random raster
  subset_id <- sample(x = seq_along(torus_trans$randomized), size = n_random)
  
  torus_trans$randomized <- torus_trans$randomized[subset_id]
  
  # results of species-habitat associations
  
  # species 1
  
  associations_species_1 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 1) %>% 
    shar::results_habitat_association(pattern = ., raster = torus_trans, verbose = TRUE)
  
  # count correct/false detections of species-habitat associations
  detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
                                                     species_type = names_species[1])
  
  # species 2
  
  associations_species_2 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2) %>% 
    shar::results_habitat_association(pattern = ., raster = torus_trans, verbose = TRUE)
  
  # count correct/false detections of species-habitat associations
  detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                     species_type = names_species[2])
  
  # species 3
  
  associations_species_3 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 3) %>% 
    shar::results_habitat_association(pattern = ., raster = torus_trans, verbose = TRUE)
  
  # count correct/false detections of species-habitat associations
  detection_species_3 <- detect_habitat_associations(input = associations_species_3,
                                                     species_type = names_species[3])
  
  # species 4
  
  associations_species_4 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 4) %>% 
    shar::results_habitat_association(pattern = ., raster = torus_trans, verbose = TRUE)
  
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

sbatch_torus <- rslurm::slurm_apply(f = foo_hpc, params = df_experiment, 
                                    global_objects = globals, jobname = "torus_trans",
                                    nodes = nrow(df_experiment), cpus_per_node = 1, 
                                    slurm_options = list("partition" = "medium",
                                                         "time" = "06:00:00"),
                                    pkgs = c("dplyr", "shar", "spatstat.geom", "stringr", "terra"),
                                    rscript_path = rscript_path, submit = FALSE)

#### Collect results #### 

suppoRt::rslurm_missing(x = sbatch_torus)

torus_trans <- rslurm::get_slurm_out(sbatch_torus, outtype = "table")

suppoRt::save_rds(object = torus_trans, filename = "torus_trans.rds",
                  path = "3_Data/", overwrite = FALSE)

rslurm::cleanup_files(sbatch_torus)
