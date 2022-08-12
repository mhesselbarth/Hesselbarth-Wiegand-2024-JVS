###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

# Purpose of script: Simulation study of habitat associations using habitat randomization

source("1_Functions/setup.R")

source("1_Functions/create_simulation_pattern.R")
source("1_Functions/create_simulation_species.R")
source("1_Functions/detect_habitat_associations.R")

#### Create simulation experiment parameters ####

df_experiment <- expand.grid(association_strength = association_strength, fract_dim = fract_dim, n = n, 
                             n_random = n_random) %>% 
  dplyr::slice(rep(x = 1:dplyr::n(), each = iterations)) %>% 
  tibble::tibble()

#### Define HPC function ####

foo_hpc <- function(fract_dim, n, association_strength, n_random) {
  
  # create simulation landscape with 5 discrete classes
  simulation_habitat <- NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows, resolution = resolution,
                                      fract_dim = fract_dim, verbose = FALSE, cPrintlevel = 0) %>% 
    terra::rast() %>% 
    shar::classify_habitats(n = n)
  
  # create simulation pattern with 4 species  
  simulation_pattern <- create_simulation_pattern(raster = simulation_habitat,
                                                  number_points = number_points,
                                                  association_strength = association_strength)
  
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
    shar::results_habitat_association(pattern = ., raster = torus_trans, verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_1 <- detect_habitat_associations(input = associations_species_1, 
                                                     species_type = names_species[1])
  
  # species 2
  
  associations_species_2 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2) %>% 
    shar::results_habitat_association(pattern = ., raster = torus_trans, verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_2 <- detect_habitat_associations(input = associations_species_2, 
                                                     species_type = names_species[2])
  
  # species 3
  
  associations_species_3 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 3) %>% 
    shar::results_habitat_association(pattern = ., raster = torus_trans, verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_3 <- detect_habitat_associations(input = associations_species_3,
                                                     species_type = names_species[3])
  
  # species 4
  
  associations_species_4 <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 4) %>% 
    shar::results_habitat_association(pattern = ., raster = torus_trans, verbose = FALSE)
  
  # count correct/false detections of species-habitat associations
  detection_species_4 <- detect_habitat_associations(input = associations_species_4, 
                                                     species_type = names_species[4])
  
  # combine results of current association strength to one data frame
  dplyr::bind_rows(detection_species_1, detection_species_2, detection_species_3, detection_species_4) %>% 
    dplyr::mutate(.before = correct, species = 1:4, fract_dim = fract_dim, n = n,
                  association_strength = association_strength, n_random = n_random)
  
}

#### Submit HPC ####

globals <- c("number_coloumns", "number_rows", "resolution", # nlm_fbm
             "number_points", # create_simulation_pattern
             # "n_random", # randomize_raster
             "create_simulation_pattern", "create_simulation_species") # helper functions

torus_trans <- rslurm::slurm_apply(f = foo_hpc, params = df_experiment, 
                                      global_objects = globals, jobname = "torus_trans",
                                      nodes = nrow(df_experiment), cpus_per_node = 1, 
                                      slurm_options = list("partition" = "medium",
                                                           "time" = "12:00:00", ## hh:mm::ss
                                                           "mem-per-cpu" = "2G"),
                                      pkgs = c("dplyr", "maptools", "mobsim", "NLMR", "sf", "spatstat.geom", 
                                               "spatstat.random", "stringr", "terra"),
                                      rscript_path = rscript_path, submit = FALSE)

#### Collect results #### 

suppoRt::rslurm_missing(x = torus_trans)

cv_result <- rslurm::get_slurm_out(torus_trans)

suppoRt::save_rds(object = torus_trans, filename = "torus_trans.rds",
                  path = "02_Data/", overwrite = FALSE)

rslurm::cleanup_files(torus_trans)
