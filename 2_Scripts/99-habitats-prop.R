##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

#### Import packages & functions ####

source("1_Functions/setup.R")

simulation_experiment_list <- readRDS("3_Data/simulation_experiment_list.rds")

#### Extract and count habitats ####

habitats_count <- purrr::map_dfr(simulation_experiment_list, function(i) {
  
    count <- terra::unwrap(i$habitat) |> 
      terra::values(mat = FALSE) |> 
      table() |> 
      as.integer()
  
    data.frame(habitat = 1:5, rel_count = count / sum(count) * 100, 
               fract_dim = i$parameter[[1]])}, .id = "id") |> 
  dplyr::mutate(fract_dim = factor(fract_dim, levels = c(1.65, 0.5), 
                                   labels = c("Low fragmentation", "High fragmentation")), 
                habitat = factor(habitat, ordered = TRUE))

dplyr::group_by(habitats_count, fract_dim) |> 
  dplyr::summarise(min = round(min(rel_count), 1), max = round(max(rel_count), 1))

#### Association count #### 

assoc_count <- purrr::map_dfr(simulation_experiment_list, function(i) {
  
  # name of species include type of association
  names_species <- as.character(unique(i$pattern$marks$species))
  
  habitat <- stringr::str_extract(string = names_species, pattern = "(\\d)+") |> 
    as.numeric()
  
  data.frame(species = 1:4, habitat = habitat, fract_dim = i$parameter[[1]])}, .id = "id") |> 
  dplyr::mutate(habitat = factor(habitat, ordered = TRUE), 
                fract_dim = factor(fract_dim, levels = c(1.65, 0.5), 
                                   labels = c("Low fragmentation", "High fragmentation"))) |> 
  dplyr::group_by(fract_dim, habitat) |> 
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |> 
  dplyr::mutate(y = 45)

#### Create ggplot ###

gg_habitats <- ggplot(data = habitats_count, aes(x = habitat, y = rel_count, col = habitat)) + 
  geom_point(position = position_jitter(width = 0.35, seed = 42), alpha = 1/4) +
  geom_text(data = assoc_count, aes(x = habitat, y = y, label = paste0("# Assoc.:\n", n)), 
            size = 3.0) + 
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  facet_wrap(. ~ fract_dim) + 
  scale_color_manual(values = MetBrewer::met.brewer(name = "Java", n = 5, type = "discrete")) +
  labs(x = "Habitat class", y = "Proportion of discrete habitat class [%]") +
  scale_y_continuous(limits = c(0, 50)) +
  theme_bw(base_size = 12) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(), strip.text = element_text(hjust = 0))

#### Save figures ####

suppoRt::save_ggplot(plot = gg_habitats, path = "4_Figures/", filename = "Fig-S3.png",
                     dpi = dpi, width = width, height = height * 1/2, units = units, 
                     overwrite = FALSE)
