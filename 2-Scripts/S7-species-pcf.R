##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

#### Import packages & functions ####

source("1-Functions/setup.R")

simulation_experiment_list <- readRDS("3-Data/main-sim-experiment.rds")

#### Extract species ####

# Random

species_1 <- purrr::map_dfr(simulation_experiment_list, function(i) {
  
  spatstat.geom::subset.ppp(i$pattern, species_code == 1) |> 
    spatstat.explore::pcf.ppp(i, divisor = "d", correction = "Ripley", 
                              r = seq(from = 0, to = 250, length.out = 515)) |> 
    tibble::as_tibble() |> dplyr::mutate(species = "csr-pos", assoc = i$parameter[[3]])}, 
  .id = "id")

species_3 <- purrr::map_dfr(simulation_experiment_list, function(i) {
  
  spatstat.geom::subset.ppp(i$pattern, species_code == 3) |> 
    spatstat.explore::pcf.ppp(i, divisor = "d", correction = "Ripley", 
                              r = seq(from = 0, to = 250, length.out = 515)) |> 
    tibble::as_tibble() |> dplyr::mutate(species = "csr-neg", assoc = i$parameter[[3]])}, 
  .id = "id")

# Cluster

species_2 <- purrr::map_dfr(simulation_experiment_list, function(i) {
  
  spatstat.geom::subset.ppp(i$pattern, species_code == 2) |> 
    spatstat.explore::pcf.ppp(i, divisor = "d", correction = "Ripley", 
                              r = seq(from = 0, to = 250, length.out = 515)) |> 
    tibble::as_tibble() |> dplyr::mutate(species = "clu-pos", assoc = i$parameter[[3]])}, 
  .id = "id")

species_4 <- purrr::map_dfr(simulation_experiment_list, function(i) {
  
  spatstat.geom::subset.ppp(i$pattern, species_code == 4) |> 
    spatstat.explore::pcf.ppp(i, divisor = "d", correction = "Ripley", 
                              r = seq(from = 0, to = 250, length.out = 515)) |> 
    tibble::as_tibble() |> dplyr::mutate(species = "clu-neg", assoc = i$parameter[[3]])}, 
  .id = "id")

#### Summarize pcf #### 

pcf_sum <- dplyr::bind_rows(species_1, species_3, species_2, species_4) |> 
  dplyr::group_by(species, assoc, r) |> 
  dplyr::summarise(iso = mean(iso), .groups = "drop") |> 
  # dplyr::filter(assoc %in% c(0.1, 0.5, 0.9)) |> 
  dplyr::mutate(species = factor(species, levels = c("csr-pos", "clu-pos", "csr-neg", "clu-neg"), 
                                 labels = c("(a) CSR (positive association)", "(b) Cluster process (positive association)",
                                            "(c) CSR (negative association)", "(d) Cluster process (negative association)")),
    assoc = factor(assoc, ordered = TRUE))

#### Create ggplot2 ####

gg_pcf <- ggplot(data = pcf_sum, aes(x = r, y = iso, color = assoc)) +
  geom_line() + 
  geom_hline(yintercept = 1, linetype = 2, color = "grey") +
  scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                                "#33a02c", "#fb9a99", "#e31a1c", 
                                "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"), 
                     name = "Association  strength") +
  # scale_color_viridis_d(name = "Association  strength", option = "C") +
  facet_wrap(. ~ species, scales = "fixed", ncol = 2, nrow = 2) + 
  labs(x = expression(paste("Distance ", italic("r"), " in meters [m]")), 
       y = expression(paste("Pair-correlation function ", italic("g(r)")))) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", strip.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(hjust = 0))

suppoRt::save_ggplot(plot = gg_pcf, path = "4-Figures/", filename = "Fig-S7.png",
                     dpi = dpi, width = width, height = height * 1/2, units = units, 
                     overwrite = FALSE)
