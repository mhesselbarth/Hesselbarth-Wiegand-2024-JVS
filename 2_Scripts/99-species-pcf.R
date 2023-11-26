##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

#### Import packages & functions ####

source("1_Functions/setup.R")

# source all functions in R_functions folder
list.files(path = "1_Functions/", full.names = TRUE) |> 
  purrr::walk(function(x) source(x))

simulation_experiment_list <- readRDS("3_Data/simulation_experiment_list.rds")

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
                                 labels = c("CSR (positive association)", "Cluster process (positive association)",
                                            "CSR (negative association)", "Cluster process (negative association)")),
    assoc = factor(assoc, ordered = TRUE))

#### Create ggplot2 ####

gg_pcf <- ggplot(data = pcf_sum, aes(x = r, y = iso, color = assoc)) +
  geom_line() + 
  geom_hline(yintercept = 1, linetype = 2, color = "grey") +
  scale_color_viridis_d(name = "Association  strength", option = "C") +
  facet_wrap(. ~ species, scales = "fixed", ncol = 2, nrow = 2) + 
  labs(x = expression(paste("Distance ", italic("r"), " in meters [m]")), 
       y = expression(paste("Pair-correlation function ", italic("g(r)")))) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom", strip.background = element_blank(), 
        strip.text = element_text(hjust = 0))

suppoRt::save_ggplot(plot = gg_pcf, path = "4_Figures/", filename = "Fig-S6.png",
                     dpi = dpi, width = width, height = height * 1/2, units = units, 
                     overwrite = FALSE)
