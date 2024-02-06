##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1-Functions/setup.R")

source("1-Functions/create-simulation-pattern.R")
source("1-Functions/create-simulation-species.R")

RandomFields::RFoptions(install = "no")

#### Create example data ####

# b = gamma; c = reconstruction; d = torus; e = random walk

# create landscape
simulation_landscape <- NLMR::nlm_fbm(ncol = number_cols, nrow = number_rows, 
                                      resolution = resolution, fract_dim = 1.5, 
                                      verbose = FALSE, cPrintlevel = 0, user_seed = 42) |> 
  terra::rast() |> 
  shar::classify_habitats(n = 5, style = "fisher")

# create pattern with 4 species
simulation_pattern <- create_simulation_pattern(raster = simulation_landscape, 
                                                number_points = 100, 
                                                association_strength = 0.35)

# pick species 2 as example
example_species <- spatstat.geom::subset.ppp(simulation_pattern, species_code == 2)

#### Randomize data ####

# fit clustered pattern to data
gamma_test <- shar::fit_point_process(spatstat.geom::unmark(example_species), n_random = 1,
                                      process = "cluster")

# translate habitats
torus_trans <- shar::translate_raster(raster = simulation_landscape, steps_x = 10, steps_y = 25)

# randomize habitats
random_walk <- shar::randomize_raster(raster = simulation_landscape, n_random  = 1)

# reconstruct pattern
pattern_recon <- shar::reconstruct_pattern(pattern = spatstat.geom::unmark(example_species), 
                                           method = "cluster", n_random = 1, max_runs = 10000)

#### Convert all to data.frames ####

# convert to tibble
gamma_test_df <- tibble::as_tibble(spatstat.geom::as.data.frame.ppp(gamma_test$randomized[[1]]))

torus_trans_df <- tibble::as_tibble(terra::as.data.frame(torus_trans$randomized[[1]], xy = TRUE))

random_walk_df <- tibble::as_tibble(raster::as.data.frame(random_walk$randomized[[1]], xy = TRUE))

pattern_recon_df <- tibble::as_tibble(spatstat.geom::as.data.frame.ppp(pattern_recon$randomized[[1]]))

simulation_landscape_df <- terra::as.data.frame(simulation_landscape, xy = TRUE)

example_species_df <- tibble::as_tibble(spatstat.geom::as.data.frame.ppp(example_species))

#### Setup ggplot globals ####

# set point size
size_point <- 1.5

size_base <- 10.0

shape_point <- 1

color_point <- "black"

#### Create single ggplots ####

ggplot_observed <- ggplot() +
  geom_raster(data = simulation_landscape_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = example_species_df, aes(x = x, y = y), size = size_point, color = color_point, 
             shape = shape_point) +
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Demuth", n = 5, type = "discrete")) +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none",
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.0))

ggplot_gamma <- ggplot() +
  geom_raster(data = simulation_landscape_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = gamma_test_df, aes(x = x, y = y), size = size_point, color = color_point, 
             shape = shape_point) +
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Demuth", n = 5, type = "discrete")) +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none",
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.0))

ggplot_recon <- ggplot() +
  geom_raster(data = simulation_landscape_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = pattern_recon_df, aes(x = x, y = y), size = size_point, color = color_point, 
             shape = shape_point) +
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Demuth", n = 5, type = "discrete")) +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none",
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.0))

ggplot_torus <- ggplot() +
  geom_raster(data = torus_trans_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = example_species_df, aes(x = x, y = y), size = size_point, color = color_point, 
             shape = shape_point) +
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Demuth", n = 5, type = "discrete")) +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none",
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.0))

ggplot_walk <- ggplot() +
  geom_raster(data = random_walk_df, aes(x = x, y = y, fill = factor(layer))) +
  geom_point(data = example_species_df, aes(x = x, y = y), size = size_point, color = color_point, 
             shape = shape_point) +
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Demuth", n = 5, type = "discrete")) +
  theme_classic(base_size = size_base) + 
  theme(aspect.ratio = 1, legend.position = "none",
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.0))


#### Create total plot ####

ggplot_random <- cowplot::plot_grid(ggplot_gamma, ggplot_recon, ggplot_torus, ggplot_walk,
                                    nrow = 2, ncol = 2, labels = c("(b) gamma-test            ", 
                                                                   "(c) pattern reconstruction",
                                                                   "(d) torus-translation     ", 
                                                                   "(e) randomized-habitats   "), 
                                    label_fontface = "plain", label_size = size_base, 
                                    label_x = 0, label_y = 0.9, hjust = -0.15)

ggplot_total <- cowplot::plot_grid(ggplot_observed, ggplot_random, ncol = 2,
                                   labels = c("(a) Observed", ""), label_fontface = "plain", label_size = size_base,
                                   label_x = 0, label_y = 0.9)

### Save ggplot ####

suppoRt::save_ggplot(plot = ggplot_total, filename = "4-Figures/Fig-1.png", overwrite = T, 
                     dpi = dpi, height = height * 0.5, width = width, units = units)

