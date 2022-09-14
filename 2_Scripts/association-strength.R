##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

# Purpose of script: 

source("1_Functions/setup.R")

#### Load data ####

gamma_df <- readRDS("3_Data/gamma_test.rds")

reconstruction_df <- readRDS("3_Data/pattern_recon.rds")

torus_df <- readRDS("3_Data/torus_trans.rds")

walk_df <- readRDS("3_Data/habitat_random.rds")

combined_df <- dplyr::bind_rows(gamma = gamma_df, reconstruction = reconstruction_df, 
                                torus = torus_df, walk = walk_df, .id = "method") %>% 
  tibble::as_tibble()

#### Summarize data ####

summarized_df <- dplyr::group_by(combined_df, method, species, fract_dim, n, 
                                 association_strength, n_random) %>% 
  dplyr::summarise(correct_mn = mean(correct) * 100, correct_sd = sd(correct) * 100,
                   false_mn = mean(false) * 100, false_sd = sd(false) * 100, 
                   .groups = "drop") %>% 
  dplyr::mutate(species = factor(species, levels = c(1, 2, 3, 4), 
                                 labels = c("CSR (positive assoc.)", "Cluster process (positive assoc.)", 
                                            "CSR (negative assoc.)", "Cluster process (negative assoc.)")), 
                fract_dim = factor(fract_dim, levels = c(1.5, 1, 0.5), 
                                   labels = c("Low fragmentation", "Medium fragmentation", "High fragmentation")), 
                n_random = factor(n_random, levels = c(19, 39, 199), 
                                  labels = c("n random: 19", "n random: 39", "n random: 199")))

#### Setup plot globals ####

color_scale <- c("CSR (positive assoc.)" = "#df4e25", "Cluster process (positive assoc.)" = "#007aa1", 
                 "CSR (negative assoc.)" = "#41b282", "Cluster process (negative assoc.)" = "#fcb252")

color_alpha <- 0.5

size_base <- 10

#### Create dummy plot for legeend ####

ggplot_dummy <- data.frame(species = levels(summarized_df$species),
                           association_strength = c(0, 0, 0, 0), correct_mn = c(1, 1, 1, 1)) %>% 
  ggplot(aes(x = association_strength,  y = correct_mn, color = species, group = species)) + 
  geom_line(alpha = color_alpha) + 
  geom_point(size = 2.5) + 
  scale_color_manual(name = "", values = color_scale) +
  guides(color = guide_legend(nrow = 2, byrow = FALSE)) + 
  theme_classic(base_size = size_base) + 
  theme(legend.position = "bottom")

#### Create ggplot correct detections ####

ggplot_correct_list <- purrr::map(c("gamma", "reconstruction", "torus", "walk"), function(i) {
  
  dplyr::filter(summarized_df, method == i) %>% 
    ggplot(aes(x = association_strength, y = correct_mn, color = species, group = species)) + 
    geom_hline(yintercept = 0.0, color = "grey", linetype = 2) +  
    geom_line(alpha = color_alpha) + 
    geom_point() + 
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_color_manual(values = color_scale) +
    facet_grid(rows = dplyr::vars(n_random), cols  = dplyr::vars(fract_dim)) + 
    labs(x = "", y = "") +
    theme_classic(base_size = size_base) + 
    theme(legend.position = "none")
  
})

# a = gamma; b = reconstruction; c = torus; d = walk
ggplot_correct_total <- cowplot::plot_grid(plotlist = ggplot_correct_list, labels = c("a)", "b)", "c)", "d)"))

ggplot_correct_total <- cowplot::ggdraw(ggplot_correct_total, xlim = c(-0.015, 1.0)) + 
  cowplot::draw_label("Habitat association strength", x = 0.5, y = 0, vjust = -0.5, angle = 0, size = size_base) + 
  cowplot::draw_label("Correct association detected [%]", x = 0.0, y = 0.5, vjust = 0.0, 
                      angle = 90, size = size_base)

ggplot_correct_total <- cowplot::plot_grid(ggplot_correct_total, cowplot::get_legend(ggplot_dummy),
                                   nrow = 2, ncol = 1, rel_heights = c(0.95, 0.05))

#### Create ggplot false detections ####

ggplot_false_list <- purrr::map(c("gamma", "reconstruction", "torus", "walk"), function(i) {
  
  dplyr::filter(summarized_df, method == i) %>% 
    ggplot(aes(x = association_strength, y = false_mn, color = species, group = species)) + 
    geom_hline(yintercept = 0.0, color = "grey", linetype = 2) +  
    geom_line(alpha = color_alpha) + 
    geom_point() + 
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_color_manual(values = color_scale) +
    facet_grid(rows = dplyr::vars(n_random), cols  = dplyr::vars(fract_dim)) + 
    labs(x = "", y = "") +
    theme_classic(base_size = size_base) + 
    theme(legend.position = "none")
  
})

# a = gamma; b = reconstruction; c = torus; d = walk
ggplot_false_total <- cowplot::plot_grid(plotlist = ggplot_false_list, labels = c("a)", "b)", "c)", "d)"))

ggplot_false_total <- cowplot::ggdraw(ggplot_false_total, xlim = c(-0.015, 1.0)) + 
  cowplot::draw_label("Habitat association strength", x = 0.5, y = 0, vjust = -0.5, angle = 0, size = size_base) + 
  cowplot::draw_label("false association detected [%]", x = 0.0, y = 0.5, vjust = 0.0, 
                      angle = 90, size = size_base)

ggplot_false_total <- cowplot::plot_grid(ggplot_false_total, cowplot::get_legend(ggplot_dummy),
                                           nrow = 2, ncol = 1, rel_heights = c(0.95, 0.05))

#### Save ggplots

suppoRt::save_ggplot(plot = ggplot_correct_total, filename = "4_Figures/Fig-3.pdf", overwrite = FALSE, 
                     dpi = dpi, height = width * 0.75, width = height, units = units)

suppoRt::save_ggplot(plot = ggplot_false_total, filename = "4_Figures/Fig-A2.pdf", overwrite = FALSE, 
                     dpi = dpi, height = width * 0.75, width = height, units = units)
