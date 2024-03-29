##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##                                               ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

source("1-Functions/setup.R")
library(landscapemetrics)

RandomFields::RFoptions(install = "no")

#### Create example data ####

# create two landscape with fragmentation parameter and convert to data frame
landscape_df <- purrr::map_dfr(fract_dim, function(i) {
  
  NLMR::nlm_fbm(ncol = number_cols, nrow = number_rows, resolution = resolution, fract_dim = i, 
                verbose = FALSE, cPrintlevel = 0, user_seed = 42) |>
    terra::rast() |> 
    shar::classify_habitats(n = 5, style = "fisher") |> 
    terra::as.data.frame(xy = TRUE) |> 
    dplyr::bind_cols(fract_dim = i)}) |> 
  dplyr::mutate(fract_dim = factor(fract_dim, levels = c(1.65, 0.5), 
                                   labels = c("(a) Low fragmentation", "(b) High fragmentation")), 
                layer = factor(layer))

#### Create ggplot #### 

ggplot_landscape <- ggplot(data = landscape_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  facet_wrap(. ~ fract_dim) + 
  scale_fill_manual(values = MetBrewer::met.brewer(name = "Demuth", n = 5, type = "discrete")) +
  theme_classic(base_size = 10) + 
  theme(aspect.ratio = 1, legend.position = "none", 
        strip.text = element_text(hjust = 0, size = 10), strip.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.0))

#### Calculate lsm patch area ####

patch_area_df <- dplyr::group_by(landscape_df, fract_dim) |> 
  dplyr::group_split() |> 
  purrr::map(function(i) terra::rast(i[ ,-4], type = "xyz")) |> 
  terra::rast() |> 
  landscapemetrics::calculate_lsm(what = "lsm_l_area_mn")

#### Save ggplot ####

suppoRt::save_ggplot(plot = ggplot_landscape, filename = "4-Figures/Fig-3.png", overwrite = FALSE, 
                     dpi = dpi, height = height * 0.35, width = width, units = units)
