##-----------------------------------------------##
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    mhk.hesselbarth@gmail.com                  ##
##    www.github.com/mhesselbarth                ##
##-----------------------------------------------##

# Purpose of script: 

source("1_Functions/setup.R")

# set seed
set.seed(42, kind = "L'Ecuyer-CMRG")

#### Create example data ####

landscape_df <- purrr::map_dfr(fract_dim, function(i) {
  
  NLMR::nlm_fbm(ncol = number_coloumns, nrow = number_rows, resolution = resolution, fract_dim = i, 
                verbose = FALSE, cPrintlevel = 0, user_seed = 42) %>%
    terra::rast() %>% 
    shar::classify_habitats(n = 5, style = "fisher") %>% 
    terra::as.data.frame(xy = TRUE) %>% 
    dplyr::bind_cols(fract_dim = i)}) %>% 
  dplyr::mutate(fract_dim = factor(fract_dim, levels = c(1.5, 1, 0.5), 
                                   labels = c("Low fragmentation", "Medium fragmentation", "High fragmentation")), 
                layer = factor(layer))

#### Create ggplot #### 

ggplot_landscape <- ggplot(data = landscape_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  facet_wrap(. ~ fract_dim) + 
  scale_fill_viridis_d() +
  theme_classic(base_size = 10) + 
  theme(aspect.ratio = 1, legend.position = "none", 
        strip.text = element_text(hjust = 0), strip.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "mm"))

#### Save ggplot ####

suppoRt::save_ggplot(plot = ggplot_landscape, filename = "4_Figures/Fig-2.pdf", overwrite = FALSE, 
                     dpi = 900, height = height * 0.5, width = width, units = units)
