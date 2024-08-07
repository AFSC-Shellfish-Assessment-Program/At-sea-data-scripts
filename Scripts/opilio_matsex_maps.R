

# Load packages
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(sf)
library(akgfmaps)
library(terra)
library(shadowtext)
library(patchwork)
library(ggh4x)
library(ggridges)
library(coldpool)
library(ggpubr)

# Load lookup tables and spatial data
source("~/GitHub/Tech-Memo/Scripts/lookup_tables.R")
source("~/GitHub/survey_data_processing/Scripts/spatial_setup.R")


# Set path for most recent FTP'd data on Kodiak server 
path <- "Y:/KOD_Survey/EBS Shelf/2024/RawData/"

# Now read in all specimen tables from FTP'd data 
dat <- list.files(path, pattern = "CRAB_SPECIMEN", recursive = TRUE) %>% 
       purrr::map_df(~read.csv(paste0(path, .x)))

# Read in master stations list (should be 349 standard stations)
stations <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/lookup_tables/survey_stations.csv") %>%
            dplyr::filter(TYPE == "standard") %>%
            rename(STATION = STATION_ID) %>%
            select(-c(TYPE))

# Visually check stations from FTP'd data
# sort(unique(dat$STATION))

# Standardize station naming, filter for stations 
data <- dat %>%
        filter(str_detect(STATION, "-")) %>% #additional filter to remove any corner stations
        #Standardize station name notation to ensure there were no station name tablet entry errors  
        separate(STATION, sep = "-", into = c("col", "row", "tow")) %>%
        filter(is.na(tow)) %>% #this will drop any "-B" 15 min tow stations
        filter(col %in% LETTERS[seq(from = 1, to = 26)]) %>% # drop non-alphabet columns (ie. slope)
        dplyr::select(-tow) %>%
        mutate(row = str_pad(row, width = 2, pad = "0")) %>% #make sure all names have leading zeros 
        unite("STATION", col:row, sep = "-") %>%
        filter(SPECIES_CODE == 68580) %>%
        mutate(WIDTH_1MM = floor(WIDTH)) %>%
        right_join(., stations) # join with standard station list to add in 0-catch stations
        # Ignore the warning message:
        # "Expected 3 pieces. Missing pieces filled with `NA`"

year = 2024
recent = 2024
stock = "Snow"

# assign sex/maturity categories
mature <- data %>%
          dplyr::filter(SEX %in% 1:2) %>%
          mutate(MAT_SEX = case_when((SEX == 1 & WIDTH_1MM >= mat_lookup$cutline[mat_lookup$stock == stock]) ~ "Mature Male",
                                     (SEX == 1 & WIDTH_1MM < mat_lookup$cutline[mat_lookup$stock == stock]) ~ "Immature Male",
                                     (SEX == 2 & CLUTCH_SIZE >= 1) ~ "Mature Female",
                                     (SEX == 2 & CLUTCH_SIZE == 0) ~ "Immature Female")) 
#for legal/pre-recruit males
leg <- data %>%  
       dplyr::filter(SEX %in% 1) %>%
       mutate(MAT_SEX = case_when((WIDTH_1MM >= mat_lookup$legal[mat_lookup$stock == stock]) ~ "Legal Male"))

#for industry-preferred males
ind_pref <- data %>%  
            dplyr::filter(SEX %in% 1) %>%
            mutate(MAT_SEX = case_when((WIDTH_1MM > mat_lookup$recruit[mat_lookup$stock == stock]) ~ "Industry Preferred Male"))

data_crab <- rbind(mature, leg, ind_pref) 

# Aggregate counts by station and sex/mat
counts <- data_crab %>%
          dplyr::group_by(STATION, MAT_SEX, LATITUDE, LONGITUDE)%>%
          dplyr::summarise(COUNT = sum(SAMPLING_FACTOR)) %>%
          na.omit()

mat_sex_combos <- c("Mature Male", "Immature Male",  
                    "Legal Male", "Industry Preferred Male",
                    "Mature Female", "Immature Female")

#Create dataframe with all stations to retain zero catch stations
haul_sex_combos <- expand_grid(MAT_SEX = mat_sex_combos,
                   stations)

#Join to positive catch stations 
station_counts <- counts %>%
                  right_join(haul_sex_combos,
                             by = c("STATION", "MAT_SEX", "LATITUDE", "LONGITUDE")) %>%
                  replace_na(list(COUNT = 0)) %>%
                  group_by(STATION, MAT_SEX) %>%
                  mutate(COMMON_NAME = rep("Snow Crab"))

# load mapping function
count_map <- function(data, mat_sex_combos){
  count_data <- data %>% 
                filter(MAT_SEX == mat_sex_combos)
  
  #Define common plotting breaks across all years, if multiple
  breaks <- eval_plot_breaks(CPUE = count_data$COUNT, styles = c("equal", "quantile", "kmeans", "hclust", "fisher", "jenks"), 
                             n.breaks = 5) %>%
            filter(style=="kmeans") 
  
  map_layers <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs="auto")
  
  
  # Load 2018 NBS survey area, project to interpolation crs
  survey_2018_layer <- st_transform(survey_2018_layer, map_layers$crs)
  
  # Transform cpue data frame to plotting crs
  count_data2 <- count_data %>%
    sf::st_as_sf(coords = c(x = "LONGITUDE", y = "LATITUDE"), 
                 crs = sf::st_crs("+proj=longlat")) %>% 
    sf::st_transform(crs = map_layers$crs) 
  
  # Inverse distance weighting
  idw_fit <- gstat::gstat(formula = COUNT ~ 1, locations = count_data2, nmax = 4)
  
  # Predict station points
  stn.predict <- predict(idw_fit, count_data2)
  
  # Generate extrapolation grid
  extrap.box = c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)
  grid.cell = c(0.02, 0.02)
  
  sp_extrap.raster <- raster::raster(xmn = extrap.box['xmn'],
                                     xmx=extrap.box['xmx'],
                                     ymn=extrap.box['ymn'],
                                     ymx=extrap.box['ymx'],
                                     ncol=(extrap.box['xmx']-extrap.box['xmn'])/grid.cell,
                                     nrow=(extrap.box['ymx']-extrap.box['ymn'])/grid.cell,
                                     crs = raster::crs("+proj=longlat")) %>% 
    raster::projectRaster(crs = raster::crs(count_data2))
  
  # Predict, rasterize, mask
  extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(crs = raster::crs(count_data2)) %>% 
    stars::st_rasterize() %>% 
    sf::st_join(map_layers$survey.area, join = st_intersects) %>%
    mutate(var1.pred = ifelse(is.na(AREA_KM2), NA, var1.pred))
  
  # Format breaks for plotting
  # Automatic break selection based on character vector.
  set.breaks <- as.numeric(breaks[,2:7])
  alt.round <- 0 # Set alternative rounding factor to zero based on user-specified breaks
  
  if(is.character(set.breaks[1])) {
    set.breaks <- tolower(set.breaks)
    
    # Set breaks
    break.vals <- classInt::classIntervals(x$COUNT, n = 5, style = set.breaks)$brks
    
    # Setup rounding for small counts 
    alt.round <- floor(-1*(min((log10(break.vals)-2)[abs(break.vals) > 0])))
    
    set.breaks <- c(-1, round(break.vals, alt.round))
  }
  
  # Ensure breaks go to zero
  if(min(set.breaks) > 0) {
    set.breaks <- c(0, set.breaks)
  }
  
  if(min(set.breaks) == 0) {
    set.breaks <- c(-1, set.breaks)
  }
  
  # Ensure breaks span the full range
  if(max(set.breaks) < max(stn.predict$var1.pred)){
    set.breaks[length(set.breaks)] <- max(stn.predict$var1.pred) + 1
  }
  
  
  # Trim breaks to significant digits to account for differences in range among species
  dig.lab <- 7
  set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
  
  if(alt.round > 0) {
    while(dig.lab > alt.round) { # Rounding for small counts
      dig.lab <- dig.lab - 1
      set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
    }
  } else { # Rounding for large counts
    while(length(grep("\\.", set.levels)) > 0) {
      dig.lab <- dig.lab - 1
      set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
    }
  }
  
  # Cut extrapolation grid to support discrete scale
  extrap.grid$var1.pred <- cut(extrap.grid$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
  
  # Which breaks need commas?
  sig.dig <- round(set.breaks[which(nchar(round(set.breaks)) >= 4)])
  
  # Drop brackets, add commas, create 'No catch' level to legend labels
  make_level_labels <- function(vec) {
    vec <- as.character(vec)
    vec[grep("-1", vec)] <- "No catch"
    vec <- sub("\\(", "\\>", vec)
    vec <- sub("\\,", "-", vec)
    vec <- sub("\\]", "", vec)
    if(length(sig.dig) > 3) {
      for(j in 1:length(sig.dig)) {
        vec <- sub(sig.dig[j], format(sig.dig[j], nsmall=0, big.mark=","), vec)
      }
    }
    return(vec)
  }
  
  # Assign level names to breaks for plotting
  extrap.grid$var1.pred <- factor(make_level_labels(extrap.grid$var1.pred), 
                                  levels = make_level_labels(levels(set.levels)))
  
  # Number of breaks for color adjustments
  n.breaks <- length(levels(set.levels))
  
  
  # Trim survey grid to survey area
  survey.grid <- map_layers$survey.grid %>%
    st_transform(crs = st_crs(map_layers$survey.area)) %>%
    st_intersection(map_layers$survey.area) 
  
  plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-178, -156), 
                                                                 y = c(54.5, 61.6)),
                                                      out.crs = map_layers$crs) 
  
  #Create line feature at 166 W for Tanner boundary
  line <- transform_data_frame_crs(data.frame(x = c(-166, -166, -166, -166), y= c(51, 58, 60, 65)),
                                   out.crs = map_layers$crs)
  
  
  #Create plot labels
  districts <- data.frame(type = c("district", "district", "district", "district", "district", "district"),
                          region = c("bs.south", "bs.south", "bs.south", "bs.south", "bs.south", "bs.north"),
                          lab = c("Bristol Bay District", "Pribilof District", "Northern District", 'St. Matthew Island Section', "166ᵒW", "Norton Sound District"),
                          x = c(-165, -171.25, -172, -173, -166.9, -166.9),
                          y = c(56, 58.3, 59.5, 59.2, 61, 64))
  
  map_labs <- labelz %>%
    rbind(districts)%>%
    filter(region == "bs.south") %>%
    mutate(y = ifelse(type == "bathymetry", y-0.7, y))%>%
    mutate(x = ifelse(type == "bathymetry" & lab == "50 m", x+2.1, x))

  
  placenames <- akgfmaps::transform_data_frame_crs(map_labs, out.crs=map_layers$crs)
  
  #Create mat_sex label
  mat.sex.lab <- akgfmaps::transform_data_frame_crs(data.frame(lab = mat_sex_combos, x = -158.5, y = 61.4), 
                                                    out.crs = map_layers$crs) 
  
  
  #Specify conditional plotting of baselayers based on function inputs
  recent = 2024
  num = ifelse(year < recent, 0, 0.2) #if num=0, layers don't have color and don't appear
  val1 = ifelse(year < recent, NA, "white")
  val2 = ifelse(year < recent, NA, "black")
  size = ifelse(year < recent, 5, 10)
  
  layer1 = geom_sf(data = st_as_sf(Pribsurveystrata_layer), fill=NA, color=NA)
  layer2 = geom_sf(data = st_as_sf(Pribsurveystrata_layer), fill=NA, color=NA)
  layer3 = geom_sf(data = st_as_sf(Pribsurveystrata_layer), fill=NA, color=NA)
  districtlabs = geom_shadowtext(data = subset(placenames, lab == c("166ᵒW")), 
                                 aes(x = x, y = y, label = lab), bg.color = NA, color = NA, size = 4, group = 99)
  
  species = "Opilio Crab"
  #Create IDW map customized for crab
  idw_plot <- ggplot() +
    stars::geom_stars(data = extrap.grid) +
    geom_sf(data = map_layers$bathymetry, color = alpha("grey70")) +
    geom_sf(data = map_layers$survey.area, fill = NA) +
    layer1+
    layer2+
    layer3+
    geom_sf(data = survey.grid, fill = NA, color = alpha("grey70", num))+
    geom_sf(data = map_layers$akland, fill = "grey80", size=0.1) +
    districtlabs+
    #stabathylabs+
    scale_fill_manual(name = "Count", 
                      values = c("white", RColorBrewer::brewer.pal(9, name = "Blues")[c(2,4,6,8,9)]), 
                      na.translate = FALSE, # Don't use NA
                      drop = FALSE) + # Keep all levels in the plot
    scale_x_continuous(breaks = map_layers$lon.breaks) + 
    scale_y_continuous(breaks = map_layers$lat.breaks) +
    geom_text(data = mat.sex.lab, aes(x = Inf, y = y, label = lab, hjust = 1), fontface = "bold", size = 5) +
    coord_sf(xlim = plot.boundary$x,
             ylim = plot.boundary$y)+
    theme(panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(fill = NA, color = "black"),
          legend.key = element_rect(fill = NA, color = "grey70"),
          legend.key.size = unit(0.65,'cm'),
          legend.position = c(ifelse(species == "Opilio Crab", 0.18, 0.13), 0.2), #was 12, 18 
          legend.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10), 
          legend.title = element_text(size = 10),
          plot.background = element_rect(fill = NA, color = NA))
  
  return(list(idw_plot=idw_plot))
  # ggsave(paste0("./Output/opie_maps/", mat_sex_combos[i], "_counts.pdf"),
  #        height = 5, width = 7, units = "in")
} 

# make maps
count_map_out <- mat_sex_combos %>% map(~count_map(station_counts, .x))

map_tile <- ggarrange(ggarrange(count_map_out[[3]]$idw_plot,
                                count_map_out[[4]]$idw_plot, 
                                count_map_out[[1]]$idw_plot,
                                count_map_out[[2]]$idw_plot,
                                count_map_out[[5]]$idw_plot,
                                count_map_out[[6]]$idw_plot,
                                nrow = 3, ncol = 2)) %>%
              annotate_figure(top = text_grob("2024 Snow Crab Counts", size = 20, face="bold"))

ggsave("./Output/opilio_matsex_count_maps.pdf", height = 16, width = 16, units = "in")








## OLD CODE - individual maps -----
# for(i in 1:length(mat_sex_combos)){
#   data <- station_counts %>% filter(MAT_SEX == mat_sex_combos[i])
# 
#   #Define common plotting breaks across all years, if multiple
#   breaks <- eval_plot_breaks(CPUE = data$COUNT, styles = c("equal", "quantile", "kmeans", "hclust", "fisher", "jenks"), 
#                              n.breaks = 5) %>%
#             filter(style=="kmeans") 
#   
#   map_layers <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs="auto")
#     
#   
#   # Load 2018 NBS survey area, project to interpolation crs
#   survey_2018_layer <- st_transform(survey_2018_layer, map_layers$crs)
#   
#   # Transform cpue data frame to plotting crs
#   cpue_data2 <- data %>%
#                 sf::st_as_sf(coords = c(x = "LONGITUDE", y = "LATITUDE"), 
#                              crs = sf::st_crs("+proj=longlat")) %>% 
#                 sf::st_transform(crs = map_layers$crs) 
#   
#   # Inverse distance weighting
#   idw_fit <- gstat::gstat(formula = COUNT ~ 1, locations = cpue_data2, nmax = 4)
#   
#   # Predict station points
#   stn.predict <- predict(idw_fit, cpue_data2)
#   
#   # Generate extrapolation grid
#   extrap.box = c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)
#   grid.cell = c(0.02, 0.02)
#   
#   sp_extrap.raster <- raster::raster(xmn = extrap.box['xmn'],
#                                      xmx=extrap.box['xmx'],
#                                      ymn=extrap.box['ymn'],
#                                      ymx=extrap.box['ymx'],
#                                      ncol=(extrap.box['xmx']-extrap.box['xmn'])/grid.cell,
#                                      nrow=(extrap.box['ymx']-extrap.box['ymn'])/grid.cell,
#                                      crs = raster::crs("+proj=longlat")) %>% 
#                      raster::projectRaster(crs = raster::crs(cpue_data2))
#   
#   # Predict, rasterize, mask
#   extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>% 
#                  sf::st_as_sf() %>% 
#                  sf::st_transform(crs = raster::crs(cpue_data2)) %>% 
#                  stars::st_rasterize() %>% 
#                  sf::st_join(map_layers$survey.area, join = st_intersects) %>%
#                  mutate(var1.pred = ifelse(is.na(AREA_KM2), NA, var1.pred))
#   
#   # Format breaks for plotting
#   # Automatic break selection based on character vector.
#   set.breaks <- as.numeric(breaks[,2:7])
#   alt.round <- 0 # Set alternative rounding factor to zero based on user-specified breaks
#   
#   if(is.character(set.breaks[1])) {
#     set.breaks <- tolower(set.breaks)
#     
#     # Set breaks
#     break.vals <- classInt::classIntervals(x$COUNT, n = 5, style = set.breaks)$brks
#     
#     # Setup rounding for small CPUE 
#     alt.round <- floor(-1*(min((log10(break.vals)-2)[abs(break.vals) > 0])))
#     
#     set.breaks <- c(-1, round(break.vals, alt.round))
#   }
#   
#   # Ensure breaks go to zero
#   if(min(set.breaks) > 0) {
#     set.breaks <- c(0, set.breaks)
#   }
#   
#   if(min(set.breaks) == 0) {
#     set.breaks <- c(-1, set.breaks)
#   }
#   
#   # Ensure breaks span the full range
#   if(max(set.breaks) < max(stn.predict$var1.pred)){
#     set.breaks[length(set.breaks)] <- max(stn.predict$var1.pred) + 1
#   }
#   
#   
#   # Trim breaks to significant digits to account for differences in range among species
#   dig.lab <- 7
#   set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
#   
#   if(alt.round > 0) {
#     while(dig.lab > alt.round) { # Rounding for small CPUE
#       dig.lab <- dig.lab - 1
#       set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
#     }
#   } else { # Rounding for large CPUE
#     while(length(grep("\\.", set.levels)) > 0) {
#       dig.lab <- dig.lab - 1
#       set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
#     }
#   }
#   
#   # Cut extrapolation grid to support discrete scale
#   extrap.grid$var1.pred <- cut(extrap.grid$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
#   
#   # Which breaks need commas?
#   sig.dig <- round(set.breaks[which(nchar(round(set.breaks)) >= 4)])
#   
#   # Drop brackets, add commas, create 'No catch' level to legend labels
#   make_level_labels <- function(vec) {
#                                       vec <- as.character(vec)
#                                       vec[grep("-1", vec)] <- "No catch"
#                                       vec <- sub("\\(", "\\>", vec)
#                                       vec <- sub("\\,", "-", vec)
#                                       vec <- sub("\\]", "", vec)
#                                       if(length(sig.dig) > 3) {
#                                         for(j in 1:length(sig.dig)) {
#                                           vec <- sub(sig.dig[j], format(sig.dig[j], nsmall=0, big.mark=","), vec)
#                                         }
#                                       }
#                                       return(vec)
#                                     }
#   
#   # Assign level names to breaks for plotting
#   extrap.grid$var1.pred <- factor(make_level_labels(extrap.grid$var1.pred), 
#                                   levels = make_level_labels(levels(set.levels)))
#   
#   # Number of breaks for color adjustments
#   n.breaks <- length(levels(set.levels))
#   
#   
#   # Trim survey grid to survey area
#   survey.grid <- map_layers$survey.grid %>%
#                  st_transform(crs = st_crs(map_layers$survey.area)) %>%
#                  st_intersection(map_layers$survey.area) 
#   
#   # Specify plot boundary based on EBS vs. NBS plotting
#   # if(calc_factor %in% c("EBS", "BBRKC", "PribRKC", "PribBKC", "StMattBKC", "TannerE", "TannerW")) {
#     
#     plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-178, -156), 
#                                                                    y = c(54.5, 61.6)),
#                                                         out.crs = map_layers$crs) 
#   #   
#   # } else if(calc_factor %in% c("NBS", "NSRKC")){
#   #   
#   #   plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-179, -154), #change for NBS plotting only
#   #                                                                  y = c(54.5, 64.5)), 
#   #                                                       out.crs = map_layers$crs) 
#   #   
#   # } else{
#   #   
#   #   plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-179, -154), 
#   #                                                                  y = c(54.5, 64.5)), 
#   #                                                       out.crs = map_layers$crs) 
#   # }
#   
#   
#   #Create line feature at 166 W for Tanner boundary
#   line <- transform_data_frame_crs(data.frame(x = c(-166, -166, -166, -166), y= c(51, 58, 60, 65)),
#                                    out.crs = map_layers$crs)
#   
#   
#   #Create plot labels
#   districts <- data.frame(type = c("district", "district", "district", "district", "district", "district"),
#                           region = c("bs.south", "bs.south", "bs.south", "bs.south", "bs.south", "bs.north"),
#                           lab = c("Bristol Bay District", "Pribilof District", "Northern District", 'St. Matthew Island Section', "166ᵒW", "Norton Sound District"),
#                           x = c(-165, -171.25, -172, -173, -166.9, -166.9),
#                           y = c(56, 58.3, 59.5, 59.2, 61, 64))
#   
#   # Specify plot labels by region in calc_factor
#   # if(calc_factor %in% c("EBS", "BBRKC", "PribRKC", "PribBKC", "StMattBKC", "TannerE", "TannerW")) {
#   #   
#     map_labs <- labelz %>%
#                 rbind(districts)%>%
#                 filter(region == "bs.south") %>%
#                 mutate(y = ifelse(type == "bathymetry", y-0.7, y))%>%
#                 mutate(x = ifelse(type == "bathymetry" & lab == "50 m", x+2.1, x))
#     
#   # } else if(calc_factor %in% c("NBS", "NSRKC")){
#   #   
#   #   labelz%>%
#   #     rbind(districts)%>%
#   #     filter(region == "bs.north") %>%
#   #     mutate(y = ifelse(type == "bathymetry", y-0.7, y))%>%
#   #     mutate(x = ifelse(type == "bathymetry" & lab == "50 m", x+2.1, x))-> map_labs
#   #   
#   # } else{
#   #   
#   #   labelz%>%
#   #     rbind(districts)%>%
#   #     mutate(y = ifelse(type == "bathymetry", y-0.7, y))%>%
#   #     mutate(x = ifelse(type == "bathymetry" & lab == "50 m", x+2.1, x))-> map_labs
#   # }
#   
#   placenames <- akgfmaps::transform_data_frame_crs(map_labs, out.crs=map_layers$crs)
#   
#   #Create year label
#   year.lab <- akgfmaps::transform_data_frame_crs(data.frame(lab = mat_sex_combos[i], x = -158.5, y = 61.4), 
#                                                  out.crs = map_layers$crs) 
#   
#   
#   #Specify conditional plotting of baselayers based on function inputs
#   recent = 2024
#   num = ifelse(year < recent, 0, 0.2) #if num=0, layers don't have color and don't appear
#   val1 = ifelse(year < recent, NA, "white")
#   val2 = ifelse(year < recent, NA, "black")
#   size = ifelse(year < recent, 5, 10)
#   
#   layer1 = geom_sf(data = st_as_sf(Pribsurveystrata_layer), fill=NA, color=NA)
#   layer2 = geom_sf(data = st_as_sf(Pribsurveystrata_layer), fill=NA, color=NA)
#   layer3 = geom_sf(data = st_as_sf(Pribsurveystrata_layer), fill=NA, color=NA)
#   districtlabs = geom_shadowtext(data = subset(placenames, lab == c("166ᵒW")), 
#                                  aes(x = x, y = y, label = lab), bg.color = NA, color = NA, size = 4, group = 99)
#   
#   species = "Opilio Crab"
#   #Create IDW map customized for crab
#   idw_plot <- ggplot() +
#               stars::geom_stars(data = extrap.grid) +
#               geom_sf(data = map_layers$bathymetry, color = alpha("grey70")) +
#               geom_sf(data = map_layers$survey.area, fill = NA) +
#               layer1+
#               layer2+
#               layer3+
#               geom_sf(data = survey.grid, fill = NA, color = alpha("grey70", num))+
#               geom_sf(data = map_layers$akland, fill = "grey80", size=0.1) +
#               districtlabs+
#               #stabathylabs+
#               scale_fill_manual(name = "Count", 
#                                 values = c("white", RColorBrewer::brewer.pal(9, name = "Blues")[c(2,4,6,8,9)]), 
#                                 na.translate = FALSE, # Don't use NA
#                                 drop = FALSE) + # Keep all levels in the plot
#               scale_x_continuous(breaks = map_layers$lon.breaks) + 
#               scale_y_continuous(breaks = map_layers$lat.breaks) +
#               geom_text(data = year.lab, aes(x = x, y = y, label = lab), fontface = "bold", size = 6) +
#               coord_sf(xlim = plot.boundary$x,
#                        ylim = plot.boundary$y)+
#               theme(panel.border = element_rect(color = "black", fill = NA),
#                     panel.background = element_rect(fill = NA, color = "black"),
#                     legend.key = element_rect(fill = NA, color = "grey70"),
#                     legend.key.size = unit(0.65,'cm'),
#                     legend.position = c(ifelse(species == "Opilio Crab", 0.18, 0.13), 0.2), #was 12, 18 
#                     legend.background = element_blank(),
#                     axis.title = element_blank(),
#                     axis.text = element_text(size = 10),
#                     legend.text = element_text(size = 10), 
#                     legend.title = element_text(size = 10),
#                     plot.background = element_rect(fill = NA, color = NA))
#   
#   ggsave(paste0("./Output/opie_maps/", mat_sex_combos[i], "_counts.pdf"),
#          height = 5, width = 7, units = "in")
# 
# }

