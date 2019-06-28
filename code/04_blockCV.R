# Description -------------------------------------------------------------

## Thu Apr 11 09:21:22 2019 
## Block cross validation of species distribution models

library(lubridate)
library(gghighlight)
library(gridExtra)
library(ggpubr)
library(sf)
library(viridis)
library(readxl)
library(raster)
library(tidyverse)
library(biomod2)
library(tictoc)
library(glue)
library(blockCV)

## Look at summary plots and then decide on a first preference of which type of blocking to do
 
## Checkboard - 2 folds for data that are highly clustered
## Spatial - well spread out occurence data across prediction area
## Environmental - also fairly well spread out with

## However, create blocking data for each method and store it in results
## The data can be processed subsequently -for instance I can remove all folds that have very few testing presence points

## For spatial blocking: if median spatial auto correlation distance is large and results in fewer than 5 blocks then 
## I need to re-run with a different (smaller) distance parameter to create a grid with at least 5 blocks. I could try do this by measuring the distance across the raster data and see how many blocks can fit in

# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% 
  print

# Import species and environmental data -----------------------------------
sppselect <- query$Value[which(query$Input == "Species")]
load(paste0("data output/sdm data processing/",sppselect,"/sdm_input_data.RData"))

# Create PB object for blockCV functions ----------------------------------
PB_data <- occ_points %>% 
  ungroup %>% 
  mutate(Species = 1) %>% 
  select(Species) %>% 
  rbind(bck_points %>% 
          ungroup %>% 
          mutate(Species = 0) %>% 
          select(Species)
        )
PB_data

# Create directory to store results ---------------------------------------
block_dir <- glue("data output/sdm data processing/{sppselect}/blockCV_results")
if(dir.exists(block_dir)) {
  print("Folder exists")
} else {
  dir.create(block_dir)
}

ncells <- ncell(envstack)

if (ncells < 5000) {
  
  sac_samp <- ncell(envstack) # Use all cells
    
} else if (ncells > 5000 & ncells < 10000) {
  
  sac_samp <- round(ncell(envstack)*0.70,0) # use 70% of raster cells for SAC sampling
  
} else{
  
  sac_samp <- round(ncell(envstack)*0.50,0) # use 50% of raster cells for SAC sampling
  
}

sac_samp

# Explore spatial autocorrelation range -----------------------------------
sac <- spatialAutoRange(
  rasterLayer = envstack,
  sampleNumber = sac_samp - 500, # number of cells to be used - throws an error 
  doParallel = TRUE,
  nCores = NULL, # use half of the CPU cores
  plotVariograms = FALSE,
  showPlots = FALSE,
  progress = TRUE
  )

# Write SAC plots and summaries -------------------------------------------
sac[["plots"]]$barchart
ggsave(glue("{block_dir}/sac_barchart.pdf"))
ggsave(glue("{block_dir}/sac_barchart.png"))
sac[["plots"]]$mapplot
ggsave(glue("{block_dir}/sac_map.pdf"))
ggsave(glue("{block_dir}/sac_map.png"))

pdf(glue("{block_dir}/sac_variogram.pdf"))
plot(sac$variograms[[1]])
dev.off()

png(glue("{block_dir}/sac_variogram.png"))
plot(sac$variograms[[1]])
dev.off()

sac[["range"]]
sac[["rangeTable"]]
sac[["sampleNumber"]]
summary(sac)

rangedf <- as_tibble(summary(sac)) %>% 
  write_csv(glue("{block_dir}/sac_raster_range.csv"))

glue("Median spatial autocorrelation range is {round(median(rangedf$range)/1000,2)} km")
median(rangedf$range)

# Check dimensions - can work out how many blocks
theRange <-median(rangedf$range)
ex <- extent(envstack[[1]])

width <- (ex@xmax-ex@xmin)*111325 # Meters
height <- (ex@ymax-ex@ymin)*111325 # Meters

if (width/theRange < 3) {
  theNewRange <- width/3
} else{
  theNewRange <- theRange
}

theRange
theNewRange

# Explore :: SAC ranges ---------------------------------------------------

# rangeExplorer(rasterLayer = envstack,
#               speciesData = PB_data,
#               species = "Species",
#               rangeTable = NULL,
#               minRange = min(rangedf$range), # limit the search domain
#               maxRange = max(rangedf$range))


# Spatial blocking :: create 3 sets ---------------------------------------

# The function spatialBlock creates spatially separated folds based on a pre-specified distance (cell size of the blocks). It then assigns blocks to the training and testing folds with random, checkerboard pattern or in a systematic manner. Another blocking strategy provided by this function is to divide the study area into vertical or horizontal bins of a given number of rows/colmuns, as used by Bahn & McGill (2013) and Wenger & Olden (2012) respectively.

# To keep the consistency with other functions, the distance (theRange) should be in metres, regardless of the unit of the reference system of the input data. When the input map has geographic coordinate system (decimal degrees), the block size is calculated based on dividing theRange by 111325 (the standard distance of a degree in metres, on the Equator) to change metre to degree. This value can be changed by the user via the degMetre argument.

# The xOffset and yOffset can be used to shift the spatial position of the blocks in horizontal and vertical axes, respectively. This only works when the block have been built based on theRange. The blocks argument allows users to define an external spatial polygon as blocking layer. The polygon layer must cover all the species points. In addition, blocks can be masked by species spatial data. This option keeps the blocks that cover species data and remove the rest.

# Roberts et. al. (2017) suggest that blocks should be substantially bigger than the range of spatial autocorrelation (in model residual) to obtain realistic error estimates

sbfunc <- function(assign_type) {
  
  sb <- spatialBlock(
    speciesData = PB_data,
    species = "Species", # Column in which 0's and 1's are stored
    rasterLayer = envstack[[1]], # Used for visualisation 
    theRange = theNewRange, # Specified range by which blocks are created and training/testing data are separated
    k = 5, #number of desired folds for cross-validation
    selection =  assign_type, 
    # rows = 5, # Use these if theRange is not specified
    # cols = 8, # Use these if theRange is not specified
    numLimit = 10, # min num of points in each cat of data (training-pres, training-abs, testing-pres and testing-abs)
    iteration = 250, # Number of attempts to ind evenly dispersed folds
    biomod2Format = TRUE,
    maskBySpecies = TRUE, # blocks created based on the raster extent, but only those blocks covering species data is kept
    xOffset = 0, # shift the blocks horizontally (value between 0 and 1) - proportion to block size
    yOffset = 0, # shift the blocks vertically 
    degMetre = 111325, # The conversion rate of metres to degree
    progress = TRUE
  )
  
  return(sb)
  
}

assign_type <- list("systematic","random","checkerboard")

sb_list <- map(assign_type,
               sbfunc)

names(sb_list) <- c("sb_sys","sb_ran","sb_check")

sb_id <- list("sb_sys","sb_ran","sb_check")
fold_length <- list(5,5,2)

write_record_tab <- function(x,sb_id,fold_length) {
  
  x[["records"]] %>% 
    as_tibble %>% 
    mutate(foldID = str_c("fold",1:fold_length),
           type = "Spatial",
           assignment = sb_id) %>% 
    select(foldID, type,assignment, everything()) %>% 
    write_csv(glue("data output/sdm data processing/{sppselect}/blockCV_results/{sb_id}_fold_table.csv"))
  }

pmap(list(sb_list,sb_id,fold_length),
     write_record_tab)

block_shp <- function(x,sb_id) {
 
  st_as_sf(x[["blocks"]]) %>% 
    write_sf(glue("{block_dir}/{sb_id}_block.shp"))
   
}

map2(sb_list,sb_id,
     block_shp)

sb_list[[1]]$plots

# Explore :: generated folds ----------------------------------------------

# foldExplorer(blocks = sb_list[[1]],
#              rasterLayer = envstack,
#              speciesData = PB_data)

# Spatial blocking :: plots -----------------------------------------------
source("code/functions/ttplots.R")

# First SB
block_data <- list(sb_list[[1]])
fold_name <- list(quo(RUN1),quo(RUN2),quo(RUN3),quo(RUN4),quo(RUN5))
fold_num <- as.list(c(1:5))
endcol <- list(quo(RUN5))
plot_base <- list(sb_list[[1]])

pwalk(.l = list(block_data, fold_name, fold_num, list(sb_id[[1]]), endcol, plot_base),
      .f = ttplots)

# SECOND SB
block_data <- list(sb_list[[2]])
fold_name <- list(quo(RUN1),quo(RUN2),quo(RUN3),quo(RUN4),quo(RUN5))
fold_num <- as.list(c(1:5))
endcol <- list(quo(RUN5))
plot_base <- list(sb_list[[2]])

pwalk(.l = list(block_data, fold_name, fold_num, list(sb_id[[2]]), endcol, plot_base),
      .f = ttplots)

# THIRD SB (CHECKBOARD)
block_data <- list(sb_list[[3]])
fold_name <- list(quo(RUN1),quo(RUN2))
fold_num <- as.list(c(1:2))
endcol <- list(quo(RUN2))
plot_base <- list(sb_list[[3]])

pwalk(.l = list(block_data, fold_name, fold_num, list(sb_id[[3]]), endcol, plot_base),
      .f = ttplots)

# Environmental block -----------------------------------------------------

# This function uses clustering methods to specify sets of similar environmental conditions based on the input covariates.
# Species data corresponding to any of these groups or clusters are assigned to a fold. This function does the clustering in raster space and species data. Clustering is done using kmeans {stats} for both approaches.

# If rasterBlock = FALSE, clustering will be done in species points and number of the folds will be the same as k.
# If rasterBlock = TRUE , the clustering is done in the raster space. In this approach the clusters will be consistent throughout the region and across species (in the same region). However, this may result in a cluster(s) that covers none of the species records, especially when species data is not dispersed throughout the region or the number of clusters (k or folds) is high. In this case, the number of folds is less than specified k

# Therefore we should use rasterBlock = FALSE for species with few points that are clustered throughout study region
rb_choice <- FALSE

eb <- envBlock(
  rasterLayer = envstack,
  speciesData = PB_data,
  species = "Species", # Indicating the name of field in which species P/A data (0s and 1s) are stored
  k = 5,
  standardization = "standard", # rescale variables between 0 and 1
  rasterBlock = rb_choice,
  biomod2Format = TRUE,
  numLimit = 10 # min number of points in each cat: (training-presence, training-absence, testing-presence and testing-absence) 
  )

eb[["records"]] %>% 
  as_tibble %>% 
  mutate(foldID = str_c("fold",1:5),
         type = "Environmental") %>% 
  select(foldID, type, everything()) %>% 
  write_csv(glue("data output/sdm data processing/{sppselect}/blockCV_results/eb_fold_table.csv"))

source("code/functions/ttplots.R")

block_data <- list(eb)
fold_name <- list(quo(RUN1),quo(RUN2),quo(RUN3),quo(RUN4),quo(RUN5))
fold_num <- as.list(c(1:5))
endcol <- list(quo(RUN5))
plot_base <- list(sb_list[[3]])

pwalk(.l = list(block_data, fold_name, fold_num, list("eb"), endcol, plot_base),
      .f = ttplots)

# Buffer blocking ---------------------------------------------------------
bf <- blockCV::buffering(speciesData = as(PB_data,"Spatial"), # Need to convert to SPDF
                species = "Species",
                theRange = 20000,
                spDataType = "PB",
                addBG = TRUE,
                progress = TRUE)
bf

# foldExplorer(blocks = bf,
#              rasterLayer = envstack,
#              speciesData = PB_data)

bf[["records"]] %>% 
  as_tibble %>% 
  mutate(type = "Buffer",
         foldID = str_c("fold",1:nrow(bf[["records"]]))) %>% 
  select(foldID,type, everything()) %>% 
  write_csv(glue("data output/sdm data processing/{sppselect}/blockCV_results/bf_fold_table.csv"))

# Write workspace ---------------------------------------------------------
save(list = c("occ_points","bck_points","envstack",
              "eb","sb","bf","sac","sb_list",
              "PB_data"), 
     file = glue("data output/sdm data processing/{sppselect}/sdm_input_data.RData"))

# Future developments -----------------------------------------------------

# TODO Create a markdown document of all blocking options to make selections?? Aid with selecting appropriate cv folds. Will need to create PNG files instead of PDFs for graphics. Alternative is to run all maxent models on all folds (first remove folds that don't have any testing presences) and see from there.
# TODO Carry on exploring the blockCV vignette and how they calculate AUC for different methods

