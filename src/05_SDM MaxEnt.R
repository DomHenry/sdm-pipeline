# Description -------------------------------------------------------------

## Thu Jan 31 12:35:22 2019
## Species distribution modelling of amphibians

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
library(dismo)
library(rasterVis)
library(glue)
library(RColorBrewer)

# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% 
  print

# Import species and environmental data -----------------------------------
sppselect <- query$Value[which(query$Input == "Species")]
load(glue("data output/sdm data processing/{sppselect}/sdm_input_data.RData"))

# Output folder
max_dir <- glue("data output/sdm maxent results/")

if(dir.exists(max_dir)) {
  print("Folder exists")
} else {
  dir.create(max_dir)
  print("Folder created")
}

# Blocking fold processing ------------------------------------------------

# Read in fold tables
foldtable <- dir(glue("data output/sdm data processing/{sppselect}/blockCV_results"),
    pattern = "fold_table", full.names = TRUE) %>% 
  map(read_csv) %>% 
  map_df(bind_rows) %>% 
  filter(type != "Buffer") # Remove buffer folds for now

if (max(foldtable$testPr) < 11){
  testpr_thresh <- max(foldtable$testPr) - 3
} else {
  testpr_thresh <-  10
}

# Filter folds which have insufficient testPr values
foldtable <- foldtable %>% 
  filter(testPr > testpr_thresh) %>% 
  mutate(assignment = ifelse(is.na(assignment),"environ", assignment)) %>% 
  mutate(fold_dir = str_c(assignment,"_",foldID)) %>% 
  print(n = 20)

fold_keep <- foldtable %>% 
  group_by(assignment) %>% 
  group_map(~ {
    .x %>% 
      mutate(foldID = str_replace(foldID,"fold","")) %>% 
      select(foldID) %>% 
      pull %>% 
      as.numeric
  })
names(fold_keep) <- unique(foldtable$assignment)

eb_folds <- eb[["biomodTable"]][,fold_keep[["environ"]]]
sb_check_folds <- sb_list$sb_check[["biomodTable"]][,fold_keep[["sb_check"]]]
sb_ran_folds <- sb_list$sb_ran[["biomodTable"]][,fold_keep[["sb_ran"]]]
sb_sys_folds <- sb_list$sb_sys[["biomodTable"]][,fold_keep[["sb_sys"]]]

# Create Maxent output folders --------------------------------------------

## Root directory
maxres_dir <- glue("data output/sdm maxent results/{sppselect}")

if(dir.exists(maxres_dir)) {
  print("Folder exists")
} else {
  dir.create(maxres_dir)
  print("Folder created")
}

## Directories for each blocking run
folder_dir <- map_chr(foldtable %>% 
                        select(fold_dir) %>% 
                        pull,
                      function (x) glue("{maxres_dir}/{x}"))

walk(folder_dir, dir.create) ## Create output directories for each model

# Run Maxent --------------------------------------------------------------

## Information on the Maxent model: 

# "x" = raster stack 
# "p" = occ data (use occtrain to "train" the model)
# "a" = background data (See Elith et al. 2011)
# removeDuplicates = selects only one point in each raster grid cell
# Note on categorical data - use "factors" argument which takes a character (from layer name or index in raster stack) - see help 

# Arguments to specify:
# randomtestpoints = 30% of data used for testing, 70% used for training (split the data twice, once kfold and once inside the model) 
# betamultiplier = regularisation coefficient = 1
# features = linear, quadratic and hinge
# Jackknife (measure importance of each predictor variable by training with each predictor variable first omitted, then used in isolation)

## TODO Create input for betamultiplier and reg parameter

walk(dir("src/functions/",full.names = TRUE, pattern = "maxent"),
    source)

## Generate data for Maxent function arguments
fold_data <- list()
fold_ref <- list()

for(i in 1:nrow(foldtable)){
  
  if(foldtable[i,"assignment"] == "environ"){
    fold_data[[i]] <- eb_folds
    fold_ref[[i]] <- eb$foldID
    
    } else if (foldtable[i,"assignment"] == "sb_check"){
      fold_data[[i]] <- sb_check_folds
      fold_ref[[i]] <- sb_list$sb_check$foldID
      
    } else if (foldtable[i,"assignment"] == "sb_sys"){
      fold_data[[i]] <- sb_sys_folds
      fold_ref[[i]] <- sb_list$sb_sys$foldID
      
    } else if(foldtable[i,"assignment"] == "sb_ran"){
      fold_data[[i]] <- sb_ran_folds
      fold_ref[[i]] <- sb_list$sb_ran$foldID
    }
  }

fold_name <- as.list(str_replace(foldtable$foldID,"fold","RUN"))
fold_num <- as.list(as.numeric(str_replace(foldtable$foldID,"fold","")))
fold_dir <- folder_dir

# Yay! It works...
maxlist <- pmap(
  .l = list(fold_data, fold_ref, fold_name, fold_num, fold_dir),
  .f = safely(maxent_fold)
  )

# test <- 1
# fold_data <- fold_data[[test]]
# fold_ref <- fold_ref[[test]]
# fold_name <- fold_name[[test]]
# fold_num <- fold_num[[test]]
# fold_dir <- folder_dir[[test]]

# If a function was run safely then I need to extract the "result" part of the list
maxlist <- maxlist %>% 
  map("result")
  
# Testing the model -------------------------------------------------------
eval_list <- pmap(.l = list(fold_data, fold_ref, fold_name, fold_num, maxlist),
                  .f = maxent_eval_models
                  )
eval_list

# Diagnostics -------------------------------------------------------------

## Create folder for diagnostics
diag_dir <- glue("{maxres_dir}/diagnostics")
dir.create(diag_dir)

## Thresholds - dismo::threshold()
  # kappa: the threshold at which kappa is highest ("max kappa")
  # spec_sens: the threshold at which the sum of the sensitivity (true pos rate) and specificity (true neg rate) is highest
  # no_omission: the highest threshold at which there is no omission
  # prevalence: modeled prevalence is closest to observed prevalence
  # equal_sens_spec: equal sensitivity and specificity
  # sensitivty: fixed (specified) sensitivity
foldtable

map_df(eval_list, 
       threshold) %>% 
  mutate(model = foldtable$fold_dir) %>% 
  select(model, everything()) %>% 
  mutate(auc = map_dbl(eval_list, "auc"),
         n_pres = map_dbl(eval_list, "np"),
         n_abs = map_dbl(eval_list, "na"),
         cor = map_dbl(eval_list, "cor")) %>%
  select(model, auc:cor, everything()) %>% 
  as_tibble() %>% 
  arrange(desc(auc)) %>% 
  print(n = 10) %>% 
  write_csv(glue("{diag_dir}/thresholds & auc.csv"))
  
pdf(glue("{diag_dir}/auc_plots.pdf"))
par(mfrow = c(2,2))
for (i in 1:nrow(foldtable)){
  plot(eval_list[[i]], "ROC", sub=foldtable$fold_dir[[i]])
}
dev.off()

for (i in 1:nrow(foldtable)){
  png(glue("{diag_dir}/auc_plot_{foldtable$fold_dir[[i]]}.png"))
  plot(eval_list[[i]], "ROC", sub=foldtable$fold_dir[[i]])
  dev.off()
}

#Create a density plots of presence and absence data and boxplots
pdf(glue("{diag_dir}/density_plots.pdf"))
par(mfrow = c(2,2))
for (i in 1:nrow(foldtable)){
  density(eval_list[[i]], sub=foldtable$fold_dir[[i]])
}
dev.off()

# Make a box plot of model evaluation data, i.e., the model predictions for known presence and absence points.
pdf(glue("{diag_dir}/predic_boxplots.pdf"))
par(mfrow = c(2,2))
for (i in 1:nrow(foldtable)){
  boxplot(eval_list[[i]], main  = quo_name(foldtable$fold_dir[[i]]))
}
dev.off()

# Model predictions - full ------------------------------------------------

## Import SA boundary for plotting
geo_proj <- query$Value[which(query$Input == "Geographic projection")]
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

if (geo_proj == "Yes") {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS) %>% 
    st_transform(aeaproj) 
} else {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS)
}

## Create range predictions folder
pred_pdf_dir <- glue("{maxres_dir}/spatial_predictions_pdf")
dir.create(pred_pdf_dir)

## Create raster directory 
pred_ras_dir <- glue("{maxres_dir}/spatial_predictions_raster")
dir.create(pred_ras_dir)

## Create PNG directory 
pred_png_dir <- glue("{maxres_dir}/spatial_predictions_png")
dir.create(pred_png_dir)

## PDF
pdf(glue("{pred_pdf_dir}/pred_full.pdf"))
par(mfrow = c(2,2))
pwalk(list(maxlist, foldtable$fold_dir,list("pdf")),
      maxent_pred_rang_full)
dev.off()

## Rasters
pwalk(list(maxlist, foldtable$fold_dir,list("raster")),
      maxent_pred_rang_full)

# PNG 
pmap(list(maxlist, foldtable$fold_dir,list("png")),
     maxent_pred_rang_full)

# Model predictions - threshold -------------------------------------------

## Using kappa
pred_thresh <- map_df(eval_list, threshold) %>% 
  select(kappa) %>% 
  pull() %>% 
  as.list()

## Manual (alternative)
pred_thresh <- list(0.6)

## Plot
pdf(glue("{pred_pdf_dir}/pred_kappa_thresh.pdf"))
par(mfrow = c(2,2))
pwalk(list(maxlist, foldtable$fold_dir,pred_thresh,list("pdf")),
      maxent_pred_rang_thresh)
dev.off()

## Raster 
pwalk(list(maxlist, foldtable$fold_dir,pred_thresh,list("raster")),
      maxent_pred_rang_thresh)

# Model predictions - mean probability  -----------------------------------

## Mean probability over the study extent the can be used as the threshold 
## (above which species is considered "present" and below which species is considered "absent")\
## raster::reclassify = reclassify "probability of presence" raster using the calculated threshold value as the "zero" point

pdf(glue("{pred_pdf_dir}/pred_mean.pdf"))
par(mfrow = c(2,2))
pwalk(list(maxlist,foldtable$fold_dir,list("pdf")),
      maxent_pred_mean)
dev.off()

## Raster 
pwalk(list(maxlist,foldtable$fold_dir,list("raster")),
      maxent_pred_mean)

# Write query to folder ---------------------------------------------------
write_csv(query, path = glue("{maxres_dir}/USER_QUERY.csv"))

# Write workspace ---------------------------------------------------------
save.image(file = glue("{maxres_dir}/maxent_sdm_output_data.RData"))

# Write blocking data workspace -------------------------------------------
save(list = c("sb_list","sb_ran_folds", 
              "sb_check_folds","sb_sys_folds", 
              "eb_folds","foldtable"), 
     file = glue("data output/sdm data processing/{sppselect}/blockCV_data.RData"))

print("SUCCESSFULLY COMPLETED")

# Overlay urban landscape -------------------------------------------------

## LEAVE THIS OUT FOR NOW ##

# aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# za <- st_read("data input/PR_SA.shp",crs = latlongCRS) %>% 
#   st_transform(aeaproj) 
# 
# urblc <- st_read("C:/Users/DominicH/Documents/GIS data/Environmental data/Urban shapefile/RasterT_Reclass3.shp") %>% 
#   st_transform(aeaproj)
# 
# rasex <- pred_range@extent
# 
# urb <- urblc %>% 
#   st_crop(xmin = rasex@xmin, xmax = rasex@xmax, ymin = rasex@ymin , ymax = rasex@ymax) %>% 
#   st_buffer(dist = 100)
# 
# urb <- urb %>% 
#   st_union()
# 
# urbspdf <- as(urb, "Spatial")
# rm(urblc)
# 
# pdf(paste0("data output/sdm maxent results/",sppselect,"/",sppselect,"_range_map_urban.pdf"),width = 16, height = 9)
# mapTheme <- rasterTheme(region=rev(terrain.colors(10))) # brewer.pal(8,"Greens")
# plt <- levelplot(pred_range, margin=F, par.settings=mapTheme)
# plt + layer(sp.lines(as(za,"Spatial"),col="black", lwd=1)) +
#   layer(sp.polygons(urbspdf,fill = alpha("black", 0.3),col="black", lwd=0.2)) #check sp.lines
# dev.off()

