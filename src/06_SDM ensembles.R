# Description -------------------------------------------------------------
## Thu Jan 31 12:35:22 2019
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
library(rasterVis)

# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% 
  print

# Import species and environmental data -----------------------------------
sppselect <- query$Value[which(query$Input == "Species")]
load(glue("data output/sdm data processing/{sppselect}/sdm_input_data.RData"))
load(glue("data output/sdm data processing/{sppselect}/blockCV_data.RData"))

# Source EM functions -----------------------------------------------------
walk(dir("src/functions/", pattern = "EM_", full.names = TRUE),
    source)

# Create a shorter name ---------------------------------------------------

## It seems as if the long species name cause problems because the file names become so large
## Take the first 2 letters of the genus and first 3 letters of the species.
## It also means I can unzip the file in the correct directory!

spp_short <- str_c(str_sub(word(sppselect,1),1,2),
                   str_sub(word(sppselect,2),1,3))
spp_short


# :: BIOMOD_FormatingData -------------------------------------------------

## resp.var is made up of 1's (presence) and NA's (psuedo abs)
pres <- occ_points %>% 
  ungroup %>% 
  mutate(pa = 1) %>% 
  select(pa)

pseabs <- bck_points %>% 
  ungroup %>% 
  mutate(pa = NA) %>% 
  select(pa)

pres <- as.data.frame(as(pres,"Spatial"))
pseabs <- as.data.frame(as(pseabs,"Spatial"))
colnames(pres) <- colnames(pseabs) <- c("pa","lon","lat")

spp_resp  <- c(pres[,"pa"],
               pseabs[,"pa"])

spp_resp_xy <- rbind(pres[,c("lon","lat")],
                 pseabs[,c("lon","lat")])

biomod_data <- BIOMOD_FormatingData(
  resp.var = spp_resp,
  resp.xy = spp_resp_xy,
  resp.name = spp_short,
  expl.var = stack(envstack),
  # PA.nb.rep = 1, # Number of pseudo abs data sets
  # PA.strategy = "random", # Look at "user.defined" option
  )

biomod_data
plot(biomod_data)

# Define biomod modelling options -----------------------------------------
biomod_options <- BIOMOD_ModelingOptions(MAXENT.Phillips = list(path_to_maxent.jar = "C:/Users/DominicH/Documents/R/win-library/3.6/dismo/java"))

# Gather blocking fold data -----------------------------------------------
foldtable
block_data <- as_tibble(cbind(sb_check_folds,sb_sys_folds,sb_ran_folds,eb_folds))
colnames(block_data) <- foldtable$fold_dir
head(block_data)

# ::  BIOMOD_Modeling -----------------------------------------------------
tic("BioMod run time")
biomod_out <- BIOMOD_Modeling(
  biomod_data,
  models = c("GBM","RF","MAXENT.Phillips"),
  models.options = biomod_options,  
  DataSplitTable = as.matrix(block_data), # block_data[["biomodTable"]], # if filled, will ignore nbruneval,datasplit,do.full.models
  NbRunEval = 3, # Number of evaulation runs. Each run uses a new set of test & train data
  DataSplit = 80, # Proportion of data kept for model calibration 
  Yweights = NULL, # Each observation has the same weight (independant of # of pres and abs)
  Prevalence = 0.5, # Prev = 0.5 means absences will be weighted equally to the presences
  VarImport = 3, # Number of permutations to estimate variable importance 
  models.eval.meth = c("TSS","ROC","KAPPA"), # Evaluation methods
  SaveObj = TRUE, 
  do.full.models = FALSE, # TRUE = models calibrated and evaluated with the whole dataset are done  
  modeling.id = paste0("Ensemble_",spp_short)
  )

toc(log = TRUE)
tl <- tic.log(format = FALSE)
runtime_biomod <- round((tl[[1]][["toc"]][["elapsed"]] - tl[[1]][["tic"]][["elapsed"]])/60,2)
tic.clearlog()
glue("Biomod model running process took {runtime_biomod} minutes")

# Diagnostic plot directory -----------------------------------------------
dir.create(glue('{getwd()}/{spp_short}/diagnostics'))

# Model score plots -------------------------------------------------------
model_scores_plot <- function(plotby,metric,mtitle,xlim,ylim){
  
  models_scores_graph(biomod_out, by = plotby , metrics = metric, 
                      xlim = xlim, ylim =ylim, main = mtitle)
  }

plotbys <- list("models","cv_run","data_set","algos")
metrics1 <- list(c("TSS","ROC"))
metrics2 <- list(c("KAPPA","TSS"))
titles <- list("Models","CV runs","Data set","Algorithm")
xlims <- ylims <- list(c(0,1))

plots_out1 <- pmap(list(plotby = plotbys, metric = metrics1, 
                       mtitle = titles, xlim = xlims, ylim = ylims),
                  model_scores_plot)

plots_out2 <- pmap(list(plotby = plotbys, metric = metrics2, 
                        mtitle = titles, xlim = xlims, ylim = ylims),
                   model_scores_plot)

pdf(glue('{spp_short}/diagnostics/avg_model_scores1.pdf'), width = 9, height = 9)
grid.arrange(grobs = plots_out1, ncol = 2,nrow = 2) # Also see ggarrange
dev.off()
pdf(glue('{spp_short}/diagnostics/avg_model_scores2.pdf'), width = 9, height = 9)
grid.arrange(grobs = plots_out2, ncol = 2,nrow = 2) # Also see ggarrange
dev.off()

p <- arrangeGrob(grobs = plots_out1, ncol = 2,nrow = 2)
ggsave(glue('{spp_short}/diagnostics/avg_model_scores1.png'), p, width = 9, height = 9)

p <- arrangeGrob(grobs = plots_out2, ncol = 2,nrow = 2)
ggsave(glue('{spp_short}/diagnostics/avg_model_scores2.png'), p, width = 9, height = 9)

# Variable importance plots -----------------------------------------------
biomod_var_import <- get_variables_importance(biomod_out)

var_import <- as_tibble(apply(biomod_var_import, c(1,2), mean)) %>% # Mean across all runs and data_sets
  mutate(var_id = dimnames(biomod_var_import)[[1]]) %>%
  gather(key = "algo",value = "mean", - var_id)

var_import %>% 
  ggplot(aes(x = var_id, y = mean))+
  geom_bar(stat = "identity",position=position_dodge())+
  coord_flip()+
  xlab("Environmental variable")+
  ylab("Mean importance")+
  facet_grid(cols = vars(algo))+
  theme(strip.text = element_text(size = 15),
        axis.text = element_text(size = 13))
ggsave(glue('{spp_short}/diagnostics/variable_importance.pdf'), width = 16, height = 9)
ggsave(glue('{spp_short}/diagnostics/variable_importance.png'), width = 16, height = 9)

# Extract model evaluation measures ---------------------------------------
biomod_eval <- get_evaluations(biomod_out) # an array,df or a list containing models eval scores
dimnames(biomod_eval)
biomod_eval["ROC","Testing.data",,,]

df <- as.data.frame.table(biomod_eval, responseName = "value") 
write_csv(df, 
          glue('{spp_short}/diagnostics/evaluation_metrics.csv'))

# The "Testing.data" col is what is important in terms of TSS,ROC and KAPPA
# No metrics evaluation data (need to go back and see where to specificy eval) - blockCV or BIOMOD_cv

# Response curves ---------------------------------------------------------
modelnames <- get_built_models(biomod_out)
model_list <- as.list(unique(word(modelnames, 4, sep = "_")))

modload1 <- BIOMOD_LoadModels(biomod_out, models = model_list[[1]])
modload2 <- BIOMOD_LoadModels(biomod_out, models = model_list[[2]])
modload3 <- BIOMOD_LoadModels(biomod_out, models = model_list[[3]])

filenames <-  as.list(glue('{spp_short}/diagnostics/Response curve_{model_list[1:3]}'))

response_plot_list <- pmap(list(modloaded = list(modload1,modload2,modload3),
                                x = list(biomod_out),
                                filename = filenames,
                                output = list("pdf")),
                           EM_response_curves)

response_plot_list <- pmap(list(modloaded = list(modload1,modload2,modload3),
                                x = list(biomod_out),
                                filename = filenames,
                                output = list("jpeg")),
                           EM_response_curves)

## TODO Need to present these figures in a better way - vis of response curves is crucial for expert evalution
## All the values used to produce these plots are stored into the returned object and are this customisable
str(response_plot_list) # Use to redo plots (may be good to present them to experts)

# :: evaluate -------------------------------------------------------------
eval_data <- cbind(spp = get_formal_data(biomod_out,'resp.var'), 
                   get_formal_data(biomod_out,'expl.var'))
colnames(eval_data)[1] <- str_replace(sppselect," ",".")

## The function show give measures based on the evalulating data (as opposed to testing...)
## Doesn't work for stat = "ROC"
# biomod2::evaluate(biomod_out, data = eval_data, stat =c('TSS')) 

# This is already evalutated using the testing data?
get_evaluations(biomod_out) 

## Try with ensemble models?
# biomod2::evaluate(biomod_EM, data = eval_data, stat =c('ROC'))

# :: BIOMOD_EnsembleModeling ----------------------------------------------

## Note - this function throws an error if working directory for models is with "data output" folder. It is strange and I have no idea what causes this - the problem is that some files won't copy once I try move the whole results folder to a directory within "sdm em results".
biomod_EM <- try(BIOMOD_EnsembleModeling(
  modeling.output = biomod_out,
  chosen.models = 'all', # Define which models are kept for EM (can remove some)
  em.by='all', # See options above
  eval.metric = c("TSS","ROC"), # See function help file for description
  eval.metric.quality.threshold = c(0.65,0.65), # models with scores below threshold excluded from EM building
  models.eval.meth = c('KAPPA','TSS','ROC'),# Used to check ensemble predicitive performance
  prob.mean = TRUE, # Estimate the mean probabilities across predictions
  prob.cv = TRUE, # Coefficient of variation across predictions
  prob.ci = TRUE, # Estimate the confidence interval around the prob.mean
  prob.ci.alpha = 0.05,
  prob.median = TRUE,
  committee.averaging = TRUE, # Estimate the committee averaging across predictions
  prob.mean.weight = TRUE, # Estimate the weighted sum of probabilities
  prob.mean.weight.decay = 'proportional',
  VarImport = 0 # Number of permutation to estimate variable importance
  )) 

biomod_EM

# Need to re-run if the model quality thresholds are too high (i.e. function throws error)
# Alternative method would be to use tryCatch()

if(class(biomod_EM) == "try-error"){
  
  biomod_EM <- BIOMOD_EnsembleModeling(
    modeling.output = biomod_out,
    chosen.models = 'all', # Define which models are kept for EM (can remove some)
    em.by='all', # See options above
    eval.metric = c("TSS","ROC"), # See function help file for description
    eval.metric.quality.threshold = c(0.3,0.3), # models with scores below threshold excluded from EM building
    models.eval.meth = c('KAPPA','TSS','ROC'),# Used to check ensemble predicitive performance
    prob.mean = TRUE, # Estimate the mean probabilities across predictions
    prob.cv = TRUE, # Coefficient of variation across predictions
    prob.ci = TRUE, # Estimate the confidence interval around the prob.mean
    prob.ci.alpha = 0.05,
    prob.median = TRUE,
    committee.averaging = TRUE, # Estimate the committee averaging across predictions
    prob.mean.weight = TRUE, # Estimate the weighted sum of probabilities
    prob.mean.weight.decay = 'proportional',
    VarImport = 0 # Number of permutation to estimate variable importance
  )
} else {
    
  biomod_EM <- biomod_EM
  
  }

# prob.mean.weight.decay argument: 
# Define the relative importance of the weights. A high value will strongly discriminate the 'good' models from the 'bad' ones (see the details section). If the value of this parameter is set to 'proportional' (default), then the attributed weights are proportional to the evaluation scores given by 'weight.method'(eval.metric)

biomod_EM
get_built_models(biomod_EM)

# At this stage all of the models above are written to the "model" folder in working dir
# The number of Ensemble models written to file is: 7 (mean,cv,median,committee,weighted, upperConf,lowerConf) x  2 (number of eval.metrics [TSS & ROC]) x 1 (em.by = 'all) THEREFORE total of 14

# Extract Ensemble Evaluations --------------------------------------------
em_vals <- get_evaluations(biomod_EM) 
names(em_vals) <- word(names(em_vals), 2, sep = "_")
em_vals

coln <- colnames(em_vals[[1]])
rown <- rownames(em_vals[[1]])

map_df(em_vals,unlist) %>% 
  mutate(measure = rep(coln,each = 3), stat = rep(rown, 4)) %>% 
  select(measure,stat, everything()) %>% 
  write_csv(glue('{spp_short}/diagnostics/ensemble_models_evaluations.csv'))

# Ensemble sample predictions ---------------------------------------------

# Predictions for each ensemble model across all pseudo and presence data
em_preds <- get_predictions(biomod_EM)
dimnames(em_preds)
dimnames(em_preds)[[2]][4]
dim(em_preds)

# Each EM is a column - eg:
em_preds[1:10,glue("{spp_short}_EMciInfByTSS_mergedAlgo_mergedRun_mergedData")] # lower CI

# :: BIOMOD_Projection ----------------------------------------------------

## These are not ensemble predictions - they're individual projections from BIOMOD_modelling step.
biomod_proj <- BIOMOD_Projection(
  modeling.output = biomod_out,
  new.env = stack(envstack),# A set of explanatory variables  
  proj.name = "current", # folder will be created with this name
  selected.models = "all", # can also use subset of models (via biomod_out@models.computed)
  # binary.meth = "TSS",#  binary predictions based on threshold argument in BIOMOD_Modeling
  binary.meth = NULL, # No binary transformation
  compress = "xz",
  clamping.mask = FALSE,
  build.clamping.mask = FALSE, # See notes below
  output.format = ".grd",
  # output.format = ".RData", # Can also be ".RData" 
  do.stack = FALSE # attempt to save all projections in a unique rasterstack 
 )

stack(envstack)
plot(biomod_proj, str.grep = "MAXENT.Phillips") 

## Custom plots
current_proj <- get_predictions(biomod_proj) # Returns raster stack
class(current_proj);names(current_proj)

pdf(glue("{spp_short}/diagnostics/current projection plots.pdf"))
par(mfrow = c(2,2))
for(i in 1:length(names(current_proj))){
  plot(current_proj[[i]], main = names(current_proj)[[i]])
}
dev.off()

# :: BIOMOD_EnsembleForecasting -------------------------------------------

# TODO create query to only include top models or those above a threshold

biomod_EF <- BIOMOD_EnsembleForecasting(
  EM.output = biomod_EM,  
  projection.output = biomod_proj, # 
  # new.env = stack(envstack), #only this argument need if projection.output = NULL (i.e. only use models from threshold selection)
  selected.models = 'all',
  proj.name = "ensembles",
  binary.meth = NULL, # vector of metrics + thresholdsto transform to binary prediction
  filtered.meth = NULL, # filter probabilities lower than threshold and set to 0 (absent)
  compress = TRUE
  )

biomod_EF

# Write EM predictive maps to folder --------------------------------------

# Create a folder to store maps
dir.create(glue("{getwd()}/{spp_short}/ensemble_maps"))

# Get ensemble model names
dir_em <-dir(glue("{spp_short}/proj_ensembles/individual_projections/"), 
             pattern = "gri",
             full.names = TRUE)

em_modname <- word(dir_em, 4, sep = "_") # Can define words by other separators!

# Write ensemble model tif file
walk(em_modname, 
     EM_write_ras)

geo_proj <- query$Value[which(query$Input == "Geographic projection")]
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

if (geo_proj == "Yes") {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS) %>% 
    st_transform(aeaproj) 
} else {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS)
}

walk2(em_modname,"pdf",
      EM_write_pdf)

walk2(em_modname,"png",
      EM_write_pdf)

# Overlay urban landscape -------------------------------------------------
geo_proj <- query$Value[which(query$Input == "Geographic projection")]

aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

if (geo_proj == "Yes") {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS) %>% 
    st_transform(aeaproj)
  
  urblc <- st_read("C:/Users/DominicH/Documents/GIS data/Environmental data/Urban shapefile/RasterT_Reclass3.shp") %>% 
    st_transform(aeaproj)
  
} else {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS)
  
  urblc <- st_read("C:/Users/DominicH/Documents/GIS data/Environmental data/Urban shapefile/RasterT_Reclass3.shp") %>% 
    st_transform(latlongCRS)
  
}

pred_range <- raster(dir(glue("{spp_short}/proj_ensembles/individual_projections/"),
                  pattern = "EMmedianByROC", full.names = TRUE)[2])
pred_range <- pred_range/1000
rasex <- pred_range@extent

if (geo_proj == "Yes") {
  
  urb <- urblc %>% 
    st_crop(xmin = rasex@xmin, xmax = rasex@xmax, ymin = rasex@ymin , ymax = rasex@ymax) %>% 
    st_buffer(dist = 100) #100 meters
  
} else {
  
  urb <- urblc %>% 
    st_crop(xmin = rasex@xmin, xmax = rasex@xmax, ymin = rasex@ymin , ymax = rasex@ymax) %>% 
    st_buffer(dist = 0.005) # 1 degree = 1000m so 0.1 degree = 100m
}

urb <- urb %>% 
  st_union()

urbspdf <- as(urb, "Spatial")
rm(urblc)

pdf(glue("{spp_short}/ensemble_maps/{spp_short}_EMmedianByROC_URB.pdf"), width = 12, height = 9)
mapTheme <- rasterTheme(region=rev(terrain.colors(10))) # brewer.pal(8,"Greens")
plt <- levelplot(pred_range, margin=F, par.settings=mapTheme)
plt + layer(sp.lines(as(za,"Spatial"),col="black", lwd=1)) +
  layer(sp.polygons(urbspdf,fill = alpha("black", 0.2),col="black", lwd=0.2)) #check sp.lines
dev.off()

# Write workspace ---------------------------------------------------------
save(list = c("occ_points","bck_points","envstack","PB_data","eb","block_data",
              "biomod_data","biomod_EF","biomod_EM","biomod_eval","biomod_options",
              "biomod_out","biomod_proj","biomod_var_import","response_plot_list", "spp_short"), 
     file = glue("{getwd()}/{spp_short}/biomod objects.RData"))

# Move entire folder to data output directory -----------------------------
ensem_dir <- glue("data output/sdm ensemble results/")
if(dir.exists(ensem_dir)) {
  print("Folder exists")
} else {
  dir.create(ensem_dir)
  print("Folder created")
}

ensem_spp_dir <- glue("data output/sdm ensemble results/{sppselect}")
if(dir.exists(ensem_spp_dir)) {
  print("Folder exists")
} else {
  dir.create(ensem_spp_dir)
  print("Folder created")
}

# TODO - write a function for all of this file copying

## Copy relevant outputs
copyfrom <- glue("{spp_short}/diagnostics/")
file.copy(copyfrom, ensem_spp_dir, recursive=TRUE)

copyfrom <- glue("{spp_short}/ensemble_maps/")
file.copy(copyfrom, ensem_spp_dir, recursive=TRUE)

copyfrom <- glue("{spp_short}/biomod objects.RData")
file.copy(copyfrom, ensem_spp_dir, recursive=TRUE)

## Zip folder (to store all outputs)
zip::zipr(zipfile = glue("{sppselect}.zip"), 
         files=glue("{getwd()}/{spp_short}"))

## Move files
copyfrom <- glue("{getwd()}/{sppselect}.zip")
file.copy(copyfrom, ensem_dir, recursive=TRUE)

## Delete originals
unlink(glue("{getwd()}/{sppselect}.zip"), recursive = TRUE) 
unlink(glue("{getwd()}/{spp_short}"), recursive = TRUE)

## Unzip folder - This should work now that I've shortened directories
# zip::unzip(zipfile = glue("{getwd()}/{ensem_dir}{sppselect}.zip"),
#            exdir = ensem_dir)

print("SUCCESSFULLY COMPLETED")

# END ---------------------------------------------------------------------
