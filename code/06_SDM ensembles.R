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
library(tictoc)
library(glue)
library(blockCV)
library(rasterVis)

# Notes -------------------------------------------------------------------

# Run import data step ----------------------------------------------------
## run "code/03_SDM import data.R"

# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% 
  print

# Import species and environmental data -----------------------------------
sppselect <- query$Value[which(query$Input == "Species")]
load(glue("data output/sdm data processing/{sppselect}/sdm_input_data.RData"))
load(glue("data output/sdm data processing/{sppselect}/blockCV_data.RData"))

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
  resp.name = sppselect,
  expl.var = stack(envstack),
  # PA.nb.rep = 1, # Number of pseudo abs data sets
  # PA.strategy = "random", # Look at "user.defined" option
  )

biomod_data
plot(biomod_data)

## Custom models cross-validation procedure - Investigate further (I think block CV is another way to do this)
# BIOMOD_cv() ## This function creates a DataSplitTable which could be used to evaluate models in Biomod with repeated k-fold cross-validation (cv) or stratified cv instead of repeated split sample runs

# Define biomod modelling options -----------------------------------------
biomod_options <- BIOMOD_ModelingOptions(MAXENT.Phillips = list(path_to_maxent.jar = "C:/Users/DominicH/Documents/R/win-library/3.5/dismo/java"))
# Look at BIOMOD_ModelingOptions() for a list of custom parameters - especially for GAMs


# Gather blocking fold data -----------------------------------------------

foldtable
block_data <- as_tibble(cbind(sb_check_folds,sb_sys_folds,sb_ran_folds,eb_folds))
colnames(block_data) <- foldtable$fold_dir
head(block_data)

# ::  BIOMOD_Modeling -----------------------------------------------------

## Compute biomod formal models
## Tune models parameters - look into this function        
## BIOMOD_tuning() 

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
  # models.eval.meth = c("ROC"), # Evaluation methods
  SaveObj = TRUE, 
  do.full.models = FALSE, # TRUE = models calibrated and evaluated with the whole dataset are done  
  modeling.id = paste0("Ensemble_",sppselect)
  )

toc(log = TRUE)
tl <- tic.log(format = FALSE)
runtime_biomod <- round((tl[[1]][["toc"]][["elapsed"]] - tl[[1]][["tic"]][["elapsed"]])/60,2)
tic.clearlog()
glue("Biomod model running process took {runtime_biomod} minutes")

# Explore the str of biomod object ----------------------------------------
class(biomod_out)
biomod_out@models.evaluation # See "link" to files on hardrive
biomod_out@variables.importances # These slots can be directly accessed (perhaps as an alternative to getter functions)
biomod_out@models.prediction # Can access these prediction values directly

## Additional objects are stored out of R in two different directories for memory storage purposes. 
## They are created by the function directly on the root of your working directory set in R.
## "models" directory -> This one contains each calibrated model for each repetition and pseudo-absence run

# The models are currently stored as objects to be read exclusively in R. To load them back (the same stands for all objects stored on the hard disk) use the load function (see examples section below).

## At this stage the ".BIOMOD_DATA" folder has the eval and model option type files
## and the "models" file has a model file for each, PA/Run/Model type combination.

# Create output directory  ------------------------------------------------

## Diagnostic plot directory
dir.create(glue('{getwd()}/{str_replace(sppselect," ",".")}/diagnostics'))

# Model score plots -------------------------------------------------------

# This function is a graphic tool to represent evaluation scores of models produced with biomod2 according to 2 different evaluation methods. Models can be grouped in several ways (by algo, by CV run, ...) to highlight potential differences in models quality due to chosen models, cross validation sampling bias,... Each point represents the average evaluation score across each group. Lines represents standard deviation of evaluation scores of the group.

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

pdf(glue('{str_replace(sppselect," ",".")}/diagnostics/avg_model_scores1.pdf'), width = 9, height = 9)
grid.arrange(grobs = plots_out1, ncol = 2,nrow = 2) # Also see ggarrange
dev.off()
pdf(glue('{str_replace(sppselect," ",".")}/diagnostics/avg_model_scores2.pdf'), width = 9, height = 9)
grid.arrange(grobs = plots_out2, ncol = 2,nrow = 2) # Also see ggarrange
dev.off()

p <- arrangeGrob(grobs = plots_out1, ncol = 2,nrow = 2)
ggsave(glue('{str_replace(sppselect," ",".")}/diagnostics/avg_model_scores1.png'), p, width = 9, height = 9)

p <- arrangeGrob(grobs = plots_out2, ncol = 2,nrow = 2)
ggsave(glue('{str_replace(sppselect," ",".")}/diagnostics/avg_model_scores2.png'), p, width = 9, height = 9)

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
ggsave(glue('{str_replace(sppselect," ",".")}/diagnostics/variable_importance.pdf'), width = 16, height = 9)
ggsave(glue('{str_replace(sppselect," ",".")}/diagnostics/variable_importance.png'), width = 16, height = 9)

# Model getter functions --------------------------------------------------

# TODO Clean up this code and remove all these bits that don't do anything. Keep them in a separate file as a reference

get_formal_data(biomod_out) # All data from "BIOMOD.formated.data" object
get_formal_data(biomod_out, subinfo = "MinMax")
get_formal_data(biomod_out, subinfo = "resp.var")
get_formal_data(biomod_out, subinfo = "expl.var.names")

# get_calib_lines: an array (or a data.frame) having the same dimention than 
# the output of get_predictions() of logical values. All lines containing TRUE have been used to calibrate the model
get_calib_lines(biomod_out) 

# get_options: a "BIOMOD.Model.Options" reporting options used to build individual models
get_options(biomod_out) # Algorithm options

# get_built_models: a character vector giving the names of models succefully computed
get_built_models(biomod_out)

# get_evaluations: an array, a data.frame or a list containing models evaluation scores
get_evaluations(biomod_out, as.data.frame = TRUE) 

# Extract model prediction for each sampling unit (total = pres + pseudo abs) ------

## get_predictions: an array (or a data.frame) containing models predictions over 
## calibrating and testing data (those used for evaluate models)

get_predictions(biomod_out, as.data.frame = FALSE)     # Returns array (dimension for each run)
dim(get_predictions(biomod_out, as.data.frame = FALSE))
dimnames(get_predictions(biomod_out, as.data.frame = FALSE))

get_predictions(biomod_out, as.data.frame = TRUE)      # Probability of relative occurence
# check the average probability of pres and pseudo abs points
presdf <- get_predictions(biomod_out, as.data.frame = TRUE)[1:nrow(pres),]
absdf <- get_predictions(biomod_out, as.data.frame = TRUE)[-c(1:nrow(pres)),]

# Check - i.e. high probability at presences and low at pseudoabsences (although look at range)
sapply(presdf, mean)
sapply(absdf, mean)

sapply(presdf, range)
sapply(absdf, range)

map(presdf,hist)
map(absdf,hist)

# Extract model evaluation measures ---------------------------------------
biomod_eval <- get_evaluations(biomod_out) # an array, a data.frame or a list containing models evaluation scores
dimnames(biomod_eval)
biomod_eval["ROC","Testing.data",,,]
biomod_eval["TSS","Testing.data",,,]

biomod_eval
df <- as.data.frame.table(biomod_eval, responseName = "value") 
write_csv(df, 
          glue('{str_replace(sppselect," ",".")}/diagnostics/evaluation_metrics.csv'))

# The "Testing.data" col is what is important in terms of TSS,ROC and KAPPA
# No metrics evaluation data (need to go back and see where to specificy eval) - blockCV or BIOMOD_cv

# Response curves ---------------------------------------------------------
modelnames <- get_built_models(biomod_out)
model_list <- as.list(unique(substring(modelnames, 
                                       str_locate_all(modelnames,"_")[[1]][3,1]+1, 
                                       nchar(modelnames)))) # Find a better way to do this (simply extract model names)

modload1 <- BIOMOD_LoadModels(biomod_out, models = model_list[[1]])
modload2 <- BIOMOD_LoadModels(biomod_out, models = model_list[[2]])
modload3 <- BIOMOD_LoadModels(biomod_out, models = model_list[[3]])

# This are examples of objects that have been loaded
# Files are stored in the "model" folder in working dir
# Breviceps.gibbosus_AllData_RUN1_GBM
# get_formal_model(Breviceps.gibbosus_AllData_RUN1_GBM)
# get_formal_model(Breviceps.gibbosus_AllData_RUN1_RF)

filenames <-  list(glue('{str_replace(sppselect," ",".")}/diagnostics/Response curve_{model_list[[1]]}'),
                   glue('{str_replace(sppselect," ",".")}/diagnostics/Response curve_{model_list[[2]]}'),
                   glue('{str_replace(sppselect," ",".")}/diagnostics/Response curve_{model_list[[3]]}'))

plot_response_curves <- function(modloaded,x,filename, output){
  
  eval_strip <- response.plot2(models = modloaded,
                 Data = get_formal_data(x,'expl.var'), # Extracts the actual raster data values (stored in model file)
                 show.variables= get_formal_data(x,'expl.var.names'), 
                 do.bivariate = FALSE, 
                 fixed.var.metric = 'median', # statistic used to fix as constant the remaining variables when the predicted response is estimated for one of the environmental variables 
                 save.file = output,
                 name = filename,
                 legend = FALSE,
                 display_title = TRUE,
                 data_species = get_formal_data(x,'resp.var'))
  return(eval_strip)
  
  }

response_plot_list <- pmap(list(modloaded = list(modload1,modload2,modload3),
                                x = list(biomod_out),
                                filename = filenames,
                                output = list("pdf")),
                           plot_response_curves)


response_plot_list <- pmap(list(modloaded = list(modload1,modload2,modload3),
                                x = list(biomod_out),
                                filename = filenames,
                                output = list("jpeg")),
                           plot_response_curves)

### all the values used to produce these plots are stored into the returned object and are this customiseable
str(response_plot_list) # Use to redo plots (may be good to present them to experts)

## TODO Need to present these figures in a better way - visualisation of response curves is crucial for expert evalution

# :: evaluate -------------------------------------------------------------

# This function will evaluate biomod2 modelling output for given metrics (e.g 'TSS', 'ROC'...) for a given dataset.
# This is a function to evaluate on an external data set - I think this relates to the "outer validation" step from the biomod review paper.

eval_data <- cbind(spp=get_formal_data(biomod_out,'resp.var'), 
              get_formal_data(biomod_out,'expl.var'))

colnames(eval_data)[1] <- str_replace(sppselect," ",".") # This seems to work (need to change name by adding a full stop)


## Function not working....
# biomod2::evaluate(biomod_out, data = eval_data, stat =c('ROC'))

## The function show give measures based on the evalulating data (as opposed to testing...)
get_evaluations(biomod_out) # But this is already evalutated using the testing data?

# biomod2::evaluate(biomod_EM, data = eval_data, stat =c('ROC')) # Try with ensemble models?

# Try search the following page for help:
# https://r-forge.r-project.org/projects/biomod/

# :: BIOMOD_EnsembleModeling ----------------------------------------------

## Build Ensemble Models 
# BUILD OPTIONS (em.by argument): see C:\Users\DominicH\Google Drive\EWT\Literature\SDM ensemble models\biomod2 vignette 2.pdf
# 5 different ways to combine models can be considered (em.by)
# Dataset used for models building (Pseudo Absences dataset and repetitions done): 'PA_dataset+repet'
# Dataset used and statistical models : 'PA_dataset+algo'
# Pseudo-absences selection dataset : 'PA_dataset'
# Statistical models : 'algo'
# A total consensus model : 'all' (the output should be a single ensemble model)

# If no evaluation data was given the at BIOMOD_FormatingData step, some ensemble models evaluation may be a bit unfair because the data that will be used for evaluating ensemble models could differ from those used for evaluate BIOMOD_Modeling models (in particular, some data used for 'basal models' calibration can be re-used for ensemble models evaluation). You have to keep it in mind ! (EnsembleModelingAssembly vignette for extra details)

## NOTE ON THE "eval.metric"
# The selected metrics here are necessary the ones chosen at the BIOMOD_Modeling step. If you select several, ensembles will be built according to each of them. The chosen metrics will be used at different stages in this function :
# 1.	to remove ‘bad models’ (having a score lower than eval.metric.quality.threshold (see bellow))
# 2.	to make the binary transformation needed for committee averaging computation
# 3.	to weight the models in the probability weighted mean model
# 4.	to test (and/or evaluate) your ensemble-models forecasting ability (at this step, each ensemble-model (ensemble will be evaluated according to each evaluation metric)
                                                                       

biomod_EM <- BIOMOD_EnsembleModeling(
  modeling.output = biomod_out,
  chosen.models = 'all', # Define which models are kept for EM (can remove some)
  em.by='all', # See options above
  eval.metric = c("TSS","ROC"), # See function help file for description
  eval.metric.quality.threshold = c(0.6,0.6), # models with scores below threshold excluded from EM building
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

# prob.mean.weight.decay argument: 
# Define the relative importance of the weights. A high value will strongly discriminate the 'good' models from the 'bad' ones (see the details section). If the value of this parameter is set to 'proportional' (default), then the attributed weights are proportional to the evaluation scores given by 'weight.method'(eval.metric)

# NOTE on committee averaging (each model votes using either a 1 or 0)
# To do this model, the probabilities from the selected models are first transformed into binary data according to the thresholds defined at BIOMOD_Modeling step (maximizing evaluation metric score over ‘testing dataset’ ). The committee averaging score is then the average of binary predictions. It is built on the analogy of a simple vote. Each model vote for the species being ether present or absent. For each site, the sum of 1 is then divided by the number of models. The interesting feature of this measure is that it gives both a prediction and a measure of uncertainty. When the prediction is close to 0 or 1, it means that all models agree to predict 0 and 1 respectively. When the prediction is around 0.5, it means that half the models predict 1 and the other half 0.

biomod_EM
get_built_models(biomod_EM)

# At this stage all of the models above are written to the "model" folder in working dir
# The number of Ensemble models written to file is: 7 (mean,cv,median,committee,weighted, upperConf,lowerConf) x  2 (number of eval.metrics [TSS & ROC]) x 1 (em.by = 'all) THEREFORE total of 14


# Extract Ensemble Evaluations --------------------------------------------
em_vals <- get_evaluations(biomod_EM) 
em_vals

coln <- colnames(em_vals[[1]])
rown <- rownames(em_vals[[1]])

map_df(em_vals,unlist) %>% 
  mutate(measure = rep(coln,each = 3), stat = rep(rown, 4)) %>% 
  select(measure,stat, everything()) %>% 
  write_csv(glue('{str_replace(sppselect," ",".")}/diagnostics/ensemble_models_evaluations.csv'))

## Should think about renaming these columns for easier reading and access

# Ensemble sample predictions ---------------------------------------------

# Predictions for each ensemble model
# predictions across all pseudo and presence data

em_preds <- get_predictions(biomod_EM)
dimnames(em_preds)
dimnames(em_preds)[[2]][4]
dim(em_preds)

# Each EM is a column 
em_preds[1:10,glue("{str_replace(sppselect, ' ', '.')}_EMciInfByTSS_mergedAlgo_mergedRun_mergedData")] # lower CI
# em_preds[1:10,"Breviceps.gibbosus_EMciSupByTSS_mergedAlgo_mergedRun_mergedData"] # upper CI
# em_preds[1:10, "Breviceps.gibbosus_EMcaByTSS_mergedAlgo_mergedRun_mergedData"] # Committee avg
# em_preds[1:10, "Breviceps.gibbosus_EMcvByTSS_mergedAlgo_mergedRun_mergedData"] # Coeff of variation 
# em_preds[1:10, "Breviceps.gibbosus_EMmeanByTSS_mergedAlgo_mergedRun_mergedData"] # Mean
# em_preds[1:10, "Breviceps.gibbosus_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData"] # Weighted mean
# em_preds[1:10, "Breviceps.gibbosus_EMmedianByTSS_mergedAlgo_mergedRun_mergedData"] # Median

# :: BIOMOD_Projection ----------------------------------------------------

## EM model predictions 

# biomod_out@models.computed # Errors when projecting the maxent models
# biomod_out@models.computed[1:2]

# Projections (predictions?) are done for all selected models, that means (by default) for all evaluation run, and pseudo absences selections if applicable. This projections may be used later to compute ensemble forecasting.

#These are not ensemble predictions - rather individual models from BIOMOD_modelling??
biomod_proj <- BIOMOD_Projection(
  modeling.output = biomod_out,
  new.env = stack(envstack),# A set of explanatory variables  
  proj.name = "current", # folder will be created with this name
  selected.models = "all", # can also use subset of models (via biomod_out@models.computed)
  # binary.meth = "TSS",#  binary predictions based on thresholdargument in BIOMOD_Modeling
  binary.meth = NULL, # No binary transformation
  compress = "xz",
  clamping.mask = FALSE,
  build.clamping.mask = FALSE, # See notes below
  output.format = ".grd",
  # output.format = ".RData", # Can also be ".RData" 
 do.stack = FALSE # attempt to save all projections in a unique rasterstack 
 )

# If build.clamping.mask is set to TRUE a file (same type than new.env arg) will be saved in your projection folder. This mask will identifies locations where predictions are uncertain because the values of the variables are outside the range used for calibrating the models. The ‘build.clamping.mask’ values correspond to the number of variables that are out of their calibrating/training range. (see vignette for more details)

## NOTE ON what the function returns 
# Returns the projections for all selected model ("BIOMOD.projection.out" object), and stored in the hard drive on the specific directory names by the name of the projection. The data is a 4-dimensions array (see ...) if new.env is a matrix or a data.frame. It is a rasterStack if new.env is a rasterStack and or several rasterLayers if the rasterStack is too large.

# A new folder is also created on your hard drive. This folder contains the created projection object (basic one and binary and filtered ones if selected). The object are loaded with the load function. The loaded object can be then plotted and analysed.

stack(envstack)
res(envstack) 
class(biomod_proj)

plot(biomod_proj)
plot(biomod_proj, str.grep = "MAXENT.Phillips") 
plot(biomod_proj, str.grep = "RF") 

## Custom plots
current_proj <- get_predictions(biomod_proj) # Returns raster stack
class(current_proj);names(current_proj)

pdf(paste0(str_replace(sppselect," ","."),"/diagnostics/current projection plots.pdf"))
par(mfrow = c(2,2))
for(i in 1:length(names(current_proj))){
  plot(current_proj[[i]], main = names(current_proj)[[i]])
}
dev.off()

# :: BIOMOD_EnsembleForecasting -------------------------------------------

## Ensemble projections of species over space and time 
## This function use projections of ‘individual models’ and ensemble model from BIOMOD_EnsembleModeling to build an ensemble of species' projections over space and time.

class(biomod_EM);class(biomod_proj)

biomod_EF <- BIOMOD_EnsembleForecasting(
  EM.output = biomod_EM,  
  # projection.output = biomod_proj, # Use this if want to include 
  new.env = stack(envstack), #only need if projection.output = NULL (i.e. only use models from threshold selection)
  selected.models = 'all',
  proj.name = "ensembles",
  binary.meth = NULL, # vector of metrics + thresholdsto transform to binary prediction
  filtered.meth = NULL, # filter probabilities lower than threshold and set to 0 (absent)
  compress = TRUE
  )

biomod_EF

# Write EM predictive maps to folder --------------------------------------

# Create a folder to store maps
dir.create(paste0(getwd(),"/",str_replace(sppselect," ","."),"/ensemble_maps"))

# Get ensemble model names
dir_em <-dir(paste0(str_replace(sppselect," ","."),"/proj_ensembles/individual_projections/"), 
             pattern = "gri",
             full.names = TRUE)

em_modname <- word(dir_em, 4, sep = "_") # Can define words by other separators!!!!

# Write ensemble model tif file
write_em_ras <- function(x){

  ras <- raster(dir(paste0(str_replace(sppselect," ","."),"/proj_ensembles/individual_projections/"),
                    pattern = x, full.names = TRUE)[2])
  ras <- ras/1000 # Rescale back to within 0 - 1
  fname <- paste0(str_replace(sppselect," ","."),"/ensemble_maps/",sppselect, "_",x,".tif")
  writeRaster(ras, filename = fname, format = "GTiff")
}

walk(em_modname, write_em_ras)

# Write ensemble to PDF/PNG
## First import SA boundary for plotting
geo_proj <- query$Value[which(query$Input == "Geographic projection")]
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

if (geo_proj == "Yes") {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS) %>% 
    st_transform(aeaproj) 
} else {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS)
}

write_em_pdf <- function(x, output){
  
  ras <- raster(dir(paste0(str_replace(sppselect," ","."),"/proj_ensembles/individual_projections/"),
                     pattern = x, full.names = TRUE)[2])
  
  if (output == "pdf") {
    
    pdf(paste0(str_replace(sppselect," ","."),"/ensemble_maps/",sppselect, "_",x,".pdf"), width = 12, height = 9)
    plot(ras, main = str_c(sppselect," - ",x))
    dev.off()
    
  }
  
  else if (output == "png") {
    
    p <- rasterVis::levelplot(ras, 
                              main= glue("{sppselect} - {x}"),
                              contour = FALSE, 
                              margin = FALSE,
                              col.regions = rev(terrain.colors(40)))+
      layer(sp.points(as(occ_points,"Spatial"),pch = 19, cex = 1.5, col = "black"))+
      layer(sp.lines(as(za, "Spatial"), lwd = 2.5, col = "black"))
    
    p <- arrangeGrob(grobs = list(p), nrow = 1)
    ggsave(
      glue("{str_replace(sppselect,' ','.')}/ensemble_maps/{str_replace(sppselect,' ','.')}_{x}.png"),
      p,
      width = 16, height = 9)
    
  }
  
}

walk2(em_modname,"pdf",
      write_em_pdf)

walk2(em_modname,"png",
      write_em_pdf)

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

pred_range <- raster(dir(paste0(str_replace(sppselect," ","."),"/proj_ensembles/individual_projections/"),
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

pdf(paste0(str_replace(sppselect," ","."),"/ensemble_maps/",sppselect, "_","EMmedianByROC_URB",".pdf"), width = 12, height = 9)
mapTheme <- rasterTheme(region=rev(terrain.colors(10))) # brewer.pal(8,"Greens")
plt <- levelplot(pred_range, margin=F, par.settings=mapTheme)
plt + layer(sp.lines(as(za,"Spatial"),col="black", lwd=1)) +
  layer(sp.polygons(urbspdf,fill = alpha("black", 0.2),col="black", lwd=0.2)) #check sp.lines
dev.off()

# Write workspace ---------------------------------------------------------
save(list = c("occ_points","bck_points","envstack","PB_data","eb","block_data",
              "biomod_data","biomod_EF","biomod_EM","biomod_eval","biomod_options",
              "biomod_out","biomod_proj","biomod_var_import"), 
     file = glue("{getwd()}/{str_replace(sppselect,' ','.')}/biomod objects.RData"))

# Move entire folder to data output directory -----------------------------
ensem_dir <- glue("data output/sdm ensemble results/")

if(dir.exists(ensem_dir)) {
  print("Folder exists")
} else {
  dir.create(ensem_dir)
  print("Folder created")
}

## Move files
copyfrom <- glue("{getwd()}/{str_replace(sppselect,' ','.')}")
file.copy(copyfrom, ensem_dir, recursive=TRUE)

# NOT COPYING FILES FROM (ERROR):
# C:\Users\DominicH\Google Drive\EWT\Analysis\Amphibians\Breviceps.gibbosus\.BIOMOD_DATA\Ensemble_Breviceps gibbosus\ensemble.models\ensemble.models.predictions

# ## Delete original folder
unlink(copyfrom, recursive = TRUE) #Delete directory

# END ---------------------------------------------------------------------
