
# Notes on biomod ensemble running code -----------------------------------

# Create EM working directory ---------------------------------------------

## Removed this step for the meantime.
ensem_dir <- glue("data output/sdm ensemble results/")

if(dir.exists(ensem_dir)) {
  print("Folder exists")
} else {
  dir.create(ensem_dir)
  print("Folder created")
}

setwd(ensem_dir)

# BIOMOD_cv ---------------------------------------------------------------

## This function creates a DataSplitTable which could be used to evaluate models in Biomod with repeated k-fold cross-validation (cv) or stratified cv instead of repeated split sample runs
## I think this is based on ENMeval
## Example from help file:
# DataSplitTable <- BIOMOD_cv(myBiomodData, k=5, rep=2, do.full.models=F)
# DataSplitTable.y <- BIOMOD_cv(myBiomodData,stratified.cv=T, stratify="y", k=2)
# colnames(DataSplitTable.y)[1:2] <- c("RUN11","RUN12")
# DataSplitTable <- cbind(DataSplitTable,DataSplitTable.y)
## TODO possible check how this compares to blockCV results


# BIOMOD_ModelingOptions --------------------------------------------------

## Look at BIOMOD_ModelingOptions() for a list of custom parameters - especially for GAMs

#  BIOMOD_Modeling -----------------------------------------------------

## Compute biomod formal models
## TODO Tune models parameters - look into this function - BIOMOD_tuning() 

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

# Model score plots -------------------------------------------------------

# models_scores_graph()

# This function is a graphic tool to represent evaluation scores of models produced with biomod2 according to 2 different evaluation methods. Models can be grouped in several ways (by algo, by CV run, ...) to highlight potential differences in models quality due to chosen models, cross validation sampling bias,... Each point represents the average evaluation score across each group. Lines represents standard deviation of evaluation scores of the group.

# Model getter functions --------------------------------------------------

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

# Response curves ---------------------------------------------------------

# This are examples of objects that have been loaded
# Files are stored in the "model" folder in working dir
# Breviceps.gibbosus_AllData_RUN1_GBM
# get_formal_model(Breviceps.gibbosus_AllData_RUN1_GBM)
# get_formal_model(Breviceps.gibbosus_AllData_RUN1_RF)

# :: evaluate -------------------------------------------------------------

# This function will evaluate biomod2 modelling output for given metrics (e.g 'TSS', 'ROC'...) for a given dataset.
# This is a function to evaluate on an external data set - I think this relates to the "outer validation" step from the biomod review paper.

## Function not working....
#biomod2::evaluate(biomod_out, data = eval_data, stat =c('ROC'))

#biomod2::evaluate(biomod_EM, data = eval_data, stat =c('ROC')) # Try with ensemble models?

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

# NOTE on committee averaging (each model votes using either a 1 or 0)
# To do this model, the probabilities from the selected models are first transformed into binary data according to the thresholds defined at BIOMOD_Modeling step (maximizing evaluation metric score over ‘testing dataset’ ). The committee averaging score is then the average of binary predictions. It is built on the analogy of a simple vote. Each model vote for the species being ether present or absent. For each site, the sum of 1 is then divided by the number of models. The interesting feature of this measure is that it gives both a prediction and a measure of uncertainty. When the prediction is close to 0 or 1, it means that all models agree to predict 0 and 1 respectively. When the prediction is around 0.5, it means that half the models predict 1 and the other half 0.


# Ensemble sample predictions ---------------------------------------------

# Predictions for each ensemble model
# predictions across all pseudo and presence data

em_preds <- get_predictions(biomod_EM)
dimnames(em_preds)
dimnames(em_preds)[[2]][4]
dim(em_preds)

# Each EM is a column - eg:
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

# If build.clamping.mask is set to TRUE a file (same type than new.env arg) will be saved in your projection folder. This mask will identifies locations where predictions are uncertain because the values of the variables are outside the range used for calibrating the models. The ‘build.clamping.mask’ values correspond to the number of variables that are out of their calibrating/training range. (see vignette for more details)

## NOTE ON what the function returns 
# Returns the projections for all selected model ("BIOMOD.projection.out" object), and stored in the hard drive on the specific directory names by the name of the projection. The data is a 4-dimensions array (see ...) if new.env is a matrix or a data.frame. It is a rasterStack if new.env is a rasterStack and or several rasterLayers if the rasterStack is too large.

# A new folder is also created on your hard drive. This folder contains the created projection object (basic one and binary and filtered ones if selected). The object are loaded with the load function. The loaded object can be then plotted and analysed.

# :: BIOMOD_EnsembleForecasting -------------------------------------------

## Ensemble projections of species over space and time 
## This function use projections of ‘individual models’ and ensemble model from BIOMOD_EnsembleModeling to build an ensemble of species' projections over space and time.

class(biomod_EM);class(biomod_proj)