EM_response_curves <- function(modloaded,x,filename, output){
  
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