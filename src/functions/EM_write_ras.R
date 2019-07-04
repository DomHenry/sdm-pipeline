EM_write_ras <- function(x){
  ras <- raster(dir(paste0(str_replace(sppselect," ","."),"/proj_ensembles/individual_projections/"),
                    pattern = x, full.names = TRUE)[2])
  ras <- ras/1000 # Rescale back to within 0 - 1
  fname <- paste0(str_replace(sppselect," ","."),"/ensemble_maps/",sppselect, "_",x,".tif")
  writeRaster(ras, filename = fname, format = "GTiff")
}
