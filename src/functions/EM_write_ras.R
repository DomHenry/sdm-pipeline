EM_write_ras <- function(x){
  ras <- raster(dir(glue("{spp_short}/proj_ensembles/individual_projections/"),
                    pattern = x, full.names = TRUE)[2])
  ras <- ras/1000 # Rescale back to within 0 - 1
  fname <- glue("{spp_short}/ensemble_maps/{spp_short}_{x}.tif")
  writeRaster(ras, filename = fname, format = "GTiff")
}
