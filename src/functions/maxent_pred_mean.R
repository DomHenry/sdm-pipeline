# Maxent SDM mean probablility plot ---------------------------------------

maxent_pred_mean <- function(x,fold_name,output){
  pred_range <- dismo::predict(x, envstack, progress = "text") 
  pred_mean <- cellStats(pred_range, stat="mean", na.rm=TRUE)
  pred_mean_mat <- matrix(c(-pred_mean, pred_mean, 0), ncol=3, byrow=TRUE)
  pred_range_mean <- reclassify(pred_range, pred_mean_mat)
  
  if (output == "pdf") {
    plot(pred_range_mean, xlab="Lon", ylab="Lat", las=1, main= glue("{quo_name(fold_name)} - Mean probablility")) 
    plot(occ_points$geometry,pch=16, cex=0.8, add = T)
  }
  
  if (output == "raster") {
    writeRaster(pred_range, 
                filename = glue("{pred_ras_dir}/{sppselect}_mean_{quo_name(fold_name)}.tif"), 
                format="GTiff", 
                overwrite=TRUE) 
  }
}