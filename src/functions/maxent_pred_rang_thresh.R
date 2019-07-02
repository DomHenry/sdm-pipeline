# Plot maxent SDM thresholds ----------------------------------------------

maxent_pred_rang_thresh <- function(x,fold_name,pred_thresh,output){
  
  pred_range <- dismo::predict(x, envstack, progress = "text") 
  pred_thresh_mat <- matrix(c(-pred_thresh, pred_thresh, 0), ncol=3, byrow=TRUE)
  pred_range_thresh <- reclassify(pred_range, pred_thresh_mat)
  
  if (output == "pdf") {
    plot(pred_range_thresh, xlab="Lon", ylab="Lat", las=1, main= glue("{quo_name(fold_name)} - Thresh {pred_thresh}")) 
    plot(occ_points$geometry,pch=16, cex=0.8, add = T)
    
  }
  
  if (output == "raster") {
    writeRaster(pred_range, 
                filename = glue("{pred_ras_dir}/{sppselect}_thresh_{quo_name(fold_name)}.tif"), 
                format="GTiff", 
                overwrite=TRUE) 
  }
}

