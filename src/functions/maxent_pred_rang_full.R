# Plot maxent SDM ---------------------------------------------------------

maxent_pred_rang_full <- function(x,fold_name,output){
  
  pred_range <- dismo::predict(x, envstack, progress = "text") 
  
  if (output == "pdf") {
    plot(pred_range, xlab="Lon", ylab="Lat", las=1, main= glue("{sppselect} - {quo_name(fold_name)}")) 
    plot(occ_points$geometry,pch=16, cex=0.8, add = T)  
  }
  
  if (output == "raster") {
    writeRaster(pred_range, 
                filename = glue("{pred_ras_dir}/{sppselect}_range_{quo_name(fold_name)}.tif"), 
                format="GTiff", 
                overwrite=TRUE) 
  }
  
  else if (output == "png") {
    
    p <- rasterVis::levelplot(pred_range, 
                              main= glue("{sppselect} - {quo_name(fold_name)}"),
                              contour = FALSE, 
                              margin = FALSE,
                              col.regions = rev(terrain.colors(40)))+
      layer(sp.points(as(occ_points,"Spatial"),pch = 19, cex = 1.5, col = "black"))+
      layer(sp.lines(as(za, "Spatial"), lwd = 2.5, col = "black"))
    
    p <- arrangeGrob(grobs = list(p), nrow = 1)
    ggsave(
      glue("{pred_png_dir}/{sppselect}_range_{quo_name(fold_name)}.png"),
      p,
      width = 16, height = 9)
    
  }
  
}