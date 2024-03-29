EM_write_pdf <- function(x, output){
  
  ras <- raster(dir(glue("{spp_short}/proj_ensembles/individual_projections/"),
                    pattern = x, full.names = TRUE)[2])
  
  if (output == "pdf") {
    
    pdf(glue("{spp_short}/ensemble_maps/{spp_short}_{x}.pdf"), width = 12, height = 9)
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
      glue("{spp_short}/ensemble_maps/{spp_short}_{x}.png"),
      p,
      width = 16, height = 9)
    
  }
  
}