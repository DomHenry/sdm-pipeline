spp_range_inset_decade <- function(x,output) {
  
  
  validate_spp <- amphsf %>% 
    filter(scientificname == spp[x]) %>% 
    as_tibble %>% 
    select(decade) %>% 
    pull
  
  if (all(is.na(validate_spp))) { # Do nothing if there are only NA values for decade
    
  } else {
    
    sppdata <- amphsf %>% filter(scientificname %in% spp[x])
    occ_bbox <- st_make_grid(st_buffer(sppdata,0.3), n = 1) # Make a grid with one cell
    
    ptheme <- theme(axis.text = element_text(size = 14),
                    plot.title = element_text(size = 18))
    
    p1 <- ggplot() +
      geom_sf(data = sa, fill = alpha("grey",0.3), size = 0.5)+
      geom_sf(data = sppdata, size = 1.5, col = "black")+
      geom_sf(data = occ_bbox, fill = NA, col = "dodgerblue", size = 1.3) +
      theme_bw()+
      ptheme +
      ggtitle(glue("{spp[x]}"))
    p1
    
    
    p2 <- ggplot()+
      geom_sf(data = sa,fill = alpha("grey",0.3))+
      geom_sf(data =  sppdata, aes(color = decade,fill = decade), size = 3)+
      scale_color_viridis_d(guide = FALSE)+
      scale_fill_viridis_d()+
      guides(fill = guide_legend(title="Decade"))+
      coord_sf(xlim = c(st_bbox(occ_bbox)$xmin,st_bbox(occ_bbox)$xmax),
               ylim = c(st_bbox(occ_bbox)$ymin,st_bbox(occ_bbox)$ymax))+
      theme_bw()+
      theme(panel.border = element_rect(colour = "dodgerblue", fill=NA, size=1.6))+
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18))+
      ggtitle(glue("{spp[x]}"))
    
    # TODO - need to figure out how to plot NA values as a color (currently they're white)
    
    plist <- list(p1,p2)
    
    if (output == "png") {
      
      # grid.arrange(grobs = plist, nrow = 1) --> doesn't work - have to write an object first
      p <- arrangeGrob(grobs = plist, nrow = 1)
      ggsave(glue("data output/spatial occurrence plots/decade_inset_{spp[x]}.png"),p,
             width = 13, height = 6)
      
    } else {
      
      return(grid.arrange(grobs = plist, nrow = 1))
    }
    
  }
  
 }