# Plot map occurence points for each species ------------------------------
plot_single_spp <- function(x,output){
  
  p <- ggplot()+
    geom_sf(data = za,fill = alpha("grey",0.5))+
    geom_sf(data =  amphsf %>% filter(scientificname %in% spp[x]),
            col = "black",
            size = 1)+
    theme(axis.text = element_text(size = 14),
          strip.text = element_text(size = 14))+
    facet_wrap(~scientificname)
  
  if (output == "png") {
    
    ggsave(glue("data output/spatial occurrence plots/national_{spp[x]}.png"))
    
  } else {
    
    return(p)
  }
}