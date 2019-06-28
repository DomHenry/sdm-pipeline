# Plot JPEG of occ points for each species per decade ---------------------
plot_spp_decade_jpg <- function(x){
  ggplot()+
    geom_sf(data = sa_merge,fill = alpha("grey",0.5))+
    geom_sf(data =  amphsf %>% filter(scientificname %in% x),
            aes(col = decade),
            size = 1.8,pch = 16)+
    scale_color_discrete()+
    theme(legend.text = element_text(size = 18),
          legend.title = element_blank())
  filedir <- paste0("data output/spatial occurrence plots/species jpegs/",x,".jpg")
  ggsave(filedir, width = 16, height = 9)
  
}

