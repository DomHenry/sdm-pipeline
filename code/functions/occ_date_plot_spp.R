# Plot single species -----------------------------------------------------
occ_date_plot_spp <- function(x,y,startdate){
  x %>% 
    filter(date > startdate) %>% 
    filter(scientificname == y) %>% 
    ggplot(aes(x = year))+
    geom_bar()+
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank())
}
