# Plot all occurence records based on time period - WLT highlighted -----------
occ_date_plot_wlt <- function(x,startdate,ptitle){
  
  vcol <- viridisLite::viridis(20)[14]
  x %>% 
    filter(date > startdate) %>% 
    ggplot(aes(x = year))+
    geom_bar(fill = alpha(vcol,0.8), col = NA) +
    ggtitle(ptitle)+
    gghighlight(scientificname == "Sclerophrys pantherinus")+
    theme(axis.text.x = element_text(angle = 90, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14))
}
