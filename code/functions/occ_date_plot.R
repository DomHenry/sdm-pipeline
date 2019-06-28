# Plot all occurence records based on time period -------------------------
occ_date_plot <- function(x,startdate,ptitle){
  x %>% 
    filter(date > startdate) %>% 
    ggplot(aes(x = year))+
    geom_bar()+
    ggtitle(ptitle)+
    ylab("Count")+
    theme(axis.text.x = element_text(angle = 90, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14))
}
