# Description -------------------------------------------------------------

## Thu Jan 10 10:47:30 2019 
## Temporal analysis of amphibian occurence records

library(tidyverse)
library(lubridate)
library(gghighlight)
library(gridExtra)
library(ggpubr)
library(glue)

# Import data -------------------------------------------------------------
load("data output/amph_data_clean.RData")
walk(dir("src/functions/", full.names = TRUE),
        source)

# Rmarkdown report information --------------------------------------------

## Check totals and missing species 
ifelse(length(unique(amph$scientificname)) == nrow(rl),"All species are accounted for","There are missing species")

cat("\nIUCN species count:", nrow(rl), "\n",
    "IUCN species in amphibian database:", length(unique(amph$scientificname)), "\n",
    "Species for which there are no data: ", spp[!spp %in% unique(amph$scientificname)], "\n",
    "Proportion of all data belonging to red-listed species: ", (nrow(amph)/nrow(amph_all)*100), "\n",
    "Proportion of all red-listed species: ", (length(spp)/134*100))

## Date errors - can use this type of code in reports/markdown docs
cat("Number of records with date parsing error:", length(which(is.na(amph$date))))
cat("Proportion records with date parsing error:", length(which(is.na(amph$date)))/nrow(amph)*100)
print(paste0("Earliest record: ", min(amph$date, na.rm = TRUE)))
print(paste0("Most recent record: ", max(amph$date, na.rm = TRUE)))

# Remove species with no data from species RL vector ----------------------
spp <- spp[spp %in% unique(amph$scientificname)]

# Plot :: Decadal occurrence summary ---------------------------------------

## Create output folder
temp_plot_dir <- glue("data output/temporal occurrence plots")
if(dir.exists(temp_plot_dir)) {
  print("Folder exists")
} else {
  dir.create(temp_plot_dir)
  print("Folder created")
}

## Theme
ptheme <- theme(axis.text.x = element_text(angle = 90, size = 16),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size = 16),
                title = element_text(size = 18))
p1 <- amph %>%  ## Note - Don't need to remove NA dates for ggplot - it does so automatically
  mutate(decade = floor_date(date, years(10))) %>% 
  group_by(decade) %>% 
  tally() %>% 
  mutate(prop = (n/nrow(amph)*100)) %>% 
  ggplot(aes(x = decade, y = prop)) +
  geom_bar(stat = "identity")+
  labs(title = "Proportion of all occurrence records per decade",
       subtitle = "All red-listed species included",
       caption = "*Data collected up until 2015", 
       x = "", y = "Proportion (%)")+
  scale_x_date(breaks = seq(ymd("1900-01-01"),ymd("2010-01-01"), by = "10 years"), 
               date_labels = "%Y") +
  coord_cartesian(ylim = c(0,40))+
  ptheme

p2 <- amph %>% 
  filter(!scientificname %in% "Sclerophrys pantherinus") %>% 
  mutate(decade = floor_date(date, years(10))) %>% 
  group_by(decade) %>% 
  tally() %>% 
  mutate(prop = (n/nrow(amph)*100)) %>% 
  ggplot(aes(x = decade, y = prop)) +
  geom_bar(stat = "identity")+
  labs(title = "Proportion of all occurrence records per decade",
       subtitle = expression(paste(italic("Sclerophrys pantherinus"), " removed")),
       caption = "", 
       x = "", y = "Proportion (%)")+
  scale_x_date(breaks = seq(ymd("1900-01-01"),ymd("2010-01-01"), by = "10 years"), 
               date_labels = "%Y") +
  coord_cartesian(ylim = c(0,40))+
  ptheme

pdf("data output/temporal occurrence plots/occ_proportion_per_decade.pdf", width = 16, height = 9)
grid.arrange(grobs = list(p1,p2), ncol = 2) # Also see ggarrange
dev.off()

# Plot :: All occurrence records - 4 time periods --------------------------

## Most of these records are Western Leopard Toad
cat("Proportion of occurrence records of made up of Sclerophrys pantherinus: ",length(which(amph$scientificname == "Sclerophrys pantherinus"))/nrow(amph)*100)

pdf("data output/temporal occurrence plots/all_occ_records.pdf", width = 14, height = 9)
p1 <- occ_date_plot(amph,dmy("01-01-1897"),"All records")
p2 <- occ_date_plot(amph,dmy("01-01-1930"), "1930 onwards")
p3 <- occ_date_plot(amph,dmy("01-01-1950"), "1950 onwards")
p4 <- occ_date_plot(amph,dmy("01-01-1980"), "1980 onwards")
grid.arrange(grobs = list(p1,p2,p3,p4), ncol = 2) 
dev.off()

pdf("data output/temporal occurrence plots/all_occ_records_excl_WLT.pdf",width = 14, height = 9)
p1 <- occ_date_plot(amph %>% filter(!scientificname %in% "Sclerophrys pantherinus"),dmy("01-01-1897"),"All records")
p2 <- occ_date_plot(amph %>% filter(!scientificname %in% "Sclerophrys pantherinus"),dmy("01-01-1930"), "1930 onwards")
p3 <- occ_date_plot(amph %>% filter(!scientificname %in% "Sclerophrys pantherinus"),dmy("01-01-1950"), "1950 onwards")
p4 <- occ_date_plot(amph %>% filter(!scientificname %in% "Sclerophrys pantherinus"),dmy("01-01-1980"), "1980 onwards")
grid.arrange(grobs = list(p1,p2,p3,p4), ncol = 2)
dev.off()

pdf("data output/temporal occurrence plots/all_occ_records_high_WLT.pdf", width = 14, height = 9)
p1 <- occ_date_plot_wlt(amph,dmy("01-01-1897"),"All records")
p2 <- occ_date_plot_wlt(amph,dmy("01-01-1930"), "1930 onwards")
p3 <- occ_date_plot_wlt(amph,dmy("01-01-1950"), "1950 onwards")
p4 <- occ_date_plot_wlt(amph,dmy("01-01-1980"), "1980 onwards")
grid.arrange(grobs = list(p1,p2,p3,p4), ncol = 2)
dev.off()

# Plot :: Species occurrence records ---------------------------------------
pdf("data output/temporal occurrence plots/occ_spp_1970.pdf", width = 16, height = 9)
spp_len <- c(1:length(spp))
plists <- split(spp_len, ceiling(seq_along(spp_len)/6))
pmap(list(list(amph),plists,list(ymd("1970-01-01"))),
     occ_date_high_plot)
dev.off()

pdf("data output/temporal occurrence plots/occ_spp_1940.pdf", width = 16, height = 9)
spp_len <- c(1:length(spp))
plists <- split(spp_len, ceiling(seq_along(spp_len)/6))
pmap(list(list(amph),plists,list(ymd("1940-01-01"))),
     occ_date_high_plot)
dev.off()

# Plot :: Western Leopard Toad  -------------------------------------------
sppchoose <- "Sclerophrys pantherinus"
vcol <- viridisLite::viridis(20)[14] # Access colors in the viridis pallette

amph %>% 
  filter(date > dmy("01-01-1960")) %>% 
  ggplot(aes(x = year,fill = scientificname))+
  geom_bar(fill = alpha(vcol,0.8), col = NA) +
  gghighlight(scientificname %in% sppchoose)+
  ylab("Count")+
  theme(axis.text.x = element_text(angle = 90, size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 18))+
  coord_cartesian(ylim = c(0,800))
ggsave("data output/temporal occurrence plots/Sclerophrys pantherinus.pdf", width = 16, height = 9)


# Plot :: Single species PNGs --------------------------------------------

pmap(list(list(amph),spp_len,list(ymd("1940-01-01"))),
     safely(occ_date_high_plot_single))

