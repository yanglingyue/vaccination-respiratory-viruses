rm(list = ls())
setwd("/Users/yly/Desktop/vaccination-respiratory-viruses")
library(geofacet);library(sf);library(readr);library(ggpubr);library(rmapshaper)
library(dplyr);library(ggplot2);library(tidyverse);library(RColorBrewer);library(sf)
source("plot_theme.R")

# Load and prepare data.
out_path0 <-  "01_Fig1"
outpath <-  "00_data"
out_path1 <- paste0(out_path0,"/output")
dir.create(out_path1)

dat1_avg1<- read.csv(paste0(outpath,"/seasonal_flu_coverage_average.csv"))
colnames(dat1_avg1)[4] <- "general_coverage"

vero <- dat1_avg1[,c("country","general_coverage")]
colnames(vero) <- c("region","freq")
vero_region<-cut(vero$freq,breaks=c(0,10,30,100),include.lowest=T,
                 labels = c("Low coverage region","Medium coverage region","High coverage region"))#
vero<-cbind(vero,vero_region)

# Plot Figure 1B
dat_vero <- read.csv(paste0(outpath,"/FLU_coverage_final_2015_2024.csv"))
dat_vero <- dat_vero[-which(dat_vero$year==2015),]
dat_vero$country <- factor(dat_vero$country,
                           levels = dat1_avg1$country[order(dat1_avg1$general_coverage, decreasing = F)],ordered=TRUE)

max_country_10 <- dat1_avg1[dat1_avg1$general_coverage <= 10, ][which.max(dat1_avg1$general_coverage[dat1_avg1$general_coverage <= 10]), "country"]
min_country_30 <- dat1_avg1[dat1_avg1$general_coverage >= 30, ][which.min(dat1_avg1$general_coverage[dat1_avg1$general_coverage >= 30]), "country"]

ps1 <- ggplot(dat_vero, aes(year,country)) +
  geom_tile(aes(fill=coverage),color = "grey60",size=0.3) +
  scale_fill_gradientn(colors = brewer.pal(9,"OrRd"), na.value = "white",
                       limits = c(0, 75),breaks = seq(0, 75, by = 25)) + 
  scale_x_continuous(expand = c(0, 0),breaks = seq(2015,2024,1))+
  labs(x="",y="") + 
  mytheme+
  geom_hline(yintercept =max_country_10 , linetype = "dashed", color = "black",size=1) +
  geom_hline(yintercept =min_country_30, linetype = "dashed", color = "black",size=1) +
  geom_vline(xintercept =2020, linetype = "solid", color = "black",size=1) +
  theme(axis.text.x = element_text(color="black",vjust = 0.5,hjust = 1, angle = 90))+
  theme(legend.key.size = unit(0.5,"cm"),legend.key.width = unit(1.5,"cm"),
        legend.key.height  = unit(0.35,"cm"),legend.position = c("bottom"))+
  guides(fill=guide_colorbar("Flu vaccine coverage",title.position = "bottom", 
                             title.theme = element_text( angle = 0,hjust = 0.5),
                             title.vjust = 0.9)) +
  scale_y_discrete(breaks = NULL) +  # 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())+
  theme(panel.spacing = unit(1, "lines"),strip.text = element_text(size = 20, face = "bold"))

ps1

ggsave(filename = file.path(out_path1, "Fig1B.pdf"),
       plot   = ps1, width = 3.5, height =6.5, device   = cairo_pdf,  family   = "Arial")

# Plot Figure 1C
worldMap <- st_read(paste0(outpath,"/golbalshp/global_shp.shp"))
worldMap <- worldMap[-which(worldMap$ADMIN =="Antarctica"),] 
worldMap <- worldMap[-which(worldMap$ADMIN =="United Kingdom"),] 

world_sf <- worldMap %>%
  mutate(region = ADMIN) %>%           
  mutate(
    region = ifelse(region == "United States of America", "United States", region),
    region = ifelse(region == "England", "United Kingdom, England", region),
    region = ifelse(region == "Northern Ireland", "United Kingdom, Northern Ireland", region),
    region = ifelse(region == "Scotland", "United Kingdom, Scotland", region),
    region = ifelse(region == "Wales", "United Kingdom, Wales", region),
  )

world_sf1 <- world_sf %>% left_join(vero, by = "region")

kaz   <- filter(world_sf1, ADMIN == "Kazakhstan")
bay   <- filter(world_sf1, ADMIN == "Baykonur Cosmodrome")
others<- filter(world_sf1, !(ADMIN %in% c("Kazakhstan","Baykonur Cosmodrome")))
kaz <- st_make_valid(kaz)
bay <- st_make_valid(bay)
kz_union_geom <- st_union(st_geometry(kaz), st_geometry(bay))
st_geometry(kaz) <- kz_union_geom  
world_sf2 <- rbind(others, kaz)

theme1 <-  theme(axis.ticks = element_blank(),
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 legend.justification=c(0.5,0),
                 legend.position = c("bottom"),
                 legend.background = element_blank(),
                 legend.key.size = unit(0.5,"cm"),
                 legend.key.width = unit(1,"cm"),
                 legend.key.height  = unit(0.35,"cm"),
                 legend.title = element_text(size = 20,face = "bold",vjust = 0.85,hjust=2),
                 legend.text = element_text(size = 20,lineheight=6),
                 legend.spacing = unit(6, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
                 legend.spacing.y = NULL,#the spacing between legends (unit)
                 axis.title = element_blank(),
                 plot.margin =  margin(0, 0, 0, 0, "cm"),
                 panel.background = element_rect(fill = "white"),
                 legend.box.background =element_blank(),
                 legend.box.margin=  margin(0, 0, 0, 0, "cm"),
                 panel.spacing = unit(0,"cm"),
                 panel.border = element_rect(fill='transparent',colour="transparent"),
                 legend.key = element_blank(),
                 plot.title = element_text(hjust = 0.06, size = 20, vjust = 0))

colors <- c("#377eb8",'#7570b3', "#dc3637","#cc2b80","#77488c","#2f9798","#88b5d6","#d4b9da")

# Simplify map geometries before plotting to improve rendering efficiency
# keep = 0.005 retains approximately 0.5% of vertices; keep_shapes = TRUE preserves small polygons
world_sf3<- rmapshaper::ms_simplify(world_sf2, keep = 0.005, keep_shapes = TRUE)

p2 <- ggplot(world_sf3) +
  geom_sf(aes(fill = vero_region), color = "azure4", size = 0.25, alpha = 0.75) +
  scale_fill_manual("",values =  colors,breaks = c("Low coverage region",
                                                   "Medium coverage region",
                                                   "High coverage region"), na.value="grey90")+
  theme1+
  theme(plot.title = element_text(hjust = 0.5,size=24, face = "bold"))+
  theme(legend.key.size = unit(0.5,"cm"),legend.key.width = unit(1,"cm"),legend.key.height  = unit(0.35,"cm"))+
  theme(panel.border = element_rect(color = "black", size = 1, fill = NA))+
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", size = 1) +  
  geom_hline(yintercept = 23.5, color = "black", linetype = "dashed", size = 1) +  # Tropic of Cancer
  geom_hline(yintercept = -23.5, color = "black", linetype = "dashed", size = 1)    # Tropic of Capricorn

p2

ggsave(filename = file.path(out_path1, "/Fig1C.pdf"),
           plot = p2, width    = 13,height   = 6,device   = cairo_pdf, family   = "Arial")

