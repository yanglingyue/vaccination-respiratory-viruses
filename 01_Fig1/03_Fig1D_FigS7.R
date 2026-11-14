rm(list = ls())
setwd("/Users/yly/Desktop/vaccination-respiratory-viruses")
library(dplyr);library(tidyverse);library(tidyr);library(lubridate);library(ggpubr)
library(anytime);library(reshape2);library(ggplot2);library(RColorBrewer);library(readxl)
library(gridExtra);library(grid);library(purrr);library(entropy)
source("plot_theme.R")
options(scipen = 999)

# Load and prepare data.
out_path0 <-  "01_Fig1"
dir.create(out_path0)
outpath <-  ("00_data")
out_path1 <-  paste0(out_path0,"/output") 
dir.create(out_path1)

dat_vero_avg<- read.csv("00_data/seasonal_flu_coverage_average.csv")
colnames(dat_vero_avg)[4] <- "general_coverage"

dat_out <- read.csv(paste0(outpath,"/virus_all_outbreak.csv"))
dat1_lat <- read.csv(paste0(outpath,"/lat_lon.csv"))

colnames(dat1_lat) <- c("country","lon","lat")
dat1_lat$FLUSEASON <- ifelse(abs(dat1_lat$lat) >= 23.5, "Temperate", "Tropic")
dat_season <- merge(dat1_lat, dat_vero_avg, by = c("country"), all.y = TRUE)

dat_out <- merge(dat_out, dat_season, by = c("country"), all.y = TRUE)

dat_out <-  dat_out %>%
  filter((flu_year >= 2015 & flu_year < 2020))     

# Define virus types, plot colors, and initialize output objects.
custom_colors <- c("#cc2b80","#377eb8","#32ba6c","#f1864f","#694fbb",'#f1c40f',
                   "#dc3637","#5c6068","#A50026", "#313695")
virus_name<-c("INF_ALL","INF_A","INF_B", "RSV","PIV","RV","ADV","HMPV")
n = length(virus_name)

dat_out_year <- data.frame()
plot_peak_temp <- list()
plot_peak_trop <- list()
plot_peak_all <- list()

plot_ei_temp <- list()
plot_ei_trop <- list()
plot_ei_all <- list()

# Compare viral peak intensity and circulation peakedness across influenza vaccination coverage groups.
for(j in 1:n)
{
  print(virus_name[j])
  D1 = subset(dat_out ,dat_out$virus_type==virus_name[j])
 
  D2 <- D1[,c("country","flu_year","virus_type","FLUSEASON", "peak_AAP",
              "epidemic_intensity","general_coverage")]
 
  if (any(D2$peak_AAP == 0) | any(D2$peak_AAP == 100) | any(is.na(D2$peak_AAP))) {
    D2 <- D2[!(D2$peak_AAP %in% c(0, 100) | is.na(D2$peak_AAP)), ]
  }
  
  D2$general_vero_type <- NA
  D2$general_coverage <- as.numeric(D2$general_coverage)
  D2[D2$general_coverage<=10,"general_vero_type"] <- "Below 10"
  D2[!D2$general_coverage<=30,"general_vero_type"] <- "Above 30"
 
  D2[which(is.na(D2$general_vero_type)),"general_vero_type"] <- "10-30"
 
  min_int.i <- min(D2$epidemic_intensity)
  max_int.i <- max(D2$epidemic_intensity)
  rang_int.i <- max_int.i-min_int.i
  D2$epidemic_intensity.norm <- D2$epidemic_intensity-min_int.i
  D2$epidemic_intensity.norm <- D2$epidemic_intensity.norm/rang_int.i
  D2$FLUSEASON1 <- D2$FLUSEASON
  
  dat_out_year <- rbind(dat_out_year,D2)
  
  D2_temp = subset(D2 ,D2$FLUSEASON1=="Temperate")
  comparisons <- combn(unique(D2_temp$general_vero_type), 2, simplify = FALSE)
  D2_temp$general_vero_type <- factor(D2_temp$general_vero_type,
                                      levels = c( "Below 10","10-30","Above 30"),ordered=TRUE)
  ps1 <- ggplot(D2_temp, aes(x=general_vero_type, y=peak_AAP,fill=general_vero_type,color=general_vero_type)) + 
    geom_boxplot(notch=F, outlier.shape = NA,outlier.size = 4,
                 outlier.fill = "black",alpha=0.45, width = 0.5)+
    stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.format",show.legend = TRUE,
                       size = 6,label.y = c(72, 82, 92)) +
    geom_point(position=position_jitterdodge(jitter.width=1, dodge.width = 0), 
               pch=21, aes(fill=factor(general_vero_type),color=factor(general_vero_type)), show.legend = F,size=1)+
    scale_y_continuous(name="Peak intensity", limits = c(0,100) ,breaks = seq(0,100,25))+
    labs(title = paste0(virus_name[j]) ,x="") + 
    mytheme+
    theme(axis.text.x = element_text(size = 18, color="black",vjust = 0.5,hjust = 0.5, angle = 0),
          strip.text = element_text(size = 18, face = "bold"),legend.text = element_text(size = 12))+
    theme(axis.title.y.left = element_text(size = 18,color="black",vjust=2,face = "bold", margin = margin(0,-0.2,0,0,'cm')),
          axis.title.x = element_text(size = 18, color="black",face = "bold",margin = margin(0,0,0,0,'cm')))+
    scale_fill_manual(name = '',values = c("Below 10" = custom_colors[1], "10-30" = custom_colors[2], 
                                           "Above 30" = custom_colors[3])) +
    scale_color_manual(name = '',values = c("Below 10" = custom_colors[1], "10-30" = custom_colors[2], 
                                            "Above 30" = custom_colors[3])) +
    theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())+ 
    guides(color = guide_legend(override.aes = list(shape = NA, size = 3)))   
  
  ps1
  
  plot_peak_temp[[j]] <- ps1
  
  ps2 <- ggplot(D2_temp, aes(x=general_vero_type, y=epidemic_intensity.norm,fill=general_vero_type,color=general_vero_type)) + 
    geom_boxplot(notch=F, outlier.shape = NA,outlier.size = 4,
                 outlier.fill = "black",alpha=0.45, width = 0.5)+
    stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.format",show.legend = TRUE,
                       size = 6,label.y = c(0.72, 0.82, 0.92)) +
    geom_point(position=position_jitterdodge(jitter.width=1, dodge.width = 0), 
               pch=21, aes(fill=factor(general_vero_type),color=factor(general_vero_type)), show.legend = F,size=1)+
    scale_y_continuous(name="Circulation peakedness",limits = c(0,1) ,breaks = seq(0,1,0.25))+
    labs(title = paste0(virus_name[j]) ,x="") + 
    mytheme+
    theme(axis.text.x = element_text(size = 18, color="black",vjust = 0.5,hjust = 0.5, angle = 0),
          strip.text = element_text(size = 18, face = "bold"),legend.text = element_text(size = 12))+
    theme(axis.title.y.left = element_text(size = 18,color="black",vjust=2,face = "bold", margin = margin(0,-0.2,0,0,'cm')),
          axis.title.x = element_text(size = 18, color="black",face = "bold",margin = margin(0,0,0,0,'cm')))+
    scale_fill_manual(name = '',values = c("Below 10" = custom_colors[1], "10-30" = custom_colors[2], 
                                           "Above 30" = custom_colors[3])) +
    scale_color_manual(name = '',values = c("Below 10" = custom_colors[1], "10-30" = custom_colors[2], 
                                            "Above 30" = custom_colors[3])) +
    theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())+ 
    guides(color = guide_legend(override.aes = list(shape = NA, size = 3)))   
  
  ps2
  
  plot_ei_temp[[j]] <- ps2
  
  
  D2_trop = subset(D2 ,D2$FLUSEASON1=="Tropic")
  comparisons <- combn(unique(D2_trop$general_vero_type), 2, simplify = FALSE)
  D2_trop$general_vero_type <- factor(D2_trop$general_vero_type,
                                      levels = c( "Below 10","10-30","Above 30"),ordered=TRUE)
  ps3 <- ggplot(D2_trop, aes(x=general_vero_type, y=peak_AAP,fill=general_vero_type,color=general_vero_type)) + 
    geom_boxplot(notch=F, outlier.shape = NA,outlier.size = 4,
                 outlier.fill = "black",alpha=0.45, width = 0.5)+
    stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.format",show.legend = TRUE,
                       size = 6,label.y = c(72, 82, 92)) +
    geom_point(position=position_jitterdodge(jitter.width=1, dodge.width = 0), 
               pch=21, aes(fill=factor(general_vero_type),color=factor(general_vero_type)), show.legend = F,size=1)+
    scale_y_continuous(name="Peak intensity", limits = c(0,100) ,breaks = seq(0,100,25))+
    labs(title = paste0(virus_name[j]) ,x="") + 
    mytheme+
    theme(axis.text.x = element_text(size = 18, color="black",vjust = 0.5,hjust = 0.5, angle = 0),
          strip.text = element_text(size = 18, face = "bold"),legend.text = element_text(size = 12))+
    theme(axis.title.y.left = element_text(size = 18,color="black",vjust=2,face = "bold", margin = margin(0,-0.2,0,0,'cm')),
          axis.title.x = element_text(size = 18, color="black",face = "bold",margin = margin(0,0,0,0,'cm')))+
    scale_fill_manual(name = '',values = c("Below 10" = custom_colors[5], "10-30" = custom_colors[6], 
                                           "Above 30" = custom_colors[7])) +
    scale_color_manual(name = '',values = c("Below 10" = custom_colors[5], "10-30" = custom_colors[6], 
                                            "Above 30" = custom_colors[7])) +
    theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())+ 
    guides(color = guide_legend(override.aes = list(shape = NA, size = 3)))   
  
  ps3
  
  plot_peak_trop[[j]] <- ps3
  
  ps4 <- ggplot(D2_trop, aes(x=general_vero_type, y=epidemic_intensity.norm,fill=general_vero_type,color=general_vero_type)) + 
    geom_boxplot(notch=F, outlier.shape = NA,outlier.size = 4,
                 outlier.fill = "black",alpha=0.45, width = 0.5)+
    stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.format",show.legend = TRUE,
                       size = 6,label.y = c(0.72, 0.82, 0.92)) +
    geom_point(position=position_jitterdodge(jitter.width=1, dodge.width = 0), 
               pch=21, aes(fill=factor(general_vero_type),color=factor(general_vero_type)), show.legend = F,size=1)+
    scale_y_continuous(name="Circulation peakedness",limits = c(0,1) ,breaks = seq(0,1,0.25))+
    labs(title = paste0(virus_name[j]) ,x="") + 
    mytheme+
    theme(axis.text.x = element_text(size = 18, color="black",vjust = 0.5,hjust = 0.5, angle = 0),
          strip.text = element_text(size = 18, face = "bold"),legend.text = element_text(size = 12))+
    theme(axis.title.y.left = element_text(size = 18,color="black",vjust=2,face = "bold", margin = margin(0,-0.2,0,0,'cm')),
          axis.title.x = element_text(size = 18, color="black",face = "bold",margin = margin(0,0,0,0,'cm')))+
    scale_fill_manual(name = '',values = c("Below 10" = custom_colors[5], "10-30" = custom_colors[6], 
                                           "Above 30" = custom_colors[7])) +
    scale_color_manual(name = '',values = c("Below 10" = custom_colors[5], "10-30" = custom_colors[6], 
                                            "Above 30" = custom_colors[7])) +
    theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())+ 
    guides(color = guide_legend(override.aes = list(shape = NA, size = 3)))   
  
  ps4
  
  plot_ei_trop[[j]] <- ps4
  
}  

# Combine all plots into a single composite figure.
p1 <- grid.arrange(grobs = plot_peak_temp, ncol = 4, 
                   top = textGrob("Temperate", gp = gpar(fontsize = 20, fontface = "bold")))
p2 <- grid.arrange(grobs = plot_peak_trop, ncol = 4, 
                   top = textGrob("Tropic", gp = gpar(fontsize = 20, fontface = "bold")))
p3 <- grid.arrange(grobs = plot_ei_temp, ncol = 4, 
                   top = textGrob("Temperate", gp = gpar(fontsize = 20, fontface = "bold")))
p4 <- grid.arrange(grobs = plot_ei_trop, ncol = 4, 
                   top = textGrob("Tropic", gp = gpar(fontsize = 20, fontface = "bold")))

# Export plots by climatic region.
ggsave(p1,filename = file.path(paste0(out_path1,"/percovid19_vero_peak_temp.pdf")),
       width = 18,height =10.5,device   = cairo_pdf,  family   = "Arial")
ggsave(p2,filename = file.path(paste0(out_path1,"/percovid19_vero_peak_trop.pdf")),
       width = 18,height =10.5,device   = cairo_pdf,  family   = "Arial")
ggsave(p3,filename = file.path(paste0(out_path1,"/percovid19_vero_ei_temp.pdf")),
       width = 18,height =10.5,device   = cairo_pdf,  family   = "Arial")
ggsave(p4,filename = file.path(paste0(out_path1,"/percovid19_vero_ei_trop.pdf")),
       width = 18,height =10.5,device   = cairo_pdf,  family   = "Arial")

# Export the analysis dataset for generating Supplementary Figures S9 and S10.
write.csv(dat_out_year,paste0(out_path1,"/","flu_dat_out_percovid19.csv"),row.names =FALSE)

# Calculate descriptive statistics by influenza vaccination coverage group, virus type, and climatic region.
peak.sum <- dat_out_year %>%
  group_by(general_vero_type,virus_type,FLUSEASON1) %>%
  summarise(
    sd = sd(peak_AAP, na.rm = TRUE),
    peak_mean = mean(peak_AAP,na.rm = TRUE),
    peak_Q50 = quantile(peak_AAP,0.5,na.rm = TRUE),
    peak_Q75=quantile(peak_AAP,0.75,na.rm = TRUE),
    peak_Q25=quantile(peak_AAP,0.25,na.rm = TRUE),
    ei_mean = mean(epidemic_intensity.norm,na.rm = TRUE),
    ei_Q50=quantile(epidemic_intensity.norm,0.5,na.rm = TRUE),
    ei_Q75=quantile(epidemic_intensity.norm,0.75,na.rm = TRUE),
    ei_Q25=quantile(epidemic_intensity.norm,0.25,na.rm = TRUE)
  )
peak.sum

peak.sum1 <- peak.sum

# Retain the low (<10%) and high (>30%) vaccination coverage groups,
# and subset the data for temperate regions for subsequent analyses and visualization.
peak.sum1 <- peak.sum1[peak.sum1$general_vero_type %in% c("Below 10",  "Above 30"), ]
peak.sum_temp <- peak.sum1[which(peak.sum1$FLUSEASON1=="Temperate"),]

# Standardize virus labels and set factor levels for plotting.
peak.sum_temp$virus_type <- as.character(peak.sum_temp$virus_type)
peak.sum_temp[which(peak.sum_temp$virus_type == "INF_ALL"), "virus_type"] <- "IV"
peak.sum_temp[which(peak.sum_temp$virus_type == "INF_A"), "virus_type"] <- "IAV"
peak.sum_temp[which(peak.sum_temp$virus_type == "INF_B"), "virus_type"] <- "IBV"

peak.sum_temp$virus_type <- factor(peak.sum_temp$virus_type,
                                   levels = c("IV","IAV","IBV",
                                              "RSV","PIV","RV","ADV","HMPV","HCoV"),ordered=TRUE)

peak.sum_temp$general_vero_type <- factor(peak.sum_temp$general_vero_type,
                                   levels = c("Below 10","Above 30"),ordered=TRUE)
# Plot Figure 1D
ps1 <- 
  ggplot(peak.sum_temp, aes(virus_type, peak_Q50)) +
  geom_pointrange(aes(ymin = peak_Q25, ymax =  peak_Q75, color =general_vero_type),
                  position = position_dodge(0.6),size=1.2)+
  scale_y_continuous(name="Peak intensity",limits = c(0,65),  breaks = seq(0,60,20))+
  labs(title = "" ,x="",y="") + 
  mytheme+
  theme(axis.text.x = element_text( color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text( face = "bold"))+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,0.4,0,0,'cm')),
        axis.title.x = element_text( color="black",face = "bold",margin = margin(0.4,0,0,0,'cm')))+
  scale_color_manual(name = '',values = c("#377eb8", "#dc3637","#5c6068"),
                     breaks = c("Below 10", "Above 30", "All") )+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) 
ps1

ps2 <- 
  ggplot(peak.sum_temp, aes(virus_type, ei_Q50)) +
  geom_pointrange(aes(ymin = ei_Q25, ymax =  ei_Q75, color =general_vero_type),
                  position = position_dodge(0.6),size=1.2)+
  scale_y_continuous(name="Circulation peakedness",limits = c(0,1),  breaks = seq(0,1,0.5))+
  labs(title = "" ,x="",y="") + 
  mytheme+
  theme(axis.text.x = element_text( color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text(face = "bold"))+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,0.4,0,0,'cm')),
        axis.title.x = element_text( color="black",face = "bold",margin = margin(0.4,0,0,0,'cm')))+
  scale_color_manual(name = '',values = c("#377eb8", "#dc3637","#5c6068"),
                     breaks = c("Below 10", "Above 30", "All") )+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) 
ps2

fig1 <-ggarrange(ps1, ps2,ncol =1, nrow = 2,heights = c(1,1),
                 labels = c("",""),  hjust= -1, vjust=-1,align = "hv",
                 legend = "bottom", common.legend = T,
                 font.label = list(size = 24, face = "bold"))
fig1 

ggsave(filename = file.path(out_path1, "Fig1D_ei_peak_temp.pdf"),
       plot   = fig1, width = 8.5,height =8, device   = cairo_pdf,  family   = "Arial")


# Subset the data for tropical regions for subsequent analyses and visualization.
peak.sum_trop <- peak.sum1[which(peak.sum1$FLUSEASON1=="Tropic"),]

# Standardize virus labels and set factor levels for plotting.
peak.sum_trop$virus_type <- as.character(peak.sum_trop$virus_type)
peak.sum_trop[which(peak.sum_trop$virus_type == "INF_ALL"), "virus_type"] <- "IV"
peak.sum_trop[which(peak.sum_trop$virus_type == "INF_A"), "virus_type"] <- "IAV"
peak.sum_trop[which(peak.sum_trop$virus_type == "INF_B"), "virus_type"] <- "IBV"

peak.sum_trop$virus_type <- factor(peak.sum_trop$virus_type,
                                   levels = c("IV","IAV","IBV",
                                              "RSV","PIV","RV","ADV","HMPV","HCoV"),ordered=TRUE)

peak.sum_trop$general_vero_type <- factor(peak.sum_trop$general_vero_type,
                                          levels = c("Below 10","Above 30"),ordered=TRUE)

# Plot Supplementary Figure S7
ps1 <- 
  ggplot(peak.sum_trop, aes(virus_type, peak_Q50)) +
  geom_pointrange(aes(ymin = peak_Q25, ymax =  peak_Q75, color =general_vero_type),
                  position = position_dodge(0.6),size=1.2)+
  scale_y_continuous(name="Peak intensity",limits = c(0,75),  breaks = seq(0,75,25))+
  labs(title = "" ,x="",y="") + 
  mytheme+
  theme(axis.text.x = element_text( color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text( face = "bold"))+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,0.4,0,0,'cm')),
        axis.title.x = element_text( color="black",face = "bold",margin = margin(0.4,0,0,0,'cm')))+
  scale_color_manual(name = '',values = c("#f1864f","#694fbb","#5c6068"),
                     breaks = c("Below 10", "Above 30", "All") )+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) 
ps1

ps2 <- 
  ggplot(peak.sum_trop, aes(virus_type, ei_Q50)) +
  geom_pointrange(aes(ymin = ei_Q25, ymax =  ei_Q75, color =general_vero_type),
                  position = position_dodge(0.6),size=1.2)+
  scale_y_continuous(name="Circulation peakedness",limits = c(0,1),  breaks = seq(0,1,0.5))+
  labs(title = "" ,x="",y="") + 
  mytheme+
  theme(axis.text.x = element_text( color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text(face = "bold"))+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,0.4,0,0,'cm')),
        axis.title.x = element_text( color="black",face = "bold",margin = margin(0.4,0,0,0,'cm')))+
  scale_color_manual(name = '',values = c("#f1864f","#694fbb","#5c6068"),
                     breaks = c("Below 10", "Above 30", "All") )+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) 
ps2

figS7 <-ggarrange(ps1, ps2,ncol =1, nrow = 2,heights = c(1,1),
                 labels = c("",""),  hjust= -1, vjust=-1,align = "hv",
                 legend = "bottom", common.legend = T,
                 font.label = list(size = 24, face = "bold"))
figS7 

ggsave(figS7,filename = file.path(paste0(out_path1,"/FigS7_ei_peak_trop_precovid19.pdf")),
       width = 8.5,height =8,device   = cairo_pdf,  family   = "Arial")

