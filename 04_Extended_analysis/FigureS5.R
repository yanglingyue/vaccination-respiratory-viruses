library(geofacet);library(sf);library(readr);library(ggpubr)
library(dplyr);library(ggplot2);library(tidyverse);library(RColorBrewer)
source("../plot_theme.R")
out_path1 <- "FigS5"
dir.create(out_path1)
dat1_avg1<- read.csv("../00_data/seasonal_flu_coverage_average.csv")
colnames(dat1_avg1)[4] <- "general_coverage"
vero <- dat1_avg1[,c("country","general_coverage")]
colnames(vero) <- c("region","freq")
vero_region<-cut(vero$freq,breaks=c(0,10,30,100),include.lowest=T,
                 labels = c("Low coverage region","Medium coverage region","High coverage region"))#
vero<-cbind(vero,vero_region)
dat_vero <- read.csv("../00_data/FLU_coverage_final_2015_2024.csv")
dat_vero <- dat_vero[-which(dat_vero$year==2015),]

dat_vero$country <- factor(dat_vero$country,
                       levels = dat1_avg1$country[order(dat1_avg1$general_coverage, decreasing = F)],ordered=TRUE)

colors <- c("#377eb8",'#7570b3', "#dc3637")
colnames(vero)[1] <- "country"
dat_vero <-merge(dat_vero, vero, by = c("country"), all.x = TRUE)

#Supplementary Figure 5 (A)
dat_vero[which(dat_vero$type=="Pre-COVID-19 pandemic"),"type"] <- "Pre-COVID-19"
dat_vero[which(dat_vero$type=="Since-COVID-19 pandemic"),"type"] <- "Since-COVID-19"

df.summary <- dat_vero %>%
  group_by(type) %>%
  summarise(
    sd = sd(coverage, na.rm = TRUE),
    coverage = mean(coverage, na.rm = TRUE)
  )
df.summary

dat_vero$type <- factor(dat_vero$type,
                         levels = c("Pre-COVID-19","Since-COVID-19"))
comparisons <- combn(as.character(levels(dat_vero$type)), 2, simplify = FALSE)
pc1 <- ggplot(dat_vero, aes(x=type, y=coverage)) + 
  geom_point(position=position_jitterdodge(jitter.width=1, dodge.width = 0), 
             pch=21, aes(fill=factor(vero_region)), show.legend = T,size=4,color="grey35")+
  geom_errorbar(aes(ymin = coverage-sd, ymax = coverage+sd),
                data = df.summary, width = 0.1,color="black",size=1) +
  geom_point(aes(y = coverage),size=7.5,data = df.summary,  color = "black") +  
  stat_compare_means(method = 't.test', label = 'p.format', 
                     size=7.5, label.y = 74, label.x = 1.35,aes(group = type)) +
  #geom_segment(aes(x = 1, xend = 2, y = 73, yend = 73), color = "black", size = 0.8) +
  scale_y_continuous(name="Flu vaccine coverage", limits = c(0,80) ,breaks = seq(0,80,20))+
  labs(title = "",x="") + 
  mytheme+
  theme(axis.text.x = element_text(color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text(face = "bold"),legend.text = element_text(size = 20))+
  #theme(plot.margin=unit(c(3,0.5,2.5,0.5),'lines'))+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,-0.2,0,0,'cm')),
        axis.title.x = element_text(color="black",face = "bold",margin = margin(0,0,0,0,'cm')))+
  scale_fill_manual(name = '',values =colors  ) +
  scale_color_manual(name = '',values =colors  ) +
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())+ 
  guides(fill = guide_legend(nrow = 3)) 
pc1


#Supplementary Figure 5 (B)
dat_vero1 <- dat_vero[dat_vero$vero_region=="Low coverage region"|dat_vero$vero_region=="High coverage region",]
dat_vero1$type1 <- NA
dat_vero1[which(dat_vero1$year >= 2023),"type1"] <- "Post-COVID-19"
dat_vero1[which(dat_vero1$year <= 2019),"type1"] <- "Pre-COVID-19"
dat_vero1[dat_vero1$year >= 2020 & dat_vero1$year <= 2022, "type1"] <- "COVID-19"


dat_vero1$type1 <- factor(dat_vero1$type1,
                         levels = c("Pre-COVID-19","COVID-19","Post-COVID-19"))
dat_vero1$vero_region <- factor(dat_vero1$vero_region,
                          levels = c("Low coverage region","High coverage region"))

colors <- c("#377eb8", "#dc3637")
pc2 <- ggplot(dat_vero1, aes(x=type1, y=coverage,fill= vero_region,color= vero_region)) + 
  geom_boxplot(notch=F, outlier.shape = NA,outlier.size = 4,alpha=0.75,
               outlier.fill = "black", width = 0.45,color="black",
               position = position_dodge(width = 0.5))+
  geom_point(position=position_jitterdodge(jitter.width=0.75, dodge.width = 0.5),  
             pch=21, aes(fill=factor(vero_region)), alpha=0.55,
             show.legend = FALSE, size=3, color="grey25") +
  stat_compare_means(method = "t.test" , label = 'p.format',show.legend = T,size=7.5,
                     label.y = 74)+
  scale_y_continuous(name="Flu vaccine coverage", limits = c(0,80) ,breaks = seq(0,80,20))+
  labs(title = "",x="") + 
  mytheme+
  theme(axis.text.x = element_text(color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text(face = "bold"),legend.text = element_text(size = 20))+
  #theme(plot.margin=unit(c(3,0.5,2.5,0.5),'lines'))+
  theme(axis.title.y.left = element_text(size = 20,color="black",vjust=2,face = "bold", margin = margin(0,-0.2,0,0,'cm')),
        axis.title.x = element_text(size = 20, color="black",face = "bold",margin = margin(0,0,0,0,'cm')))+
  scale_fill_manual(name = '',values =colors  ) +
  scale_color_manual(name = '',values =colors  ) +
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())+ 
  
  guides(fill = guide_legend(nrow = 3)) 

pc2

compare_means(
  coverage ~ vero_region,
  data     = dat_vero1,
  method   = "t.test",
  group.by = "type1",
  method.args = list(alternative = "greater")   # 单尾检验
)


library(ggpubr)
figs5<-ggarrange(pc1,pc2,
                 ncol =2, nrow = 1,widths = c(1,1),
                 labels = c("A","B"),  vjust=1,align = "hv",
                 font.label = list(size = 24, face = "bold"))
figs5
ggsave(figs5,filename = file.path(paste0(out_path1,"/FigS5_general.pdf")),
       width = 12.5,height =7.5,device   = cairo_pdf,  family   = "Arial")

