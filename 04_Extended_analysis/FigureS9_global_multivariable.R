library(geofacet);library(sf);library(readr);library(ggpubr)
library(dplyr);library(ggplot2);library(tidyverse);library(RColorBrewer)

source("../plot_theme.R")
out_path0 <-  "04_Extended_analysis"
out_path1 <- "FigS9"
dir.create(out_path1)

# Prepare data for the pre-COVID-19 period
dat1_other_pre <- read.csv("../00_data/multi_vaccine_indirect_precovid19.csv")
dat1_other_pre <- dat1_other_pre[,c("country","virus",  "cen_lon","cen_lat","EPISEASON" ,"FLUSEASON","flu_year",
                             "temp2m","abshum" ,"pop_density" ,"gdp","coverage" )]
dat1_other_mean_pre <- dat1_other_pre %>%
  group_by(country, virus, cen_lon, cen_lat, EPISEASON, FLUSEASON, flu_year) %>%
  summarise(
    temp2m      = mean(temp2m, na.rm = TRUE),
    abshum      = mean(abshum, na.rm = TRUE),
    pop_density = mean(pop_density, na.rm = TRUE),
    gdp         = mean(gdp, na.rm = TRUE),
    coverage    = mean(coverage, na.rm = TRUE),
    .groups = "drop"
  )

dat_pre_epi <- read.csv("../01_Fig1/output/flu_dat_out_percovid19.csv")
dat_pre_epi <- dat_pre_epi[,c("country","flu_year","virus_type","peak_AAP","epidemic_intensity", "epidemic_intensity.norm")]

dat_pre_epi[which(dat_pre_epi$virus_type=="INF_ALL"),"virus_type"] <- "IV"
dat_pre_epi[which(dat_pre_epi$virus_type=="INF_A"),"virus_type"] <- "IAV"
dat_pre_epi[which(dat_pre_epi$virus_type=="INF_B"),"virus_type"] <- "IBV"
dat_pre_flu <- dat_pre_epi[dat_pre_epi$virus_type %in% c("IV", "IAV", "IBV"), ]
dat_pre_flu <- dat_pre_flu[,c("country","flu_year","virus_type","peak_AAP",
                              "epidemic_intensity","epidemic_intensity.norm")]
dat_pre_flu <- dat_pre_flu %>%pivot_wider(id_cols = c(country, flu_year),names_from = virus_type,
                                          values_from = c(peak_AAP,epidemic_intensity,epidemic_intensity.norm))
virus_name<- c("RSV","PIV","RV","ADV","HMPV")
dat_pre_epi1 <- dat_pre_epi[dat_pre_epi$virus_type %in% virus_name, ]

dat_pre_epi1$virus_type <- factor(dat_pre_epi1$virus_type,levels = virus_name,ordered=TRUE)

dat_pre_epi2 <-  merge(dat_pre_epi1, dat_pre_flu, by = c("country","flu_year"), all.x = TRUE)
colnames(dat_pre_epi2)[3] <- "virus"
dat_pre_epi3 <- merge(dat_pre_epi2, dat1_other_mean_pre , by = c("country","flu_year","virus"), all.x = TRUE)

# Prepare data for the post-COVID-19 period
dat1_other_post <- read.csv("../00_data/multi_vaccine_indirect_postcovid19.csv")
dat1_other_post <- dat1_other_post[,c("country","virus",  "cen_lon","cen_lat","EPISEASON" ,"FLUSEASON","flu_year",
                                    "temp2m","abshum" ,"pop_density" ,"gdp","coverage" )]
dat1_other_mean_post <- dat1_other_post %>%
  group_by(country, virus, cen_lon, cen_lat, EPISEASON, FLUSEASON, flu_year) %>%
  summarise(
    temp2m      = mean(temp2m, na.rm = TRUE),
    abshum      = mean(abshum, na.rm = TRUE),
    pop_density = mean(pop_density, na.rm = TRUE),
    gdp         = mean(gdp, na.rm = TRUE),
    coverage    = mean(coverage, na.rm = TRUE),
    .groups = "drop"
  )

dat_post_epi <- read.csv("../01_Fig1/output/flu_dat_out_postcovid19.csv")
dat_post_epi <- dat_post_epi[,c("country","flu_year","virus_type","peak_AAP","epidemic_intensity", "epidemic_intensity.norm")]

dat_post_epi[which(dat_post_epi$virus_type=="INF_ALL"),"virus_type"] <- "IV"
dat_post_epi[which(dat_post_epi$virus_type=="INF_A"),"virus_type"] <- "IAV"
dat_post_epi[which(dat_post_epi$virus_type=="INF_B"),"virus_type"] <- "IBV"
dat_post_flu <- dat_post_epi[dat_post_epi$virus_type %in% c("IV", "IAV", "IBV"), ]
dat_post_flu <- dat_post_flu[,c("country","flu_year","virus_type","peak_AAP",
                              "epidemic_intensity","epidemic_intensity.norm")]
dat_post_flu <- dat_post_flu %>%pivot_wider(id_cols = c(country, flu_year),names_from = virus_type,
                                          values_from = c(peak_AAP,epidemic_intensity,epidemic_intensity.norm))
virus_name<- c("RSV","PIV","RV","ADV","HMPV")
dat_post_epi1 <- dat_post_epi[dat_post_epi$virus_type %in% virus_name, ]

dat_post_epi1$virus_type <- factor(dat_post_epi1$virus_type,levels = virus_name,ordered=TRUE)

dat_post_epi2 <-  merge(dat_post_epi1, dat_post_flu, by = c("country","flu_year"), all.x = TRUE)
colnames(dat_post_epi2)[3] <- "virus"
dat_post_epi3 <- merge(dat_post_epi2, dat1_other_mean_post , by = c("country","flu_year","virus"), all.x = TRUE)


results_list <- list()

n = length(virus_name)
for(j in 1:n)  
{
print(virus_name[j])
data_virus_pre <- subset(dat_pre_epi3 ,dat_pre_epi3$virus==virus_name[j])

m1_pre_IAV <- lm(epidemic_intensity.norm ~ coverage + temp2m+abshum+log(gdp)+log(pop_density) +cen_lat +epidemic_intensity.norm_IAV, data =  data_virus_pre)
print(summary(m1_pre_IAV))

m1_pre_IBV <- lm(epidemic_intensity.norm ~ coverage + temp2m+abshum+log(gdp)+log(pop_density) +cen_lat +epidemic_intensity.norm_IBV, data =  data_virus_pre)
print(summary(m1_pre_IBV))

m1_pre_IV <- lm(epidemic_intensity.norm ~ coverage + temp2m+abshum+log(gdp)+log(pop_density) +cen_lat +epidemic_intensity.norm_IV, data =  data_virus_pre)
print(summary(m1_pre_IV))


# Pre-COVID-19: Extract model coefficients and 95% confidence intervals (IAV)
coef_df_pre <- as.data.frame(summary(m1_pre_IAV)$coefficients)
ci_df_pre   <- as.data.frame(confint(m1_pre_IAV, level = 0.95))
# Pre-COVID-19: Combine results (IAV)
result_df_pre_IAV <- data.frame(
  virus = virus_name[j],
  term       = rownames(coef_df_pre),
  estimate   = coef_df_pre[, "Estimate"],
  std_error  = coef_df_pre[, "Std. Error"],
  p_value    = coef_df_pre[, "Pr(>|t|)"],
  conf_low   = ci_df_pre[, 1],
  conf_high  = ci_df_pre[, 2],
  row.names  = NULL
)
result_df_pre_IAV$virus_type <- "IAV"

# Pre-COVID-19: Extract model coefficients and 95% confidence intervals (IBV)
coef_df_pre <- as.data.frame(summary(m1_pre_IBV)$coefficients)
ci_df_pre   <- as.data.frame(confint(m1_pre_IBV, level = 0.95))
# Pre-COVID-19: Combine results (IBV)
result_df_pre_IBV <- data.frame(
  virus = virus_name[j],
  term       = rownames(coef_df_pre),
  estimate   = coef_df_pre[, "Estimate"],
  std_error  = coef_df_pre[, "Std. Error"],
  p_value    = coef_df_pre[, "Pr(>|t|)"],
  conf_low   = ci_df_pre[, 1],
  conf_high  = ci_df_pre[, 2],
  row.names  = NULL
)
result_df_pre_IBV$virus_type <- "IBV"

# Pre-COVID-19: Extract model coefficients and 95% confidence intervals (IV)
coef_df_pre <- as.data.frame(summary(m1_pre_IV)$coefficients)
ci_df_pre   <- as.data.frame(confint(m1_pre_IV, level = 0.95))
# Pre-COVID-19: Combine results (IV)
result_df_pre_IV <- data.frame(
  virus = virus_name[j],
  term       = rownames(coef_df_pre),
  estimate   = coef_df_pre[, "Estimate"],
  std_error  = coef_df_pre[, "Std. Error"],
  p_value    = coef_df_pre[, "Pr(>|t|)"],
  conf_low   = ci_df_pre[, 1],
  conf_high  = ci_df_pre[, 2],
  row.names  = NULL
)

result_df_pre_IV$virus_type <- "IV"
result_df_pre <- rbind(result_df_pre_IAV,result_df_pre_IBV,result_df_pre_IV)
result_df_pre$type <- "Pre-COVID-19 pandemic"


data_virus_post <- subset(dat_post_epi3 ,dat_post_epi3$virus==virus_name[j])
m1_post_IAV <- lm(epidemic_intensity.norm ~ coverage + temp2m+abshum+log(gdp)+log(pop_density)+cen_lat +epidemic_intensity.norm_IAV, data =  data_virus_post)
print(summary(m1_post_IAV))

m1_post_IBV <- lm(epidemic_intensity.norm ~ coverage + temp2m+abshum+log(gdp)+log(pop_density)+cen_lat +epidemic_intensity.norm_IBV, data =  data_virus_post)
print(summary(m1_post_IBV))

m1_post_IV <- lm(epidemic_intensity.norm ~ coverage + temp2m+abshum+log(gdp)+log(pop_density)+cen_lat +epidemic_intensity.norm_IV, data =  data_virus_post)
print(summary(m1_post_IV))

# Post-COVID-19: Extract model coefficients and 95% confidence intervals (IAV)
coef_df_post <- as.data.frame(summary(m1_post_IAV)$coefficients)
ci_df_post   <- as.data.frame(confint(m1_post_IAV, level = 0.95))
# Post-COVID-19: Combine results (IAV)
result_df_post_IAV <- data.frame(
  virus = virus_name[j],
  term       = rownames(coef_df_post),
  estimate   = coef_df_post[, "Estimate"],
  std_error  = coef_df_post[, "Std. Error"],
  p_value    = coef_df_post[, "Pr(>|t|)"],
  conf_low   = ci_df_post[, 1],
  conf_high  = ci_df_post[, 2],
  row.names  = NULL
)
result_df_post_IAV$virus_type <- "IAV"

# Post-COVID-19: Extract model coefficients and 95% confidence intervals (IBV)
coef_df_post <- as.data.frame(summary(m1_post_IBV)$coefficients)
ci_df_post   <- as.data.frame(confint(m1_post_IBV, level = 0.95))
# Post-COVID-19: Combine results (IBV)
result_df_post_IBV <- data.frame(
  virus = virus_name[j],
  term       = rownames(coef_df_post),
  estimate   = coef_df_post[, "Estimate"],
  std_error  = coef_df_post[, "Std. Error"],
  p_value    = coef_df_post[, "Pr(>|t|)"],
  conf_low   = ci_df_post[, 1],
  conf_high  = ci_df_post[, 2],
  row.names  = NULL
)
result_df_post_IBV$virus_type <- "IBV"

# Post-COVID-19: Extract model coefficients and 95% confidence intervals (IV)
coef_df_post <- as.data.frame(summary(m1_post_IV)$coefficients)
ci_df_post   <- as.data.frame(confint(m1_post_IV, level = 0.95))
# Post-COVID-19: Combine results (IV)
result_df_post_IV <- data.frame(
  virus = virus_name[j],
  term       = rownames(coef_df_post),
  estimate   = coef_df_post[, "Estimate"],
  std_error  = coef_df_post[, "Std. Error"],
  p_value    = coef_df_post[, "Pr(>|t|)"],
  conf_low   = ci_df_post[, 1],
  conf_high  = ci_df_post[, 2],
  row.names  = NULL
)
result_df_post_IV$virus_type <- "IV"
result_df_post <- rbind(result_df_post_IAV,result_df_post_IBV,result_df_post_IV)

result_df_post$type <- "Post-COVID-19 pandemic"

results_df <- rbind(result_df_pre,result_df_post)
results_list[[j]] <- results_df

}
results_out <- do.call(rbind, results_list)

# Plot Supplementary Figure S9
results_vero <- results_out[results_out$term %in% c("coverage"),]
results_vero$virus <- factor(results_vero$virus,levels = c("RSV","PIV","RV","ADV","HMPV","HCoV"),ordered=TRUE)
results_vero$virus_type <- factor(results_vero$virus_type,levels = c("IAV","IV","IBV"),ordered=TRUE)

results_vero_pre <- results_vero[results_vero$type %in% c("Pre-COVID-19 pandemic"),]

ps1_pre <- 
  ggplot(results_vero_pre, aes(virus, estimate)) +
  geom_pointrange(aes(ymin = conf_low, ymax = conf_high,colour = virus_type),
                  position = position_dodge(0.6),size=1.2)+
  facet_wrap(~ type, scales = "free_y",ncol = 1)+ 
  scale_y_continuous(limits = c(-0.008,0.002),  breaks = seq(-0.008,0.002,0.002))+
  labs(title = "" ,x="",y="Effect on viral circulation peakedness") + 
  mytheme+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.text.x = element_text( color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text( face = "bold", hjust = 0,size=20))+
  scale_color_manual(name = '',values = c("#5c4591","#4393C3","#5f9723") )+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,0,0,0,'cm')),
        axis.title.x = element_text( color="black",face = "bold",margin = margin(0.4,0,0,0,'cm')))+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) 
ps1_pre

results_vero_post <- results_vero[results_vero$type %in% c("Post-COVID-19 pandemic"),]

ps1_post <- 
  ggplot(results_vero_post, aes(virus, estimate)) +
  geom_pointrange(aes(ymin = conf_low, ymax = conf_high,colour = virus_type),
                  position = position_dodge(0.6),size=1.2)+
  facet_wrap(~ type, scales = "free_y",ncol = 1)+ 
  scale_y_continuous(limits = c(-0.02,0.02),  breaks = seq(-0.02,0.02,0.01))+
  labs(title = "" ,x="",y="Effect on viral circulation peakedness") + 
  mytheme+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.text.x = element_text( color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text( face = "bold", hjust = 0,size=20))+
  scale_color_manual(name = '',values = c("#5c4591","#4393C3","#5f9723") )+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,0,0,0,'cm')),
        axis.title.x = element_text( color="black",face = "bold",margin = margin(0.4,0,0,0,'cm')))+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) 
ps1_post

results_cp <- results_out[results_out$term %in% c("epidemic_intensity.norm_IAV","epidemic_intensity.norm_IV","epidemic_intensity.norm_IBV"),]
results_cp$virus <- factor(results_cp$virus,levels = c("RSV","PIV","RV","ADV","HMPV","HCoV"),ordered=TRUE)
results_cp$virus_type <- factor(results_cp$virus_type,levels = c("IAV","IV","IBV"),ordered=TRUE)

results_cp_pre <- results_cp[results_cp$type %in% c("Pre-COVID-19 pandemic"),]

ps2_pre <- 
  ggplot(results_cp_pre, aes(virus, estimate)) +
  geom_pointrange(aes(ymin = conf_low, ymax = conf_high,colour = virus_type),
                  position = position_dodge(0.6),size=1.2)+
  facet_wrap(~ type, scales = "free_y",nrow = 1)+ 
  scale_y_continuous(limits = c(-0.5,1),breaks = seq(-0.5,1,0.5))+
  labs(title = "" ,x="",y="Effect on viral circulation peakedness") + 
  mytheme+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.text.x = element_text( color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text( face = "bold", hjust = 0,size=20))+
  scale_color_manual(name = '',values = c("#5c4591","#4393C3","#5f9723") )+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,0,0,0,'cm')),
        axis.title.x = element_text( color="black",face = "bold",margin = margin(0.4,0,0,0,'cm')))+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) 
ps2_pre

results_cp_post <- results_cp[results_cp$type %in% c("Post-COVID-19 pandemic"),]

ps2_post <- 
  ggplot(results_cp_post, aes(virus, estimate)) +
  geom_pointrange(aes(ymin = conf_low, ymax = conf_high,colour = virus_type),
                  position = position_dodge(0.6),size=1.2)+
  facet_wrap(~ type, scales = "free_y",nrow = 1)+ 
  scale_y_continuous(limits = c(-2.3,3),breaks = seq(-2,3,1))+
  labs(title = "" ,x="",y="Effect on viral circulation peakedness") + 
  mytheme+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  theme(axis.text.x = element_text( color="black",vjust = 0.5,hjust = 0.5, angle = 0),
        strip.text = element_text( face = "bold", hjust = 0,size=20))+
  #theme(plot.margin=unit(c(3,0.5,2.5,0.5),'lines'))+
  scale_color_manual(name = '',values = c("#5c4591","#4393C3","#5f9723") )+
  theme(axis.title.y.left = element_text(color="black",vjust=2,face = "bold", margin = margin(0,0,0,0,'cm')),
        axis.title.x = element_text( color="black",face = "bold",margin = margin(0.4,0,0,0,'cm')))+
  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) 
ps2_post

figS9 <-ggarrange(ps1_pre,ps2_pre,ps1_post,ps2_post, ncol =2, nrow = 2,heights = c(1,1),
                 labels = c("A","","B",""),  hjust= -1, vjust=-1,align = "hv",
                 legend = "bottom", common.legend = T,
                 font.label = list(size = 24, face = "bold"))
figS9 

ggsave(figS9,filename = file.path(paste0(out_path1,"/FigS9_global_multivariable.pdf")),
       width = 15,height =10,device   = cairo_pdf,  family   = "Arial")

