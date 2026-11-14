setwd("/Users/yly/Desktop/vaccination-respiratory-viruses/03_Fig5")

file_path <- "01.output_post_pandemic_IAV"
dir.create(file_path)

virus_vars <- c("ADV", "HMPV", "PIV", "RSV","RV")
out.best.fit <- data.frame()
for (j in 1:length(virus_vars))
{
print(virus_vars[j])  

# load packages and pre-processed data
source("00_post_load_packages_data.R")
# Load saved model selection results
table0 <- fread(paste0(model_out,"/",virus_vars[j],"_model_selection.csv"))
table0 <- table0[!grepl("model\\.best_", table0$name), ]

# View table
table0

# define position of best fitting model
#best.fit <- which.min(table0$DIC)
best.fit <- which.min(table0$waic)

res.best.fit <- table0[best.fit]
res.best.fit$virus <- virus_vars[j]
res.best.fit$num_best_fit <- best.fit
out.best.fit <- rbind(out.best.fit,res.best.fit)
fwrite(out.best.fit , file = paste0(file_path,"/best_model_selection_post_covid.csv"), quote = FALSE, 
       row.names = FALSE) 

# load model output
# Select the best-fitting model without interactions
load(paste0(model_out,"/",table0$name[best.fit],".RData"))
model0 <- model
# load vaccine interaction models
load(paste0(model_out,"/","model.best_high.RData"))
model1.1 <- model
load(paste0(model_out,"/","model.best_med.RData"))
model1.2 <- model
load(paste0(model_out,"/","model.best_low.RData"))
model1.3 <- model
load(paste0(model_out,"/","model.best_lin.RData"))
model1.4 <- model
# Load the best-fitting model without interactions
model <- model0

fig_out <- paste0(file_path,"/",j,".",virus_vars[j],"_figs")
dir.create(fig_out, showWarnings = FALSE, recursive = TRUE)


mod.name <- c("model0", "model1.1", "model1.2", "model1.3","model1.4")
lab <- c("mean", "high", "med", "low","total") 


for (i in 1:length(mod.name))
{
  ###FLU
  model <- eval(parse(text = as.name(mod.name[i]))) 
  
  # extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # Identify coefficients associated with the IAV cross-basis
  indt <- grep("basis_flua",model$names.fixed)
  
  # Extract predictions from the IAV DLNM centred on zero (no IAV activity)
  predt <- crosspred(basis_flua, coef = coef[indt], vcov=vcov[indt,indt],
                     model.link = "log", by=0.1, bylag = 0.25, cen = 0,cumul=TRUE) 
  
  ############################################################
  # Extract lag-specific estimates for IAV activity
  flu_matlow <- predt$matRRlow
  flu_matlow <- as.data.frame(flu_matlow)
  flu_matlow$fit_type <- "matRRlow"
  flu_matlow$var <- rownames(flu_matlow)
  
  flu_matfit <- predt$matRRfit
  flu_matfit <- as.data.frame(flu_matfit)
  flu_matfit$fit_type <- "matRRfit"
  flu_matfit$var <- rownames(flu_matfit)
  
  flu_mathigh <- predt$matRRhigh
  flu_mathigh <- as.data.frame(flu_mathigh)
  flu_mathigh$fit_type <- "matRRhigh"
  flu_mathigh$var <- rownames(flu_mathigh)
  
  res_flu_mat <- rbind(flu_matlow,flu_matfit,flu_mathigh)
  res_flu_mat <- res_flu_mat %>%
    pivot_longer(
      cols = c("lag0":"lag6"), 
      names_to = "Lag",        
      values_to = "value"          
    )
  
  res_flu_mat <- res_flu_mat %>%
    pivot_wider(
      names_from = fit_type, 
      values_from = value  
    )
  res_flu_mat$virus <- virus_vars[j]
  res_flu_mat$var_type <- "IAV"
  res_flu_mat$model_type <- lab[i]
  res_flu_mat$period_type <- "Post-COVID19"
  write.csv(res_flu_mat,file = paste0(fig_out,"/",virus_vars[j],"_IAV_matRRfit_",lab[i],".csv"),row.names = F)
  
}

}