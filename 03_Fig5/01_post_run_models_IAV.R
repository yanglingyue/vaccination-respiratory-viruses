#########################################################################################################################################
# R script for INLA-DLNM model fitting and selection
# install INLA
#install.packages("Rgraphviz")
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
setwd("/Users/yly/Desktop/vaccination-respiratory-viruses/03_Fig5")
file_path <- "01.output_post_pandemic_IAV"
dir.create(file_path)

virus_vars <- c("ADV", "HMPV", "PIV", "RSV","RV")
for (j in 1:length(virus_vars))
{
print(virus_vars[j])  
  
# load packages and pre-processed data
source("00_post_load_packages_data.R")

#########################################################################################################################################
# run models of increasing complexity in INLA
# fit a baseline model including spatiotemporal random effects
#########################################################################################################################################
# Formulate a base model including:
# Influenza transmission zone-specific monthly random effects to account for variation in seasonality between zones (cyclic first-order random walk prior).
# Year-specific spatial random effects to account for interannual variation in spatial overdispersion and dependence structures (modified Besag-York-Mollié prior, BYM2).
# Baseline model  
set.seed(1234)
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = paste0(shp_out,"/map.graph"),
    scale.model = TRUE, hyper = precision.prior) 

###############################################################################################
formula_1.1 <- update.formula(baseformula, ~. + basis_flua)

formula_2.1 <- update.formula(baseformula, ~. + basis_flua + basis_temp)
formula_2.2 <- update.formula(baseformula, ~. + basis_flua + basis_ahum)
formula_2.3 <- update.formula(baseformula, ~. + basis_flua + log(G))
formula_2.4 <- update.formula(baseformula, ~. + basis_flua + log(P))

formula_3.1 <- update.formula(baseformula, ~. + basis_flua + basis_temp + basis_ahum)
formula_3.2 <- update.formula(baseformula, ~. + basis_flua + basis_temp + log(G))
formula_3.3 <- update.formula(baseformula, ~. + basis_flua + basis_temp + log(P))
formula_3.4 <- update.formula(baseformula, ~. + basis_flua + basis_ahum + log(G))
formula_3.5 <- update.formula(baseformula, ~. + basis_flua + basis_ahum + log(P))
formula_3.6 <- update.formula(baseformula, ~. + basis_flua + log(G) + log(P))

formula_4.1 <- update.formula(baseformula, ~. + basis_flua + basis_temp + basis_ahum + log(G))
formula_4.2 <- update.formula(baseformula, ~. + basis_flua + basis_temp + basis_ahum + log(P))
formula_4.3 <- update.formula(baseformula, ~. + basis_flua + basis_temp + log(G) + log(P))
formula_4.4 <- update.formula(baseformula, ~. + basis_flua + basis_ahum + log(G) + log(P))

formula_5.1 <- update.formula(baseformula, ~. + basis_flua +  basis_temp + basis_ahum + log(G)+log(P))

# create a list of formulas
formulas <- list(baseformula,  formula_1.1, 
                 formula_2.1, formula_2.2, formula_2.3, formula_2.4,
                 formula_3.1, formula_3.2, formula_3.3, formula_3.4,formula_3.5, formula_3.6,
                 formula_4.1, formula_4.2, formula_4.3, formula_4.4,
                 formula_5.1)

# create model label string
lab <- c("basemodel", "model_1.1",
         "model_2.1", "model_2.2", "model_2.3", "model_2.4",
         "model_3.1", "model_3.2", "model_3.3", "model_3.4", "model_3.5", "model_3.6",
         "model_4.1", "model_4.2", "model_4.3", "model_4.4",
         "model_5.1")

# create a function to run a model for each formula in the list and save the model output to file
models <- lapply(1:length(formulas), 
                 function(i) {
                   set.seed(1234+i)
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0(model_out,"/", lab[i],".RData"))})

# create table to store WAIC and select best model 
table0 <- data.table(Model  = c("base",  "flua",
                                "flua+temp","flua+ahum","flua_GDP", "flua_POP",
                                "flua+temp+ahum",  "flua+temp_GDP", "flua+temp_POP","flua+ahum_GDP", "flua+ahum_POP", "flua_GDP_POP",
                                "flua+temp+ahum_GDP",   "flua+temp+ahum_POP",    "flua+temp_GDP_POP"  ,"flua+ahum_GDP_POP",
                                "flua+temp+ahum_POP_GDP"), 
                     DIC = NA,
                     logscore = NA,
                     waic = NA,
                     p_eff = NA)

for(i in 1:length(formulas))
{
  load(paste0(model_out,"/",lab[i],".RData"))
  table0$name[i] <-lab[i] 
  table0$DIC[i] <- round(model$dic$dic, 2)
  table0$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table0$waic[i] <- round(model$waic$waic, 2)
  table0$p_eff[i] <- round(model$waic$p.eff, 2) # 有效参数量
}

# View table
table0

# define position of best fitting model
#best.fit <- which.min(table0$DIC)
best.fit <- which.min(table0$waic)

# Add interactions between influenza vaccination coverage and IAV activity
formula_vero_hig <- update.formula(formulas[[best.fit]],~. +vero_basis1_flua +Vc)
formula_vero_med <- update.formula(formulas[[best.fit]],~. +vero_basis2_flua +Vc)
formula_vero_low <- update.formula(formulas[[best.fit]],~. +vero_basis3_flua +Vc)
formula_vero_lin <- update.formula(formulas[[best.fit]],~. +basis_vero+Fa:Vc)

# create a list of formulas
formulas <- list(formula_vero_hig,formula_vero_med,formula_vero_low,formula_vero_lin)
# create model label string
lab1 <- c("model.best_high", "model.best_med", "model.best_low", "model.best_lin")

# Fit each model and save the results.
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   set.seed(1234+i)
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0(model_out,"/", lab1[i],".RData"))})

# Create a table to store model selection criteria.
table1 <- data.frame(Model  = c(paste0(table0$Model[best.fit],"+vero_hig"),
                                paste0(table0$Model[best.fit],"+vero_med"),
                                paste0(table0$Model[best.fit],"+vero_low"),
                                paste0(table0$Model[best.fit],"+vero_lin")), 
                     DIC = NA,
                     waic = NA,
                     p_eff = NA)


for(i in 1:length(formulas))
{
  load(paste0(model_out,"/",lab1[i],".RData"))
  table1$name[i] <-lab1[i] 
  table1$DIC[i] <- round(model$dic$dic, 2)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table1$waic[i] <- round(model$waic$waic, 2)
  table1$p_eff[i] <- round(model$waic$p.eff, 2) # 有效参数量
}

#best.fit1 <- which.min(table1$waic)
#best.fit1 <- which.min(table1$DIC)
table2 <- rbind(table0,table1)
# Write results of model selection
fwrite(table2, file = paste0(model_out,"/",virus_vars[j],"_model_selection.csv"), quote = FALSE, 
       row.names = FALSE) 
}
