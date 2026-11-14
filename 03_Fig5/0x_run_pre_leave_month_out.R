# !/usr/bin/env Rscript
setwd("/Users/yly/Desktop/vaccination-respiratory-viruses/03_Fig5")

file_path <- "01.output_pre_pandemic_IAV"
dir.create(file_path)

virus_vars <- c("ADV", "HMPV", "PIV", "RSV","RV")
for (j in 1:length(virus_vars))
{
print(virus_vars[j])  
pred_out <- paste0(file_path,"/",j,".",virus_vars[j],"_preds")
dir.create(pred_out, showWarnings = FALSE, recursive = TRUE)

################################################################################################################################  
# load packages and pre-processed data
source("00_pre_load_packages_data.R")
#inla.setOption(num.threads = "48:1")

#########################################################################################################################################
# Step 1: rerun the selected model (fitted with config = TRUE for sampling) 
# fit full final model to use with control.mode = list(result = model1, restart = TRUE)

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

# Load saved model selection results
table0 <- fread(paste0(model_out,"/",virus_vars[j],"_model_selection.csv"))
table0 <- table0[!grepl("model\\.best_", table0$name), ]

# View table
table0

# define position of best fitting model
best.fit <- which.min(table0$waic)
formula <- formulas[[best.fit]]

cfg_file <- paste0(pred_out, "/model1_config.RData")
if (!file.exists(cfg_file)) {
  model1 <- inla(formula, data = df, family = "nbinomial", offset = log(E), 
                 control.inla = list(strategy = 'adaptive'), 
                 control.compute = list(dic = TRUE, config = TRUE, 
                                        cpo = TRUE, return.marginals = FALSE),
                 control.fixed = list(correlation.matrix = TRUE, 
                                      prec.intercept = 1, prec = 1),
                 control.predictor = list(link = 1, compute = TRUE), 
                 verbose = FALSE)
  model1 <- inla.rerun(model1)
  save(model1, file = cfg_file)
} else {
  load(cfg_file)
  model1$misc$configs <- NULL 
  gc()
}

for(yyear in 1:4){
  for(mmonth in 1:12){
    
# Step 2: Produce cross-validated posterior predictive samples by leaving out one month of a given year at a time
# define number of samples
s <- 1000

# Replace respiratory virus observations in the testing period with NA for out-of-sample prediction
casestopred <- data$cases_cases # response variable
idx.pred <- which(data$year_index == yyear & data$month == mmonth)
casestopred[idx.pred] <- NA # replace cases in year and month of interest to NA
mpred <- length(idx.pred)

# Set response variable for out-of-sample prediction
df$Y <- casestopred

model <- inla(formula, data = df, family = "nbinomial", offset = log(E), 
              control.inla = list(strategy = 'adaptive'), 
              control.compute = list(dic = TRUE, config = TRUE, 
                                     cpo = TRUE, return.marginals = FALSE),
              control.fixed = list(correlation.matrix = TRUE, 
                                   prec.intercept = 1, prec = 1),
              control.predictor = list(link = 1, compute = TRUE), 
              control.mode = list(result = model1, restart = TRUE),
              verbose = FALSE)
model <- inla.rerun(model)

xx <- inla.posterior.sample(s, model)
xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx)
y.pred <- matrix(NA, mpred, s)
for(s.idx in 1:s) {
  xx.sample <- xx.s[, s.idx]
  y.pred[, s.idx] <- rnbinom(mpred, mu = exp(xx.sample[-1]), size = xx.sample[1])
}
preds <- list(year = 2015 + yyear, month = mmonth, idx.pred = idx.pred, 
              mean = apply(y.pred, 1, mean), median = apply(y.pred, 1, median),
              lci = apply(y.pred, 1, quantile, probs = c(0.025)),
              uci = apply(y.pred, 1, quantile, probs = c(0.975)))

save(preds, file = paste0(pred_out,"/preds_all_",2015 + yyear, "_", mmonth, ".RData"))
}
}
}