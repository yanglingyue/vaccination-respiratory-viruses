# load INLA
library(INLA)

# select other packages
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "dlnm", "tsModel", "hydroGOF","RColorBrewer", 
              "geofacet", "ggpubr", "ggthemes","splines")

# install.packages
# lapply(packages, install.packages, character.only = TRUE)
# load packages
lapply(packages, library, character.only = TRUE)

data0<-read.csv('01_data/multi_vaccine_indirect_precovid19.csv')
data0[which(data0$country=="United Kingdom, England"),"country"] <- "England"
data0[which(data0$country=="United Kingdom, Scotland"),"country"] <- "Scotland"
data0[which(data0$country=="United Kingdom, Wales"),"country"] <- "Wales"
data0[which(data0$country=="United Kingdom, Northern Ireland"),"country"] <- "Northern Ireland"
data0 <-  data0 %>%
  filter((year >= 2016 & year <= 2019))   

data0 <- data0[,c("country", "year", "month", "year_decimal","ISOYM","virus",
                  "cases", "INF_A", "INF_B","INF_ALL",  "AAP","flu_AAP",
                  "fluA_AAP","fluB_AAP","strategy2_coverage","strategy2_general_coverage",
                  "temp2m","abshum", 
                  "pop_density", "cases_year","gdp")]
colnames(data0)[7] <- "cases_cases"
colnames(data0)[16] <- "Vaccine"

print(virus_vars[j])  
model_out <- paste0(file_path,"/",j,".",virus_vars[j],"_model")
dir.create(model_out, showWarnings = FALSE, recursive = TRUE)
#########################################################################################################################################
# load shapefile for research
shp_out <- paste0("01_data/shp_precovid/",virus_vars[j])
map <- read_sf(paste0(shp_out,"/",virus_vars[j],".shp"))

data <- data0[data0$virus==virus_vars[j],]  
data<- data[order(data$country,data$year_decimal,decreasing=F),]
data$country <- factor(data$country, levels = map$ADMIN)
data$country_index <- as.integer(data$country)

# Define the mapping between countries and WHO influenza transmission zones
zone_mapping <- data.frame(
  country = c(
    "Argentina", "Chile", "Paraguay", "Uruguay",                       # Temperate South America
    "Australia",  "New Zealand",                                       # Oceania
    "South Africa",                                                    # Southern Africa
    "Canada", "United States",                                         # Northern America
    "England", "Scotland", "Wales", "Northern Ireland", "Denmark",   "Finland", "Estonia", "Ireland","Sweden", # Northern Europe
    "France", "Germany", "Netherlands",  "Slovenia","Spain" ,           # South West Europe
    "Czechia", "Hungary", "Russia",                                    # Eastern Europe
    "Japan", "South Korea", "Mongolia", "Hong Kong",                   # Eastern Asia
    "Mexico","Costa Rica", "Dominican Republic",  "El Salvador",  "Guatemala",   "Honduras",   "Nicaragua", "Panama","Bolivia", "Cuba", # Central America Caribbean
    "Colombia", "Ecuador",  "Peru",                                    # Tropical South America
    "Qatar" ,"Israel"                                                          # Western Asia
  ),
  transmission_zone = c(
    rep("Temperate_South_America", 4),
    rep("Oceania",2),
    "Southern_Africa",
    rep("Northern_America", 2),
    rep("Northern_Europe", 9), 
    rep("Western_Europe", 5),
    rep("Eastern_Europe", 3),
    rep("Eastern_Asia", 4),
    rep("Central_America_Caribbean", 10),     
    rep("Tropical_Americas", 3),     
    rep("Western_Asia",2)
  )
)

# Merge into the main data frame
data <- data %>%
  left_join(zone_mapping, by = "country") %>%
  mutate(
    zone_index = as.integer(factor(transmission_zone))
  )


g.file <- paste0(shp_out,"/map.graph")
if (!file.exists(g.file)) {
  nb.map <- poly2nb(as_Spatial(map$geometry))
  nb2INLA(g.file, nb.map)
} else {
  message("Graph file already exists, skip poly2nb and nb2INLA.")
}

#########################################################################################################################################

# Create lagged variables
# define matrices of lagged terms for monthly influenza vaccine coverage, influenza activity and climate variables
# set maximum lag
nlag = 6

# Creating lagged variables
# Influenza vaccination coverage
lag_vero <- tsModel::Lag(data$Vaccine, group = data$country_index, k = 0:nlag)
#2m mean temperature
lag_temp <- tsModel::Lag(data$temp2m, group = data$country_index, k = 0:nlag)
#Absolute humidity
lag_ahum <- tsModel::Lag(data$abshum, group = data$country_index, k = 0:nlag)
#Influenza A
lag_flua <- tsModel::Lag(data$fluA_AAP, group = data$country_index, k = 0:nlag)
#Influenza B
lag_flub <- tsModel::Lag(data$fluB_AAP, group = data$country_index, k = 0:nlag)
#Influenza all (A+B)
lag_flut <- tsModel::Lag(data$flu_AAP, group = data$country_index, k = 0:nlag)


# define cross-basis matrix (combining nonlinear exposure and lag functions)
# set lag knots
lagknot = equalknots(0:nlag, 2)


#Flu vaccine
var <- lag_vero
basis_vero <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$Vaccine, 1)),
                       arglag = list(fun = "ns", knots = lagknot))
summary(basis_vero)

#Influenza A
var <- lag_flua
basis_flua <- crossbasis(var,
                         argvar = list(fun = "ns", knots = equalknots(data$fluA_AAP, 1)),
                         arglag = list(fun = "ns", knots = lagknot))
summary(basis_flua)

#Influenza B
var <- lag_flub
basis_flub <- crossbasis(var,
                         argvar = list(fun = "ns", knots = equalknots(data$fluB_AAP, 1)),
                         arglag = list(fun = "ns", knots = lagknot))
summary(basis_flub)


#Influenza all (A+B)
var <- lag_flut
basis_flut <- crossbasis(var,
                         argvar = list(fun = "ns",  knots = equalknots(data$flu_AAP, 1)),
                         arglag = list(fun = "ns", knots = lagknot))
summary(basis_flut)


#2m mean temperature
var <- lag_temp
basis_temp <- crossbasis(var,
                       argvar = list(fun = "ns",  knots = equalknots(data$temp2m, 1)),
                       arglag = list(fun = "ns", knots = lagknot))
summary(basis_temp)


#Absolute humidity
var <- lag_ahum
basis_ahum <- crossbasis(var,
                       argvar = list(fun = "ns",  knots = equalknots(data$abshum, 1)),
                       arglag = list(fun = "ns", knots = lagknot))
summary(basis_ahum)

summary(data$Vaccine)
# Centre influenza vaccination coverage at the 90th, 50th, and 10th percentiles.
# Prepare interaction terms between vaccination coverage and influenza activity.
# set indicator to zero at point of interest (centring point)
# re-parameterise model to extract different predictions
vero_ind1 <- data$Vaccine - quantile(data$Vaccine, p = 0.90) # High vaccination coverage
vero_ind2 <- data$Vaccine - quantile(data$Vaccine, p = 0.50) # Medium vaccination coverage
vero_ind3 <- data$Vaccine - quantile(data$Vaccine, p = 0.10) # Low vaccination coverage


# Multiply each cross-basis variable by the linear terms (see Gasparrini et al. EHP 2015)
# note: exploit the column by column product 
# Multiply influenza cross-basis matrices by centred vaccination coverage
vero_basis1_flua <- basis_flua*vero_ind1
vero_basis2_flua <- basis_flua*vero_ind2
vero_basis3_flua <- basis_flua*vero_ind3

vero_basis1_flub <- basis_flub*vero_ind1
vero_basis2_flub <- basis_flub*vero_ind2
vero_basis3_flub <- basis_flub*vero_ind3

vero_basis1_flut <- basis_flut*vero_ind1
vero_basis2_flut <- basis_flut*vero_ind2
vero_basis3_flut <- basis_flut*vero_ind3

# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
colnames(basis_vero) = paste0("basis_vero.", colnames(basis_vero))
colnames(basis_flua) = paste0("basis_flua.", colnames(basis_flua))
colnames(basis_flub) = paste0("basis_flub.", colnames(basis_flub))
colnames(basis_flut) = paste0("basis_flut.", colnames(basis_flut))
colnames(basis_temp) = paste0("basis_temp.", colnames(basis_temp))
colnames(basis_ahum) = paste0("basis_ahum.", colnames(basis_ahum))


colnames(vero_basis1_flua) = paste0("vero_basis1_flua.", colnames(vero_basis1_flua))
colnames(vero_basis2_flua) = paste0("vero_basis2_flua.", colnames(vero_basis2_flua))
colnames(vero_basis3_flua) = paste0("vero_basis3_flua.", colnames(vero_basis3_flua))

colnames(vero_basis1_flub) = paste0("vero_basis1_flub.", colnames(vero_basis1_flub))
colnames(vero_basis2_flub) = paste0("vero_basis2_flub.", colnames(vero_basis2_flub))
colnames(vero_basis3_flub) = paste0("vero_basis3_flub.", colnames(vero_basis3_flub))

colnames(vero_basis1_flut) = paste0("vero_basis1_flut.", colnames(vero_basis1_flut))
colnames(vero_basis2_flut) = paste0("vero_basis2_flut.", colnames(vero_basis2_flut))
colnames(vero_basis3_flut) = paste0("vero_basis3_flut.", colnames(vero_basis3_flut))


#########################################################################################################################################
data$year_index <- data$year-2015
data$month <- as.integer(data$month) 
data$cases_cases <- as.integer(round(data$cases_cases,0)) 
data$country_index <- as.integer(data$country_index) 
# Epidemic seasons with zero total detections
zero_season <- data$cases_year == 0
data$cases_cases[zero_season] <- NA

# set up data and priors for INLA model
# set data for models
#Y  <- data$cases_cases # response variable
#Y[zero_season] <- NA # RP is undefined for these seasons
Y <- data$cases_cases
E  <- c(data$cases_year)/10^2 # Offset based on cumulative positive detections within each epidemic season,
# so that the modeled monthly detection rate corresponds to RP (%).
E[zero_season] <- 0.001 
# random variable
T1 <- data$month # for random effect to account for annual cycle (seasonality)
T2 <- data$year_index # for random effect to account for inter-annual variability
S1 <- data$country_index  # Country-level random effect
S2  <- data$zone_index    #Influenza transmission zon
Fa <- data$fluA_AAP # Flu A
Vc <- data$Vaccine #Flu vaccine coverage
G <- data$gdp
P <- data$pop_density

# create dataframe for model testing
df <- data.frame(Y, E, T1, T2, S1,S2,Fa, Vc,G, P)

# define priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# inla model function
# include formula and set defaults for data, family (to allow other prob dist models e.g. Poisson) and config (to allow for sampling)
mymodel <- function(formula, data = df, family = "nbinomial", config = FALSE)
#mymodel <- function(formula, data = df, family = "poisson", config = FALSE)
    
{
  set.seed(1234)
  model <- inla(formula = formula, data = data, family = family, offset = log(E),
                control.inla = list(strategy = 'adaptive'), 
                control.compute = list(dic = TRUE, config = config, waic = TRUE,
                                       cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = FALSE)
  #model <- inla.rerun(model)
  return(model)
}