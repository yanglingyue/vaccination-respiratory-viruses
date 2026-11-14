needed_packages = c("lubridate","rEDM","tidyr","ggpubr","reshape2",
                    "ggplot2","gplots","RColorBrewer","readxl","lubridate","cowplot",
                    "gtable","reshape","scales","showtext","dplyr",
                    "Kendall","data.table","gridExtra","cowplot","rmarkdown",
                    "foreach", "doParallel","future","future.apply","purrr",
                    "plotly","webshot","htmlwidgets","grid")

lapply(needed_packages, require, character.only = TRUE)

new.packages <- needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sessionInfo()