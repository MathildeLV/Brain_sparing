.libPaths(c("H:/Documents/R/win-library/4.1","C:/Program Files/R/R-4.3.2/library"))

#packages
library(conflicted)
library(lubridate)
library(openxlsx)
library(here)
library(dplyr)
library(readxl)
library(tidyverse)
library(rmarkdown)
library(Hmisc)
library(ggm)
library(gt)
library(gtsummary) # for gt summary tables of models (gtsummary::tbl_regression(m))
library(mgcv)
library(ggplot2)
library(ftExtra) # to add parameters for gt tables
library(effects)
library(ggeffects)
library(emmeans)
library(scales)
library(mice)
library(ggmice)
library(splines)
library(gridExtra)
library(janitor)
library(tinytex)
library(questionr) # for predicted probabilites/OR by different subgroups etc ggpredict, ggeffects
library("Hmisc") #I use it for labeling variables
library(ggdist) # for rainclouds/box plots, visualizing distributions and uncertainty
library(egg)
library(grid)
library(smd) # for adding add_difference in tbl_summary
library(car) #to check for VIF factor of collinearity
library(copula)
library(collinear)

# for plotting incidence/hospitalization cases in Lausanne/Vaud
library(cowplot)
library(ggsci)
library(naniar)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("count", "dplyr")

# colours adapted for colour blind persons
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#data
laus <- read_excel(here("data", "laus_cleaned_2024-03-27.xlsx"))
laus_vd_waves <- read.csv2(here("data", "laus_vaud_flu_waves.csv"))

#graph parameters
lwdline <- 1.2
size_axis <- 14
size_axis_title <- 16
size_legend <- 12
size_legend_title <- 14
axis.title.x.position <- element_text(margin = margin(t =12))


#loading R codes 
source("R/data_cleaning.R") # run separately
source("R/functions.R") 
source("R/flu_cleaning.R") 
source("R/Inclusions.R") 

#render Rmd files in html
# render("R/PTB_brain_sparing_UCL.Rmd", output_file = paste0("../output/",today(),"PTB_brain_sparing.html"))
# rmarkdown::render("R/PTB_brain_sparing_pdf.Rmd", output_file = paste0("../output/", today(), "Lausanne_pregancy_outcomes_and_infections.pdf"))
