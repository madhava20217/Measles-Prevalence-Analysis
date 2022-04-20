# packages' installation
# install.packages("readr");
# install.packages("dplyr");
# install.packages("tibble");
# install.packages("modeest");
# install.packages("statip");
# install.packages("ggplot2")
# install.packages("tidyr");
# install.packages("knitr");
# install.packages("modelsummary");
# install.packages("kableExtra");
# install.packages("gt");
# install.packages("tidyverse")
# install.packages("cowplot")
# install.packages("xtable")

#clear console and variables
rm(list = ls());
cat("\014");


library(dplyr);
library(readr);
library(modeest);
library(statip);
library(ggplot2);
library(tidyr);
library(modelsummary);
library(kableExtra);
library(tidyverse);
library(cowplot);
library(xtable);

#this is for modelsummary
webshot::install_phantomjs()

#reading the dataframe values into "main_df"
main_df <- read_csv("../Data/main_final.csv");

#renaming variable of interest: v36 to Measles_Pct in the main_df
names(main_df)[names(main_df) == "v36"] <- "Measles_Pct"

#getting numeric data only, from main_df
num_df <- main_df[, unlist(lapply(main_df, is.numeric))]

# ********************GETTING MODEL PARAMETERS********************

#determining which all features to be used among the numerical information
cov_mat_num <- cov(num_df, use = "complete.obs")
print(xtable(as.data.frame(cov_mat_num)), type = "html")

#Our variable of interest is infant deaths due to measles (percentage of total
#deaths)
# It should not scale with rowID (since it's just row_id), nor with country, 
# could scale with state (as a categorical variable), could scale with
# districtLGDCode (similar to state), could with year, Area under production,
# yield, state (as a categorical variable) etc...

# seems like the viable variables are: area under production (areahectares),
# yieldtonneshectare, v1 (pregnant women registered for ante natal care), 
# pregnant women registered for anti natal care within first trimester (v2), 
# received 3 ANC checks: v3, v4 (tetanus toxoid), 100 iron and folic acid (v5),
#  moderately anaemic (v6), anaemia (v7), home deliveries (v8) ? , v9 ? , v10 ?,
# Note; v9 v10 v11 are categories of v8, which is also a category of v14 with v13
# v12 (pct discharged within 48 hours of delivery), v18(postpartum 48hr) ~ to v8
# safe deliveries v16, % home deliveries (probably not useful), 

# measles deaths in infants should be related to healthcare facilities.
# therefore, it must have some correlation with v1 to v20 (birth and pregnancy
# variables), birth weight issues (v20 to v30), sex ratio as well at birth (v31)
# and also, with fully immunised children (as it relates to healthcare facility 
# and attitude of people to it). It could also scale with sterlisation, which
# is done in order to prevent unwanted pregnancy, which may relate to the 
# people's potential to handle the children.

# Measles is a viral respiratory disease. It spreads via air, saliva, touching
# a contaminated surface (hygiene), skin to skin contact (hygiene again), 
# mother to child via pregnancy, labour or nursing.

# Polio is transmitted through contaminated water and food, and necessitates
# hygiene, similar to Measles.
# Therefore, Measles could be related to Polio deaths.
# Same with diarrhoea

#



# ********************MONTE CARLO SIMULATION********************

#Dataset for running Monte Carlo simulation (Measles_Pct and index)
mc_dataset <- main_df[, c("index", "Measles_Pct", "season")]
mc_dataset <- na.omit(mc_dataset)

#visualisation of the data

#all seasons
all_seasons_mc <- mc_dataset
rabi_mc <- mc_dataset[mc_dataset$season == "Rabi", ]
kharif_mc <- mc_dataset[mc_dataset$season == "Kharif", ]

all_seasons_Measles_yield <- ggplot(data = all_seasons_mc, mapping = (aes(x = index, 
                    y = Measles_Pct))) +
  geom_point(size = 0.5) + labs(x = "Yield Index", 
                                y = "Children with measles",
                                title = "Percentage of measles infections vs
                      Yield Index (All Seasons)")

rabi_Measles_yield <- ggplot(data = rabi_mc, 
                      mapping = (aes(x = index, y = Measles_Pct))) +
  geom_point(size = 0.5) + labs(x = "Yield Index",
                                          y = "Children with measles",
                      title = "Percentage of measles infections vs
                      Yield Index (Rabi)")

kharif_Measles_yield <- ggplot(data = kharif_mc, 
                        mapping = (aes(x = index, y = Measles_Pct))) +
  geom_point(size = 0.5) + labs(x = "Yield Index", 
                                y = "Children with measles",
                                title = "Percentage of measles infections vs
                      Yield Index (Kharif)")

p <- plot_grid(all_seasons_Measles_yield, rabi_Measles_yield, 
               kharif_Measles_yield)

#save the plot in the working directory
save_plot("measles_vs_yield.png", p, ncol = 2, base_asp = 1.2)

#formula for linear model
mc_form <- Measles_Pct ~ index

#linear model creation
all_seasons_lm <- lm(mc_form, mc_dataset)
rabi_lm <- lm(mc_form, rabi_mc)
kharif_lm <- lm(mc_form, kharif_mc)


modelsummary(list("All Seasons"  = all_seasons_lm, 
                  "Rabi" = rabi_lm,
                  "Kharif" = kharif_lm), 
             
             output = "measles_vs_index_lm_summary.png")


