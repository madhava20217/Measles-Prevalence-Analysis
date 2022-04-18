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

#reading the dataframe values into "main_df"
main_df <- read_csv("../Data/main_final.csv");

#renaming variable of interest: v36 to Measles_Pct in the main_df
names(main_df)[names(main_df) == "v36"] <- "Measles_Pct"

#getting numeric data only, from main_df
num_df <- main_df[, unlist(lapply(main_df, is.numeric))]



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
                      y = "Measles %deaths",
                      title = "Percentage of Measles-induced deaths vs Yield
                      Index (All Seasons)")

rabi_Measles_yield <- ggplot(data = rabi_mc, 
                      mapping = (aes(x = index, y = Measles_Pct))) +
  geom_point(size = 0.5) + labs(x = "Yield Index",
                                          y = "Measles %deaths",
                      title = "Percentage of Measles-induced deaths vs
                      Yield Index (Rabi)")

kharif_Measles_yield <- ggplot(data = kharif_mc, 
                        mapping = (aes(x = index, y = Measles_Pct))) +
  geom_point(size = 0.5) + labs(x = "Yield Index", 
                                            y = "Measles %deaths",
                        title = "Percentage of Measles-induced deaths vs
                        Yield Index (Kharif)")

p <- plot_grid(all_seasons_Measles_yield, rabi_Measles_yield, 
               kharif_Measles_yield)

save_plot("measles_vs_yield.png", p, ncol = 2, base_asp = 1.2)

mc_form <- Measles_Pct ~ index

