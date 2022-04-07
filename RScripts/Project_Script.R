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

#reading the dataframe values into "main_df"
main_df <- read_csv("../Data/main_final.csv");

#renaming variable of interest: v36 to Measles_Pct in the main_df
names(main_df)[names(main_df) == "v36"] <- "Measles_Pct"
