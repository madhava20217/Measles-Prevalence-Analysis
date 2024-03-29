
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
# install.packages("corrplot")
# install.packages("lessR")
# install.packages("data.table")
# install.packages("olsrr")
# install.packages("jtools")
install.package("dummies")
# clear console and variables
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
library(corrplot);
library(lessR);
library(data.table);
library(olsrr);
library(jtools);


#this is for modelsummary
#webshot::install_phantomjs()

#reading the dataframe values into "main_df"
main_df <- read_csv("../Data/main_final.csv");

#renaming variable of interest: v36 to Measles in the main_df
names(main_df)[names(main_df) == "v36"] <- "Measles"

#getting numeric data only, from main_df
num_df <- main_df[, unlist(lapply(main_df, is.numeric))]

# ********************SUMMARY OF VARIABLES********************
summary(num_df)



# ********************GETTING MODEL PARAMETERS********************

#determining which all features to be used among the numerical information
cov_mat_num <- cov(num_df, use = "complete.obs")
print(xtable(as.data.frame(cov_mat_num)), type = "html")

#Our variable of interest is infections due to measles (percentage of total
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

# measles infections infants should be related to healthcare facilities.
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
# Same with diarrhoea and malaria
covs <- as.data.frame(cov(num_df, use = "complete.obs"))
print(covs$Measles)

corrs <- as.data.frame(cor(num_df, use = "complete.obs"))
write_csv(corrs, "correlation_matrix.csv")

#looking at the correlation coefficient matrix (corrs), we can notice a strong
#correlation between v46 and v36, because the former is the death percentage due
#to measles, and the latter is the number of measles infected infants

#this is followed by v9, which is number of home deliveries attended by doctor
# or nurse

abs_corrs <- abs(corrs)


# *** correlation plot ***
#dropping params (either redundant or other)
valid_cols <- c("Measles" , "v5", "v20", "v23", "v26", "v30", "v37", "v38", 
                "index", "gdp", "beds", "tap", "season")

test_df <- main_df[valid_cols]

rabi_df <- na.omit(test_df[test_df$season == "Rabi", ])
kharif_df <- na.omit(test_df[test_df$season == "Kharif", ])


correlation_plot <- corrplot(cor(test_df[, 1: ncol(test_df) - 1], use = "complete.obs"),
                             method = "number")
corrplot(cor(num_df, use = "complete.obs"), method = "ellipse")

# save_plot("correlation_plot.png", correlation_plot)

# ********************MODELING PARAMETERS********************

#original formula R^2: 0.7752,  0.7796 (had a lot more variables)
#the below formula had R^2: 0.8074, 0.8093

measles_form <- Measles ~ v5 + v20 + v23 + v26 + v30 + v37 + v38 + 
                          index + gdp + beds + tap


measles_model_rabi <- lm(measles_form, rabi_df, na.action = na.omit)

measles_model_kharif <- lm(measles_form, kharif_df, na.action = na.omit)

summ(measles_model_rabi)

summary(measles_model_rabi)
summary(measles_model_kharif)

#ols regression
ols_regress(measles_form, test_df)


measles_model <- lm(measles_form, test_df, na.action = na.omit)

ols_regress(measles_form, test_df)
ols_vif_tol(measles_model)

anova(rabi_df);

# ********************IMPROVING MODEL PARAMETERS********************
#for (param in names(num_df[, 9:length(names(num_df))])){
#  hist(num_df[[param]], xlab = param, bins = 20)
#  cat("Press [enter] to continue")
#  line <- readline()
#  dev.off()
#  if(line == "stop") break
#}



#scatterplots vs Measles
#for (param in names(num_df[, 9:length(names(num_df))])){
#  if (param != "Measles"){
#    scatter.smooth(num_df$Measles, num_df[[param]], xlab = "Measles", ylab = param)
#    cat("Press [enter] to continue")
#    line <- readline()
#    dev.off()
#    if(line == "stop") break
#  }
#}

# ********************VISUALISING RESIDUALS AND VS INDEX********************
measles_model_kharif
measles_model_rabi

kharif_resid <- residuals(measles_model_kharif)
rabi_resid <- residuals(measles_model_rabi)



plotter <- function(df, model){
  dev.off()
  par(mfrow  = c(2, 2))
  plot(y = df$Measles, x = df$index, xlab = "Yield Index", 
       ylab = "Measles pct children", main = "Measles vs Yield Index")
  

  
  plot(y = residuals(model), x = df$index, xlab = "Yield Index", 
       ylab = "OLS Residuals", main = "Yield Index vs Residuals")
  
  
  plot(y = fitted.values(model), x = df$Measles, ylab = "Predicted values",
       xlab = "Measles pct children", main = "Predicted vs Actual")

  }

plotter(kharif_df, measles_model_kharif)
plotter(rabi_df, measles_model_rabi)

# ***********histogram of residuals
dev.off()
hist(kharif_resid, main = "Residuals of OLS for Kharif")
hist(rabi_resid, main = "Residuals of OLS for Rabi")

#numerically showing that sum of residuals is 0
sum(kharif_resid)
sum(rabi_resid)

# sum of residuals for kharif and rabi are respectively, 0.00000000000006244874
# and 0.000000000003591221, which can be attributed to floating point operation
# precision. Therefore, sum of residuals is 0.

kharif_resid <- measles_model_kharif$residuals
kharif_xi_ui_df <- kharif_df[, 3:ncol(kharif_df)-1] * t(kharif_resid)

kharif_xi_ui <- as.data.frame(flatten(kharif_xi_ui_df))

rabi_xi_ui <- flatten(rabi_df[, 3:ncol(rabi_df)-1] * 
                        t(measles_model_rabi$residuals))


#plotting histograms
hist(as.numeric(kharif_xi_ui), xlab = "u_lt * x_it for Kharif", 
     main = "Histogram of u*x for Kharif", bins = 20
     )

hist(as.numeric(rabi_xi_ui), xlab = "u_lt * x_it for Rabi", 
     main = "Histogram of u*x for Rabi", bins = 20
)

#sum for kharif is -0.0003767827, and for rabi is 0.00001958485,w hich are very
#close to 0.
sum(as.numeric(kharif_xi_ui))
sum(as.numeric(rabi_xi_ui))





# ********************MONTE CARLO SIMULATION********************

#Dataset for running Monte Carlo simulation (Measles and index)
mc_dataset <- main_df[, c("index", "Measles", "season")]
mc_dataset <- na.omit(mc_dataset)

#visualisation of the data

#all seasons
all_seasons_mc <- mc_dataset
rabi_mc <- mc_dataset[mc_dataset$season == "Rabi", ]
kharif_mc <- mc_dataset[mc_dataset$season == "Kharif", ]

all_seasons_Measles_yield <- ggplot(data = all_seasons_mc, mapping = (aes(x = index,
                    y = Measles))) +
  geom_point(size = 0.5) + labs(x = "Yield Index",
                                y = "Children with measles",
                                title = "Percentage of measles infections vs
                      Yield Index (All Seasons)")

rabi_Measles_yield <- ggplot(data = rabi_mc,
                      mapping = (aes(x = index, y = Measles))) +
  geom_point(size = 0.5) + labs(x = "Yield Index",
                                          y = "Children with measles",
                      title = "Percentage of measles infections vs
                      Yield Index (Rabi)")

kharif_Measles_yield <- ggplot(data = kharif_mc,
                        mapping = (aes(x = index, y = Measles))) +
  geom_point(size = 0.5) + labs(x = "Yield Index",
                                y = "Children with measles",
                                title = "Percentage of measles infections vs
                      Yield Index (Kharif)")

p <- plot_grid(all_seasons_Measles_yield, rabi_Measles_yield,
               kharif_Measles_yield)

plot(p)

#save the plot in the working directory
save_plot("measles_vs_yield.png", p, ncol = 2, base_asp = 1.2)

#formula for linear model
mc_form <- Measles ~ index

#linear model creation
all_seasons_lm <- lm(mc_form, mc_dataset)
rabi_lm <- lm(mc_form, rabi_mc)
kharif_lm <- lm(mc_form, kharif_mc)


modelsummary(list("All Seasons"  = all_seasons_lm,
                  "Rabi" = rabi_lm,
                  "Kharif" = kharif_lm),

             output = "measles_vs_index_lm_summary.png")

#For Rabi
#intercept coefficient
Rabibeta_0 <- summary(rabi_lm)$coefficients[1,1]

#slope coefficient
Rabibeta_1 <- summary(rabi_lm)$coefficients[2,1]

#Discarding 20% of data and considering the remaining dataset
reduced_rabi_data <- rabi_mc[.(random(0.8)), .(index:season)]

n_iter <- 500

#Sample size : Number of rows in remaining dataset
rabi_samplesize <- nrow(reduced_rabi_data)

#500 is the number of iterations
#Vector to store computed slope coefficients
rabi_slope <- rep(0, n_iter)

#Vector to store computed intercept coefficients
rabi_intercept <- rep(0, n_iter)

#Iterations = 500
for (i in 1:n_iter) {
  u_i <- rnorm(rabi_samplesize, mean = 0, sd = 1)
  x_i <- rnorm(rabi_samplesize, mean = 2, sd = 16)
  y_i <- Rabibeta_0 + Rabibeta_1*x_i + u_i

  data_i <- data.table(Y = y_i, X = x_i)

  #Regressing
  ols_data <- lm(y_i ~ x_i, data_i)

  #Adding the regression coefficients to vectors
  rabi_slope[i] <- summary(ols_data)$coefficients[2,1]
  rabi_intercept[i] <- summary(ols_data)$coefficients[1,1]
}
print(Rabibeta_0)
print(Rabibeta_1)

#Calculating the estimated value
mean(rabi_intercept)
mean(rabi_slope)


#For Kharif
#intercept coefficient
Kharifbeta_0 <- summary(kharif_lm)$coefficients[1,1]

#slope coefficient
Kharifbeta_1 <- summary(kharif_lm)$coefficients[2,1]

#Discarding 20% of data and considering the remaining dataset
reduced_kharif_data <- kharif_mc[.(random(0.8)), .(index:season)]

#Sample size : Number of rows in remaining dataset
kharif_samplesize <- nrow(reduced_kharif_data)

#500 is the number of iterations
#Vector to store computed slope coefficients
kharif_slope <- rep(0, n_iter)

#Vector to store computed intercept coefficients
kharif_intercept <- rep(0, n_iter)

#Iterations = 500
for (i in 1:n_iter) {
  u_i <- rnorm(kharif_samplesize, mean = 0, sd = 1)
  x_i <- rnorm(kharif_samplesize, mean = 2, sd = 16)
  y_i <- Kharifbeta_0 + Kharifbeta_1*x_i + u_i

  data_i <- data.table(Y = y_i, X = x_i)

  #Regressing
  ols_data <- lm(y_i ~ x_i, data_i)

  #Adding the regression coefficients to vectors
  kharif_intercept[i] <- summary(ols_data)$coefficients[1,1]
  kharif_slope[i] <- summary(ols_data)$coefficients[2,1]
}
print(Kharifbeta_0)
print(Kharifbeta_1)

#Calculating the estimated value
mean(kharif_intercept)
mean(kharif_slope)


# *****************************Q-3***************************************

# ---- Part A ---- 

# get all the columns used by regression along with other columns
# req_cols <- c(unlist(colnames(num_df)),"state")
req_cols <- c(valid_cols,"state","season")
dummy_df <- main_df[, req_cols]

south_dists = c("Andhra Pradesh","Telangana", "Karnataka", "Kerala", "Tamil Nadu")
# north_dists = c("Himachal Pradesh", "Punjab", "Uttarakhand", "Uttar Pradesh", "Haryana")

Dsouth <- ifelse(dummy_df$state %in% south_dists, 1, 0)
dummy_df$Dsouth <- Dsouth


# ---- Part B ----

measles_form <- Measles ~ v5 + v20 + v23 + v26 + v30 + v37 + v38 + 
  index + gdp + beds + tap + Dsouth

rabi_df <- na.omit(dummy_df[dummy_df$season == "Rabi", ])
kharif_df <- na.omit(dummy_df[dummy_df$season == "Kharif", ])


measles_model_rabi <- lm(measles_form, rabi_df, na.action = na.omit)

measles_model_kharif <- lm(measles_form, kharif_df, na.action = na.omit)

measles_model_total <- lm(measles_form, dummy_df, na.action = na.omit)


summary(measles_model_rabi)
summary(measles_model_kharif)
summary(measles_model_total)


# Inference : 

# H0 (null hypothesis) Beta(Dsouth) = 0
# Ha (alt hypothesis) Beta(Dsouth) != 0

# Testing for Kharif, Rabi and the total model as well
# The confidence intervals on all these cases are:
# rabi: 0.000000000022822
# Kharif: 0.000000000344165
# total: < 0.0000000000000002
# which are quite low, and hence D south is statistically significant for our model
# as P values < 0.05 implies null hypothesis is rejected
# This further implies a structural break in mean outcome level across southern and 
# non southern groups

# ***********************************************************************


# ---- Part C ---- 
measles_form_restricted <- Measles ~ v5 + v20 + v23 + v26 + v30 + v37 + v38 + 
  index + gdp + beds + tap 

restricted_measles_model_rabi <- lm(measles_form_restricted, rabi_df, na.action = na.omit)

restricted_measles_model_kharif <- lm(measles_form_restricted, kharif_df, na.action = na.omit)

restricted_measles_model_total <- lm(measles_form_restricted, dummy_df, na.action = na.omit)

#Printing the summaries of 2 models
summary(measles_model_kharif)
summary(restricted_measles_model_kharif)

#Performing an F-test
anova(restricted_measles_model_kharif,measles_model_kharif)

#Repeating for rabi 
#Printing the summaries of 2 models
summary(measles_model_rabi)
summary(restricted_measles_model_rabi)

#Performing an F-test
anova(restricted_measles_model_rabi,measles_model_rabi)

#Inference

#Performing Anova on both kharif and rabi for restricted (without dummy variable
# for south zone) and unrestricted model(with the dummy variable for south zone)
# we see that the p-value for kharif is 1.381e-08 which is less than the 
# significance level of 0.05, we can with a 95% confidence say that the models 
# are therefore statistically dissimilar and hence we reject null hypothesis and
# say that there IS a structural break across southern and non-southern state-groups.



# ---- Part D ----


eco_df <- read_csv("../eco.csv")

head(eco_df[, 70:ncol(eco_df)])
ncol(main_df)
ncol(eco_df)


# eco_df doesn't have rowid and has an additional column called %Vaccine
# which is the coverage of the National Immunization Programme.

valid_cols <- c("Measles" , "v5", "v20", "v23", "v26", "v30", "v37", "v38", 
                "index", "gdp", "beds", "tap", "season", "Vaccine", "year")

names(eco_df)[names(eco_df) == 'v36'] <- "Measles"
names(eco_df)[names(eco_df) == '%Vaccine'] <- "Vaccine"

analysis_df <- eco_df[, valid_cols];
analysis_df <- na.omit(analysis_df);

show(valid_cols);

measles_form <- Measles ~ v5 + v20 + v23 + v26 + v30 + v37 + v38 + index + gdp +
                          beds + tap

hist(analysis_df$Vaccine)
hist(log(analysis_df$Vaccine))
hist(sqrt(analysis_df$Vaccine))

mod <- lm(measles_form, analysis_df)

summary(mod)



# Creating dummy variables ------------------------------------------------

unique(analysis_df$year)

#we have six years: 2011 2012 2013 2014 2015 2016, considering 2011 as the base:
year = factor(analysis_df$year)
dummies = model.matrix(~year)

season = factor(analysis_df$season);
season_mat = model.matrix(~season);

test_analysis <- data.frame(analysis_df, dummies, season_mat)


# Changing the model ------------------------------------------------------

measles_model <- Measles ~ v5 + v20 + v23 + v26 + v30 + v37 + v38 + index + gdp +
  beds + tap + year2012 + year2013 + year2014 + year2015 + year2016 + seasonRabi +
  seasonSummer + seasonWhole.Year

time_analysis_model <- lm(measles_model, test_analysis);

summary(time_analysis_model);
