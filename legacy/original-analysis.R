# ---------------------------------------------------#
# Call the Dependencies/ package so you can use it   #
# --------------------------------------------------#
library(dplyr)
library(VineCopula)
library(tidyverse)
library(e1071)
library(tseries)
library(zoo)
library(rugarch)
library(readxl)
library(rvinecopulib)
library(network)
library(copula)
library(portvine)
library(zoo)


# -------------#
# IMPORT DATA #
# ------------#
df <- read_excel("~/Data Sheet 06-10-2020.xlsx")
View(df)

# Set Date column  as index
# ---------------------
data_new <- df %>% 
  column_to_rownames(., var = "Date")
View(data_new)

# -------------------------------#
# Data Wrangling and Preparation #
# -------------------------------#
# Descriptive statistics
head(data_new)

# Missing value check 
# ---------------------
sum(is.na(data_new))

# If data contains missing values, RUN THIS
# Print which column in our dataframe contains missing value
for (i in colnames(data_new)){
  print(sum(is.na(data_new[i])))
}

# So, column of India contains two missing values
# Impute this NA with interpolation
data_new <- data_new %>%
  mutate(India = na.approx(India))

# Check missing value again
sum(is.na(data_new)) # no missing value, we are good to go

# ------------------------#
# Descriptive Statistics  #
# ------------------------#
# Calculate central tendecy and variability of data
mean_data <- sapply(data_new, mean)
min_data <- sapply(data_new, min)
max_data <- sapply(data_new, max)
std_data <- sapply(data_new, sd) #standar deviation
skewness_data <- sapply(data_new, skewness)
kurtosis_data <- sapply(data_new, kurtosis)

# Combine it into one data frame
descriptive_stats <- data.frame(mean_data, 
                                max_data, min_data, std_data,
                                skewness_data,kurtosis_data)

# Jarque Bera Test Normality
normality_data <- apply(data_new, 2, jarque.bera.test)
normality_data_2 <- lapply(normality_data, `[`, c('statistic'))
normality_data_2<- normality_data_2%>%
  as.data.frame() %>%
  t()
colnames(normality_data_2) <- "JB Test"

# Append normality_data_2 to descriptive_stats dataframe
descriptive_stats$JB_test <- normality_data_2
descriptive_stats <- as.data.frame(descriptive_stats)


# -------------#
# Correlation #
# -------------#
# Pearson Correlation
# ---------------------
pearson_cor <- data_new %>% 
  cor(method = "pearson" ) %>%
  round(2)
pearson_cor


# Kendall Correlation
kendall_cor <- data_new %>% 
  cor(method = "kendall") %>%
  round(2)
print(kendall_cor)


# ----------------------#
# Statistical Modeling  #
# ---------------------#
# GARCH(1,1) MODEL     #
# ---------------------#
# Let's take example for Australia
# model specification
garchspec_aus <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                        distribution.model = "norm")

# model fitting
garchfit_aus <- ugarchfit(data = data_new$Australia, spec = garchspec_aus, solver.control = list(tol = 1e-12))
garchfit_aus


# --------------------#
# EGARCH(1,1) MODEL   #
# --------------------#
# Let's take example for Australia
# model specification
e_garchspec_aus <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                            variance.model = list(model = "eGARCH", garchOrder=c(1,1)), 
                            distribution.model = "norm")
# model fitting
e_garchfit_aus <- ugarchfit(data = data_new$Australia, spec = e_garchspec_aus, solver.control = list(tol = 1e-12))
e_garchfit_aus


# ---------------------#
# GJR-GARCH(1,1) MODEL #
# ---------------------#
# Let's take example for Australia
# model specification
gjr_garchspec_aus <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "gjrGARCH", garchOrder=c(1,1)), 
                             distribution.model = "norm")
# model fitting
gjr_garchfit_aus <- ugarchfit(data = data_new$Australia, spec = gjr_garchspec_aus, solver.control = list(tol = 1e-12))
gjr_garchfit_aus


# -----------------------------------#
# Extract Info Criteria to Find BIC  #
# -----------------------------------#
infocriteria(garchfit_aus)
infocriteria(e_garchfit_aus)
infocriteria(gjr_garchfit_aus)

# Look at Bayes value for each model's info criteria
# Please visit this link http://www.cs.toronto.edu/~mbrubake/teaching/C11/Handouts/BIC.pdf
# If difference between two BIC is less than 1, than there r no significant proof 
# To say that one model is better than another model
# When BIC scores are positive, choose model with the lowest BIC
# However, if BIC scores r negative, we choose BIC value that closer to zero, or the minimal absolute of the negative scores
# Here is also the reference: https://ascopubs.org/doi/full/10.1200/JCO.21.00277

# So, we get BIC scores for each models:
# GARCH = -6.43 ; EGARCH = -6.34 ; GJR GARCH = -6.46
# We choose models whose BIC score is closer to zero.
# We choose EGARCH, since it has the lowest absolute value of BIC.

# -------------------------------------#
# GARCH with Different DIstribution    #
# -------------------------------------#
# Normal Dist
egarch_norm <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "eGARCH", garchOrder=c(1,1)), 
                               distribution.model = "norm")
egarch_norm_fit <- ugarchfit(data = data_new$Australia, spec = egarch_norm, solver.control = list(tol = 1e-12))

# Skewed Normal Dist
egarch_snorm <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                           variance.model = list(model = "eGARCH", garchOrder=c(1,1)), 
                           distribution.model = "snorm")
egarch_snorm_fit <- ugarchfit(data = data_new$Australia, spec = egarch_snorm, solver.control = list(tol = 1e-12))

# Student t Dist
egarch_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
           variance.model = list(model = "eGARCH", garchOrder=c(1,1)), 
           distribution.model = "std")
egarch_std_fit <- ugarchfit(data = data_new$Australia, spec = egarch_std, solver.control = list(tol = 1e-12))

# Skewed Student t Dist
egarch_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                         variance.model = list(model = "eGARCH", garchOrder=c(1,1)), 
                         distribution.model = "sstd")
egarch_sstd_fit <- ugarchfit(data = data_new$Australia, spec = egarch_sstd, solver.control = list(tol = 1e-12))

# Generalized Error Dist
egarch_ged <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                         variance.model = list(model = "eGARCH", garchOrder=c(1,1)), 
                         distribution.model = "ged")
egarch_ged_fit <- ugarchfit(data = data_new$Australia, spec = egarch_ged, solver.control = list(tol = 1e-12))

# Skew-Generalized Error Dist
egarch_sged <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                         variance.model = list(model = "eGARCH", garchOrder=c(1,1)), 
                         distribution.model = "sged")
egarch_sged_fit <- ugarchfit(data = data_new$Australia, spec = egarch_sged, solver.control = list(tol = 1e-12))
                               

# --------------------------------------------------------------------------#
# Extract Info Criteria to Find BIC of EGRACH with different distribution  #
# -------------------------------------------------------------------------#
infocriteria(egarch_norm_fit)
infocriteria(egarch_snorm_fit)
infocriteria(egarch_std_fit)
infocriteria(egarch_sstd_fit)
infocriteria(egarch_ged_fit)
infocriteria(egarch_sged_fit)

# From infocriteria of these egarch models, we get BIC for:
# normal_dist: -6.34 ; skewed_normal_dist: -6.37 ; student_t_dist: -6.632
# skewed_student_t: -6.637 ; ged_dist:-6.577 ; skew-ged_dist: -6.658
# We choose EGARCH with Normal Distribution model.


# -------------------------------------#
# Overall, choose AR(1)-EGARCH(1,1)    #
# -------------------------------------#
# Let's define our EGARCH model
egarch_spec <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                                                  variance.model = list(model = "eGARCH", garchOrder=c(1,1)), 
                                                  distribution.model = "norm")
# ---------------------------------#
# FIT EGARCH MODEL TO ALL OF DATA  #
# ---------------------------------#
# Australia
aus_fit <- ugarchfit(data = data_new$Australia, spec = egarch_spec, solver = "hybrid")
aus_fit
infocriteria(aus_fit)

# Canada
canada_fit <- ugarchfit(data = data_new$Canada, spec = egarch_spec, solver = "hybrid")
canada_fit
infocriteria(canada_fit)

# Germany
germany_fit <- ugarchfit(data = data_new$Germany, spec = egarch_spec, solver = "hybrid")
germany_fit
infocriteria(germany_fit)

# Japan
japan_fit <- ugarchfit(data = data_new$Japan, spec = egarch_spec, solver = "hybrid")
japan_fit
infocriteria(japan_fit)

# UK
uk_fit <- ugarchfit(data = data_new$UK, spec = egarch_spec, solver = "hybrid")
uk_fit
infocriteria(uk_fit)

# USA
usa_fit <- ugarchfit(data = data_new$USA, spec = egarch_spec, solver = "hybrid")
usa_fit
infocriteria(usa_fit)

# Bangladesh
bangladesh_fit <- ugarchfit(data = data_new$Bangladesh, spec = egarch_spec, solver = "hybrid")
usa_fit
infocriteria(bangladesh_fit)

# China
china_fit <- ugarchfit(data = data_new$China, spec = egarch_spec, solver = "hybrid")
usa_fit
infocriteria(china_fit)

# India
india_fit <- ugarchfit(data = data_new$India, spec = egarch_spec, solver = "hybrid")
india_fit
infocriteria(india_fit)

# Indonesia
indonesia_fit <- ugarchfit(data = data_new$Indonesia, spec = egarch_spec, solver = "hybrid")
indonesia_fit
infocriteria(usa_fit)

# Pakistan
pakistan_fit <- ugarchfit(data = data_new$Pakistan, spec = egarch_spec, solver = "hybrid")
pakistan_fit
infocriteria(pakistan_fit)

# Russia
russia_fit <- ugarchfit(data = data_new$Russia, spec = egarch_spec, solver = "hybrid")
russia_fit
infocriteria(russia_fit)

# -----------------------#
# Forecast for 360 days  #
# -----------------------#
aus_forecast = ugarchforecast(aus_fit, n.ahead = 360)

# -------------------------------------------------------------#
# Generate The Residuals from EGARCH Model and Standardize it  #
# ------------------------------------------------------------#
z_aus = residuals(aus_fit, standardize = T)
z_canada = residuals(canada_fit, standardize = T)
z_germany = residuals(germany_fit, standardize = T)
z_japan = residuals(japan_fit, standardize = T)
z_uk = residuals(uk_fit, standardize = T)
z_usa = residuals(usa_fit, standardize = T)
z_bangladesh = residuals(bangladesh_fit, standardize = T)
z_china = residuals(china_fit, standardize = T)
z_india = residuals(india_fit, standardize = T)
z_indonesia = residuals(indonesia_fit, standardize = T)
z_pakistan = residuals(pakistan_fit, standardize = T)
z_russia = residuals(russia_fit, standardize = T)

# ----------------------------------------------------------------------------------------#
# Combine All Standardize Residuals to One Data Frame and Change The Format into Matrix   #
# ----------------------------------------------------------------------------------------#
z_all <- data.frame(z_aus, z_canada, z_germany, z_japan, z_uk, z_usa, z_bangladesh,
                    z_china, z_india, z_indonesia, z_pakistan, z_russia) %>% as.matrix()

# --------------------------------------------------------#
# Convert The Standar Residuals into Pseudo Observation   #
# -------------------------------------------------------#
uniform_all <- apply(z_all, MARGIN = 2, pobs)


# -----------------#
# Fit Vine Copula  #
# ----------------#
rvine <- RVineStructureSelect(data = uniform_all,
                                    progress = TRUE,
                                    type = 'RVine',
                                    selectioncrit = 'BIC',
                                    treecrit = 'tau')


# -------------------------------------------------------------#
# Dependence Structure if data with R-Vine Copula (TABLE 7)    #
# -------------------------------------------------------------#
dependence_structure <- summary(rvine)

# ------------------------#
# BIC score R-Vine Copula #
# ------------------------#
BIC_r_vine_copula <- rvine$BIC
BIC_r_vine_copula


# -------------------------------------------------------------#
# Plot Dependence Structure of RVine Copula (FIGURE 1)         #
# -------------------------------------------------------------#
plot(rvine)


# ----------------------------#
# BACKTESTING FOR GARCH MODEL #
# ----------------------------#

# -----------#
# Australia  #
# -----------#
# alpha 1%
aus_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$Australia, n.ahead = 1, n.start = 2229,
                            refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                            VaR.alpha = 0.01)
# alpha 5%
aus_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$Australia, n.ahead = 1, n.start = 2229,
                                refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                VaR.alpha = 0.05)
# alpha 10%
aus_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$Australia, n.ahead = 1, n.start = 2229,
                                refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                VaR.alpha = 0.1)

 # Print the VAR Backtesting for Australia
report(aus_garch_roll_01, type ="VaR") # fro alpha 1% 
report(aus_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(aus_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# Look at the output report
# Violation Rate in TABLE 8 journal == Actual % (in output)
# UC Value in TABLE 8 journal == LR.uc Statistic (in output)
# CC Value in TABLE 8 journal == LR.cc Statistic (in output)

# -----------#
# Canada     #
# -----------#
# alpha 1%
can_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$Canada, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
can_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$Canada, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
can_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$Canada, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for Canada
report(can_garch_roll_01, type ="VaR") # fro alpha 1% 
report(can_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(can_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Germany    #
# -----------#
# alpha 1%
ger_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$Germany, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
ger_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$Germany, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
ger_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$Germany, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for Germany
report(ger_garch_roll_01, type ="VaR") # fro alpha 1% 
report(ger_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(ger_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Japan      #
# -----------#
# alpha 1%
jap_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$Japan, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
jap_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$Japan, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
jap_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$Japan, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for Japan
report(jap_garch_roll_01, type ="VaR") # fro alpha 1% 
report(jap_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(jap_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%


# -----------#
#  UK        #
# -----------#
# alpha 1%
uk_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$UK, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
uk_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$UK, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
uk_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$UK, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for UK
report(uk_garch_roll_01, type ="VaR") # fro alpha 1% 
report(uk_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(uk_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# USA        #
# -----------#
# alpha 1%
usa_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$USA, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
usa_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$USA, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
usa_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$USA, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for USA
report(usa_garch_roll_01, type ="VaR") # fro alpha 1% 
report(usa_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(usa_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Bangladesh #
# -----------#
# alpha 1%
bang_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$Bangladesh, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
bang_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$Bangladesh, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
bang_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$Bangladesh, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for Bangladesh
report(bang_garch_roll_01, type ="VaR") # fro alpha 1% 
report(bang_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(bang_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# China      #
# -----------#
# alpha 1%
china_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$China, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
china_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$China, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
china_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$China, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for China
report(china_garch_roll_01, type ="VaR") # fro alpha 1% 
report(china_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(china_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# India     #
# -----------#
# alpha 1%
ind_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$India, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
ind_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$India, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
ind_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$India, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for India
report(ind_garch_roll_01, type ="VaR") # fro alpha 1% 
report(ind_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(ind_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Indonesia  #
# -----------#
# alpha 1%
ina_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$Indonesia, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
ina_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$Indonesia, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
ina_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$Indonesia, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for Indonesia
report(ina_garch_roll_01, type ="VaR") # fro alpha 1% 
report(ina_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(ina_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Pakistan   #
# -----------#
# alpha 1%
pak_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$Pakistan, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
pak_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$Pakistan, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
pak_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$Pakistan, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for Pakistan
report(pak_garch_roll_01, type ="VaR") # fro alpha 1% 
report(pak_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(pak_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Russia     #
# -----------#
# alpha 1%
rus_garch_roll_01 = ugarchroll(egarch_spec, data = data_new$Russia, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.01)
# alpha 5%
rus_garch_roll_05 = ugarchroll(egarch_spec, data = data_new$Russia, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
rus_garch_roll_1 = ugarchroll(egarch_spec, data = data_new$Russia, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting for Russia
report(rus_garch_roll_01, type ="VaR") # fro alpha 1% 
report(rus_garch_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5%
report(rus_garch_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%




# -----------------------------------------------------#
# Plot the Back testing Var from Marginal Model GARCH #
# ---------------------------------------------------#
# Set the date
x = as.Date(rownames(data_new))
dates = x[2230:2429] #200 days backtesting


# ----------#
# Australia #
# ----------#
aus_actual_var = aus_garch_roll_01@forecast$VaR[,2]
aus_fore_var01 = aus_garch_roll_01@forecast$VaR[,1]
aus_fore_var05 = aus_garch_roll_05@forecast$VaR[,1]
aus_fore_var1 = aus_garch_roll_1@forecast$VaR[,1]
plot(dates, aus_actual_var, type = "l", xlab = "Day", ylab = "Australia")
lines(dates, aus_fore_var01, col = "red")
lines(dates, aus_fore_var05, col = "blue")
lines(dates, aus_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)
       

# ----------#
# Canada    #
# ----------#
can_actual_var = can_garch_roll_01@forecast$VaR[,2]
can_fore_var01 = can_garch_roll_01@forecast$VaR[,1]
can_fore_var05 = can_garch_roll_05@forecast$VaR[,1]
can_fore_var1 = can_garch_roll_1@forecast$VaR[,1]
plot(dates, can_actual_var, type = "l", xlab = "Day", ylab = "Canada")
lines(dates, can_fore_var01, col = "red")
lines(dates, can_fore_var05, col = "blue")
lines(dates, can_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# Germany   #
# ----------#
ger_actual_var = ger_garch_roll_01@forecast$VaR[,2]
ger_fore_var01 = ger_garch_roll_01@forecast$VaR[,1]
ger_fore_var05 = ger_garch_roll_05@forecast$VaR[,1]
ger_fore_var1 = ger_garch_roll_1@forecast$VaR[,1]
plot(dates, ger_actual_var, type = "l", xlab = "Day", ylab = "Germany")
lines(dates, ger_fore_var01, col = "red")
lines(dates, ger_fore_var05, col = "blue")
lines(dates, ger_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# Japan     #
# ----------#
jap_actual_var = jap_garch_roll_01@forecast$VaR[,2]
jap_fore_var01 = jap_garch_roll_01@forecast$VaR[,1]
jap_fore_var05 = jap_garch_roll_05@forecast$VaR[,1]
jap_fore_var1 = jap_garch_roll_1@forecast$VaR[,1]
plot(dates, jap_actual_var, type = "l", xlab = "Day", ylab = "Japan")
lines(dates, jap_fore_var01, col = "red")
lines(dates, jap_fore_var05, col = "blue")
lines(dates, jap_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# UK        #
# ----------#
uk_actual_var = uk_garch_roll_01@forecast$VaR[,2]
uk_fore_var01 = uk_garch_roll_01@forecast$VaR[,1]
uk_fore_var05 = uk_garch_roll_05@forecast$VaR[,1]
uk_fore_var1 = uk_garch_roll_1@forecast$VaR[,1]
plot(dates, uk_actual_var, type = "l", xlab = "Day", ylab = "UK")
lines(dates, uk_fore_var01, col = "red")
lines(dates, uk_fore_var05, col = "blue")
lines(dates, uk_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# USA      #
# ----------#
usa_actual_var = usa_garch_roll_01@forecast$VaR[,2]
usa_fore_var01 = usa_garch_roll_01@forecast$VaR[,1]
usa_fore_var05 = usa_garch_roll_05@forecast$VaR[,1]
usa_fore_var1 = usa_garch_roll_1@forecast$VaR[,1]
plot(dates, usa_actual_var, type = "l", xlab = "Day", ylab = "USA")
lines(dates, usa_fore_var01, col = "red")
lines(dates, usa_fore_var05, col = "blue")
lines(dates, usa_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# -----------#
# Bangladesh #
# -----------#
bang_actual_var = bang_garch_roll_01@forecast$VaR[,2]
bang_fore_var01 = bang_garch_roll_01@forecast$VaR[,1]
bang_fore_var05 = bang_garch_roll_05@forecast$VaR[,1]
bang_fore_var1 = bang_garch_roll_1@forecast$VaR[,1]
plot(dates, bang_actual_var, type = "l", xlab = "Day", ylab = "Bangladesh")
lines(dates, bang_fore_var01, col = "red")
lines(dates, bang_fore_var05, col = "blue")
lines(dates, bang_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# China     #
# ----------#
china_actual_var = china_garch_roll_01@forecast$VaR[,2]
china_fore_var01 = china_garch_roll_01@forecast$VaR[,1]
china_fore_var05 = china_garch_roll_05@forecast$VaR[,1]
china_fore_var1 = china_garch_roll_1@forecast$VaR[,1]
plot(dates, china_actual_var, type = "l", xlab = "Day", ylab = "China")
lines(dates, china_fore_var01, col = "red")
lines(dates, china_fore_var05, col = "blue")
lines(dates, china_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# India     #
# ----------#
ind_actual_var = ind_garch_roll_01@forecast$VaR[,2]
ind_fore_var01 = ind_garch_roll_01@forecast$VaR[,1]
ind_fore_var05 = ind_garch_roll_05@forecast$VaR[,1]
ind_fore_var1 = ind_garch_roll_1@forecast$VaR[,1]
plot(dates, ind_actual_var, type = "l", xlab = "Day", ylab = "India")
lines(dates, ind_fore_var01, col = "red")
lines(dates, ind_fore_var05, col = "blue")
lines(dates, ind_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# Indonesia #
# ----------#
ina_actual_var = ina_garch_roll_01@forecast$VaR[,2]
ina_fore_var01 = ina_garch_roll_01@forecast$VaR[,1]
ina_fore_var05 = ina_garch_roll_05@forecast$VaR[,1]
ina_fore_var1 = ina_garch_roll_1@forecast$VaR[,1]
plot(dates, ina_actual_var, type = "l", xlab = "Day", ylab = "Indonesia")
lines(dates, ina_fore_var01, col = "red")
lines(dates, ina_fore_var05, col = "blue")
lines(dates, ina_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# Pakistan  #
# ----------#
pak_actual_var = pak_garch_roll_01@forecast$VaR[,2]
pak_fore_var01 = pak_garch_roll_01@forecast$VaR[,1]
pak_fore_var05 = pak_garch_roll_05@forecast$VaR[,1]
pak_fore_var1 = pak_garch_roll_1@forecast$VaR[,1]
plot(dates, pak_actual_var, type = "l", xlab = "Day", ylab = "Pakistan")
lines(dates, pak_fore_var01, col = "red")
lines(dates, pak_fore_var05, col = "blue")
lines(dates, pak_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# Russia   #
# ----------#
rus_actual_var = rus_garch_roll_01@forecast$VaR[,2]
rus_fore_var01 = rus_garch_roll_01@forecast$VaR[,1]
rus_fore_var05 = rus_garch_roll_05@forecast$VaR[,1]
rus_fore_var1 = rus_garch_roll_1@forecast$VaR[,1]
plot(dates, rus_actual_var, type = "l", xlab = "Day", ylab = "Russia")
lines(dates, rus_fore_var01, col = "red")
lines(dates, rus_fore_var05, col = "blue")
lines(dates, rus_fore_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# --------------------------------------------#
# BACKTESTING FOR GARCH-VINE COPULA VAR MODEL #
# --------------------------------------------#
uniform_all = as.data.frame(uniform_all)

# -----------#
# Australia  #
# -----------#
# alpha 1%
aus_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_aus, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE,
                               VaR.alpha = 0.01)
# alpha 5%
aus_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_aus, n.ahead = 1, n.start = 2229,
                               refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                               VaR.alpha = 0.05)
# alpha 10%
aus_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_aus, n.ahead = 1, n.start = 2229,
                              refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                              VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for Australia
report(aus_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(aus_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # fro alpha 1% 
report(aus_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Canada     #
# -----------#
# alpha 1%
can_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_canada, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01)
# alpha 5%
can_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_canada, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
can_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_canada, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for Canada
report(can_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(can_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(can_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Germany    #
# -----------#
# alpha 1%
ger_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_germany, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE,
                                  VaR.alpha = 0.01)
# alpha 5%
ger_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_germany, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
ger_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_germany, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for Germany
report(ger_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(ger_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(ger_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Japan      #
# -----------#
# alpha 1%
jap_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_japan, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01)
# alpha 5%
jap_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_japan, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
jap_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_japan, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for Japan
report(jap_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(jap_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(jap_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# UK         #
# -----------#
# alpha 1%
uk_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_uk, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE,
                                  VaR.alpha = 0.01)
# alpha 5%
uk_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_uk, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
uk_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_uk, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for UK
report(uk_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(uk_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(uk_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%


# -----------#
# USA        #
# -----------#
# alpha 1%
usa_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_usa, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE,
                                  VaR.alpha = 0.01)
# alpha 5%
usa_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_usa, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
usa_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_usa, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for USA
report(usa_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(usa_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(usa_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%


# -----------#
# Bangladesh #
# -----------#
# alpha 1%
bang_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_bangladesh, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE,
                                  VaR.alpha = 0.01)
# alpha 5%
bang_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_bangladesh, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
bang_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_bangladesh, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for Bangladesh
report(bang_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(bang_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(bang_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# China      #
# -----------#
# alpha 1%
china_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_china, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE,
                                  VaR.alpha = 0.01)
# alpha 5%
china_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_china, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
china_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_china, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for China
report(china_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(china_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(china_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# India      #
# -----------#
# alpha 1%
ind_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_india, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01)
# alpha 5%
ind_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_india, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
ind_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_india, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for India
report(ind_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(ind_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # fro alpha 5% 
report(ind_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Indonesia  #
# -----------#
# alpha 1%
ina_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_indonesia, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE,
                                  VaR.alpha = 0.01)
# alpha 5%
ina_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_indonesia, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
ina_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_indonesia, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for Indonesia
report(ina_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(ina_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(ina_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Pakistan   #
# -----------#
# alpha 1%
pak_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_pakistan, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE,
                                  VaR.alpha = 0.01)
# alpha 5%
pak_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_pakistan, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
pak_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_pakistan, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for Pakistan
report(pak_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(pak_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(pak_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%

# -----------#
# Russia     #
# -----------#
# alpha 1%
rus_garchcop_roll_01 = ugarchroll(egarch_spec, data = uniform_all$z_russia, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.01)
# alpha 5%
rus_garchcop_roll_05 = ugarchroll(egarch_spec, data = uniform_all$z_russia, n.ahead = 1, n.start = 2229,
                                  refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                  VaR.alpha = 0.05)
# alpha 10%
rus_garchcop_roll_1 = ugarchroll(egarch_spec, data = uniform_all$z_russia, n.ahead = 1, n.start = 2229,
                                 refit.window = "moving", refit.every = 10, calculate.VaR = TRUE, 
                                 VaR.alpha = 0.1)

# Print the VAR Backtesting Garch Vine Copula for Russia
report(rus_garchcop_roll_01, type ="VaR", VaR.alpha = 0.01)
report(rus_garchcop_roll_05, type ="VaR", VaR.alpha = 0.05) # for alpha 5% 
report(rus_garchcop_roll_1, type ="VaR", VaR.alpha = 0.1) # for alpha 10%


# -------------------------------------------------------#
# Plot the Back testing Var from GARCH Vine Copula Model #
# -------------------------------------------------------#

# ----------#
# Australia #
# ----------#
aus_actual_cop_var = aus_garchcop_roll_01@forecast$VaR[,2]
aus_fore_cop_var01 = aus_garchcop_roll_01@forecast$VaR[,1]
aus_fore_cop_var05 = aus_garchcop_roll_05@forecast$VaR[,1]
aus_fore_cop_var1 = aus_garchcop_roll_1@forecast$VaR[,1]
plot(dates, aus_actual_cop_var, type = "l", xlab = "Day", ylab = "Australia", ylim = c(-0.5,1))
lines(dates, aus_fore_cop_var01, col = "red")
lines(dates, aus_fore_cop_var05, col = "blue")
lines(dates, aus_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# Canada    #
# ----------#
can_actual_cop_var = can_garchcop_roll_01@forecast$VaR[,2]
can_fore_cop_var01 = can_garchcop_roll_01@forecast$VaR[,1]
can_fore_cop_var05 = can_garchcop_roll_05@forecast$VaR[,1]
can_fore_cop_var1 = can_garchcop_roll_1@forecast$VaR[,1]
plot(dates, can_actual_cop_var, type = "l", xlab = "Day", ylab = "Canada", ylim = c(-0.5,1))
lines(dates, can_fore_cop_var01, col = "red")
lines(dates, can_fore_cop_var05, col = "blue")
lines(dates, can_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# Germany   #
# ----------#
ger_actual_cop_var = ger_garchcop_roll_01@forecast$VaR[,2]
ger_fore_cop_var01 = ger_garchcop_roll_01@forecast$VaR[,1]
ger_fore_cop_var05 = ger_garchcop_roll_05@forecast$VaR[,1]
ger_fore_cop_var1 = ger_garchcop_roll_1@forecast$VaR[,1]
plot(dates, ger_actual_cop_var, type = "l", xlab = "Day", ylab = "Germany", ylim = c(-0.5,1))
lines(dates, ger_fore_cop_var01, col = "red")
lines(dates, ger_fore_cop_var05, col = "blue")
lines(dates, ger_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# Japan     #
# ----------#
jap_actual_cop_var = jap_garchcop_roll_01@forecast$VaR[,2]
jap_fore_cop_var01 = jap_garchcop_roll_01@forecast$VaR[,1]
jap_fore_cop_var05 = jap_garchcop_roll_05@forecast$VaR[,1]
jap_fore_cop_var1 = jap_garchcop_roll_1@forecast$VaR[,1]
plot(dates, jap_actual_cop_var, type = "l", xlab = "Day", ylab = "Japan", ylim = c(-0.5,1))
lines(dates, jap_fore_cop_var01, col = "red")
lines(dates, jap_fore_cop_var05, col = "blue")
lines(dates, jap_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# UK        #
# ----------#
uk_actual_cop_var = uk_garchcop_roll_01@forecast$VaR[,2]
uk_fore_cop_var01 = uk_garchcop_roll_01@forecast$VaR[,1]
uk_fore_cop_var05 = uk_garchcop_roll_05@forecast$VaR[,1]
uk_fore_cop_var1 = uk_garchcop_roll_1@forecast$VaR[,1]
plot(dates, uk_actual_cop_var, type = "l", xlab = "Day", ylab = "UK", ylim = c(-0.5,1))
lines(dates, uk_fore_cop_var01, col = "red")
lines(dates, uk_fore_cop_var05, col = "blue")
lines(dates, uk_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# USA      #
# ----------#
usa_actual_cop_var = usa_garchcop_roll_01@forecast$VaR[,2]
usa_fore_cop_var01 = usa_garchcop_roll_01@forecast$VaR[,1]
usa_fore_cop_var05 = usa_garchcop_roll_05@forecast$VaR[,1]
usa_fore_cop_var1 = usa_garchcop_roll_1@forecast$VaR[,1]
plot(dates, usa_actual_cop_var, type = "l", xlab = "Day", ylab = "USA", ylim = c(-0.5,1))
lines(dates, usa_fore_cop_var01, col = "red")
lines(dates, usa_fore_cop_var05, col = "blue")
lines(dates, usa_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# -----------#
# Bangladesh #
# -----------#
bang_actual_cop_var = bang_garchcop_roll_01@forecast$VaR[,2]
bang_fore_cop_var01 = bang_garchcop_roll_01@forecast$VaR[,1]
bang_fore_cop_var05 = bang_garchcop_roll_05@forecast$VaR[,1]
bang_fore_cop_var1 = bang_garchcop_roll_1@forecast$VaR[,1]
plot(dates, bang_actual_cop_var, type = "l", xlab = "Day", ylab = "Bangladesh", ylim= c(-0.5,1))
lines(dates, bang_fore_cop_var01, col = "red")
lines(dates, bang_fore_cop_var05, col = "blue")
lines(dates, bang_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# China     #
# ----------#
china_actual_cop_var = china_garchcop_roll_01@forecast$VaR[,2]
china_fore_cop_var01 = china_garchcop_roll_01@forecast$VaR[,1]
china_fore_cop_var05 = china_garchcop_roll_05@forecast$VaR[,1]
china_fore_cop_var1 = china_garchcop_roll_1@forecast$VaR[,1]
plot(dates, china_actual_cop_var, type = "l", xlab = "Day", ylab = "China", ylim= c(-0.5,1))
lines(dates, china_fore_cop_var01, col = "red")
lines(dates, china_fore_cop_var05, col = "blue")
lines(dates, china_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# India     #
# ----------#
ind_actual_cop_var = ind_garchcop_roll_01@forecast$VaR[,2]
ind_fore_cop_var01 = ind_garchcop_roll_01@forecast$VaR[,1]
ind_fore_cop_var05 = ind_garchcop_roll_05@forecast$VaR[,1]
ind_fore_cop_var1 = ind_garchcop_roll_1@forecast$VaR[,1]
plot(dates, ind_actual_cop_var, type = "l", xlab = "Day", ylab = "India", ylim = c(-0.5,1))
lines(dates, ind_fore_cop_var01, col = "red")
lines(dates, ind_fore_cop_var05, col = "blue")
lines(dates, ind_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# Indonesia #
# ----------#
ina_actual_cop_var = ina_garchcop_roll_01@forecast$VaR[,2]
ina_fore_cop_var01 = ina_garchcop_roll_01@forecast$VaR[,1]
ina_fore_cop_var05 = ina_garchcop_roll_05@forecast$VaR[,1]
ina_fore_cop_var1 = ina_garchcop_roll_1@forecast$VaR[,1]
plot(dates, ina_actual_cop_var, type = "l", xlab = "Day", ylab = "Indonesia", ylim = c(-0.5,1))
lines(dates, ina_fore_cop_var01, col = "red")
lines(dates, ina_fore_cop_var05, col = "blue")
lines(dates, ina_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------#
# Pakistan  #
# ----------#
pak_actual_cop_var = pak_garchcop_roll_01@forecast$VaR[,2]
pak_fore_cop_var01 = pak_garchcop_roll_01@forecast$VaR[,1]
pak_fore_cop_var05 = pak_garchcop_roll_05@forecast$VaR[,1]
pak_fore_cop_var1 = pak_garchcop_roll_1@forecast$VaR[,1]
plot(dates, pak_actual_cop_var, type = "l", xlab = "Day", ylab = "Pakistan", ylim = c(-0.5,1))
lines(dates, pak_fore_cop_var01, col = "red")
lines(dates, pak_fore_cop_var05, col = "blue")
lines(dates, pak_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)


# ----------#
# Russia   #
# ----------#
rus_actual_cop_var = rus_garchcop_roll_01@forecast$VaR[,2]
rus_fore_cop_var01 = rus_garchcop_roll_01@forecast$VaR[,1]
rus_fore_cop_var05 = rus_garchcop_roll_05@forecast$VaR[,1]
rus_fore_cop_var1 = rus_garchcop_roll_1@forecast$VaR[,1]
plot(dates, rus_actual_cop_var, type = "l", xlab = "Day", ylab = "Russia", ylim = c(-0.5,1))
lines(dates, rus_fore_cop_var01, col = "red")
lines(dates, rus_fore_cop_var05, col = "blue")
lines(dates, rus_fore_cop_var1, col = "purple")
legend("topright", legend = c("Daily Return", "99% VaR", "95% VaR", "90% VaR"), 
       col = c("black", "red", "blue", "purple"), pch = 19)

# ----------------------------------------#
# DEPENDENCE STRUCTURE WITH C-VINE COPULA #
# ----------------------------------------#
# Fit Vine Copula  #
# ----------------#
cvine <- RVineStructureSelect(data = uniform_all,
                              progress = TRUE,
                              type = 'CVine',
                              selectioncrit = 'BIC',
                              treecrit = 'tau')


# -----------------------------------------------------------------------#
# Dependence Structure if data with R-Vine Copula (TABLE A1 APPENDIX)    #
# -----------------------------------------------------------------------#
dependence_structure_cvine <- summary(cvine)


# --------------------------------------#
# Plot the C-Vine Dependence Structure  #
# -------------------------------------#
plot(cvine)








