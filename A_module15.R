# A_module15
# Drew Robison, Korik Vargas, and Connor Breton
# Last modified: December 4, 2017


# We'll continue to work with the data from last week.  The dataset includes mean February temperature (in ???C, Feb_temp), 
# mean snowfall (in cm, Feb_snow) and the day of year that plants leaf out (in days, leaf_DOY) around Durham, NH.  
# The goal of this analysis is to determine whether high February temperature and/or low February snowfall accelerate leaf 
# out, and to find the best fit model for our data given this set of predictors.  Remember that leaf out is a day of year, 
# so lower values indicate earlier leaf out.


setwd("C:/Users/Connor/Documents/word_files/graduate_courses/r/module_15")
#setwd("C:\\Users\\Drew\\Documents\\UNH\\Courses\\NR 995 - R\\Modules\\15")

# Read in data and do exmaine data basics
Pheno = read.table("Bayes_pheno.csv", sep=",", header = T)
head(Pheno)
summary(Pheno) # no missing data
str(Pheno)

# Check relationships between predictor and response variables
plot(Pheno$Feb_temp, Pheno$leaf_DOY) # Apparent negative correlation
plot(Pheno$Feb_snow, Pheno$leaf_DOY) # No obvious correlation
plot(Pheno$Feb_snow, Pheno$Feb_temp) # No obvious correlation


# Check collinearity
hist(Pheno$Feb_temp) 
hist(Pheno$Feb_snow) 
hist(Pheno$leaf_DOY) 
# All histograms are approximately normally distributed

cor.test(Pheno$Feb_temp, Pheno$leaf_DOY) # Significant negative correlation
cor.test(Pheno$Feb_snow, Pheno$leaf_DOY) # No statistically significant correlation
cor.test(Pheno$Feb_snow, Pheno$Feb_temp) # No statistically significant correlation


###############################################################################################################################################
# (1)	Complete a DIC-based model selection using three candidate models (a) leaf_DOY ~ Feb_temp, (b) leaf_DOY ~ Feb_snow, 
# and (c) leaf_DOY ~ Feb_temp.  Use three chains per model and non-informative priors.  Be sure to check that each model 
# has converged (tune as needed). Which model is best supported by the data?  Briefly justify your choice.

# Load the rjags package and write the JAGS model
library(rjags)


################################### Model 1 ###################################
model_string.1 <- "model{

# Likelihood
for (i in 1:Ntotal){
y[i] ~ dnorm(mu[i], inv.var)
mu[i] <- beta[1] + beta[2]*x1[i]
}

# Priors for beta, one for each beta parameter
for(j in 1:2){
beta[j] ~ dnorm(0, 0.0001)
}

# Prior for inverse variance
inv.var ~ dgamma(0.01, 0.01)

}"

# Data setup
list.1 = list(y = Pheno$leaf_DOY, x1 = Pheno$Feb_temp, Ntotal = length(Pheno$leaf_DOY)) 
# List with response variable, predictor variables, and number of samples


# Get model suited with 3 chains
model.1 <- jags.model(textConnection(model_string.1), data = list.1, n.chains = 3)


# Update model with burn-in iterations (50000)
update(model.1, 50000)


# Sample the posterior with 100000 iterations
samp.1 <- coda.samples(model.1, variable.names = c("beta"), n.iter = 100000, thin = 5) 


# Plot summary parameters
plot(samp.1)
# The three chains run horizontally across each "Trace of beta[x]" plot and sufficiently mix across the 
# length/number of iterations. Each "Density of beta[x]" plot approximates a normal distribution and has 
# a relatively smooth curve.


# Plot autocorrelation
autocorr.plot(samp.1)
# Looks fine since autocorrelation rapidly decreases from one to zero. Light thinning was required to achieve this.

# Gelman-Brooks-Rubin plot
gelman.plot(samp.1)
# Each plot is ideal in that the two lines (97.5% and median) converge and rapidly decrease/remain at a value of one.


# Compile model 1 again, but with a new name
model.1.v2 <- jags.model(textConnection(model_string.1), 
                      data = list.1, n.chains = 3)


# Burn-in iterations
update(model.1.v2, 50000)


# Take DIC samples instead of coda samples
dic.1 <- dic.samples(model.1.v2, n.iter = 100000, thin = 5, type = "pD")


# Return DIC value
dic.1


################################### Model 2 ###################################
model_string.2 <- "model{

# Likelihood
for (i in 1:Ntotal){
y[i] ~ dnorm(mu[i], inv.var)
mu[i] <- beta[1] + beta[2]*x1[i]
}

# Priors for beta, one for each beta parameter
for(j in 1:2){
beta[j] ~ dnorm(0, 0.0001)
}

# Prior for inverse variance
inv.var ~ dgamma(0.01, 0.01)

}"

# Data setup
list.2 = list(y = Pheno$leaf_DOY, x1 = Pheno$Feb_snow, Ntotal = length(Pheno$leaf_DOY)) 
# List with response variable, predictor variables, and number of samples


# Get model suited with 3 chains
model.2 <- jags.model(textConnection(model_string.2), data = list.2, n.chains = 3)


# Update model with burn-in iterations (50000)
update(model.2, 50000)


# Sample the posterior with 600000 iterations
samp.2 <- coda.samples(model.2, variable.names = c("beta"), n.iter = 100000, thin = 60) 


# Plot summary parameters
plot(samp.2)
# The three chains run horizontally across each "Trace of beta[x]" plot and sufficiently mix across the 
# length/number of iterations. Each "Density of beta[x]" plot approximates a normal distribution and has
# a relatively smooth curve.


# Plot autocorrelation
autocorr.plot(samp.2)
# Looks fine since autocorrelation rapidly decreases from one to zero. Heavy thinning was required to achieve this.


# Gelman-Brooks-Rubin plot
gelman.plot(samp.2)
# Each plot is ideal in that the two lines (97.5% and median) converge and rapidly decrease/remain at a value of one.


# Compile model 2 again, but with a new name
model.2.v2 <- jags.model(textConnection(model_string.2), 
                         data = list.2, n.chains = 3)


# Burn-in iterations
update(model.2.v2, 50000)


# Take DIC samples instead of coda samples
dic.2 <- dic.samples(model.2.v2, n.iter = 100000, thin = 60, type = "pD")


# Return DIC value
dic.2


################################### Model 3 ###################################
model_string.3 <- "model{

# Likelihood
for (i in 1:Ntotal){
y[i] ~ dnorm(mu[i], inv.var)
mu[i] <- beta[1] + beta[2]*x1[i] + beta[3]*x2[i]
}

# Priors for beta, one for each beta parameter
for(j in 1:3){
beta[j] ~ dnorm(0, 0.0001)
}

# Prior for inverse variance
inv.var ~ dgamma(0.01, 0.01)

}"

# Data setup
list.3 = list(y = Pheno$leaf_DOY, x1 = Pheno$Feb_temp, x2 = Pheno$Feb_snow, Ntotal = length(Pheno$leaf_DOY)) 
# List with response variable, predictor variables, and number of samples


# Get model suited with 3 chains
model.3 <- jags.model(textConnection(model_string.3), data = list.3, n.chains = 3)


# Update model with burn-in iterations (40000)
update(model.3, 50000)


# Sample the posterior with 500000 iterations
samp.3 <- coda.samples(model.3, variable.names = c("beta"), n.iter = 100000, thin = 50) 


# Plot summary parameters
plot(samp.3)
# The three chains run horizontally across each "Trace of beta[x]" plot and sufficiently mix across the 
# length/number of iterations. Each "Density of beta[x]" plot approximates a normal distribution and has 
# a relatively smooth curve.


# Plot autocorrelation
autocorr.plot(samp.3)
# Looks fine since autocorrelation rapidly decreases from one to zero. Heavy thinning was required to achieve this.


# Gelman-Brooks-Rubin plot
gelman.plot(samp.3)
# Each plot is ideal in that the two lines (97.5% and median) converge and rapidly decrease/remain at a value of one.


# Compile model 3 again, but with a new name
model.3.v2 <- jags.model(textConnection(model_string.3), 
                         data = list.3, n.chains = 3)


# Burn-in iterations
update(model.3.v2, 50000)


# Take DIC samples instead of coda samples
dic.3 <- dic.samples(model.3.v2, n.iter = 100000, thin = 50, type = "pD")


# Return DIC value
dic.3

################################### Model Comparison ###################################
# Compare models via DIC
dic.1-dic.2 # Value is negative, DIC.1 is smaller than DIC.2
dic.1-dic.3 # Value is negative, DIC.1 is smaller than DIC.3
dic.2-dic.3 # Value is positive, DIC.3 is smaller than DIC.2

# Model.1 (only Feb_temp as predictor) is best supported by the data and has the lowest DIC value of the 3 compiled models.
# We can be confident in this selection since all three compiled models were of acceptable quality during model diagnostics tests.


###############################################################################################################################################
# (2)	Interpret your best fit model from question 1.  Do high February temperature and/or low February snowfall accelerate 
# leaf out?  By how much (including units)?  

summary(samp.1)

# February temperature accelerates leaf off at a rate of roughly 0.71 days per degree Celsius increase in mean February temperature.
# This result is significant since the 95% credible interval for the estimate does not overlap zero.


###############################################################################################################################################
# (3)	Previous research suggests that warm February temperatures accelerate leaf out date by about 1 day on average, with 
# a standard deviation of 0.5 days.  Put an informative prior on the parameter for Feb_temp and re-run your 
# leaf_DOY ~ Feb_temp model.  Briefly describe changes in model performance (burn-in, number of iterations, and/or thinning
# needed) and the parameter estimates as compared to the equivalent model with non-informative priors.


model_string.4 <- "model{

# Likelihood
for (i in 1:Ntotal){
y[i] ~ dnorm(mu[i], inv.var)
mu[i] <- beta[1] + beta[2]*x1[i]
}


# Priors for beta (no more loop since they're not the same)
beta[1] ~ dnorm(0, 0.001)
beta[2] ~ dnorm(-1.0, 4.0) #informative prior, std.dev =0.5, therefore variance is .25 and inverse variance is 1/.25 = 4


# Prior for inverse variance
inv.var ~ dgamma(0.01, 0.01)

}"

# Data setup
list.4 = list(y = Pheno$leaf_DOY, x1 = Pheno$Feb_temp, Ntotal = length(Pheno$leaf_DOY)) 
# List with response variable, predictor variables, and number of samples


# Get model suited with 3 chains
model.4 <- jags.model(textConnection(model_string.4), data = list.4, n.chains = 3)


# Update model with burn-in iterations (50000)
update(model.4, 50000)


# Sample the posterior with 100000 iterations
samp.4 <- coda.samples(model.4, variable.names = c("beta"), n.iter = 100000, thin = 5) 


# Plot summary parameters
plot(samp.4)
# Similar to original model (model.1) with uninformative priors. The three chains run horizontally across 
# each "Trace of beta[x]" plot and sufficiently mix across the length/number of iterations. Each 
# "Density of beta[x]" plot approximates a normal distribution and has a relatively smooth curve.


# Plot autocorrelation
autocorr.plot(samp.4)
# Again, similar to original model (model.1). Looks fine since autocorrelation rapidly decreases from one 
# to zero. Light thinning was required to achieve this.


# Gelman-Brooks-Rubin plot
gelman.plot(samp.4)
# Again, similar to original model (model.1). Each plot is ideal in that the two lines (97.5% and median)
# converge and rapidly decrease/remain at a value of one.


# Compile model 4 again, but with a new name
model.4.v2 <- jags.model(textConnection(model_string.4), 
                         data = list.4, n.chains = 3)


# Burn-in iterations
update(model.4.v2, 50000)


# Take DIC samples instead of coda samples
dic.4 <- dic.samples(model.4.v2, n.iter = 100000, thin = 5, type = "pD")


# Return DIC value
dic.4


# Compare models via DIC
dic.1-dic.4 # Value is positive, DIC.4 is smaller than DIC.1


# Summarize model.4
summary(samp.4)

# Predictions/estimates of model.4 are similar to that of model.1. Leaf out is expected to occur roughly 0.72 
# days quicker for each degree Celsius increase in mean February temperature. This result is significant since 
# the 95% credible interval does not overalp zero. Transitioning from model.1 without informative priors to 
# model.4 with informative priors, no changes to model.1 parameters (i.e. burn-ins, iterations, thinning) were 
# necessary to suit model.4 diagnostics. 


###############################################################################################################################################
# In 1-2 sentences, identify the contribution of each group member to the assignment. 

# All group members individually completed the assignment and then met to compile, test, and edit code. Connor's base
# code was used for submission, but Drew and Korik lead efforts to test, edit, and heavily comment.
