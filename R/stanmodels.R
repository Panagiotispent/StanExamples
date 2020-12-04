library(rstan)
library(gdata)
library(bayesplot)

# Adding stringsAsFactors = F means that numeric variables won't be
# read in as factors/categorical variables
LifeExp <- read.csv("Life Expectancy Data.csv", stringsAsFactors = F)

#view data
head(LifeExp)

#change column names
#colnames(seaice) <- c("Country")

#Question is life expectancy affected by BMI?
plot(Life.expectancy ~ BMI, pch = 20, data = LifeExp)

# #general linear model
lm1 <- lm(Life.expectancy ~ BMI, data = LifeExp)
# summary(lm1)


#add fit to the plot 
abline(lm1, col = 2, lty = 2, lw = 3)

#rename for the sake of stan
x <- I(LifeExp$BMI)
y <- LifeExp$Life.expectancy
N <- length(LifeExp$BMI)

#plot that uses y,x
lm1 <- lm(y ~ x)
summary(lm1)

# key summary statistics from our simple model, so that we can compare them with the outputs of the Stan models
lm_alpha <- summary(lm1)$coeff[1]  # the intercept
lm_beta <- summary(lm1)$coeff[2]  # the slope
lm_sigma <- sigma(lm1)  # the residual error

#pass the data to stan
stan_data <- list(N = N, x = x, y = y)

# #creating and saving the stan model 
# write("// Stan model for simple linear regression
# 
# data {
#   int < lower = 1 > N; // Sample size
#   vector[N] x; // Predictor
#   vector[N] y; // Outcome
# }
# 
# parameters {
#   real alpha; // Intercept
#   real beta; // Slope (regression coefficients)
#   real < lower = 0 > sigma; // Error SD
# }
# 
# model {
#   y ~ normal(alpha + x * beta , sigma);
# }
# 
# 
# generated quantities {
# } // The posterior predictive distribution",
# 
# "stan_model1.stan")
# 
# #check the stan model 
# stanc("stan_model1.stan")

#running stan
fit <- stan(file = 'stan_model1.stan', data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 12, thin = 1)

#look at the posterior of our parameters
posterior <- extract(fit)
str(posterior)

#compare to the previous estimate
plot(y ~ x, pch = 20)

abline(lm1, col = 2, lty = 2, lw = 3)
abline( mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)

#One way to visualize the variability in our estimation of the regression line is to plot multiple estimates from the posterior.
plot(y ~ x, pch = 20)

for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}

abline(mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)

#second stan model with different posteriors
fit2 <- stan(file = "stan_model2.stan", data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 12, thin = 1)

posterior2 <- extract(fit2)

plot(y ~ x)
#graph both model to compare
abline(mean(posterior2$alpha), mean(posterior2$beta), col = 3, lw = 2)
abline(mean(posterior$alpha), mean(posterior$beta), col = 36, lw = 3)

#traceplots/convergence
plot(posterior$alpha, type = "l")
plot(posterior$beta, type = "l")
plot(posterior$sigma, type = "l")

#50 iterations model
fit_bad <- stan(file='stan_model1.stan', data = stan_data, warmup = 25, iter = 50, chains = 4, cores = 2, thin = 1)
posterior_bad <- extract(fit_bad)

#poor convergence
plot(posterior_bad$alpha, type = "l")
plot(posterior_bad$beta, type = "l")
plot(posterior_bad$sigma, type = "l")

#plot the non-Bayesian linear model values
par(mfrow = c(1,3))

plot(density(posterior$alpha), main = "Alpha")
abline(v = lm_alpha, col = 4, lty = 2)

plot(density(posterior$beta), main = "Beta")
abline(v = lm_beta, col = 4, lty = 2)

plot(density(posterior$sigma), main = "Sigma")
abline(v = lm_sigma, col = 4, lty = 2)

#diagnostic plots
traceplot(fit)

#posterior densities and historigrams
stan_dens(fit)
stan_hist(fit)

#Parameter estimates from the Stan model
plot(fit, show_density = FALSE, ci_level = 0.5, outer_level = 0.95, fill_color = "salmon")

#generated quantities fit3
fit3 <- stan(file='stan_model2_GQ.stan', data = stan_data, iter = 10000, chains = 4, cores = 12, thin = 1)

posterior3 <- extract(fit3)

plot(y ~ x)
abline(mean(posterior3$alpha), mean(posterior3$beta), col = 36, lw = 3)

plot(posterior3$alpha, type = "l")
plot(posterior3$beta, type = "l")
plot(posterior3$sigma, type = "l")