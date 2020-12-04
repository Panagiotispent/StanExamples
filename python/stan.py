#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 22 10:48:08 2020

@author: panayiotis
"""


import pystan
import arviz

schools_code = """
data {
    int<lower=0> J; // number of schools
    vector[J] y; // estimated treatment effects
    vector<lower=0>[J] sigma; // s.e. of effect estimates
}
parameters {
    real mu;
    real<lower=0> tau;
    vector[J] eta;
}
transformed parameters {
    vector[J] theta;
    theta = mu + tau * eta;
}
model {
    eta ~ normal(0, 1);
    y ~ normal(theta, sigma);
}
"""

schools_dat = {'J': 8,
               'y': [28,  8, -3,  7, -1,  1, 18, 12],
               'sigma': [15, 10, 16, 11,  9, 11, 10, 18]}


#use the data from above
#sm = pystan.StanModel(model_code=schools_code)

#Or load from file
sm = pystan.StanModel(file='8schools.stan')

fit = sm.sampling(data=schools_dat, iter=1000, chains=4)


## return an array of three dimensions: iterations, chains, parameters
a = fit.extract(permuted=False)

print(fit)

arviz.plot_trace(fit)