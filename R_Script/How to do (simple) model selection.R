### HOW TO MAKE A MODEL WITH POLYNOMIAL TERMS ###
#################################################

### If you suspect that there might be some polynomial terms in the appropriate model
### for your data, this i how you add it - it is very similar to how you fit a standard
### linear model, with a few minor differences. I'll use the (rather bad) model from the
### original script - you should of course think of some better variables.
### OBS! Think very thoroughly, if you should include quadractic term (and if yes, which
### variable) - it is not always a good thing to do


### The original model from the script:
glm_devel<-with(sdmdata_devel,                                                                    # This is the data we're using
                glm(pb_devel~Min.Temperature.of.Coldest.Month + Precipitation.of.Wettest.Month,   # This is the formula - only linear terms
                    "binomial"))                                                                  # The 'family' - we're fitting a binomial (0/1) model

### A model with quadratic terms ('making the graph curve') - I will here make the quadratic term
### for the precipitation
glm_devel_quadratic <-with(sdmdata_devel,
                           glm(pb_devel~Min.Temperature.of.Coldest.Month + Precipitation.of.Wettest.Month + I(Precipitation.of.Wettest.Month^2),
                               "binomial"))     
### The way of writing the quadratic term (I(x^2)) is just syntax - that's just how R reads things
summary(glm_devel)
summary(glm_devel_quadratic)

### In this case, you can see some differences in the output


### HOW TO DO SOME SIMPLE MODEL SELECTION ###
#############################################

### You can always build a model, and you can even build A GOOD model with the variables
### you have picked out, but the question is: have you made THE BEST model?

### To check that, you have to do some model selection.
###  do model selection on the models I used in the example above:

### What this function does is to remove one variable at a time, until it gets the best (lowest) AIC.
### I'll show and explain the results/the output - look at my comments ot the right, after the #-sign:
step(glm_devel_quadratic)

# Start:  AIC=391.38                                                                     # The AIC of the model you wanted to chech
# pb_devel ~ Min.Temperature.of.Coldest.Month + Precipitation.of.Wettest.Month + 
#   I(Precipitation.of.Wettest.Month^2)

#                                        Df Deviance   AIC
# - Precipitation.of.Wettest.Month       1   383.53 389.53        # The AIC if you remove Precipitation.of.Wettest.Month
# - I(Precipitation.of.Wettest.Month^2)  1   385.13 391.13        # The AIC if you remove (Precipitation.of.Wettest.Month)^2 - the quadratic term
# <none>                                     383.38 391.38        # The AIC if you remove nothing
# - Min.Temperature.of.Coldest.Month     1   551.76 557.76        # The AIC if you remove the Min.Temperature.of.Coldest.Month

# Step:  AIC=389.53                                                                    # It now takes the best model and continues with that
# pb_devel ~ Min.Temperature.of.Coldest.Month + I(Precipitation.of.Wettest.Month^2)

#                                       Df Deviance    AIC
# <none>                                     383.53 389.53        # The AIC if you remove nothing
# - I(Precipitation.of.Wettest.Month^2)  1   511.61 515.61        # The AIC if you remove (Precipitation.of.Wettest.Month)^2 - the quadratic term
# - Min.Temperature.of.Coldest.Month     1   571.70 575.70        # The AIC if you remove the Min.Temperature.of.Coldest.Month


# Call:  glm(formula = pb_devel ~ Min.Temperature.of.Coldest.Month + I(Precipitation.of.Wettest.Month^2), 
#            family = "binomial")

# Coefficients:
#   (Intercept)     Min.Temperature.of.Coldest.Month  I(Precipitation.of.Wettest.Month^2)  
# 5.2248920                            0.0592481                           -0.0001248  

# Degrees of Freedom: 612 Total (i.e. Null);  610 Residual
# (38 observations deleted due to missingness)
# Null Deviance:	    585.9 
# Residual Deviance: 383.5 	AIC: 389.5




### In this case: the best model would be the one with AIC=389.53 --> pb_devel ~ Min.Temperature.of.Coldest.Month + I(Precipitation.of.Wettest.Month^2)
### OBS! As a rule of thumb, if the difference in AIC is less than 2, go for the simplest model

### You can get a little simpler overview of the things from above with this function:
drop1(glm_devel_quadratic, test="Chi")

# Single term deletions

# Model:
#   pb_devel ~ Min.Temperature.of.Coldest.Month + Precipitation.of.Wettest.Month + 
#   I(Precipitation.of.Wettest.Month^2)
#                                     Df Deviance    AIC     LRT Pr(>Chi)    
# <none>                                   383.38 391.38                     
# Min.Temperature.of.Coldest.Month     1   551.76 557.76 168.382   <2e-16 ***
#   Precipitation.of.Wettest.Month     1   383.53 389.53   0.143   0.7049    
# I(Precipitation.of.Wettest.Month^2)  1   385.13 391.13   1.752   0.1856

