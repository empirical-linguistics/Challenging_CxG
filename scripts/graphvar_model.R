library(tidyverse)
library(pscl)
library(MASS)
library(lme4)
library(effects)
library(glmmTMB)
library(AER)


# read data ---------------------------------------------------------------

d <- read_csv("../data/graphvar_summary_1980ff.csv")


# grades:
d$grade %>% hist

# errors:
d$errors %>% hist

# zeros?
any(d$errors==0)

# minimum and maximum
range(d$errors)


# fit a Poisson model
m <- glm(errors ~ year + subject + gender, 
    family = "poisson", data = d)
summary(m)

# test for overdispersion
pr <- resid(m, type = "pearson") # residuals
pchi2 <- sum(residuals(m, type="pearson")^2) #Pearson chi-squared
disp <- pchi2/m$df.residual # Pearson disperson statistic
pchi2; disp

# fit a negative binomial model
m2 <- glm.nb(errors ~ year + subject + gender, data = d)
summary(m2)


