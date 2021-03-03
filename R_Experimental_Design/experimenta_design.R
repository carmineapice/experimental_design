setwd("C:/Users/SP453RH/Desktop/R_Experimental_Design")

#Exercises

# Section 2

# Questions
# 2.1.1 - Describe the steps in the planning process on the height of bread (see section 2.3)
# 2.1.2 - Describe the potential problems (see section 1.7)
# 2.1.3 - Discuss how careful planning using the checklist in Chapter 1

# Solutions
# 2.1.1, 2.12, 2.1.3

# Objectives: the objective of the study is to determine the effect of time to rise on the height of bread
# dought. 

# Experimental Unit: the experimental unit are the loaf pans. 
# Response/Dependent Variable: the variable is the measured height of a bread dought
# Indipendent Variables: the variable is amount of time in minutes
# Lurking Variables: any possible difference out of time affecting the heigth of bread loaf
# Pilot Test: let bread loaf growing at different time, realizing if we can control time extension
# and be able to correctly measure the height.
# Choose Experimental Design: We Choose the CRD with One Factor (time) and multiple levels (35,40,45) 
# affecting our measurable variable of interest (height)

# PLUS: the experiment should be run always in the same conditions (also the expternal temperature
# could affect the height.); We should be careful on the fact that bread loaf should be homogeneous.



# Questions 2
# 2.2.1 - The experimental unit is the single helicopter
# 2.2.2 - Replicates are the different experimental unit with different cut. Duplicates are essentially 
# the same experimental unit
# 2.2.3 - The treatment factor is the amount of centimeters we cut from the blades
# 2.2.4 - Lurking variables could be: the wind in that specific moment, the different weight of the paper
# any minimal difference in the height from which you leave the helicopter to fly;
# Randomization could be important since we can assess if the differences in the time to fly can be really affected
# by the differences in the cut level of the paper

# f) Create a randomized list of experiments for examining four wing lengths of 4, 4.75, 5.5, 6 with eight replicate
# experiment at each level

setwd("C:/Users/SP453RH/Desktop/R_Experimental_Design")

# Create the plan (exported to .txt)
set.seed(1)
f <- factor(rep(c(4, 4.75, 5.5, 6), each = 8))
fac <- sample(f, 32)
eu <- 1:32
plan <- data.frame(hel=eu, cuts=fac)
write.csv(plan, file="Plan.csv", row.names=FALSE)

# After the run of the experiment, we collect the data and import
# the Dataser
helicop <- read.table("Plan.txt", sep=",", header=T)
str(helicop)
helicop$cuts <- as.factor(helicop$cuts)
str(helicop)
helicop

# We run the ANOVA test
mod1 <- aov(result ~ cuts, data=helicop)
summary(mod1)

# From the ANOVA table we can conclude that we have
# significative differences among the means of the result 
# among the levels of cut applied to the blades.



# ---

plot(mod1, which=5)
plot(mod1, which=1)
plot(mod1, which=2)
plot(residuals(mod1)~ hel, data=helicop)
# From the plot showed above we can see that 
# the variability in the Std.Residuals vs. Factor Level Comb
# show an (even slightly) different variability in the errors.



# ---


library(MASS)
bc <- boxcox(mod1)
lambda <- bc$x[which.max(bc$y)]
lambda
# We look at with box-cox transformation 
# to see which is the normalizing parameter that
# could affect the variance of our error


#---

tres <- transform(helicop, tres=result^(1.757576))
helicop <- data.frame(helicop, tres)
mod2 <- aov(tres ~ cuts, data=helicop)
summary(mod2)
# Even if the result suggester to put 1.75 as lambda,
# we did not see significative improvement in the 
# P value of our anova test

#---

contrasts(helicop$cuts) <- contr.poly(4)
contrasts(helicop$cuts) 

mod3 <- aov(result ~ cuts, helicop)
summary.lm(mod3)
# From the result of the model above there a significative
# linear, quadratic and cubic trend.


# Example Constraints Table
con <- matrix(c(1, -1/3, -1/3, -1/3, 0, 1, -1, 0,
                + 0, 0, 1, -1 ), 4, 3 )
L <- t(con)
rownames(L) <- c("Nome1", "Nome2", "Nome3")

# N.B. In this case use fit.contrast





#

library(MASS)


library(daewr)
mod0 <- lm( result ~ cuts, data = helicop)
summary(mod0)

