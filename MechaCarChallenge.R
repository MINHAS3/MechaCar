setwd("~/Bootcamp/BootCamp_1/Wk_15_R")

mpg <- read.csv("MechaCar_mpg.csv")

library(tidyverse)
library(ggpubr)

plot(mpg ~ ground.clearance, data = mpg)

# Y = beta0 + beta1*x1 + beta2*x
# mpg = beta0 + beta1 * vehicle.length + beta2*vehicle.weight + epsilon

mpg_out <- lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + AWD + ground.clearance,
              data = mpg)
summary(mpg_out)

abline(lm(mpg$mpg ~ mpg$spoiler.angle),col='red')

plot(mpg ~ ground.clearance, data = mpg)
abline(lm(mpg$mpg ~ mpg$ground.clearance),col='red')

suspension <- read.csv ("Suspension_Coil.csv")

summary(suspension$PSI)
var(suspension$PSI)
sd(suspension$PSI)

# T-test
# Null Hypothesis (what we test) : mean PSI = 1500
# Alt Hypothesis: PSI != 1500
t.test(x = suspension$PSI, mu = 1500, alternative = "two.sided")
newdata <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(min_PSI=min(PSI), max_PSI=max(PSI), med_PSI=median(PSI), mean_PSI=mean(PSI))
newdata
