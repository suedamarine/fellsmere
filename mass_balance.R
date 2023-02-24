# load libraries
library(tidyverse)

# define water quality parameters
desired.tss <- 250 # max 50 mg/l in tanks
desired.o2 <- 5.0 # min 5 mg/l in tanks
desired.tan <- 1 # max 1 mg/l TAN in tanks
desired.nitrate <- 50 # max 50 mg/l nitrate in tanks
desired.co2 <- 10 # max 15 mg/l in tanks
desired.t <- 30 # system operating temp in C
site.altitude <- 10 # site altitude in metres
cone.bar <- 0.7
feeding.period <- 24
metabolic.period <- case_when(feeding.period < 20 ~ feeding.period + 4, TRUE ~ 24)

# constants
k <- 1.42903
beta.oxygen <- exp(-58.3877 + 85.8079 *  (100/(desired.t + 273.15)) + 23.8439 * log((desired.t+273.15) / 100))
barometric.pressure <- 10^(2.88014 - site.altitude/19748.2)
water.v.pressure <- 4.7603 * exp(0.0645 * desired.t)
oxygen.percent <- 20.95
oxygen.saturation <- 1000 * k * beta.oxygen * oxygen.percent / 100 * ((barometric.pressure - water.v.pressure) / 760) # if cone used at pressure swap out barometric.pressure for cone.pressure
cone.pressure <- cone.bar * 750.06 + barometric.pressure
oxygen.saturation.saline <- oxygen.saturation * 0.8

# component specifics
# system efficiency 
tan.c1 <- desired.tan
tan.cbest <- 0
tan.te <- 0.9
tan.c2 <- tan.c1 + tan.te * (tan.cbest - tan.c1)

co2.c1 <- desired.co2
co2.cbest <- 0.5
co2.te <- 0.5
co2.c2 <- co2.c1 + co2.te * (co2.cbest - co2.c1)

tss.c1 <- desired.tss
tss.cbest <- 0
tss.te <- 0.21
tss.c2 <- tss.c1 + tss.te * (tss.cbest - tss.c1)

oxy.c1 <- desired.o2 # min 5 mg/l in tanks
oxy.cbest <- oxygen.saturation.saline
oxy.te <- 0.50
oxy.c2 <- oxy.c1 + oxy.te * (oxy.cbest - oxy.c1)


