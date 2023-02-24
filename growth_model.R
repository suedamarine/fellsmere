# import libraries
library(tidyverse)

# import data
shrimp_growout <- read.csv("data/shrimp_growout.csv")

# define parameters
bw_0 <- 0.04
quarantine.max <- 1.2
nursery.max <- 3
juvenile.max <- 10
growout.max <- 32
nursery.harvest <- 5

# find weight gain for weight midpoint
shrimp.growth <- shrimp_growout %>%
  mutate(wt.mid = (wt_2 + wt_1) / 2,
         g.gain.day = (wt_2 - wt_1) / (t_2 - t_1),
         geometric.mid = sqrt(wt_1 * wt_2))

# try base code log-log relationship - looks good!
shrimp.fit.twinlog <- lm(log(g.gain.day) ~ log(wt.mid), data = shrimp.growth)
summary(shrimp.fit.twinlog)

plot(log(g.gain.day) ~ log(wt.mid), data = shrimp.growth, col = "grey", pch = 20, cex = 1.5, main = "g per day by weight")
abline(shrimp.fit.twinlog, col = "darkorange", lwd = 2)

plot(fitted(shrimp.fit.twinlog), resid(shrimp.fit.twinlog), col = "grey", pch = 20,
     xlab = "fitted", ylab = "residuals", main = "fitted vs residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(shrimp.fit.twinlog), main = "normal Q-Q plot", col = "darkgrey")
qqline(resid(shrimp.fit.twinlog), col = "dodgerblue", lwd = 2)

plot(g.gain.day ~ wt.mid, data = shrimp.growth, col = "grey", pch = 20, cex = 1.5, main = "g per day gained")
curve(exp(shrimp.fit.twinlog$coef[1]) * x ^ shrimp.fit.twinlog$coef[2], from = 0, to = 32, add = TRUE, col = "darkorange", lwd = 2)

# calculate coefficients for growth curve bw_1 = [bw_0^c_1 + c_2 * days]^c_4
c_1 <- 1 - shrimp.fit.twinlog$coef[2]
c_2 <- c_1 * exp(shrimp.fit.twinlog$coef[1])
c_4 <- 1 / c_1


# set some colours
cols <- c("quarantine" = "#bdc9e1", "nursery" = "#74a9cf", "juvenile" = "#2b8cbe", "growout" = "#045a8d")

# plot relationship between weight and grams gained per day 
p_shrimp_gain <- shrimp.growth %>%
  ggplot(aes(wt.mid, g.gain.day, color = phase)) +
  geom_point() +
  scale_color_manual(name = "Phase",
  labels = c("quarantine" = "Quarantine", "nursery" = "Nursery", "juvenile" = "Juvenile", "growout" = "Growout"),
  values = c(cols)) +
  stat_function(fun = function(x) exp(shrimp.fit.twinlog$coef[1]) * x^shrimp.fit.twinlog$coef[2], color = 'dodgerblue') +
  stat_function(fun = function(x) exp(shrimp.geometric.twinlog$coef[1]) * x^shrimp.geometric.twinlog$coef[2], color = 'black') +
  xlab("Mass (g)") +
  ylab("Daily mass gain (g/shrimp/day)") +
  theme_minimal()

# print plot to file
pdf("plots/p_shrimp_gain.pdf", height = 4)
print(p_shrimp_gain)
dev.off()

# try base code log-log relationship for geometric weight
shrimp.geometric.twinlog <- lm(log(g.gain.day) ~ log(geometric.mid), data = shrimp.growth)
summary(shrimp.geometric.twinlog)

plot(log(g.gain.day) ~ log(geometric.mid), data = shrimp.growth, col = "grey", pch = 20, cex = 1.5, main = "g per day by weight")
abline(shrimp.geometric.twinlog, col = "darkorange", lwd = 2)

plot(g.gain.day ~ geometric.mid, data = shrimp.growth, col = "grey", pch = 20, cex = 1.5, main = "g per day gained")
curve(exp(shrimp.geometric.twinlog$coef[1]) * x ^ shrimp.geometric.twinlog$coef[2], from = 0, to = 30, add = TRUE, col = "darkorange", lwd = 2)

# calculate coefficients for growth curve bw_1 = [bw_0^c_1 + c_2 * days]^c_4
geometric_c_1 <- 1 - shrimp.geometric.twinlog$coef[2]
geometric_c_2 <- c_1 * exp(shrimp.geometric.twinlog$coef[1])
geometric_c_4 <- 1 / geometric_c_1

# plot relationship between weight and grams gained per day 
p_geometric_gain <- shrimp.growth %>%
  ggplot(aes(geometric.mid, g.gain.day)) +
  geom_point() +
  stat_function(fun = function(x) exp(shrimp.geometric.twinlog$coef[1]) * x^shrimp.geometric.twinlog$coef[2], color = 'dodgerblue') +
  xlab("Mass (g)") +
  ylab("Daily mass gain (g/shrimp/day)") +
  theme_minimal()

# print plot to file
pdf("plots/p_geometric_gain.pdf", height = 4)
print(p_geometric_gain)
dev.off()

# estimate body-weight over time
bodyweight <- tibble(day = seq(1,180,1)) %>%
  mutate(bw = (bw_0^c_1 + c_2 * day)^c_4)

# export csv
write.csv(bodyweight, "tabs/bodyweight.csv")

# import fellsmere growth data
fellsmere.growth <- read.csv("data/fellsmere_growth.csv")

# create model to incorporate fellsmere growth data
bodyweight.model <- bodyweight %>%
  mutate(data.source = "model") %>%
  bind_rows(fellsmere.growth)

# shrimp growth model compared to fellsmere
shrimp.model.plot <- 
  bodyweight.model %>%
  ggplot(aes(day, bw, color = data.source)) +
  geom_line() +
  stat_function(fun = function(x) (bw_0^geometric_c_1 + geometric_c_2 * x)^geometric_c_4, color = 'black') +
  labs(x = "Days", y = "Mass (g)") +
  scale_color_discrete(name = "Data source", labels = c("Fellsmere", "Model")) +
  theme_minimal()

# plot estimated growth curve
shrimp.growth.plot <- bodyweight %>%
  ggplot(aes(day, bw)) +
  geom_line() +
  labs(x = "Days", y = "Mass (g)") +
  theme_minimal()

# print plot to file
pdf("plots/shrimp_growth_plot.pdf", height = 4)
print(shrimp.growth.plot)
dev.off()

# estimate body-weight over time geometric model
bodyweight_geometric <- tibble(day = seq(1,180,1)) %>%
  mutate(bw = (bw_0^geometric_c_1 + geometric_c_2 * day)^geometric_c_4)

# write csv
write.csv(bodyweight_geometric, "tabs/bodyweight_geometric.csv")

# plot estimated growth curve geometric
shrimp.geometric.plot <- bodyweight_geometric %>%
  ggplot(aes(day, bw)) +
  geom_line() +
  labs(x = "Days", y = "Mass (g)") +
  theme_minimal()

# find when key bodyweights attained
shrimp.1_2.g <- ceiling(which(abs(bodyweight$bw - quarantine.max) == min(abs(bodyweight$bw - quarantine.max))))

shrimp.3.g <- ceiling(which(abs(bodyweight$bw - nursery.max) == min(abs(bodyweight$bw - nursery.max))))

shrimp.5.g <- ceiling(which(abs(bodyweight$bw - nursery.harvest) == min(abs(bodyweight$bw - nursery.harvest))))

shrimp.10.g <- ceiling(which(abs(bodyweight$bw - juvenile.max) == min(abs(bodyweight$bw - juvenile.max))))

shrimp.32.g <- ceiling(which(abs(bodyweight$bw - growout.max) == min(abs(bodyweight$bw - growout.max))))

shrimp.25.g <- ceiling(which(abs(bodyweight$bw - 25) == min(abs(bodyweight$bw - 25))))


