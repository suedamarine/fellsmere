# import libraries
library(tidyverse)

# import data
feed <- read.csv("data/feed.csv")

feed %>% ggplot(aes(wt, feed_pc, color = phase)) +
  geom_point()

feed %>% filter(phase == "quarantine") %>%
  ggplot(aes(log(wt), log(feed_pc))) +
  geom_point()

# try base code log-log relationship for geometric weight
quarantine.feed <- feed %>%
  filter(phase == "quarantine")

juvenile.feed <- feed %>%
  filter(phase == "juvenile")

juvenile.nursery.feed <- feed %>%
  filter(phase == "juvenile" | phase == "nursery")

quarantine.feed.twinlog <- lm(log(feed_pc) ~ log(wt), data = quarantine.feed)
summary(quarantine.feed.twinlog)

plot(log(feed_pc) ~ log(wt), data = quarantine.feed, col = "grey", pch = 20, cex = 1.5, main = "feed_wt")
abline(quarantine.feed.twinlog, col = "darkorange", lwd = 2)

plot(feed_pc ~ wt, data = quarantine.feed, col = "grey", pch = 20, cex = 1.5, main = "Feed to weight")
curve(exp(quarantine.feed.twinlog$coef[1]) * x ^ quarantine.feed.twinlog$coef[2], from = 0, to = 1.5, add = TRUE, col = "darkorange", lwd = 2)

feed %>% filter(phase == "nursery") %>%
  ggplot(aes(wt, feed_pc)) +
  geom_point()

feed %>% filter(phase == "juvenile") %>%
  ggplot(aes(wt, log(feed_pc))) +
  geom_point()

juvenile.feed.log <- lm(log(feed_pc) ~ wt, data = juvenile.feed)
summary(juvenile.feed.log)

plot(log(feed_pc) ~ wt, data = juvenile.feed, col = "grey", pch = 20, cex = 1.5, main = "feed_wt")
abline(juvenile.feed.log, col = "darkorange", lwd = 2)

plot(feed_pc ~ wt, data = juvenile.feed, col = "grey", pch = 20, cex = 1.5, main = "Feed to weight")
curve(exp(juvenile.feed.log$coef[1] + x * juvenile.feed.log$coef[2]), from = 3, to = 10, add = TRUE, col = "darkorange", lwd = 2)

feed %>% filter(phase == "growout") %>%
  ggplot(aes(wt, feed_pc)) +
  geom_point()

feed.plot <- feed %>%
  ggplot(aes(wt, feed_pc, color = phase)) +
  geom_line()+
  stat_function(fun = function(x) exp(quarantine.feed.twinlog$coef[1]) * x ^ quarantine.feed.twinlog$coef[2],
                color = 'orange', xlim = c(0.007, 1.2), alpha = 0.4) +
  stat_function(fun = function(x) nursery.feed,
                color = "orange", xlim = c(1.2, 3.0), alpha = 0.4) +
  stat_function(fun = function(x) exp(juvenile.feed.log$coef[1] + x * juvenile.feed.log$coef[2]),
                color = 'orange', xlim = c(3.0, 10), alpha = 0.4) +
  stat_function(fun = function(x) growout.feed,
                color = "orange", xlim = c(10, 32), alpha = 0.4) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "juvenile", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Juvenile", "Growout", "Depurate")) +
  labs(x = "Shrimp bodyweight (g)", y = "Feed as percentage of bodyweight (%)") + 
  theme_minimal()

pdf("plots/feed_plot.pdf", height = 4)
feed.plot
dev.off()

# create feed model for combined nursery/ juvenile phase for primo and 101
juvenile.nursery.feed.log <- lm(log(feed_pc) ~ wt, data = juvenile.nursery.feed)
summary(juvenile.nursery.feed.log)

plot(log(feed_pc) ~ wt, data = juvenile.nursery.feed, col = "grey", pch = 20, cex = 1.5, main = "feed_wt")
abline(juvenile.nursery.feed.log, col = "darkorange", lwd = 2)

plot(feed_pc ~ wt, data = juvenile.nursery.feed, col = "grey", pch = 20, cex = 1.5, main = "Feed to weight")
curve(exp(juvenile.nursery.feed.log$coef[1] + x * juvenile.nursery.feed.log$coef[2]), from = 1.0, to = 10, add = TRUE, col = "darkorange", lwd = 2)
