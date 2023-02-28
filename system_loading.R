# load libraries
library(tidyverse)

# define parameters
quarantine.survival <- 0.8
nursery.survival <- 0.8
juvenile.survival <- 0.8
growout.survival <- 0.8

quarantine.in <- 35000
nursery.in <- 19500
juvenile.in <- 15000
growout.in <- 11000
growout.out <- 8800
depurate.in <- 1600

quarantine.system.division <- 1
nursery.system.division <- 3
juvenile.system.division <- 1
growout.system.division <- 2
depurate.system.division <- 1
depurate.tanks <- 4

initial.stock <- 35000

nursery.feed <- 0.05
growout.feed <- 0.04
depurate.feed <- 0

quarantine.protein <- 0.5
nursery.protein <- 0.4
juvenile.protein <- 0.4
growout.protein <- 0.35
depurate.protein <- 0.35



# build system loading onto production plan
system.loading <- prod.plan %>%
  mutate(total.number = case_when(system == "quarantine" ~ initial.stock * exp(1)^(log(quarantine.survival) / quarantine.period * batch_day),
                                  system == "nursery" ~ nursery.in * exp(1)^(log(nursery.survival) / nursery.period * (batch_day - (quarantine.period + 1))),
                                  system == "juvenile" ~ juvenile.in * exp(1)^(log(juvenile.survival) / juvenile.period * (batch_day - (quarantine.period + 1) - nursery.period)),
                                  system == "growout" ~ growout.in * exp(1)^(log(growout.survival) / growout.period * (batch_day - (quarantine.period + 1) - nursery.period - juvenile.period)),
                                  system == "depurate" ~ depurate.in),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 ~ total.number - harvest.95.1 * growout.system.division, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 + 14 ~ 0, TRUE ~ total.number),
         process.number = case_when(system == "quarantine" ~ total.number / quarantine.system.division,
                                 system == "nursery" ~ total.number / nursery.system.division,
                                 system == "juvenile" ~ total.number / juvenile.system.division,
                                 system == "growout" ~ total.number / growout.system.division,
                                 system == "depurate" ~ total.number / depurate.system.division),
         bw = case_when(system == "depurate" ~ 32, TRUE ~ (bw_0^c_1 + c_2 * batch_day)^c_4),
         feed.rate = case_when(system == "quarantine" ~ exp(quarantine.feed.twinlog$coef[1]) * bw ^ quarantine.feed.twinlog$coef[2],
                               system == "nursery" ~ nursery.feed,
                               system == "juvenile" ~ exp(juvenile.feed.log$coef[1] + bw * juvenile.feed.log$coef[2]),
                               system == "growout" ~ growout.feed,
                               system == "depurate" ~ depurate.feed),
         protein.proportion = case_when(system == "quarantine" ~ quarantine.protein,
                                        system == "nursery" ~ nursery.protein,
                                        system == "juvenile" ~ juvenile.protein,
                                        system == "growout" ~ growout.protein,
                                        system == "depurate" ~ depurate.protein),
         total.bw.kg = bw * total.number / 1000,
         total.feed.kg = total.bw.kg * feed.rate,
         total.protein = total.feed.kg * protein.proportion,
         process.bw.kg = bw * process.number / 1000,
         process.feed.kg = process.bw.kg * feed.rate,
         process.protein = process.feed.kg * protein.proportion)

write.csv(system.loading, "tabs/system_loading.csv")



system.loading %>%
  group_by(facility_day, water.management) %>%
  filter(facility_day < 220) %>%
  ggplot(aes(facility_day, cumsum(water.management), color = water.designation)) +
  geom_line()

system.n.95 <- system.loading %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, total.number, color = system)) +
  geom_line() +
  scale_color_manual("", breaks = c("quarantine", "nursery", "juvenile", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Juvenile", "Growout", "Depurate")) +
  labs(x = "Day", y = "Number of Shrimp in Batch") + 
  theme_minimal()

pdf("plots/system_n_95.pdf", height = 4)
system.n.95
dev.off()

tank.n.95 <- system.loading %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, process.number, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Batch Day", y = "Number of Shrimp in Process") + 
  theme_minimal()

pdf("plots/tank_n_95.pdf", height = 4)
tank.n.95
dev.off()

# get max loading for each system (overlapping batches where appropriate)
avg.loading.95 <- system.loading %>%
  group_by(system, facility_day) %>%
  summarise(n_sum = sum(total.number),
            feed.sum = sum(total.feed.kg),
            protein.sum = sum(total.protein)) %>%
  summarise(n_max = max(n_sum),
            feed.avg = mean(feed.sum),
            feed.max = max(feed.sum),
            protein.avg = mean(protein.sum),
            protein.max = max(protein.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max loading for individual system processes - filter one batch
process.loading_95 <- system.loading %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(n_max = max(process.number),
            feed.avg = mean(process.feed.kg),
            feed.max = max(process.feed.kg),
            protein.avg = mean(process.protein),
           protein.max = max(process.protein)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))


# system loading for primo
quarantine.in.primo <- 65000
nursery.in.primo <- 44000
growout.in.primo <- 16500
growout.out.primo <- 13200
depurate.in.primo <- 1600
nursery.harvest.primo <- 4000

harvest.primo.1 <- 1200
harvest.primo.2 <- 1000
harvest.primo.3 <- 600

quarantine.primo.division <- 1
nursery.primo.division <- 2
growout.primo.division <- 3
depurate.primo.division <- 1
depurate.tanks.primo <- 4

# build system loading onto production plan
system.loading.primo <- prod.plan.primo %>%
  mutate(total.number = case_when(system == "quarantine" ~ quarantine.in.primo * exp(1)^(log(quarantine.survival) / quarantine.period.primo * batch_day),
                                  system == "nursery" ~ nursery.in.primo * exp(1)^(log(nursery.survival) / nursery.period.primo * (batch_day - (quarantine.period.primo + 1))),
                                  system == "growout" ~ growout.in.primo * exp(1)^(log(growout.survival) / growout.period.primo * (batch_day - (quarantine.period.primo + 1) - nursery.period.primo)),
                                  system == "depurate" ~ depurate.in.primo),
         total.number = case_when(system == "nursery" & batch_day >= shrimp.5.g ~ total.number - nursery.harvest.primo * nursery.primo.division, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 ~ total.number - harvest.primo.1 * growout.primo.division, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 + 14 ~ total.number - harvest.primo.2 * growout.primo.division, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 + 28 ~ 0, TRUE ~ total.number),
         process.number = case_when(system == "quarantine" ~ total.number / quarantine.primo.division,
                                    system == "nursery" ~ total.number / nursery.primo.division,
                                    system == "growout" ~ total.number / growout.primo.division,
                                    system == "depurate" ~ total.number / depurate.primo.division),
         bw = case_when(system == "depurate" ~ 32, TRUE ~ (bw_0^c_1 + c_2 * batch_day)^c_4),
         feed.rate = case_when(system == "quarantine" ~ exp(quarantine.feed.twinlog$coef[1]) * bw ^ quarantine.feed.twinlog$coef[2],
                               system == "nursery" ~ exp(juvenile.nursery.feed.log$coef[1] + bw * juvenile.nursery.feed.log$coef[2]),
                               system == "growout" ~ growout.feed,
                               system == "depurate" ~ depurate.feed),
         protein.proportion = case_when(system == "quarantine" ~ quarantine.protein,
                                        system == "nursery" ~ nursery.protein,
                                        system == "growout" ~ growout.protein,
                                        system == "depurate" ~ depurate.protein),
         total.bw.kg = bw * total.number / 1000,
         total.feed.kg = total.bw.kg * feed.rate,
         total.protein = total.feed.kg * protein.proportion,
         process.bw.kg = bw * process.number / 1000,
         process.feed.kg = process.bw.kg * feed.rate,
         process.protein = process.feed.kg * protein.proportion)

system.n.primo <- system.loading.primo %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, total.number, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Day", y = "Number of Shrimp in Batch") + 
  theme_minimal()

pdf("plots/system_n_primo.pdf", height = 4)
system.n.primo
dev.off()

tank.n.primo <- system.loading.primo %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, process.number, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Batch Day", y = "Number of Shrimp in Process") + 
  theme_minimal()

pdf("plots/tank_n_primo.pdf", height = 4)
tank.n.primo
dev.off()

# get max loading for each system in primo (overlapping batches where appropriate)
avg.loading.primo <- system.loading.primo %>%
  group_by(system, facility_day) %>%
  summarise(n_sum = sum(total.number),
            feed.sum = sum(total.feed.kg),
            protein.sum = sum(total.protein)) %>%
  summarise(n_max = max(n_sum),
            feed.avg = mean(feed.sum),
            feed.max = max(feed.sum),
            protein.avg = mean(protein.sum),
            protein.max = max(protein.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "growout", "depurate")))

# get max loading for individual system processes - filter one batch
process.loading_primo <- system.loading.primo %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(n_max = max(process.number),
            feed.avg = mean(process.feed.kg),
            feed.max = max(process.feed.kg),
            protein.avg = mean(process.protein),
            protein.max = max(process.protein)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "growout", "depurate")))

# system loading for 101
quarantine.in.101.a <- 65000
quarantine.in.101.b <- 100000
nursery.in.101.a <- 23000 * 2
nursery.in.101.b <- 23000 * 3
growout.in.101.a <- 5200 * 4
growout.in.101.b <- 5200 * 6
depurate.in.101 <- 1600
nursery.harvest.101 <- 4000

harvest.101.1 <- 1200
harvest.101.2 <- 600
harvest.101.3 <- 600

quarantine.101.division <- 1
nursery.101.division <- 2
growout.101.division.a <- 4
growout.101.division.b <- 2
growout.101.tank.multiplier <- 6
depurate.101.division <- 1
depurate.tanks.101 <- 4

# build system loading onto production plan 101
system.loading.101 <- prod.plan.101 %>%
  mutate(total.number = case_when(system == "quarantine" ~ quarantine.in.101.a * exp(1)^(log(quarantine.survival) / quarantine.period.101 * batch_day),
                                  system == "nursery" ~ nursery.in.101.a * exp(1)^(log(nursery.survival) / nursery.period.101 * (batch_day - (quarantine.period.101 + 1))),
                                  system == "growout" ~ growout.in.101.a * exp(1)^(log(growout.survival) / growout.period.101 * (batch_day - (quarantine.period.101 + 1) - nursery.period.101)),
                                  system == "depurate" ~ depurate.in.101),
         total.number = case_when(system == "quarantine" & batch %in% c("lag.batch.1", "lag.batch.3", "lag.batch.5", "lag.batch.7", "lag.batch.9", "lag.batch.11") ~ quarantine.in.101.b * exp(1)^(log(quarantine.survival) / quarantine.period.101 * batch_day), TRUE ~ total.number),
         total.number = case_when(system == "nursery" & batch %in% c("lag.batch.1", "lag.batch.3", "lag.batch.5", "lag.batch.7", "lag.batch.9", "lag.batch.11") ~ nursery.in.101.b * exp(1)^(log(nursery.survival) / nursery.period.101 * (batch_day - (quarantine.period.101 + 1))), TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch %in% c("lag.batch.1", "lag.batch.3", "lag.batch.5", "lag.batch.7", "lag.batch.9", "lag.batch.11") ~ growout.in.101.b * exp(1)^(log(growout.survival) / growout.period.101 * (batch_day - (quarantine.period.101 + 1) - nursery.period.101)), TRUE ~ total.number),
         total.number = case_when(system == "nursery" & batch_day >= shrimp.5.g ~ total.number - nursery.harvest.101 * nursery.101.division, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 ~ total.number - harvest.101.1 * growout.101.division.a, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 + 14 ~ total.number - harvest.101.2 * growout.101.division.a, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 + 28 ~ 0, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 & batch %in% c("lag.batch.1", "lag.batch.3", "lag.batch.5", "lag.batch.7", "lag.batch.9", "lag.batch.11") ~ total.number - harvest.101.1 * growout.101.division.b, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 + 14 & batch %in% c("lag.batch.1", "lag.batch.3", "lag.batch.5", "lag.batch.7", "lag.batch.9", "lag.batch.11") ~ total.number - harvest.101.2 * growout.101.division.b, TRUE ~ total.number),
         total.number = case_when(system == "growout" & batch_day >= shrimp.harvest.day.32 + 28 & batch %in% c("lag.batch.1", "lag.batch.3", "lag.batch.5", "lag.batch.7", "lag.batch.9", "lag.batch.11") ~ 0, TRUE ~ total.number),
         process.number = case_when(system == "quarantine" ~ total.number / quarantine.101.division,
                                    system == "nursery" ~ total.number / nursery.101.division,
                                    system == "growout" ~ total.number / growout.101.division.a,
                                    system == "depurate" ~ total.number / depurate.101.division),
         process.number = case_when(system == "growout" & batch %in% c("lag.batch.1", "lag.batch.3", "lag.batch.5", "lag.batch.7", "lag.batch.9", "lag.batch.11") ~ total.number / growout.101.tank.multiplier, TRUE ~ process.number),
         bw = case_when(system == "depurate" ~ 32, TRUE ~ (bw_0^c_1 + c_2 * batch_day)^c_4),
         feed.rate = case_when(system == "quarantine" ~ exp(quarantine.feed.twinlog$coef[1]) * bw ^ quarantine.feed.twinlog$coef[2],
                               system == "nursery" ~ exp(juvenile.nursery.feed.log$coef[1] + bw * juvenile.nursery.feed.log$coef[2]),
                               system == "growout" ~ growout.feed,
                               system == "depurate" ~ depurate.feed),
         protein.proportion = case_when(system == "quarantine" ~ quarantine.protein,
                                        system == "nursery" ~ nursery.protein,
                                        system == "growout" ~ growout.protein,
                                        system == "depurate" ~ depurate.protein),
         total.bw.kg = bw * total.number / 1000,
         total.feed.kg = total.bw.kg * feed.rate,
         total.protein = total.feed.kg * protein.proportion,
         process.bw.kg = bw * process.number / 1000,
         process.feed.kg = process.bw.kg * feed.rate,
         process.protein = process.feed.kg * protein.proportion)

batch.names <- c("lag.batch.0" = "Batch 1", "lag.batch.1" = "Batch 2")
         
system.n.101 <- system.loading.101 %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.1")) %>%
  ggplot(aes(batch_day, total.number, color = system)) +
  geom_line()+
  facet_wrap(~batch, ncol = 1, labeller = as_labeller(batch.names)) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Batch Day", y = "Number of Shrimp in Batch") + 
  theme_minimal()

pdf("plots/system_n_101.pdf", height = 4)
system.n.101
dev.off()

tank.n.101 <- system.loading.101 %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.1")) %>%
  ggplot(aes(batch_day, process.number, color = system)) +
  geom_line()+
  facet_wrap(~batch, ncol = 1, labeller = as_labeller(batch.names)) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Batch Day", y = "Number of Shrimp in Process") + 
  theme_minimal()

pdf("plots/tank_n_101.pdf", height = 4)
tank.n.101
dev.off()

# get max loading for each system in primo (overlapping batches where appropriate)
avg.loading.101 <- system.loading.101 %>%
  group_by(system, facility_day) %>%
  summarise(n_sum = sum(total.number),
            feed.sum = sum(total.feed.kg),
            protein.sum = sum(total.protein)) %>%
  summarise(n_max = max(n_sum),
            feed.avg = mean(feed.sum),
            feed.max = max(feed.sum),
            protein.avg = mean(protein.sum),
            protein.max = max(protein.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "growout", "depurate")))

# get max loading for individual system processes - filter one batch
process.loading_101.a <- system.loading.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(n_max = max(process.number),
            feed.avg = mean(process.feed.kg),
            feed.max = max(process.feed.kg),
            protein.avg = mean(process.protein),
            protein.max = max(process.protein)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "growout", "depurate")))

# get max loading for individual system processes - filter one batch
process.loading_101.b <- system.loading.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(n_max = max(process.number),
            feed.avg = mean(process.feed.kg),
            feed.max = max(process.feed.kg),
            protein.avg = mean(process.protein),
            protein.max = max(process.protein)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "growout", "depurate")))

system.n.101.a <- system.loading.101 %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, total.number, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Day", y = "Number of Shrimp in Batch") + 
  theme_minimal()

