# load libraries 
library(tidyverse)


# get max tss loading for each system 95th st 2022 (overlapping batches where appropriate)
tss.system.95 <- system.parameters.95 %>%
  group_by(system, facility_day) %>%
  summarise(tss.sum = sum(total.tss)) %>%
  summarise(tss.s.max = max(tss.sum),
            tss.s.avg = mean(tss.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max tss loading for individual system processes 95th st 2022
tss.process.95 <- system.parameters.95 %>%
  group_by(system) %>%
  summarise(tss.p.max = max(process.tss),
            tss.p.avg = mean(process.tss)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join tss summary tibbles 95th st 2022
tss.summary.95 <- left_join(tss.system.95, tss.process.95, by = "system")

# get max tss loading for each system 95th st 2023 (overlapping batches where appropriate)
tss.system.primo <- system.parameters.primo %>%
  group_by(system, facility_day) %>%
  summarise(tss.sum = sum(total.tss)) %>%
  summarise(tss.s.max = max(tss.sum),
            tss.s.avg = mean(tss.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max tss loading for individual system processes 95th st 2023
tss.process.primo <- system.parameters.primo %>%
  group_by(system) %>%
  summarise(tss.p.max = max(process.tss),
            tss.p.avg = mean(process.tss)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join tss summary tibbles 95th st 2023
tss.summary.primo <- left_join(tss.system.primo, tss.process.primo, by = "system")

# get max tss loading for each system 101th st 2024 (overlapping batches where appropriate)
tss.system.101 <- system.parameters.101 %>%
  group_by(system, facility_day) %>%
  summarise(tss.sum = sum(total.tss)) %>%
  summarise(tss.s.max = max(tss.sum),
            tss.s.avg = mean(tss.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max tss loading for individual system processes 101st st 2024 4 raceway batch
tss.process.101a <- system.parameters.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(tss.p.max.a = max(process.tss),
            tss.p.avg.a = mean(process.tss)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max tss loading for individual system processes 101st st 2024 6 raceway batch
tss.process.101b <- system.parameters.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(tss.p.max.b = max(process.tss),
            tss.p.avg.b = mean(process.tss)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join tss summary tibbles 101st st 2024
tss.summary.101 <- left_join(tss.system.101, tss.process.101a, by = "system") %>%
  left_join(., tss.process.101b, by = "system")


# get cumulative sum grouped by system 95th st 2022
cumulative.tss.95 <- system.parameters.95 %>%
  group_by(system, batch) %>%
  mutate(cs = cumsum(process.tss),
         cumulative.tss = case_when(system == "juvenile"  ~ cs * 1e6 / (1000 * raceway.tank),
                                    system == "growout" ~ cs * 1e6 / (1000 * raceway.tank),
                                    system == "nursery" ~ cs * 1e6 / (1000 * nursery.tank),
                                    TRUE ~ 0),
         total.drum.flow = abs(total.tss * 1e6 / (tss.c2 - tss.c1) / 24 / 1000),
         process.drum.flow = abs(process.tss * 1e6 / (tss.c2 - tss.c1) / 24 / 1000),
         total.drum.water.use = total.drum.flow * 50 / 1000,
         process.drum.water.use = process.drum.flow * 50 / 1000)

cumulative.tss.95 %>% filter(batch == "lag.batch.0") %>%
  write.csv("tabs/cum_tss_95.csv")



# get cumulative sum grouped by system 95th st 2023
cumulative.tss.primo <- system.parameters.primo %>%
  group_by(system, batch) %>%
  mutate(cs = cumsum(process.tss),
         cumulative.tss = case_when(system == "growout" ~ cs * 1e6 / (1000 * raceway.tank),
                                    system == "nursery" ~ cs * 1e6 / (1000 * raceway.tank),
                                    TRUE ~ 0),
         total.drum.flow = abs(total.tss * 1e6 / (tss.c2 - tss.c1) / 24 / 1000),
         process.drum.flow = abs(process.tss * 1e6 / (tss.c2 - tss.c1) / 24 / 1000),
         total.drum.water.use = total.drum.flow * 50 / 1000,
         process.drum.water.use = process.drum.flow * 50 / 1000)

# get cumulative sum grouped by system 101st st 2024
cumulative.tss.101 <- system.parameters.101 %>%
  group_by(system, batch) %>%
  mutate(cs = cumsum(process.tss),
         cumulative.tss = case_when(system == "growout" ~ cs * 1e6 / (1000 * raceway.tank),
                                    system == "nursery" ~ cs * 1e6 / (1000 * raceway.tank),
                                    TRUE ~ 0),
         total.drum.flow = abs(total.tss * 1e6 / (tss.c2 - tss.c1) / 24 / 1000),
         process.drum.flow = abs(process.tss * 1e6 / (tss.c2 - tss.c1) / 24 / 1000),
         total.drum.water.use = total.drum.flow * 50 / 1000,
         process.drum.water.use = process.drum.flow * 50 / 1000)

# plot cumulative tss loading across batch run 95th st 2022
tss.cumplot.95 <- cumulative.tss.95 %>%
  filter(batch == "lag.batch.0" & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(batch_day, cumulative.tss, color = system)) +
  geom_line()+
  geom_hline(yintercept=250, linetype="dashed", color = "darkgrey") + 
  scale_color_manual("", breaks = c("nursery", "juvenile", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Juvenile", "Growout")) +
  labs(x = "Batch Day", y = "Cumulative TSS (mg/l)") + 
  theme_minimal()

pdf("plots/cumulative_tss_95.pdf", height = 4)
tss.cumplot.95
dev.off()

# plot cumulative tss loading across batch run 95th st 2023
tss.cumplot.primo <- cumulative.tss.primo %>%
  filter(batch == "lag.batch.0" & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(batch_day, cumulative.tss, color = system)) +
  geom_line()+
  geom_hline(yintercept=250, linetype="dashed", color = "darkgrey") +
  scale_color_manual("", breaks = c("nursery", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Growout")) +
  labs(x = "Batch Day", y = "Cumulative TSS (mg/l)") + 
  theme_minimal()

pdf("plots/cumulative_tss_primo.pdf", height = 4)
tss.cumplot.primo
dev.off()

# plot cumulative tss loading across batch run 101st st 2024
tss.cumplot.101 <- cumulative.tss.101 %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.1") & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(facility_day, cumulative.tss, color = system)) +
  geom_line()+
  geom_hline(yintercept=250, linetype="dashed", color = "darkgrey") +
  facet_wrap(~batch, ncol = 1, labeller = as_labeller(batch.names)) +
  scale_color_manual("", breaks = c("nursery", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Growout")) +
  labs(x = "Facility Day", y = "Cumulative TSS (mg/l)") + 
  theme_minimal()

pdf("plots/cumulative_tss_101.pdf", height = 4)
tss.cumplot.101
dev.off()

# plot drum filter flow requirement 95th 2022
drum.flowplot.95 <- cumulative.tss.95 %>%
  filter(batch == "lag.batch.0" & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(batch_day, process.drum.flow, color = system)) +
  geom_line()+ 
  scale_color_manual("", breaks = c("nursery", "juvenile", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Juvenile", "Growout")) +
  labs(x = "Batch Day", y = expression(paste("Drum Filter ", (m^3/hr)))) + 
  theme_minimal()

pdf("plots/drum_flowplot_95.pdf", height = 4)
drum.flowplot.95
dev.off()

# plot drum filter flow requirement 95th 2023
drum.flowplot.primo <- cumulative.tss.primo %>%
  filter(batch == "lag.batch.0" & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(batch_day, process.drum.flow, color = system)) +
  geom_line()+ 
  scale_color_manual("", breaks = c("nursery", "juvenile", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Juvenile", "Growout")) +
  labs(x = "Batch Day", y = expression(paste("Drum Filter ", (m^3/hr)))) + 
  theme_minimal()

pdf("plots/drum_flowplot_primo.pdf", height = 4)
drum.flowplot.primo
dev.off()

# plot drum filter flow requirement 101st 2024
drum.flowplot.101 <- cumulative.tss.101 %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.1") & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(facility_day, process.drum.flow, color = system)) +
  geom_line()+
  facet_wrap(~batch, ncol = 1, labeller = as_labeller(batch.names)) +
  scale_color_manual("", breaks = c("nursery", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Growout")) +
  labs(x = "Facility Day", y = "Cumulative TSS (mg/l)") + 
  theme_minimal()

pdf("plots/drum_flowplot_101.pdf", height = 4)
drum.flowplot.101
dev.off()

# get drum water usage  for each system 95th st 2022 (overlapping batches where appropriate)
drum.use.system.95 <- cumulative.tss.95 %>%
  group_by(system, facility_day) %>%
  summarise(total.drum.use.sum = sum(total.drum.water.use) * 24) %>%
  summarise(drum.use.s.max = max(total.drum.use.sum),
            drum.use.s.avg = mean(total.drum.use.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get drum water usage for individual system processes 95th st 2022
drum.use.process.95 <- cumulative.tss.95 %>%
  group_by(system) %>%
  summarise(drum.use.p.max = max(process.drum.water.use) * 24,
            drum.use.p.avg = mean(process.drum.water.use) * 24) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join tan summary tibbles 95th st 2022
drum.use.summary.95 <- left_join(drum.use.system.95,
                                 drum.use.process.95,
                                 by = "system") %>%
  filter(!system %in% c("quarantine", "depurate"))

# get drum water usage  for each system 95th st 2023 (overlapping batches where appropriate)
drum.use.system.primo <- cumulative.tss.primo %>%
  group_by(system, facility_day) %>%
  summarise(total.drum.use.sum = sum(total.drum.water.use) * 24) %>%
  summarise(drum.use.s.max = max(total.drum.use.sum),
            drum.use.s.avg = mean(total.drum.use.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get drum water usage for individual system processes 95th st 2023
drum.use.process.primo <- cumulative.tss.primo %>%
  group_by(system) %>%
  summarise(drum.use.p.max = max(process.drum.water.use) * 24,
            drum.use.p.avg = mean(process.drum.water.use) * 24) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join drum use summary tibbles 95th st 2023
drum.use.summary.primo <- left_join(drum.use.system.primo,
                                 drum.use.process.primo,
                                 by = "system") %>%
  filter(!system %in% c("quarantine", "depurate"))

# get max drum use for each system 101th st 2024 (overlapping batches where appropriate)
drum.use.system.101 <- cumulative.tss.101 %>%
  group_by(system, facility_day) %>%
  summarise(total.drum.use.sum = sum(total.drum.water.use) * 24) %>%
  summarise(drum.use.s.max = max(total.drum.use.sum),
            drum.use.s.avg = mean(total.drum.use.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max drum use for individual system processes 101st st 2024 4 raceway batch
drum.use.process.101a <- cumulative.tss.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(drum.use.p.max.a = max(process.drum.water.use) * 24,
            drum.use.p.avg.a = mean(process.drum.water.use) * 24) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max drum use for individual system processes 101st st 2024 6 raceway batch
drum.use.process.101b <- cumulative.tss.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(drum.use.p.max.b = max(process.drum.water.use) * 24,
            drum.use.p.avg.b = mean(process.drum.water.use) * 24) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join drum use tibbles 101st st 2024
drum.use.summary.101 <- left_join(drum.use.system.101,
                                  drum.use.process.101a,
                                  by = "system") %>%
  left_join(., drum.use.process.101b, by = "system") %>% 
  filter(!system %in% c("quarantine", "depurate"))

# get max cumulative tss loading for each system 95th st 2022 (overlapping batches where appropriate)
cumtss.system.95 <- cumulative.tss.95 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(tss.max = max(cs)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max cumulative tss loading for each system 95th st 2023 (overlapping batches where appropriate)
cumtss.system.primo <- cumulative.tss.primo %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(tss.max = max(cs)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max cumulative tss loading for each system 101st st 2024 (overlapping batches where appropriate)
cumtss.system.101.a <- cumulative.tss.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(tss.max = max(cs)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max cumulative tss loading for each system 101st st 2024 (overlapping batches where appropriate)
cumtss.system.101.b <- cumulative.tss.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(tss.max = max(cs)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join cumtss summary tibbles 2022 - 2024
cumtss.summary <- left_join(cumtss.system.95,
                            cumtss.system.primo,
                                  by = "system") %>%
  left_join(., cumtss.system.101.a, by = "system") %>%
  left_join(., cumtss.system.101.b, by = "system") %>%
  filter(!system %in% c("quarantine", "depurate"))

# find amount of solids to be removed per tank/ process
solids.removal.summary <- cumtss.summary %>%
  mutate(tss.95 = case_when(system == "nursery" ~ tss.max.x - (250 / 1e6 * nursery.tank * 1000),
                            system == "juvenile" | system == "growout" ~ tss.max.x - (250 / 1e6 * raceway.tank * 1000)),
         tss.primo = case_when(system == "nursery" ~ tss.max.y - (250 / 1e6 * nursery.tank * 1000),
                            system == "juvenile" | system == "growout" ~ tss.max.y - (250 / 1e6 * raceway.tank * 1000)),
         tss.101.a = case_when(system == "nursery" ~ tss.max.x.x - (250 / 1e6 * nursery.tank * 1000),
                            system == "juvenile" | system == "growout" ~ tss.max.x.x - (250 / 1e6 * raceway.tank * 1000)),
         tss.101.b = case_when(system == "nursery" ~ tss.max.y.y - (250 / 1e6 * nursery.tank * 1000),
                            system == "juvenile" | system == "growout" ~ tss.max.y.y - (250 / 1e6 * raceway.tank * 1000))) %>%
  select(system, tss.95 : tss.101.b)

# calculate water use to remove quantity of solids
drum.use.summary <- solids.removal.summary %>%
  mutate(use.95 = tss.95 / 0.25 * 50 / 1000 / 0.21,
         use.primo = tss.primo / 0.25 * 50 / 1000/ 0.21,
         use.101.a = tss.101.a / 0.25 * 50 / 1000/ 0.21,
         use.101.b = tss.101.b / 0.25 * 50 / 1000/ 0.21) %>%
  select(system, use.95 : use.101.b)

# calculate sum of drum filter water use across system/ batch 95th st 2022
drum.sum.95 <- cumulative.tss.95 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(drum.sum = sum(process.drum.water.use) * 24) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# calculate sum of drum filter water use across system/ batch 95th st 2023
drum.sum.primo <- cumulative.tss.primo %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(drum.sum = sum(process.drum.water.use) * 24) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# calculate sum of drum filter water use across system/ batch 101st st 2024 4 tank strategy
drum.sum.101.a <- cumulative.tss.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(drum.sum = sum(process.drum.water.use) * 24) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# calculate sum of drum filter water use across system/ batch 101st st 2024 6 tank strategy
drum.sum.101.b <- cumulative.tss.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(drum.sum = sum(process.drum.water.use) * 24) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join drum sum summary tibbles 2022 - 2024
drum.sum.summary <- left_join(drum.sum.95,
                              drum.sum.primo,
                            by = "system") %>%
  left_join(., drum.sum.101.a, by = "system") %>%
  left_join(., drum.sum.101.b, by = "system") %>%
  filter(!system %in% c("quarantine", "depurate"))



