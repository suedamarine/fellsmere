# load libraries
library(tidyverse)

# get max co2 produced for each system 95th st 2022 (overlapping batches where appropriate)
co2.system.95 <- system.parameters.95 %>%
  group_by(system, facility_day) %>%
  summarise(co2.sum = sum(total.co2)) %>%
  summarise(co2.s.max = max(co2.sum),
            co2.s.avg = mean(co2.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max co2 produced for individual system processes 95th st 2022
co2.process.95 <- system.parameters.95 %>%
  group_by(system) %>%
  summarise(co2.p.max = max(process.co2),
            co2.p.avg = mean(process.co2)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join co2 produced summary tibbles 95th st 2022
co2.summary.95 <- left_join(co2.system.95, co2.process.95, by = "system")

# get max co2 produced for each system 95th st 2023 (overlapping batches where appropriate)
co2.system.primo <- system.parameters.primo %>%
  group_by(system, facility_day) %>%
  summarise(co2.sum = sum(total.co2)) %>%
  summarise(co2.s.max = max(co2.sum),
            co2.s.avg = mean(co2.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max co2 produced for individual system processes 95th st 2023
co2.process.primo <- system.parameters.primo %>%
  group_by(system) %>%
  summarise(co2.p.max = max(process.co2),
            co2.p.avg = mean(process.co2)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join co2 produced summary tibbles 95th st 2023
co2.summary.primo <- left_join(co2.system.primo, co2.process.primo, by = "system")

# get max co2 produced for each system 101th st 2024 (overlapping batches where appropriate)
co2.system.101 <- system.parameters.101 %>%
  group_by(system, facility_day) %>%
  summarise(co2.sum = sum(total.co2)) %>%
  summarise(co2.s.max = max(co2.sum),
            co2.s.avg = mean(co2.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max co2 produced for individual system processes 101st st 2024 4 raceway batch
co2.process.101a <- system.parameters.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(co2.p.max.a = max(process.co2),
            co2.p.avg.a = mean(process.co2)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max co2 produced for individual system processes 101st st 2024 6 raceway batch
co2.process.101b <- system.parameters.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(co2.p.max.b = max(process.co2),
            co2.p.avg.b = mean(process.co2)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join co2 produced summary tibbles 101st st 2024
co2.summary.101 <- left_join(co2.system.101, co2.process.101a, by = "system") %>%
  left_join(., co2.process.101b, by = "system")

# get flow requirement to degas tanks 95th st 2022
co2.flow.95 <- system.parameters.95 %>%
  mutate(c02.flow = abs(process.co2 * 1e6 / (co2.c2 - co2.c1) / 24 / 1000))

# get flow requirement to degas tanks 95th st 2023
co2.flow.primo <- system.parameters.primo %>%
  mutate(c02.flow = abs(process.co2 * 1e6 / (co2.c2 - co2.c1) / 24 / 1000))

# get flow requirement to degas tanks 101st st 2024
co2.flow.101 <- system.parameters.101 %>%
  mutate(c02.flow = abs(process.co2 * 1e6 / (co2.c2 - co2.c1) / 24 / 1000))

# plot co2 flow requirement across batch run 95th st 2022
tank.co2.95 <- co2.flow.95 %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, c02.flow, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "juvenile", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Juvenile", "Growout", "Depurate")) +
  labs(x = "Facility Day", y = expression(paste("Flow to maintain ", CO[2], " at 10 mg/l ", (m^3/hr)))) + 
  theme_minimal()

pdf("plots/tank_co2_95.pdf", height = 4)
tank.co2.95
dev.off()

# plot co2 flow requirement across batch run 95th st 2023
tank.co2.primo <- co2.flow.primo %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, c02.flow, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "juvenile", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Juvenile", "Growout", "Depurate")) +
  labs(x = "Facility Day", y = expression(paste("Flow to maintain ", CO[2], " at 10 mg/l ", (m^3/hr)))) + 
  theme_minimal()

pdf("plots/tank_co2_primo.pdf", height = 4)
tank.co2.primo
dev.off()

# plot co2 flow requirement across batch run 101st st 2024
tank.co2.101 <- co2.flow.101 %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.1")) %>%
  ggplot(aes(facility_day, c02.flow, color = system)) +
  geom_line()+
  facet_wrap(~batch, ncol = 1, labeller = as_labeller(batch.names)) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery","Growout", "Depurate")) +
  labs(x = "Facility Day", y = expression(paste("Flow to maintain ", CO[2], " at 10 mg/l ", (m^3/hr)))) + 
  theme_minimal()

pdf("plots/tank_co2_101.pdf", height = 4)
tank.co2.101
dev.off()

