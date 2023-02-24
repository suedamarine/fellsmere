# load libraries
library(tidyverse)

# get max oxygen demand for each system 95th st 2022 (overlapping batches where appropriate)
oxy.system.95 <- system.parameters.95 %>%
  group_by(system, facility_day) %>%
  summarise(oxy.sum = sum(total.od)) %>%
  summarise(oxy.s.max = max(oxy.sum),
            oxy.s.avg = mean(oxy.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max oxygen demand for individual system processes 95th st 2022
oxy.process.95 <- system.parameters.95 %>%
  group_by(system) %>%
  summarise(oxy.p.max = max(process.od),
            oxy.p.avg = mean(process.od)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join oxygen demand summary tibbles 95th st 2022
oxy.summary.95 <- left_join(oxy.system.95, oxy.process.95, by = "system")

# get max oxygen demand for each system 95th st 2023 (overlapping batches where appropriate)
oxy.system.primo <- system.parameters.primo %>%
  group_by(system, facility_day) %>%
  summarise(oxy.sum = sum(total.od)) %>%
  summarise(oxy.s.max = max(oxy.sum),
            oxy.s.avg = mean(oxy.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max oxygen demand for individual system processes 95th st 2023
oxy.process.primo <- system.parameters.primo %>%
  group_by(system) %>%
  summarise(oxy.p.max = max(process.od),
            oxy.p.avg = mean(process.od)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join oxygen demand summary tibbles 95th st 2023
oxy.summary.primo <- left_join(oxy.system.primo, oxy.process.primo, by = "system")

# get max oxygen demand for each system 101th st 2024 (overlapping batches where appropriate)
oxy.system.101 <- system.parameters.101 %>%
  group_by(system, facility_day) %>%
  summarise(oxy.sum = sum(total.od)) %>%
  summarise(oxy.s.max = max(oxy.sum),
            oxy.s.avg = mean(oxy.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max oxygen demand for individual system processes 101st st 2024 4 raceway batch
oxy.process.101a <- system.parameters.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(oxy.p.max.a = max(process.od),
            oxy.p.avg.a = mean(process.od)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max oxygen demand for individual system processes 101st st 2024 6 raceway batch
oxy.process.101b <- system.parameters.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(oxy.p.max.b = max(process.od),
            oxy.p.avg.b = mean(process.od)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join oxygen demand summary tibbles 101st st 2024
oxy.summary.101 <- left_join(oxy.system.101, oxy.process.101a, by = "system") %>%
  left_join(., oxy.process.101b, by = "system")





