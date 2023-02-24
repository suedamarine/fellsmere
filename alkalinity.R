# load libraries
library(tidyverse)

# get max alkalinity demand for each system 95th st 2022 (overlapping batches where appropriate)
alk.system.95 <- system.parameters.95 %>%
  group_by(system, facility_day) %>%
  summarise(alk.sum = sum(total.alk)) %>%
  summarise(alk.s.max = max(alk.sum),
            alk.s.avg = mean(alk.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max alkalinity demand for individual system processes 95th st 2022
alk.process.95 <- system.parameters.95 %>%
  group_by(system) %>%
  summarise(alk.p.max = max(process.alk),
            alk.p.avg = mean(process.alk)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join alkalinity demand summary tibbles 95th st 2022
alk.summary.95 <- left_join(alk.system.95, alk.process.95, by = "system")

# get max alkalinity demand for each system 95th st 2023 (overlapping batches where appropriate)
alk.system.primo <- system.parameters.primo %>%
  group_by(system, facility_day) %>%
  summarise(alk.sum = sum(total.alk)) %>%
  summarise(alk.s.max = max(alk.sum),
            alk.s.avg = mean(alk.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max alkalinity demand for individual system processes 95th st 2023
alk.process.primo <- system.parameters.primo %>%
  group_by(system) %>%
  summarise(alk.p.max = max(process.alk),
            alk.p.avg = mean(process.alk)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join alkalinity demand summary tibbles 95th st 2023
alk.summary.primo <- left_join(alk.system.primo, alk.process.primo, by = "system")

# get max alkalinity demand for each system 101th st 2024 (overlapping batches where appropriate)
alk.system.101 <- system.parameters.101 %>%
  group_by(system, facility_day) %>%
  summarise(alk.sum = sum(total.alk)) %>%
  summarise(alk.s.max = max(alk.sum),
            alk.s.avg = mean(alk.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max alkalinity demand for individual system processes 101st st 2024 4 raceway batch
alk.process.101a <- system.parameters.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(alk.p.max.a = max(process.alk),
            alk.p.avg.a = mean(process.alk)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max alkalinity demand for individual system processes 101st st 2024 6 raceway batch
alk.process.101b <- system.parameters.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(alk.p.max.b = max(process.alk),
            alk.p.avg.b = mean(process.alk)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join alkalinity demand summary tibbles 101st st 2024
alk.summary.101 <- left_join(alk.system.101, alk.process.101a, by = "system") %>%
  left_join(., alk.process.101b, by = "system")

# plot alkalinity requirement across batch run 95th st 2022
tank.alk.95 <- system.parameters.95 %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, process.alk, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "juvenile", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Juvenile", "Growout", "Depurate")) +
  labs(x = "Batch Day", y = "Supplimentary alkalinity required (kg per day)") + 
  theme_minimal()

pdf("plots/tank_alk_95.pdf", height = 4)
tank.alk.95
dev.off()

# plot alkalinity requirement across batch run 95th st 2023
tank.alk.primo <- system.parameters.primo %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, process.alk, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery","Growout", "Depurate")) +
  labs(x = "Batch Day", y = "Supplimentary alkalinity required (kg per day)") + 
  theme_minimal()

pdf("plots/tank_alk_primo.pdf", height = 4)
tank.alk.primo
dev.off()

# plot alkalinity requirement across batch run 101st st 2024
tank.alk.101 <- system.parameters.101 %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.1")) %>%
  ggplot(aes(facility_day, process.alk, color = system)) +
  geom_line()+
  facet_wrap(~batch, ncol = 1, labeller = as_labeller(batch.names)) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery","Growout", "Depurate")) +
  labs(x = "Facility Day", y = "Supplimentary alkalinity required (kg per day)") + 
  theme_minimal()

pdf("plots/tank_alk_101.pdf", height = 4)
tank.alk.101
dev.off()

ft <- flextable(alk.summary.95)
ft <- set_header_labels(ft,
                        values = list(
                          system = "System",
                          alk.s.max = "System max Alk",
                          alk.s.avg = "System avg Alk",
                          alk.p.max = "Process max Alk",
                          alk.p.avg = "Process avg Alk"
                        ))



