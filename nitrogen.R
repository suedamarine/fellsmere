# load libraries
library(tidyverse)

# get max tan loading for each system 95th st 2022 (overlapping batches where appropriate)
tan.system.95 <- system.parameters.95 %>%
  group_by(system, facility_day) %>%
  summarise(tan.sum = sum(total.tan)) %>%
  summarise(tan.s.max = max(tan.sum),
            tan.s.avg = mean(tan.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max tan loading for individual system processes 95th st 2022
tan.process.95 <- system.parameters.95 %>%
  group_by(system) %>%
  summarise(tan.p.max = max(process.tan),
            tan.p.avg = mean(process.tan)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join tan summary tibbles 95th st 2022
tan.summary.95 <- left_join(tan.system.95, tan.process.95, by = "system")

# get max tan loading for each system 95th st 2023 (overlapping batches where appropriate)
tan.system.primo <- system.parameters.primo %>%
  group_by(system, facility_day) %>%
  summarise(tan.sum = sum(total.tan)) %>%
  summarise(tan.s.max = max(tan.sum),
            tan.s.avg = mean(tan.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max tan loading for individual system processes 95th st 2023
tan.process.primo <- system.parameters.primo %>%
  group_by(system) %>%
  summarise(tan.p.max = max(process.tan),
            tan.p.avg = mean(process.tan)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join tan summary tibbles 95th st 2023
tan.summary.primo <- left_join(tan.system.primo, tan.process.primo, by = "system")

# get max tan loading for each system 101th st 2024 (overlapping batches where appropriate)
tan.system.101 <- system.parameters.101 %>%
  group_by(system, facility_day) %>%
  summarise(tan.sum = sum(total.tan)) %>%
  summarise(tan.s.max = max(tan.sum),
            tan.s.avg = mean(tan.sum)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max tan loading for individual system processes 101st st 2024 4 raceway batch
tan.process.101a <- system.parameters.101 %>%
  filter(batch == "lag.batch.0") %>%
  group_by(system) %>%
  summarise(tan.p.max.a = max(process.tan),
            tan.p.avg.a = mean(process.tan)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# get max tan loading for individual system processes 101st st 2024 6 raceway batch
tan.process.101b <- system.parameters.101 %>%
  filter(batch == "lag.batch.1") %>%
  group_by(system) %>%
  summarise(tan.p.max.b = max(process.tan),
            tan.p.avg.b = mean(process.tan)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "juvenile", "growout", "depurate")))

# join tan summary tibbles 101st st 2024
tan.summary.101 <- left_join(tan.system.101, tan.process.101a, by = "system") %>%
  left_join(., tan.process.101b, by = "system")

# plot tan loading across batch run 95th st 2022
tank.tan.95 <- system.parameters.95 %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, process.tan, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "juvenile", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Juvenile", "Growout", "Depurate")) +
  labs(x = "Batch Day", y = "TAN--N (kg)") + 
  theme_minimal()

pdf("plots/tank_tan_95.pdf", height = 4)
tank.tan.95
dev.off()

# plot tan loading across batch run 95th st 2023
tank.tan.primo <- system.parameters.primo %>%
  filter(batch == "lag.batch.0") %>%
  ggplot(aes(batch_day, process.tan, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Batch Day", y = "TAN--N (kg)") + 
  theme_minimal()

pdf("plots/tank_tan_primo.pdf", height = 4)
tank.tan.primo
dev.off()

# plot tan loading across batch run 101st st 2024
tank.tan.101 <- system.parameters.101 %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.1")) %>%
  ggplot(aes(facility_day, process.tan, color = system)) +
  geom_line()+
  facet_wrap(~batch, ncol = 1, labeller = as_labeller(batch.names)) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Facility Day", y = "Number of Shrimp in Process") + 
  theme_minimal()

pdf("plots/tank_tan_101.pdf", height = 4)
tank.tan.101
dev.off()

# plot carb requirement across batch run 95th st 2022
tank.carb.95 <- system.parameters.95 %>%
  filter(batch == "lag.batch.0" & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(batch_day, process.carb, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("nursery", "juvenile", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Juvenile", "Growout")) +
  labs(x = "Batch Day", y = "Supplimentary carbohydrate required (kg per day)") + 
  theme_minimal()

pdf("plots/tank_carb_95.pdf", height = 4)
tank.carb.95
dev.off()

# plot carb requirement across batch run 95th st 2023
tank.carb.primo <- system.parameters.primo %>%
  filter(batch == "lag.batch.0" & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(batch_day, process.carb, color = system)) +
  geom_line()+
  scale_color_manual("", breaks = c("nursery", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Growout")) +
  labs(x = "Batch Day", y = "Supplimentary carbohydrate required (kg per day)") + 
  theme_minimal()

pdf("plots/tank_carb_primo.pdf", height = 4)
tank.carb.primo
dev.off()

# plot carb requirement across batch run 101st st 2024
tank.carb.101 <- system.parameters.101 %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.1") & !system %in% c("quarantine", "depurate")) %>%
  ggplot(aes(facility_day, process.carb, color = system)) +
  geom_line()+
  facet_wrap(~batch, ncol = 1, labeller = as_labeller(batch.names)) +
  scale_color_manual("", breaks = c("nursery", "growout"),
                     values = c(system_cols),
                     labels = c("Nursery", "Growout")) +
  labs(x = "Facility Day", y = "Supplimentary carbohydrate required (kg per day)") + 
  theme_minimal()

pdf("plots/tank_carb_101.pdf", height = 4)
tank.carb.101
dev.off()


