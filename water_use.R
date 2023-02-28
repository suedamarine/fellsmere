# load libraries
library(tidyverse)


# set production period to two growth periods to observe system loading
prod.plan_water <- data.frame(facility_day = seq(0, 365 - 1)) %>%
  mutate(batch_1 = case_when(facility_day < 365 ~ facility_day))

for (lag_size in c(0:12)) {
  
  new_col_name <- paste0("lag.batch.", lag_size)
  
  prod.plan_water <- prod.plan_water %>%
    mutate(!!sym(new_col_name) := lag(batch_1, n = lag_size * batch_frequency, default = NA))
}

# remove column called batch_1
prod.plan.water <- prod.plan_water %>% select(-batch_1) %>%
  pivot_longer(c(lag.batch.0:lag.batch.12), names_to = "batch", values_to = "batch_day", values_drop_na = TRUE) %>%
  mutate(system = case_when(batch_day %in% c(0:quarantine.period) ~ "quarantine",
                            batch_day %in% c(quarantine.period + 1: nursery.period) ~ "nursery",
                            batch_day %in% c(quarantine.period + nursery.period + 1 : juvenile.period) ~ "juvenile",
                            batch_day %in% c(quarantine.period + nursery.period + juvenile.period + 1 : growout.period) ~ "growout",
                            batch_day %in% c(quarantine.period + nursery.period + juvenile.period + growout.period + 1 : depuration.period)  ~ "depurate",
                            TRUE ~ "harvested")) %>%
  drop_na() %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.2", "lag.batch.5", "lag.batch.7", "lag.batch.10")) %>%
  mutate(water.management = case_when(batch_day == 29 ~ nursery.tank * nursery.system.division,
                                      batch_day == 59 ~ raceway.tank - nursery.tank * nursery.system.division,
                                      batch_day == 89 ~ raceway.tank,
                                      batch_day == 151 ~ raceway.tank * growout.system.division * 0.1,
                                      batch_day == 165 ~ raceway.tank * growout.system.division * 0.1,
                                      batch_day == 167 ~ purging.unit,
                                      batch_day == 169 ~ packing.volume * growout.system.division,
                                      batch_day == 179 ~ raceway.tank * growout.system.division * 0.1,
                                      batch_day == 183 ~ packing.volume * growout.system.division,
                                      batch_day == 193 ~ raceway.tank * growout.system.division * 0.1,
                                      batch_day == 199 ~ 53 /3 * 2 * growout.system.division,
                                      TRUE ~ 0),
         water.management = case_when(facility_day == 32 & batch == "lag.batch.0" ~ 198,
                                      facility_day == 62  & batch == "lag.batch.0" ~ 152,
                                      facility_day == 90  & batch == "lag.batch.0" ~ 152,
                                      facility_day == 120  & batch == "lag.batch.0" ~ 198,
                                      facility_day == 150  & batch == "lag.batch.0" ~ 213,
                                      facility_day == 180  & batch == "lag.batch.0" ~ 182,
                                      facility_day == 212  & batch == "lag.batch.0" ~ 198,
                                      facility_day == 242  & batch == "lag.batch.0" ~ 213,
                                      facility_day == 272  & batch == "lag.batch.0" ~ 213,
                                      facility_day == 304  & batch == "lag.batch.0" ~ 152,
                                      facility_day == 334  & batch == "lag.batch.0" ~ 236,
                                      facility_day == 364  & batch == "lag.batch.0" ~ 220,
                                      TRUE ~ water.management),
         water.designation = case_when(facility_day == 32 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 62 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 90 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 120 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 150 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 180 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 212 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 242 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 272 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 304 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 334 & batch == "lag.batch.0" ~ "delivery",
                                       facility_day == 364 & batch == "lag.batch.0" ~ "delivery",
                                       TRUE ~ "used"),
         water.management.drum = case_when(batch_day == 29 ~ nursery.tank * nursery.system.division,
                                           batch_day == 59 ~ raceway.tank - nursery.tank * nursery.system.division,
                                           batch_day == 61 ~ as.numeric(drum.sum.95[2,2]) * nursery.system.division,
                                           batch_day == 89 ~ raceway.tank,
                                           batch_day == 91 ~ as.numeric(drum.sum.95[3,2]) * juvenile.system.division,
                                           batch_day == 151 ~ raceway.tank * growout.system.division * 0.1,
                                           batch_day == 165 ~ raceway.tank * growout.system.division * 0.1,
                                           batch_day == 167 ~ purging.unit,
                                           batch_day == 169 ~ packing.volume * growout.system.division,
                                           batch_day == 179 ~ raceway.tank * growout.system.division * 0.1,
                                           batch_day == 183 ~ packing.volume * growout.system.division,
                                           batch_day == 193 ~ raceway.tank * growout.system.division * 0.1,
                                           batch_day == 201 ~ as.numeric(drum.sum.95[4,2]) * growout.system.division,
                                           TRUE ~ 0),
         water.management.drum = case_when(facility_day == 32 & batch == "lag.batch.0" ~ 198,
                                      facility_day == 62  & batch == "lag.batch.0" ~ 152,
                                      facility_day == 90  & batch == "lag.batch.0" ~ 152,
                                      facility_day == 120  & batch == "lag.batch.0" ~ 198,
                                      facility_day == 150  & batch == "lag.batch.0" ~ 213,
                                      facility_day == 180  & batch == "lag.batch.0" ~ 182,
                                      facility_day == 212  & batch == "lag.batch.0" ~ 198,
                                      facility_day == 242  & batch == "lag.batch.0" ~ 213,
                                      facility_day == 272  & batch == "lag.batch.0" ~ 213,
                                      facility_day == 304  & batch == "lag.batch.0" ~ 152,
                                      facility_day == 334  & batch == "lag.batch.0" ~ 236,
                                      facility_day == 364  & batch == "lag.batch.0" ~ 220,
                                      TRUE ~ water.management.drum))

write.csv(prod.plan.water, "tabs/prod.plan.water.csv")

water.use.2022 <- prod.plan.water %>%
  group_by(facility_day) %>%
  group_by(water.designation) %>%
  summarise(water.sum = sum(water.management))

water.use.drum.2022 <- prod.plan.water %>%
  group_by(facility_day) %>%
  group_by(water.designation) %>%
  summarise(water.sum.drum = sum(water.management.drum))

prod.plan.water.cs <- prod.plan.water %>%
  group_by(water.designation) %>%
  mutate(water.cs = cumsum(water.management))

prod.plan.water.cs.plot <- prod.plan.water.cs %>%
  ggplot(aes(facility_day, water.cs, color = water.designation)) +
  geom_line() +
  scale_color_manual("", breaks = c("delivery", "required"),
                     values = c("#d0d1e6", "#74a9cf"),
                     labels = c("Delivery", "Required")) +
  labs(x = "Day", y = expression(paste("Water Volume ", (m^3)))) + 
  theme_minimal()

# print plot to file
pdf("plots/prod_plan_water_use.pdf", height = 4)
print(prod.plan.water.cs.plot)
dev.off()

drJ.tank.v = 0.5 / (pi * (0.35/2)^2) / 60^2

drJ.settling.velocities <- tibble(particle.size = c(50, 100, 150, 200, 250, 300, 400, 500, 1000)) %>%
  mutate(settling.velocity.fw = 9.81 * (particle.size * 1e-6)^2 * (1050 - 998) / (18 * 1.002e-3),
         pc.efficiency.fw = settling.velocity.fw / drJ.tank.v * 100,
         pc.efficiency.fw = case_when(pc.efficiency.fw > 100 ~ 100, TRUE ~ pc.efficiency.fw),
         settling.velocity.sw = 9.81 * (particle.size * 1e-6)^2 * (1050 - 1021.1) / (18 * 1.065e-3),
         pc.efficiency.sw = settling.velocity.sw / drJ.tank.v * 100,
         pc.efficiency.sw = case_when(pc.efficiency.sw > 100 ~ 100, TRUE ~ pc.efficiency.sw))

write.csv(drJ.settling.velocities, "tabs/drJ_settling_velocities.csv")

micron.50.velocity <- c(0.0000707, 0.0000370)
micron.50.d <- sqrt(1/(60^2*micron.50.velocity*pi))







