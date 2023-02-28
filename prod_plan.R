# import libraries
library(tidyverse)
library(RColorBrewer)

# define parameters
harvest.day.1 <- 150
harvest.day.2 <- 180

harvest.95.1 <- 2200
harvest.95.2 <- 600
harvest.101.1 <- 1800
harvest.101.2 <- 400

packing.volume <- 15.6

# tank volumes
nursery.tank <- 15
raceway.tank <- 80
purging.unit <- 5 * 4

shrimp.harvest.day.32 <- as.integer(shrimp.32.g / 7) * 7

batch_frequency <- 30 # days between batches
batch_system_periods <- 1 # overall number of system periods
production_cycle <- 220  # period whose factor includes frequency, system period and exceeds minimum growout period
quarantine.period <- 29
nursery.period <- 30
juvenile.period <- 30
growout.period <- 120
depuration.period <- ceiling((harvest.95.1 + harvest.95.2) * 2 / 1600)

# set production period to two growth periods to observe system loading
prod_plan <- data.frame(facility_day = seq(0, 2 * production_cycle - 1)) %>%
  mutate(batch_1 = case_when(facility_day < production_cycle ~ facility_day))

for (lag_size in c(0:12)) {
  
  new_col_name <- paste0("lag.batch.", lag_size)
  
  prod_plan <- prod_plan %>%
    mutate(!!sym(new_col_name) := lag(batch_1, n = lag_size * batch_frequency, default = NA))
}

# remove column called batch_1
prod.plan <- prod_plan %>% select(-batch_1) %>%
  pivot_longer(c(lag.batch.0:lag.batch.12), names_to = "batch", values_to = "batch_day", values_drop_na = TRUE) %>%
  mutate(system = case_when(batch_day %in% c(0:quarantine.period) ~ "quarantine",
                            batch_day %in% c(quarantine.period + 1: nursery.period) ~ "nursery",
                            batch_day %in% c(quarantine.period + nursery.period + 1 : juvenile.period) ~ "juvenile",
                            batch_day %in% c(quarantine.period + nursery.period + juvenile.period + 1 : growout.period) ~ "growout",
                            batch_day %in% c(quarantine.period + nursery.period + juvenile.period + growout.period + 1 : depuration.period)  ~ "depurate")) %>%
  drop_na() %>%
  filter(batch %in% c("lag.batch.0", "lag.batch.2", "lag.batch.5", "lag.batch.7", "lag.batch.10"))

# custom color
system_cols <- c(quarantine = "#d0d1e6", nursery = "#a6bddb", juvenile = "#74a9cf", growout = "#2b8cbe", depurate = "#045a8d")

systems_plot <- prod.plan %>%
  filter(batch_day <= production_cycle,
         batch %in% c("lag.batch.0", "lag.batch.2", "lag.batch.5", "lag.batch.7", "lag.batch.10")) %>%
  ggplot(aes(facility_day, batch, color = system)) +
  geom_line(size = 3) +
  scale_y_discrete(limits = c("lag.batch.0", "lag.batch.2", "lag.batch.5", "lag.batch.7", "lag.batch.10"),
                   labels = c("Batch 1", "Batch 2", "Batch 3", "Batch 4", "Batch 5")) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "juvenile", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Juvenile", "Growout", "Depurate")) +
  labs(x = "Day", y = "") + 
  theme_minimal()

pdf("plots/systems_plots.pdf", height = 4)
systems_plot
dev.off()

# production plan primo 95
# define parameters
batch_frequency.primo <- 60 # days between batches
batch_system_periods.primo <- 1 # overall number of system periods
production_cycle.primo <- 220
quarantine.period.primo <- 29
nursery.period.primo <- 60
growout.period.primo <- 120
depuration.period.primo <- ceiling((harvest.95.1 + harvest.95.2) * 2 / 1600)

# set primo production period to two growth periods to observe system loading
prod_plan_primo <- data.frame(facility_day = seq(0, 2 * production_cycle.primo - 1)) %>%
  mutate(batch_1 = case_when(facility_day < production_cycle.primo ~ facility_day))

for (lag_size in c(0:12)) {
  
  new_col_name <- paste0("lag.batch.", lag_size)
  
  prod_plan_primo <- prod_plan_primo %>%
    mutate(!!sym(new_col_name) := lag(batch_1, n = lag_size * batch_frequency.primo, default = NA))
}

# remove column called batch_1 from prod_plan_primo
prod.plan.primo <- prod_plan_primo %>% select(-batch_1) %>%
  pivot_longer(c(lag.batch.0:lag.batch.12), names_to = "batch", values_to = "batch_day", values_drop_na = TRUE) %>%
  mutate(system = case_when(batch_day %in% c(0:quarantine.period.primo) ~ "quarantine",
                            batch_day %in% c(quarantine.period.primo + 1 : nursery.period.primo ) ~ "nursery",
                            batch_day %in% c(quarantine.period.primo + nursery.period.primo + 1 : growout.period.primo) ~ "growout",
                            batch_day %in% c(quarantine.period.primo + nursery.period.primo  + growout.period.primo + 1 : depuration.period.primo)  ~ "depurate")) %>%
  drop_na()

# custom color
system_cols.primo <- c(quarantine = "#d0d1e6", nursery = "#a6bddb", growout = "#2b8cbe", depurate = "#045a8d")

systems_plot.primo <- prod.plan.primo %>%
  filter(batch_day <= production_cycle.primo,
         batch %in% c('lag.batch.0', 'lag.batch.1', 'lag.batch.2', 'lag.batch.3', 'lag.batch.4', 'lag.batch.5', 'lag.batch.6')) %>%
  ggplot(aes(facility_day, batch, color = system)) +
  geom_line(size = 3) +
  scale_y_discrete(limits = c('lag.batch.0', 'lag.batch.1', 'lag.batch.2', 'lag.batch.3', 'lag.batch.4', 'lag.batch.5', 'lag.batch.6'),
                   labels = c("Batch 1", "Batch 2", "Batch 3", "Batch 4", "Batch 5", "Batch 6", "Batch 7")) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Day", y = "") + 
  theme_minimal()

pdf("plots/systems_plots.primo.pdf", height = 4)
systems_plot.primo
dev.off()

# production plan 101
# define parameters
batch_frequency.101 <- 60 # days between batches
batch_system_periods.101 <- 1 # overall number of system periods
production_cycle.101 <- 220
quarantine.period.101 <- 29
nursery.period.101 <- 60
growout.period.101 <- 120
depuration.period.101 <- ceiling((harvest.101.1 + harvest.101.2) * 6 / 1600)

# set primo production period to two growth periods to observe system loading
prod_plan_101 <- data.frame(facility_day = seq(0, 2 * production_cycle.101 - 1)) %>%
  mutate(batch_1 = case_when(facility_day < production_cycle.101 ~ facility_day))

for (lag_size in c(0:12)) {
  
  new_col_name <- paste0("lag.batch.", lag_size)
  
  prod_plan_101 <- prod_plan_101 %>%
    mutate(!!sym(new_col_name) := lag(batch_1, n = lag_size * batch_frequency.101, default = NA))
}

# remove column called batch_1 from prod_plan_101
prod.plan.101 <- prod_plan_101 %>% select(-batch_1) %>%
  pivot_longer(c(lag.batch.0:lag.batch.12), names_to = "batch", values_to = "batch_day", values_drop_na = TRUE) %>%
  mutate(system = case_when(batch_day %in% c(0:quarantine.period.101) ~ "quarantine",
                            batch_day %in% c(quarantine.period.101 + 1 : nursery.period.101) ~ "nursery",
                            batch_day %in% c(quarantine.period.101 + nursery.period.101 + 1 : growout.period.101) ~ "growout",
                            batch_day %in% c(quarantine.period.101 + nursery.period.101  + growout.period.101 + 1 : depuration.period.101)  ~ "depurate")) %>%
  drop_na()

# custom color
system_cols.101 <- c(quarantine = "#d0d1e6", nursery = "#a6bddb", growout = "#2b8cbe", depurate = "#045a8d")

systems_plot.101 <- prod.plan.101 %>%
  filter(batch_day <= production_cycle.101,
         batch %in% c('lag.batch.0', 'lag.batch.1', 'lag.batch.2', 'lag.batch.3', 'lag.batch.4', 'lag.batch.5', 'lag.batch.6')) %>%
  ggplot(aes(facility_day, batch, color = system)) +
  geom_line(size = 3) +
  scale_y_discrete(limits = c('lag.batch.0', 'lag.batch.1', 'lag.batch.2', 'lag.batch.3', 'lag.batch.4', 'lag.batch.5', 'lag.batch.6'),
                   labels = c("Batch 1", "Batch 2", "Batch 3", "Batch 4", "Batch 5", "Batch 6", "Batch 7")) +
  scale_color_manual("", breaks = c("quarantine", "nursery", "growout", "depurate"),
                     values = c(system_cols),
                     labels = c("Quarantine", "Nursery", "Growout", "Depurate")) +
  labs(x = "Day", y = "") + 
  theme_minimal()

pdf("plots/systems_plots.101.pdf", height = 4)
systems_plot.101
dev.off()

