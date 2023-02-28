# load libraries
library(tidyverse)


# 95 st 2022 loading parameters
# for marine shrimp in zero-exchange system, Ptan = F * PC * 0.144
# for starvation, TAN =  0.067 mg N-NH3 / h / g
# starvation oxygen consumption ~ 1 mg Oxy/g/h
# carb calculations: TAN * 15.17 is carb requirement, less feed carb content feed * 0.1089 / 0.4
system.parameters.95 <- system.loading %>%
  mutate(total.tan = case_when(system == "depurate" ~ total.bw.kg * 0.067 * 24 / 1000,
                               TRUE ~ total.feed.kg * protein.proportion * 0.144), # total tan in kg
         process.tan = case_when(system == "depurate" ~ process.bw.kg * 0.067 * 24 / 1000,
                                 TRUE ~ process.feed.kg * protein.proportion * 0.144), 
         total.tss = case_when(system == "quarantine" ~ total.feed.kg * 0.25,
                               system == "depurate" ~ total.tan * 0.2,
                               TRUE ~ (total.feed.kg * 0.25) + (total.tan * 8.07)),
         process.tss = case_when(system == "quarantine" ~ process.feed.kg * 0.25,
                                 system == "depurate" ~ process.tan * 0.2,
                                 TRUE ~ (process.feed.kg * 0.25) + (process.tan * 8.07)),
         total.od = case_when(system == "quarantine" ~ total.feed.kg * 0.25 + total.tan * 4.57,
                              system == "depurate" ~ total.bw.kg * 1 / 1000 * 24,
                              TRUE ~ (total.feed.kg * 0.25) + (total.tan * 4.71)),
         process.od = case_when(system == "quarantine" ~ process.feed.kg * 0.25 + process.tan * 4.57,
                                system == "depurate" ~ process.bw.kg * 1 / 1000 * 24,
                                TRUE ~ (process.feed.kg * 0.25) + (process.tan * 4.71)),
         total.alk = case_when(system == "quarantine" | system == "depurate" ~ total.tan * 7.05,
                               TRUE ~ total.tan * 3.57), # 3.57 g of alkalinity consumed in heterotrophic systems
         process.alk = case_when(system == "quarantine" | system == "depurate" ~ process.tan * 7.05,
                                 TRUE ~ process.tan * 3.57),
         total.carb = case_when(system == "quarantine" | system == "depurate" ~ 0,
                                TRUE ~ total.feed.kg * protein.proportion * 0.144 * 15.17 - (total.feed.kg * 0.1089 / 0.4)),
         process.carb = case_when(system == "quarantine" | system == "depurate" ~ 0,
                                  TRUE ~ process.feed.kg * protein.proportion * 0.144 * 15.17 - (process.feed.kg * 0.1089 / 0.4)),
         total.co2 =  total.od * 1.375,
         process.co2 = process.od * 1.375)

# 95 st 2023 loading parameters
# for marine shrimp in zero-exchange system, Ptan = F * PC * 0.144
system.parameters.primo <- system.loading.primo %>%
  mutate(total.tan = case_when(system == "depurate" ~ total.bw.kg * 0.067 * 24 / 1000,
                               TRUE ~ total.feed.kg * protein.proportion * 0.144), # total tan in kg
         process.tan = case_when(system == "depurate" ~ process.bw.kg * 0.067 * 24 / 1000,
                                 TRUE ~ process.feed.kg * protein.proportion * 0.144), 
         total.tss = case_when(system == "quarantine" ~ total.feed.kg * 0.25,
                               system == "depurate" ~ total.tan * 0.2,
                               TRUE ~ (total.feed.kg * 0.25) + (total.tan * 8.07)),
         process.tss = case_when(system == "quarantine" ~ process.feed.kg * 0.25,
                                 system == "depurate" ~ process.tan * 0.2,
                                 TRUE ~ (process.feed.kg * 0.25) + (process.tan * 8.07)),
         total.od = case_when(system == "quarantine" ~ total.feed.kg * 0.25 + total.tan * 4.57,
                              system == "depurate" ~ total.bw.kg * 1 / 1000 * 24,
                              TRUE ~ (total.feed.kg * 0.25) + (total.tan * 4.71)),
         process.od = case_when(system == "quarantine" ~ process.feed.kg * 0.25 + process.tan * 4.57,
                                system == "depurate" ~ process.bw.kg * 1 / 1000 * 24,
                                TRUE ~ (process.feed.kg * 0.25) + (process.tan * 4.71)),
         total.alk = case_when(system == "quarantine" | system == "depurate" ~ total.tan * 7.05,
                               TRUE ~ total.tan * 3.57), # 3.57 g of alkalinity consumed in heterotrophic systems
         process.alk = case_when(system == "quarantine" | system == "depurate" ~ process.tan * 7.05,
                                 TRUE ~ process.tan * 3.57),
         total.carb = case_when(system == "quarantine" | system == "depurate" ~ 0,
                                TRUE ~ total.feed.kg * protein.proportion * 0.144 * 15.17 - (total.feed.kg * 0.1089 / 0.4)),
         process.carb = case_when(system == "quarantine" | system == "depurate" ~ 0,
                                  TRUE ~ process.feed.kg * protein.proportion * 0.144 * 15.17 - (process.feed.kg * 0.1089 / 0.4)),
         total.co2 =  total.od * 1.375,
         process.co2 = process.od * 1.375)

# 101 st 2024 loading parameters
# for marine shrimp in zero-exchange system, Ptan = F * PC * 0.144
system.parameters.101 <- system.loading.101 %>%
  mutate(total.tan = case_when(system == "depurate" ~ total.bw.kg * 0.067 * 24 / 1000,
                               TRUE ~ total.feed.kg * protein.proportion * 0.144), # total tan in kg
         process.tan = case_when(system == "depurate" ~ process.bw.kg * 0.067 * 24 / 1000,
                                 TRUE ~ process.feed.kg * protein.proportion * 0.144), 
         total.tss = case_when(system == "quarantine" ~ total.feed.kg * 0.25,
                               system == "depurate" ~ total.tan * 0.2,
                               TRUE ~ (total.feed.kg * 0.25) + (total.tan * 8.07)),
         process.tss = case_when(system == "quarantine" ~ process.feed.kg * 0.25,
                                 system == "depurate" ~ process.tan * 0.2,
                                 TRUE ~ (process.feed.kg * 0.25) + (process.tan * 8.07)),
         total.od = case_when(system == "quarantine" ~ total.feed.kg * 0.25 + total.tan * 4.57,
                              system == "depurate" ~ total.bw.kg * 1 / 1000 * 24,
                              TRUE ~ (total.feed.kg * 0.25) + (total.tan * 4.71)),
         process.od = case_when(system == "quarantine" ~ process.feed.kg * 0.25 + process.tan * 4.57,
                                system == "depurate" ~ process.bw.kg * 1 / 1000 * 24,
                                TRUE ~ (process.feed.kg * 0.25) + (process.tan * 4.71)),
         total.alk = case_when(system == "quarantine" | system == "depurate" ~ total.tan * 7.05,
                               TRUE ~ total.tan * 3.57), # 3.57 g of alkalinity consumed in heterotrophic systems
         process.alk = case_when(system == "quarantine" | system == "depurate" ~ process.tan * 7.05,
                                 TRUE ~ process.tan * 3.57),
         total.carb = case_when(system == "quarantine" | system == "depurate" ~ 0,
                                TRUE ~ total.feed.kg * protein.proportion * 0.144 * 15.17 - (total.feed.kg * 0.1089 / 0.4)),
         process.carb = case_when(system == "quarantine" | system == "depurate" ~ 0,
                                  TRUE ~ process.feed.kg * protein.proportion * 0.144 * 15.17 - (process.feed.kg * 0.1089 / 0.4)),
         total.co2 =  total.od * 1.375,
         process.co2 = process.od * 1.375)

# get max loading for individual system processes - filter one batch
tss.process.loading.101 <- system.parameters.101 %>%
  filter(system %in% c("nursery", "growout")) %>%
  group_by(system) %>%
  summarise(feed.max = max(process.feed.kg),
            tss.max = max(process.tss),
            tss.avg = mean(process.tss),
            flow.max = max(required.flow),
            flow.avg = mean(required.flow),
            stijn.max = max(stijn.experience),
            stijn.avg = mean(stijn.experience)) %>%
  arrange(factor(system, levels = c("quarantine", "nursery", "growout", "depurate")))
  
