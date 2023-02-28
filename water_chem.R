# load libraries
library(tidyverse)

# import data
water_chem <- read.csv("data/water_chem.csv")

# clean up spreadsheet
water_chem <- water_chem %>%
  mutate(DATE = as.Date(DATE, origin = "1899-12-30"),
         pH = as.numeric(str_replace(pH, "-", "")),
         Tempmax = str_replace(Tempmax, "-", ""),
         Tempmax = as.numeric(str_replace(Tempmax, ",", ".")),
         Feed = as.numeric(str_remove_all(Feed, "[:alpha:]|\\s")),
         Solids = as.numeric(str_remove_all(Solids, "[:alpha:]|\\s|\\(|\\)|\\-")),
         Settling.Tank = str_to_lower(Settling.Tank),
         Foam.Level = as.numeric(str_remove_all(Foam.Level, "[:alpha:]|\\/|\\s")))

# mutate some useful parameters
water_chem_4 <- water_chem %>%
  filter(DATE > "2021-12-31") %>%
  mutate(batch = case_when(DATE < "2022-02-17" ~ "batch_1",
                           DATE >= "2022-02-17" & DATE <= "2022-08-18" ~ "batch_2",
                           DATE >= "2022-08-20" & DATE <= "2022-11-30" ~ "batch_3"),
         total.feed = Feed * 8,
         Nitrite = case_when(Nitrite > 100 ~ Nitrite / 1000,
                             TRUE ~ Nitrite),
         pH = replace(pH, pH > 14, NA_real_),
         Settling.Tank = case_when(Settling.Tank == "no" ~ 0,
                                   Settling.Tank == "yes" ~ 1,
                                   TRUE ~ 0),
         pka = 0.0901821 + 2729.92/(Tempmin + 273.15),
         ionic.strength = 19.9273 * Salinity / (1000 - 1.005109 * Salinity),
         pkas = pka + (0.1552 - 0.000314 * Tempmin) * ionic.strength,
         mole.fraction = 1 / (1 + 10^(pkas - pH)),
         free.ammonia = TAN * mole.fraction,
         molasses = case_when(Solids < 1 ~ 1.6, TRUE ~ 0))

# plot TAN
rw_4_tan <- water_chem_4 %>%
  ggplot(aes(DATE, TAN, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "TAN (mg/l)") + 
  theme_minimal()

# plot molasses
rw_4_molasses <- water_chem_4 %>%
  ggplot(aes(DATE, molasses, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "TAN (mg/l)") + 
  theme_minimal()

# plot free ammonia
rw_4_free.nh4 <- water_chem_4 %>%
  ggplot(aes(DATE, free.ammonia, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "Free Ammonia (mg/l)") + 
  theme_minimal()

water_chem_4 %>%
  ggplot() +
  stat_count(mapping = aes(x = TAN, y = ..prop..))
  

# plot nitrite
rw_4_nit <- water_chem_4 %>%
  ggplot(aes(DATE, Nitrite, color = batch)) +
  geom_line() +
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "Nitrite (mg/l)") + 
  theme_minimal()

# plot Alk
rw_4_alk <- water_chem_4 %>%
  ggplot(aes(DATE, ALK, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "Alkalinity (mg/l)") + 
  theme_minimal()

# plot pH
rw_4_ph <- water_chem_4 %>%
  ggplot(aes(DATE, pH, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "pH") + 
  theme_minimal()

# plot salinity
rw_4_sal <- water_chem_4 %>%
  ggplot(aes(DATE, Salinity, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "Salinity (ppt)") + 
  theme_minimal()

# plot DO
rw_4_do <- water_chem_4 %>%
  ggplot(aes(DATE, DOmin, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "DO min (mg/l)") + 
  theme_minimal()

# plot solids
rw_4_solids <- water_chem_4 %>%
  ggplot(aes(DATE, Solids, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "Solids (ml)") + 
  theme_minimal()

# plot settling tank
rw_4_settling <- water_chem_4 %>%
  ggplot(aes(DATE, Settling.Tank, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "Settling Tank (1/0)") + 
  theme_minimal()

# plot total feed
rw_4_feed <- water_chem_4 %>%
  ggplot(aes(DATE, total.feed / 1000, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "Total Feed (kg)") + 
  theme_minimal()


# estimate cumulative nitrate based on nitrite production
cumulative.nitrate.95 <- water_chem_4 %>%
  mutate(total.volume = 90000 * Water.Level / 36,
         total.nitrite = Nitrite * total.volume) %>%
  group_by(batch) %>%
  mutate(cs = cumsum(total.nitrite),
         cumulative.nitrate = cs / total.volume)

# plot cumulative nitrate
rw_4_nitrate <- cumulative.nitrate.95 %>%
  ggplot(aes(DATE, cumulative.nitrate, color = batch)) +
  geom_line()+
  scale_color_manual("", breaks = c("batch_1", "batch_2", "batch_3"),
                     values = c("#d0d1e6", "#a6bddb", "#74a9cf"),
                     labels = c("Batch 1", "Batch 2", "Batch 3")) +
  labs(x = "Date", y = "Cumulative Nitrate (mg/l)") + 
  theme_minimal()

rainfall <- read.csv("data/rain_aet.csv") %>%
  mutate(date = as.Date(date))

month.order <- c("January", "February", "March", "April",
                 "May", "June", "July", "August",
                 "September", "October", "November", "December")

# custom color
rain.cols <- c(rainfall = "#a6bddb", aet = "#045a8d")

rainfall %>%
  ggplot(aes(date, measure_in, color = condition)) +
  geom_line() +
  scale_color_manual("", breaks = c("rainfall", "aet"),
                     values = c(rain.cols),
                     labels = c("Rainfall", "AET")) +
  labs(x = "Date", y = "Precipitation/ Evapo-transpiration (Inches)") + 
  theme_minimal()

primo.water <- read.csv("data/primo_water_21.csv")

primo.water.21 <- primo.water %>%
  mutate(vol.cube = vol_gal / 0.26417205235815 / 1000)

primo.water.21 %>%
  ggplot(aes(factor(Month), vol.cube)) +
  geom_col(color = "#045a8d",fill =  "#a6bddb") +
  scale_x_discrete(limits = month.order, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "", y = "Monthly water demand (m^3)") + 
  theme_minimal()
  
# calculate free ammonia fraction
pka = 0.0901821 + 2729.92/(273.2 + 30)
1 / (10^(pka - seq(7,10,0.5)) + 1)
