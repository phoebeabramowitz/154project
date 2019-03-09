#setwd("~/Desktop/Berkeley/Spring19/stat_154/154project")

library(tidyverse)
library(GGally)
library(dplyr)
library(ggplot2)
library(lubridate)

#location data
mote_location <- read.delim("./data/mote-location-data.txt", sep="")

#datetime
datetime <- read.csv("./data/sonoma-dates", header=FALSE)

#data retrieved from the flash logs
data_log <- read.csv("./data/sonoma-data-log.csv")

#data retrieved over the wireless network
data_net <- read.csv("./data/sonoma-data-net.csv")

#simply concatenated
data_all <- read.csv("./data/sonoma-data-all.csv")

# Incident PAR histograms
data_log <- filter(data_log, hamatop < 10*quantile(data_log$hamatop, na.rm=TRUE)[4])
ggplot(data_net) +
  geom_histogram(aes(x=hamatop),color="darkblue", fill="lightblue",binwidth=20)+
  ggtitle("Net Incident PAR")+
  theme_minimal()
ggplot(data_log) +
  geom_histogram(aes(x=hamatop),color="darkblue", fill="lightblue",binwidth=20)+
  ggtitle("Log Incident PAR")+
  theme_minimal()

# Reflected PAR histograms
ggplot(data_net) +
  geom_histogram(aes(x=hamabot),color="darkblue", fill="lightblue",binwidth=6)+
  ggtitle("Net Reflected PAR")+
  theme_minimal()
ggplot(data_log) +
  geom_histogram(aes(x=hamabot),color="darkblue", fill="lightblue",binwidth=6)+
  ggtitle("Log Reflected PAR")+
  theme_minimal()

# Voltage histograms
ggplot(data_net) +
  geom_histogram(aes(x=voltage),color="darkblue", fill="lightblue",binwidth=10)+
  ggtitle("Net Unconverted Voltage")+
  theme_minimal()
ggplot(data_log) +
  geom_histogram(aes(x=voltage),color="darkblue", fill="lightblue",binwidth=0.2)+
  ggtitle("Log Voltage")+
  theme_minimal()

# Converting data to same range ONLY RUN THIS ONCE
#Incident and Reflected PAR
data_net$hamatop <- data_net$hamatop*0.0185
data_net$hamabot <- data_net$hamabot*0.0185
data_log$hamatop <- data_log$hamatop*0.0185
data_log$hamabot <- data_log$hamabot*0.0185
# Voltage
data_net$voltage = data_net$voltage*12.33 / 1023

# Removing missing data...

#Number of missing values for each variable(don't display this)
sum(is.na(df$temp))
sum(is.na(df$humid))
sum(is.na(df$incident_PAR))
sum(is.na(df$reflect_PAR))
#Initial Number of rows missing any measurements
na_vals <- df %>% filter(is.na(temp)) %>% filter(is.na(humid)) %>%
  filter(is.na(incident_PAR)) %>% filter(is.na(reflect_PAR))
#we dont want it to evaluate this value
nrow(na_vals)
#Remove missing Data rows
df <- df %>% filter(!is.na(temp)) %>% filter(!is.na(humid)) %>%
  filter(!is.na(incident_PAR)) %>% filter(!is.na(reflect_PAR))

# Concatenate everything in data_net with everything in data_log
# that isn't in data_net, defined by epoch and nodeid
just_log <- anti_join(data_log, data_net, by = c("nodeid" = "nodeid", "epoch" = "epoch"))
all_readings <- full_join(data_net, just_log)
all_readings <- all_readings[,1:11]

# Cleaning all_readings before adding mote_location
df <- all_readings %>%
  select(c("result_time", "epoch", "nodeid", "voltage", "humid_temp", "humid_adj", "hamatop", "hamabot")) %>%
  rename(temp = humid_temp) %>%
  rename(humid = humid_adj) %>%
  rename(incident_PAR = hamatop) %>%
  rename(reflect_PAR = hamabot) %>%
  mutate(result_time = as.POSIXct(result_time))

# Incorporate mote_location into all_readings
mote_location <- mote_location %>%
  rename(nodeid = ID)

main <- left_join(df, mote_location) %>%
  rename(height = Height) %>%
  rename(direc = Direc) %>%
  rename(dist = Dist) %>%
  rename(tree = Tree)

# using histogram and quantiles to visually identify easy outliers
ggplot(main) +
geom_histogram(aes(x=voltage),color="darkblue", fill="lightblue",binwidth=1)+
  ggtitle("Voltage")+
  theme_minimal()

ggplot(main) +
  geom_histogram(aes(x=temp),color="darkblue", fill="lightblue",binwidth=4)+
  ggtitle("Temperature")+
  theme_minimal()

ggplot(main) +
  geom_histogram(aes(x=humid),color="darkblue", fill="lightblue",binwidth=4)+
  ggtitle("Humidity")+
  theme_minimal()

summary(main$reflect_PAR)
summary(main$incident_PAR)

# Remove outliers based on faulty voltage readings
# removes 33,833 rows..

data_main <- main %>%
  filter(voltage >= 2.4 & voltage <= 3)
