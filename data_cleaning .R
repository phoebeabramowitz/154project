#setwd("~/Desktop/Berkeley/Spring19/stat_154/154project")

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
