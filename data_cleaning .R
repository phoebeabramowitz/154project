setwd("~/Desktop/Berkeley/Spring19/stat_154/154project")

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
  geom_histogram(aes(x=hamatop),color="darkblue", fill="lightblue")+
  ggtitle("Net Incident PAR")+
  theme_minimal()
ggplot(data_log) +
  geom_histogram(aes(x=hamatop),color="darkblue", fill="lightblue")+
  ggtitle("Log Incident PAR")+
  theme_minimal()

# Reflected PAR histograms
ggplot(data_net) +
  geom_histogram(aes(x=hamabot),color="darkblue", fill="lightblue")+
  ggtitle("Net Reflected PAR")+
  theme_minimal()
ggplot(data_log) +
  geom_histogram(aes(x=hamabot),color="darkblue", fill="lightblue")+
  ggtitle("Log Reflected PAR")+
  theme_minimal()

# Voltage histograms
ggplot(data_net) +
  geom_histogram(aes(x=voltage),color="darkblue", fill="lightblue")+
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

# Cleaning all_readings before adding mote_location3
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
# Concatenate everything in data_net with everything in data_log
# that isn't in data_net, defined by epoch and nodeid
just_log <- anti_join(data_log, data_net, by = c("nodeid" = "nodeid", "epoch" = "epoch"))
all_readings <- full_join(data_net, just_log)
# to combat different error all_readings <- all_readings[,1:11]

# Cleaning all_readings before adding mote_location
df <- all_readings %>%
  select(c("result_time", "epoch", "nodeid", "voltage", "humid_temp", "humid_adj", "hamatop", "hamabot")) %>%
  rename(temp = humid_temp) %>%
  rename(humid = humid_adj) %>%
  rename(incident_PAR = hamatop) %>%
  rename(reflect_PAR = hamabot) %>%
  mutate(result_time = as.POSIXct(result_time))

#Number of missing values for each variable(don't display this)
sum(is.na(df$temp))
sum(is.na(df$humid))
sum(is.na(df$incident_PAR))
sum(is.na(df$reflect_PAR))
#Initial Number of rows missing any measurements
na_vals <- df %>% filter(is.na(temp)) %>% filter(is.na(humid)) %>%
  filter(is.na(incident_PAR)) %>% filter(is.na(reflect_PAR))
#Remove missing Data rows
df <- df %>% filter(!is.na(temp)) %>% filter(!is.na(humid)) %>%
  filter(!is.na(incident_PAR)) %>% filter(!is.na(reflect_PAR))

# Incorporate mote_location into all_readings
mote_location <- mote_location %>%
  rename(nodeid = ID)
main <- left_join(df, mote_location) %>%
  rename(height = Height) %>%
  rename(direc = Direc) %>%
  rename(dist = Dist) %>%
  rename(tree = Tree)

#337743 observations of 12 variables

# use histograms and quantiles on main, to show outliers. data_main below gets rid of faulty voltage readings..
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
data_main <- main %>%
  filter(voltage >= 2.4 & voltage <= 3)
data_main <- data_main %>% filter(!is.na(height))
# removes 33,833 rows

for(i in 1:nrow(data_log)){
  epoch_i <- data_log[i,"epoch"]
  net_epoch_i <- filter(data_net,epoch==epoch_i)
  if (nrow(net_epoch_i)>0){
    date_time_i <- net_epoch_i[1,"result_time"]
    data_log[i,"result_time"] <- date_time_i
  }
}

ggplot(data_main %>% filter(result_time <= as.Date("2004-11-10"))) +
  geom_histogram(aes(x=result_time), color="violet",fill="darkblue", binwidth=100)+
  ggtitle("Occurences of Data Values Over Time")+
  theme_minimal()

data_sub <- data_main %>%
  filter(result_time >= as.Date("2004-05-23") & result_time <= as.Date("2004-05-30"))


ggplot(data_sub)+
  geom_point(aes(x=height, y=nodeid),color="darkgoldenrod")+
  ggtitle("nodeid by height")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=epoch, y=humid, alpha=0.3),color="firebrick2")+
  ggtitle("Humidity Over Time")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=height, y=humid, alpha=0.3),color="violet")+
  ggtitle("Humidity by Height")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=epoch, y=temp, alpha=0.2),color="orangered1")+
  ggtitle("Temperature Over Time")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=height, y=temp, alpha=0.3),color="deeppink1")+
  ggtitle("Temperature by Height")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=epoch, y=reflect_PAR, alpha=0.3),color="deeppink1")+
  ggtitle("Reflective PAR Over Time")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=height, y=reflect_PAR, alpha=0.3), color="deeppink1")+
  ggtitle("Reflective PAR by Height")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=epoch, y=incident_PAR, alpha=0.3),color="orangered1")+
  ggtitle("Incident PAR Over Time")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=height, y=incident_PAR, alpha=0.3), color="deeppink1")+
  ggtitle("Incident PAR by Height")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=nodeid, y=incident_PAR, alpha=0.3), color="deeppink1")+
  ggtitle("Incident PAR by Node")+
  theme_minimal()
ggplot(data_main %>% filter(result_time <= as.Date("2004-11-10"))) +
  geom_point(aes(x=result_time, y=epoch))+
  ggtitle("Linear relationship between epoch and result_time")

ggplot(data_sub)+
  geom_point(aes(x=epoch, y=incident_PAR, alpha=0.3,color=height))+
  ggtitle("Incident PAR Over Time")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=epoch, y=reflect_PAR, alpha=0.3,color=height))+
  ggtitle("Reflective PAR Over Time")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=epoch, y=temp, alpha=0.3,color=height))+
  ggtitle("Temperature Over Time")+
  theme_minimal()

ggplot(data_sub)+
  geom_point(aes(x=epoch, y=humid, alpha=0.3,color=height))+
  ggtitle("Humidity Over Time")+
  theme_minimal()

dat <- data_sub %>%
  select(c("temp", "humid", "incident_PAR", "reflect_PAR", "height"))

pca <- prcomp(na.omit(dat), scale. = TRUE)
summary(pca)

loadings <- pca$rotation
scores <- pca$x
eigenvalues <- pca$sdev^2
eigenvalues
sum(eigenvalues)

eigs_cum = cumsum(eigenvalues) / sum(eigenvalues)
ggplot() + geom_point(aes(x = 1:length(eigenvalues), y=eigs_cum)) +
  labs(x = "Principal Component", y = "Fraction of Total Variance Explained") +
  ggtitle("Screeplot") + theme_minimal()

ggplot() +
  geom_point(aes(x = scores[, 1], y=scores[, 2], alpha=0.1)) +
  labs(x = "PC1", y = "PC2") + ggtitle("PC1 vs. PC2")

biplot(pca, scale=0)

hierarch <- hclust(dist(scale(dat)), method="complete")
summary(hierarch)
plot(hierarch, main="Complete Linkage", xlab="", sub="",
     cex = .01)
abline(h=7, col="red")

K6 <- kmeans(na.omit(dat), 6)
km.clusters <- K6$cluster
table(km.clusters, hc.clusters[1:length(km.clusters)] )

# perform hierarchical clustering on the first 2 or  principal components..
hc <- hclust(dist(scores[,1:2]))
plot(hc, main="Hier. Clustering on First 2 Principal Components", xlab="", sub="",
     cex = .01)
abline(h=5, col="red")

# hierarchical clustering on the data vs on the first two principal components
table(hc.clusters) # on the dat
table(cutree(hc, 6)) # on the principal components

ggplot(data_sub)+
  geom_point(aes(x=incident_PAR, y=reflect_PAR, alpha=0.3),color="darkgoldenrod")+
  ggtitle("Incident vs Refletive PAR")+
  theme_minimal()

ggplot(data_sub)+
  geom_line(aes(x=result_time, y=incident_PAR, color="red"))+
  geom_line(aes(x=result_time,y=reflect_PAR, color="blue"))+
  ggtitle("Incident and Reflective PAR over Time")+
  theme_minimal()

ggplot(data_main) +
  geom_histogram(aes(x=incident_PAR),color="darkblue", fill="lightblue") +
  ggtitle("Incident PAR") +
  scale_y_sqrt() +
  theme_minimal()

ggplot(data_main) +
  geom_histogram(aes(x=reflect_PAR),color="darkblue", fill="lightblue") +
  ggtitle("Reflected PAR") +
  scale_y_sqrt() +
  theme_minimal()
