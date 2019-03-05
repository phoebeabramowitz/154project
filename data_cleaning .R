#setwd("~/Desktop/Berkeley/Spring19/stat_154/154project")

mote_location <- read.delim("./data/mote-location-data.txt", sep="")

#data retrieved over the wireless network
data_net <- read.csv("./data/sonoma-data-net.csv")

#data retrieved from the flash logs
data_log <- read.csv("./data/sonoma-data-log.csv")

#simply concatenated
data_all <- read.csv("./data/sonoma-data-all.csv")







