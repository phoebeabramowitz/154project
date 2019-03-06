#setwd("~/Desktop/Berkeley/Spring19/stat_154/154project")

mote_location <- read.delim("./data/mote-location-data.txt", sep="")
<<<<<<< HEAD

datetime <- read.csv("./data/sonoma-dates", header=FALSE)

#data retrieved over the wireless network
data_net<- read.csv("./data/sonoma-data-net.csv")

#data retrieved from the flash logs
data_log <- read.csv("./data/sonoma-data-log.csv")

#concatenated
=======
#data retrieved over the wireless network
data_net <- read.csv("./data/sonoma-data-net.csv")
#data retrieved from the flash logs
data_log <- read.csv("./data/sonoma-data-log.csv")
#simply concatenated
>>>>>>> 03edcb8bf1f34c0f0802daad34d3fc3e00bb03de
data_all <- read.csv("./data/sonoma-data-all.csv")

#nodeid and epoch together provides a unique identifier for one measure. 



