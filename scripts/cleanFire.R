### Used for merging NASA fire hotspot arhive data, retrieved from https://firms.modaps.eosdis.nasa.gov/download/

#setwd("/Users/ronniebarker/Documents/Uni/3164/bushfire")
fire1 <- read.csv('fire_archive_M6_145466.csv')
fire2 <- read.csv('fire_archive_V1_145468.csv')
fire3 <- read.csv('fire_nrt_J1V-C2_145467.csv')
fire4 <- read.csv('fire_nrt_M6_145466.csv')
fire5 <- read.csv('fire_nrt_V1_145468.csv')

#see what columns
colnames(fire1)
colnames(fire2)
colnames(fire3)
colnames(fire4)
colnames(fire5)

#pre-process make have same columns
fire1 <- fire1[,-c(3,12,14,15)] #remove brightness, type and daynight
fire2 <- fire2[,-c(3,12,14)] #remove brightness and type
fire3 <- fire3[,-c(3,12,14)] #remove brightness and daynight
fire4 <- fire4[,-c(3,12,14)] #remove brightness and daynight
fire5 <- fire5[,-c(3,12,14)] #remove brightness and 

#join
fire <- rbind(fire1, fire2)
fire <- rbind(fire, fire3)
fire <- rbind(fire, fire4)
fire <- rbind(fire, fire5)

#export
write.csv(fire, "cleanFire.csv")
