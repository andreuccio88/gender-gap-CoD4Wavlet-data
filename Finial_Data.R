load("Wavelet_Data_F.RData")
load("Wavelet_Data_M.RData")
library(tidyverse)
library(ggplot2)
library(dplyr)

head(tot_F)
str(tot_F)


tot_F <-tot_F %>% filter(Cause_Rev%in%c(1,2)) %>% rename(mx_F=mx)

head(tot_F)
head(tot_M)
tot_M <-tot_M %>% filter(Cause_Rev%in%c(1,2)) %>% rename(mx_M=mx)


TOTAL <- cbind(tot_M,tot_F)
TOTAL$Ggap <- TOTAL$mx_M/TOTAL$mx_F
head(TOTAL)
str(TOTAL)
TOTAL <- TOTAL[,c(1,2,7)]


TOTAL$Cause_Rev[TOTAL$Cause_Rev==1] <- "Cancer"
TOTAL$Cause_Rev[TOTAL$Cause_Rev==2] <- "CVD"

TOTAL %>%ggplot(aes(year,Ggap,col=as.factor(Cause_Rev)))+geom_line()

write.csv2(TOTAL,"GGAP.csv2")



