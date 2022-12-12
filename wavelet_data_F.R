# 1-males, 2-females, 3-both sexes

########################
#                      #
# Number of death      #
#                      #
########################

library(tidyverse)
library(reshape)
library(data.table)

s=2

usa <- read.csv("USA_d_short_idr.csv",header = T)
usa_F <- usa %>% filter(sex==sex,cause!=0)
usa_F <- usa_F[,-c(1,3,4,5,7,26,28,30)]

head(usa_F)

usa_F <- melt(usa_F, id.vars =c("cause","year"), measure.vars = c("d0", "d1", "d5", "d10", 
                                                                  "d15", "d20", "d25", "d30", 
                                                                  "d35", "d40", "d45", "d50", 
                                                                  "d55", "d60", "d65", "d70",
                                                                  "d75", "d80", "d85", "d90",
                                                                  "d95","d100p"))


head(usa_F)

usa_F$Age <- NA
usa_F$Age[usa_F$variable=="d0"] <- 0
usa_F$Age[usa_F$variable=="d1"] <- 1
usa_F$Age[usa_F$variable=="d5"] <- 5
usa_F$Age[usa_F$variable=="d10"] <- 10
usa_F$Age[usa_F$variable=="d15"] <- 15
usa_F$Age[usa_F$variable=="d20"] <- 20
usa_F$Age[usa_F$variable=="d25"] <- 25
usa_F$Age[usa_F$variable=="d30"] <- 30
usa_F$Age[usa_F$variable=="d35"] <- 35
usa_F$Age[usa_F$variable=="d40"] <- 40
usa_F$Age[usa_F$variable=="d45"] <- 45
usa_F$Age[usa_F$variable=="d50"] <- 50
usa_F$Age[usa_F$variable=="d55"] <- 55
usa_F$Age[usa_F$variable=="d60"] <- 60
usa_F$Age[usa_F$variable=="d65"] <- 65
usa_F$Age[usa_F$variable=="d70"] <- 70
usa_F$Age[usa_F$variable=="d75"] <- 75
usa_F$Age[usa_F$variable=="d80"] <- 80
usa_F$Age[usa_F$variable=="d85"] <- 85
usa_F$Age[usa_F$variable=="d90"] <- 90
usa_F$Age[usa_F$variable=="d95"] <- 95
usa_F$Age[usa_F$variable=="d100p"] <- 100


usa_F <- as.data.table(usa_F)

names(usa_F)[4]<-"dx.tot.by.Caus"

usa_F$Cause_Rev[usa_F$cause==1] <- 1 # inf
usa_F$Cause_Rev[usa_F$cause==2] <- 2 # neop
usa_F$Cause_Rev[usa_F$cause==3] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==4] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==5] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==6] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==7] <- 3 # CVD
usa_F$Cause_Rev[usa_F$cause==8] <- 3 # CVD
usa_F$Cause_Rev[usa_F$cause==9] <- 3 # CVD
usa_F$Cause_Rev[usa_F$cause==10] <- 4 # RESP
usa_F$Cause_Rev[usa_F$cause==11] <- 4 # RESP
usa_F$Cause_Rev[usa_F$cause==12] <- 5 # DIG
usa_F$Cause_Rev[usa_F$cause==13] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==14] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==15] <- 7 # oth
usa_F$Cause_Rev[usa_F$cause==16] <- 6 # ext
#usa_F$Age <- as.character(usa_F$Age)

usa_F %>%filter(year==1979,Age%in%c(0),cause%in%c(7,8,9)) %>%select(dx.tot.by.Caus) %>%  sum()

head(usa_F)
usa_F=usa_F[,.(dx.tot.by.Caus = sum(dx.tot.by.Caus)), keyby = .(year, Cause_Rev, Age)]

usa_F$Sex <- "f"

usa_F=usa_F[,.(dx.tot.by.Caus = sum(dx.tot.by.Caus)), keyby = .(year, Cause_Rev)]
head(usa_F)

save(usa_F,file="dx_usa_F.RData")

########################
# Exposure
########################



rm(list = ls())


library(tidyverse)
library(reshape)
library(data.table)

s=2
usa <- read.csv("USA_e.csv",header = T)
head(usa)

exp_usa_F <- usa %>% filter(sex==sex)
exp_usa_F <- exp_usa_F[,-c(1,3,4,5,24,26,28)]
head(exp_usa_F)


exp_usa_F <- melt(exp_usa_F, id.vars =c("year"), measure.vars = c("e0", "e1", "e5", "e10", 
                                                                  "e15", "e20", "e25", "e30", 
                                                                  "e35", "e40", "e45", "e50", 
                                                                  "e55", "e60", "e65", "e70",
                                                                  "e75", "e80", "e85", "e90",
                                                                  "e95","e100p"))


exp_usa_F$Age <- NA
exp_usa_F$Age[exp_usa_F$variable=="e0"] <- 0
exp_usa_F$Age[exp_usa_F$variable=="e1"] <- 1
exp_usa_F$Age[exp_usa_F$variable=="e5"] <- 5
exp_usa_F$Age[exp_usa_F$variable=="e10"] <- 10
exp_usa_F$Age[exp_usa_F$variable=="e15"] <- 15
exp_usa_F$Age[exp_usa_F$variable=="e20"] <- 20
exp_usa_F$Age[exp_usa_F$variable=="e25"] <- 25
exp_usa_F$Age[exp_usa_F$variable=="e30"] <- 30
exp_usa_F$Age[exp_usa_F$variable=="e35"] <- 35
exp_usa_F$Age[exp_usa_F$variable=="e40"] <- 40
exp_usa_F$Age[exp_usa_F$variable=="e45"] <- 45
exp_usa_F$Age[exp_usa_F$variable=="e50"] <- 50
exp_usa_F$Age[exp_usa_F$variable=="e55"] <- 55
exp_usa_F$Age[exp_usa_F$variable=="e60"] <- 60
exp_usa_F$Age[exp_usa_F$variable=="e65"] <- 65
exp_usa_F$Age[exp_usa_F$variable=="e70"] <- 70
exp_usa_F$Age[exp_usa_F$variable=="e75"] <- 75
exp_usa_F$Age[exp_usa_F$variable=="e80"] <- 80
exp_usa_F$Age[exp_usa_F$variable=="e85"] <- 85
exp_usa_F$Age[exp_usa_F$variable=="e90"] <- 90
exp_usa_F$Age[exp_usa_F$variable=="e95"] <- 95
exp_usa_F$Age[exp_usa_F$variable=="e100p"] <- 100


exp_usa_F <- as.data.table(exp_usa_F)

head(exp_usa_F)
names(exp_usa_F)[3]<-"exp"

exp_usa_F=exp_usa_F[,.(Exp = sum(exp)), keyby = .(year)]
exp_usa_F$Sex <- "f"
head(exp_usa_F)
save(exp_usa_F,file="Ex_usa_F.RData")

rm(list = ls())

load("dx_usa_F.RData")
load("Ex_usa_F.RData")

head(exp_usa_F)
exp_usa_F
usa_F$Sex <- "f"
str(usa_F)
str(exp_usa_F)
tot_F <- left_join(usa_F,exp_usa_F) 

tot_F <- tot_F[,.(mx = dx.tot.by.Caus/Exp), keyby = .(year,Cause_Rev)]
str(tot_F)
tot_F %>% ggplot(aes(year,mx,color=as.factor(Cause_Rev)))+geom_line()

save(tot_F,file="Wavelet_Data_F.RData")
