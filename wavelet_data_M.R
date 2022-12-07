# 1-males, 2-females, 3-both sexes

########################
#                      #
# Number of death      #
#                      #
########################

library(tidyverse)
library(reshape)
library(data.table)

sex=2

usa <- read.csv("USA_d_short_idr.csv",header = T)
usa_M <- usa %>% filter(sex==sex,cause!=0)
usa_M <- usa_M[,-c(1,3,4,5,7,26,28,30)]

head(usa_M)

usa_M <- melt(usa_M, id.vars =c("cause","year"), measure.vars = c("d0", "d1", "d5", "d10", 
                                                                  "d15", "d20", "d25", "d30", 
                                                                  "d35", "d40", "d45", "d50", 
                                                                  "d55", "d60", "d65", "d70",
                                                                  "d75", "d80", "d85", "d90",
                                                                  "d95","d100p"))


head(usa_M)

usa_M$Age <- NA
usa_M$Age[usa_M$variable=="d0"] <- 0
usa_M$Age[usa_M$variable=="d1"] <- 1
usa_M$Age[usa_M$variable=="d5"] <- 5
usa_M$Age[usa_M$variable=="d10"] <- 10
usa_M$Age[usa_M$variable=="d15"] <- 15
usa_M$Age[usa_M$variable=="d20"] <- 20
usa_M$Age[usa_M$variable=="d25"] <- 25
usa_M$Age[usa_M$variable=="d30"] <- 30
usa_M$Age[usa_M$variable=="d35"] <- 35
usa_M$Age[usa_M$variable=="d40"] <- 40
usa_M$Age[usa_M$variable=="d45"] <- 45
usa_M$Age[usa_M$variable=="d50"] <- 50
usa_M$Age[usa_M$variable=="d55"] <- 55
usa_M$Age[usa_M$variable=="d60"] <- 60
usa_M$Age[usa_M$variable=="d65"] <- 65
usa_M$Age[usa_M$variable=="d70"] <- 70
usa_M$Age[usa_M$variable=="d75"] <- 75
usa_M$Age[usa_M$variable=="d80"] <- 80
usa_M$Age[usa_M$variable=="d85"] <- 85
usa_M$Age[usa_M$variable=="d90"] <- 90
usa_M$Age[usa_M$variable=="d95"] <- 95
usa_M$Age[usa_M$variable=="d100p"] <- 100


usa_M <- as.data.table(usa_M)

names(usa_M)[4]<-"dx.tot.by.Caus"

usa_M$Cause_Rev[usa_M$cause==1] <- 1 # inf
usa_M$Cause_Rev[usa_M$cause==2] <- 2 # neop
usa_M$Cause_Rev[usa_M$cause==3] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==4] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==5] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==6] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==7] <- 3 # CVD
usa_M$Cause_Rev[usa_M$cause==8] <- 3 # CVD
usa_M$Cause_Rev[usa_M$cause==9] <- 3 # CVD
usa_M$Cause_Rev[usa_M$cause==10] <- 4 # RESP
usa_M$Cause_Rev[usa_M$cause==11] <- 4 # RESP
usa_M$Cause_Rev[usa_M$cause==12] <- 5 # DIG
usa_M$Cause_Rev[usa_M$cause==13] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==14] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==15] <- 7 # oth
usa_M$Cause_Rev[usa_M$cause==16] <- 6 # ext
#usa_M$Age <- as.character(usa_M$Age)

usa_M %>%filter(year==1979,Age%in%c(0),cause%in%c(7,8,9)) %>%select(dx.tot.by.Caus) %>%  sum()

head(usa_M)
usa_M=usa_M[,.(dx.tot.by.Caus = sum(dx.tot.by.Caus)), keyby = .(year, Cause_Rev, Age)]

usa_M$Sex <- "m"

usa_M=usa_M[,.(dx.tot.by.Caus = sum(dx.tot.by.Caus)), keyby = .(year, Cause_Rev)]
head(usa_M)

save(usa_M,file="dx_usa_M.RData")

########################
# Exposure
########################



rm(list = ls())


library(tidyverse)
library(reshape)
library(data.table)

sex=2
usa <- read.csv("USA_e.csv",header = T)
head(usa)

exp_usa_M <- usa %>% filter(sex==sex)
exp_usa_M <- exp_usa_M[,-c(1,3,4,5,24,26,28)]
head(exp_usa_M)


exp_usa_M <- melt(exp_usa_M, id.vars =c("year"), measure.vars = c("e0", "e1", "e5", "e10", 
                                                                  "e15", "e20", "e25", "e30", 
                                                                  "e35", "e40", "e45", "e50", 
                                                                  "e55", "e60", "e65", "e70",
                                                                  "e75", "e80", "e85", "e90",
                                                                  "e95","e100p"))


exp_usa_M$Age <- NA
exp_usa_M$Age[exp_usa_M$variable=="e0"] <- 0
exp_usa_M$Age[exp_usa_M$variable=="e1"] <- 1
exp_usa_M$Age[exp_usa_M$variable=="e5"] <- 5
exp_usa_M$Age[exp_usa_M$variable=="e10"] <- 10
exp_usa_M$Age[exp_usa_M$variable=="e15"] <- 15
exp_usa_M$Age[exp_usa_M$variable=="e20"] <- 20
exp_usa_M$Age[exp_usa_M$variable=="e25"] <- 25
exp_usa_M$Age[exp_usa_M$variable=="e30"] <- 30
exp_usa_M$Age[exp_usa_M$variable=="e35"] <- 35
exp_usa_M$Age[exp_usa_M$variable=="e40"] <- 40
exp_usa_M$Age[exp_usa_M$variable=="e45"] <- 45
exp_usa_M$Age[exp_usa_M$variable=="e50"] <- 50
exp_usa_M$Age[exp_usa_M$variable=="e55"] <- 55
exp_usa_M$Age[exp_usa_M$variable=="e60"] <- 60
exp_usa_M$Age[exp_usa_M$variable=="e65"] <- 65
exp_usa_M$Age[exp_usa_M$variable=="e70"] <- 70
exp_usa_M$Age[exp_usa_M$variable=="e75"] <- 75
exp_usa_M$Age[exp_usa_M$variable=="e80"] <- 80
exp_usa_M$Age[exp_usa_M$variable=="e85"] <- 85
exp_usa_M$Age[exp_usa_M$variable=="e90"] <- 90
exp_usa_M$Age[exp_usa_M$variable=="e95"] <- 95
exp_usa_M$Age[exp_usa_M$variable=="e100p"] <- 100


exp_usa_M <- as.data.table(exp_usa_M)

head(exp_usa_M)
names(exp_usa_M)[3]<-"exp"

exp_usa_M=exp_usa_M[,.(Exp = sum(exp)), keyby = .(year)]
exp_usa_M$Sex <- "m"
head(exp_usa_M)
save(exp_usa_M,file="Ex_usa_M.RData")

rm(list = ls())

load("dx_usa_M.RData")
load("Ex_usa_M.RData")

head(exp_usa_M)
exp_usa_M
usa_M$Sex <- "m"
str(usa_M)
str(exp_usa_M)
tot_M <- left_join(usa_M,exp_usa_M) 

tot_M <- tot_M[,.(mx = dx.tot.by.Caus/Exp), keyby = .(year,Cause_Rev)]
str(tot_M)
tot_M %>% ggplot(aes(year,mx,color=as.factor(Cause_Rev)))+geom_line()

save(tot_M,file="Wavelet_Data_M.RData")
