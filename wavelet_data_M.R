# 1-males, 2-females, 3-both sexes

########################
#                      #
# Number of death      #
#                      #
########################

library(tidyverse)
library(reshape)
library(data.table)

sex=1

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
############################################################

