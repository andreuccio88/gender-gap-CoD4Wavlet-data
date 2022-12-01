# 1-males, 2-females, 3-both sexes

########################
#                      #
# Number of death      #
#                      #
########################



usa <- read.csv("USA_d_short_idr.csv",header = T)
head(usa)

#Files with age-specific death rates
#Files with age-specific death rates have same structure as death files. Death rates are calculated per
#1,000,000 and published as integer numbers.
51
# 
# usa <- read.csv("USA_d_short_idr.csv",header = T)
# head(usa)
# 
# 89.92/1761647*(1000000)
# 
# usa <- read.csv("USA_e.csv",header = T)
# head(usa)
# 253/1000000
# 
# 445.89/1761647*(1000000)

str(usa)
library(tidyverse)
library(reshape)

usa_M <- usa %>% filter(sex==1,cause!=0)
usa_M$country

head(usa_M)
str(usa_M)

head(usa_M[,2])

head(usa_M[,c(1,2,3,4,5,6,7,25,26,27,28,29,30,31)])

head(usa_M[,-c(1,3,4,5,7,26,28,30)])

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

str(usa_M)

usa$cause
library(data.table)

usa_M <- as.data.table(usa_M)

head(usa_M)
names(usa_M)[4]<-"dx.tot.by.Caus"
#names(usa)[2]<-"Year"

head(usa_M)

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
head(usa_M)


usa_M %>%filter(year==1979,Age%in%c(0),cause%in%c(7,8,9)) %>%select(dx.tot.by.Caus) %>%  sum()

str(usa)
usa_M=usa_M[,.(dx.tot.by.Caus = sum(dx.tot.by.Caus)), keyby = .(year, Cause_Rev, Age)]
head(usa_M)
usa_M %>% filter(Cause_Rev==3)

usa_M %>%filter(year==1979) %>% ggplot(aes(Age,log(dx.tot.by.Caus)))+geom_line()+facet_wrap(~Cause_Rev)

usa_M$Sex <- "m"


save(usa_M,file="dx_usa_M.RData")
############################################################

rm()
# 1-males, 2-females, 3-both sexes

usa <- read.csv("USA_d_short_idr.csv",header = T)
head(usa)

#Files with age-specific death rates
#Files with age-specific death rates have same structure as death files. Death rates are calculated per
#1,000,000 and published as integer numbers.
51
# 
# usa <- read.csv("USA_d_short_idr.csv",header = T)
# head(usa)
# 
# 89.92/1761647*(1000000)
# 
# usa <- read.csv("USA_e.csv",header = T)
# head(usa)
# 253/1000000
# 
# 445.89/1761647*(1000000)

str(usa)
library(tidyverse)
library(reshape)

usa_F <- usa %>% filter(sex==2,cause!=0)
usa_F$country

head(usa_F)
str(usa_F)

head(usa_F[,2])

head(usa_F[,c(1,2,3,4,5,6,7,25,26,27,28,29,30,31)])

head(usa_F[,-c(1,3,4,5,26,28,30)])

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

str(usa_F)

usa$cause
library(data.table)

usa_F <- as.data.table(usa_F)

head(usa_F)
names(usa_F)[4]<-"dx.tot.by.Caus"
#names(usa)[2]<-"Year"

head(usa_F)

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
head(usa_F)



usa_F %>%filter(year==1979,Age%in%c(0),cause%in%c(7,8,9)) %>%select(dx.tot.by.Caus) %>%  sum()

str(usa)
usa_F=usa_F[,.(dx.tot.by.Caus = sum(dx.tot.by.Caus)), keyby = .(year, Cause_Rev, Age)]
head(usa_F)
usa_F %>% filter(Cause_Rev==3)

usa_F %>%filter(year==1979) %>% ggplot(aes(Age,log(dx.tot.by.Caus)))+geom_line()+facet_wrap(~Cause_Rev)

usa_F$Sex <- "F"


load("dx_usa_M.RData")

usa_M

names(usa_F)[4]<-"dx.tot.by.Caus_F"
head(usa_F)

tot <- cbind(usa_F,usa_M)
head(tot)


tot$gender_gap <- tot$dx.tot.by.Caus/tot$dx.tot.by.Caus_F 

head(tot)
tot <- tot[,c(1,2,3,11)]
tot %>%filter(year==1990) %>% ggplot(aes(Age,log(gender_gap)))+geom_line()+facet_wrap(~Cause_Rev)
