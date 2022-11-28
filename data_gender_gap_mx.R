# 1-males, 2-females, 3-both sexes

########################
#                      #
# RATES                #
#                      #
########################


usa <- read.csv("USA_m_short_idr.csv",header = T)
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

usa_M <- usa_M[,-c(1,3,4,5,25,27,29)]

head(usa_M)

usa_M <- melt(usa_M, id.vars =c("cause","year"), measure.vars = c("m0", "m1", "m5", "m10", 
                                                              "m15", "m20", "m25", "m30", 
                                                              "m35", "m40", "m45", "m50", 
                                                              "m55", "m60", "m65", "m70",
                                                              "m75", "m80", "m85", "m90",
                                                              "m95","m100p"))


head(usa_M)
usa_M$Age <- NA


usa_M$Age[usa_M$variable=="m0"] <- 0
usa_M$Age[usa_M$variable=="m1"] <- 1
usa_M$Age[usa_M$variable=="m5"] <- 5
usa_M$Age[usa_M$variable=="m10"] <- 10
usa_M$Age[usa_M$variable=="m15"] <- 15
usa_M$Age[usa_M$variable=="m20"] <- 20
usa_M$Age[usa_M$variable=="m25"] <- 25
usa_M$Age[usa_M$variable=="m30"] <- 30
usa_M$Age[usa_M$variable=="m35"] <- 35
usa_M$Age[usa_M$variable=="m40"] <- 40
usa_M$Age[usa_M$variable=="m45"] <- 45
usa_M$Age[usa_M$variable=="m50"] <- 50
usa_M$Age[usa_M$variable=="m55"] <- 55
usa_M$Age[usa_M$variable=="m60"] <- 60
usa_M$Age[usa_M$variable=="m65"] <- 65
usa_M$Age[usa_M$variable=="m70"] <- 70
usa_M$Age[usa_M$variable=="m75"] <- 75
usa_M$Age[usa_M$variable=="m80"] <- 80
usa_M$Age[usa_M$variable=="m85"] <- 85
usa_M$Age[usa_M$variable=="m90"] <- 90
usa_M$Age[usa_M$variable=="m95"] <- 95
usa_M$Age[usa_M$variable=="m100p"] <- 100

str(usa_M)

usa$cause
library(data.table)

usa_M <- as.data.table(usa_M)

head(usa_M)
names(usa_M)[4]<-"mx.tot.by.Caus"
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

usa_M$mx.tot.by.Caus <- usa_M$mx.tot.by.Caus/1000000

usa_M %>%filter(year==1979,Age%in%c(0),cause%in%c(7,8,9)) %>%select(mx.tot.by.Caus) %>%  sum()

str(usa)
usa_M=usa_M[,.(mx.tot.by.Caus = sum(mx.tot.by.Caus)), keyby = .(year, Cause_Rev, Age)]
head(usa_M)
usa_M %>% filter(Cause_Rev==3)

usa_M %>%filter(year==1979) %>% ggplot(aes(Age,log(mx.tot.by.Caus)))+geom_line()+facet_wrap(~Cause_Rev)

usa_M$Sex <- "M"


save(usa_M,file="usa_M.RData")
############################################################

rm()
# 1-males, 2-females, 3-both sexes

usa <- read.csv("USA_m_short_idr.csv",header = T)
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

usa_F <- usa_F[,-c(1,3,4,5,25,27,29)]

head(usa_F)

usa_F <- melt(usa_F, id.vars =c("cause","year"), measure.vars = c("m0", "m1", "m5", "m10", 
                                                                  "m15", "m20", "m25", "m30", 
                                                                  "m35", "m40", "m45", "m50", 
                                                                  "m55", "m60", "m65", "m70",
                                                                  "m75", "m80", "m85", "m90",
                                                                  "m95","m100p"))


head(usa_F)
usa_F$Age <- NA


usa_F$Age[usa_F$variable=="m0"] <- 0
usa_F$Age[usa_F$variable=="m1"] <- 1
usa_F$Age[usa_F$variable=="m5"] <- 5
usa_F$Age[usa_F$variable=="m10"] <- 10
usa_F$Age[usa_F$variable=="m15"] <- 15
usa_F$Age[usa_F$variable=="m20"] <- 20
usa_F$Age[usa_F$variable=="m25"] <- 25
usa_F$Age[usa_F$variable=="m30"] <- 30
usa_F$Age[usa_F$variable=="m35"] <- 35
usa_F$Age[usa_F$variable=="m40"] <- 40
usa_F$Age[usa_F$variable=="m45"] <- 45
usa_F$Age[usa_F$variable=="m50"] <- 50
usa_F$Age[usa_F$variable=="m55"] <- 55
usa_F$Age[usa_F$variable=="m60"] <- 60
usa_F$Age[usa_F$variable=="m65"] <- 65
usa_F$Age[usa_F$variable=="m70"] <- 70
usa_F$Age[usa_F$variable=="m75"] <- 75
usa_F$Age[usa_F$variable=="m80"] <- 80
usa_F$Age[usa_F$variable=="m85"] <- 85
usa_F$Age[usa_F$variable=="m90"] <- 90
usa_F$Age[usa_F$variable=="m95"] <- 95
usa_F$Age[usa_F$variable=="m100p"] <- 100

str(usa_F)

usa$cause
library(data.table)

usa_F <- as.data.table(usa_F)

head(usa_F)
names(usa_F)[4]<-"mx.tot.by.Caus"
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

usa_F$mx.tot.by.Caus <- usa_F$mx.tot.by.Caus/1000000

usa_F %>%filter(year==1979,Age%in%c(0),cause%in%c(7,8,9)) %>%select(mx.tot.by.Caus) %>%  sum()

str(usa)
usa_F=usa_F[,.(mx.tot.by.Caus = sum(mx.tot.by.Caus)), keyby = .(year, Cause_Rev, Age)]
head(usa_F)
usa_F %>% filter(Cause_Rev==3)

usa_F %>%filter(year==1979) %>% ggplot(aes(Age,log(mx.tot.by.Caus)))+geom_line()+facet_wrap(~Cause_Rev)

usa_F$Sex <- "F"


load("usa_M.RData")

usa_M

names(usa_F)[4]<-"mx.tot.by.Caus_F"
head(usa_F)

tot <- cbind(usa_F,usa_M)

tot$gender_gap <- tot$mx.tot.by.Caus/tot$mx.tot.by.Caus_F 

head(tot)
tot <- tot[,c(1,2,3,11)]
tot %>%filter(year==1979) %>% ggplot(aes(Age,log(gender_gap)))+geom_line()+facet_wrap(~Cause_Rev)
