
#########################
#                       #
# Check number of death #
# with mx               #
#                       #
#########################



usa <- read.csv("USA_e.csv",header = T)
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

usa_M <- usa %>% filter(sex==1)
usa_M$country

head(usa_M[,c(1,2,3,4,5,6,7,24,25,26,27,28,29,30)])

head(usa_M[,-c(1,3,4,5,7,24,26,28)])

usa_M <- usa_M[,-c(1,3,4,5,24,26,28)]
head(usa_M)


head(usa_M)

usa_M <- melt(usa_M, id.vars =c("year"), measure.vars = c("e0", "e1", "e5", "e10", 
                                                                  "e15", "e20", "e25", "e30", 
                                                                  "e35", "e40", "e45", "e50", 
                                                                  "e55", "e60", "e65", "e70",
                                                                  "e75", "e80", "e85", "e90",
                                                                  "e95","e100p"))


head(usa_M)
usa_M$Age <- NA


usa_M$Age[usa_M$variable=="e0"] <- 0
usa_M$Age[usa_M$variable=="e1"] <- 1
usa_M$Age[usa_M$variable=="e5"] <- 5
usa_M$Age[usa_M$variable=="e10"] <- 10
usa_M$Age[usa_M$variable=="e15"] <- 15
usa_M$Age[usa_M$variable=="e20"] <- 20
usa_M$Age[usa_M$variable=="e25"] <- 25
usa_M$Age[usa_M$variable=="e30"] <- 30
usa_M$Age[usa_M$variable=="e35"] <- 35
usa_M$Age[usa_M$variable=="e40"] <- 40
usa_M$Age[usa_M$variable=="e45"] <- 45
usa_M$Age[usa_M$variable=="e50"] <- 50
usa_M$Age[usa_M$variable=="e55"] <- 55
usa_M$Age[usa_M$variable=="e60"] <- 60
usa_M$Age[usa_M$variable=="e65"] <- 65
usa_M$Age[usa_M$variable=="e70"] <- 70
usa_M$Age[usa_M$variable=="e75"] <- 75
usa_M$Age[usa_M$variable=="e80"] <- 80
usa_M$Age[usa_M$variable=="e85"] <- 85
usa_M$Age[usa_M$variable=="e90"] <- 90
usa_M$Age[usa_M$variable=="e95"] <- 95
usa_M$Age[usa_M$variable=="e100p"] <- 100

str(usa_M)

library(data.table)

usa_M <- as.data.table(usa_M)

head(usa_M)
names(usa_M)[3]<-"exp"

usa_exp <- usa_M

load("dx_usa_M.RData")
head(usa_M)
head(usa_exp)
str(usa_exp)

usa_exp$year <- as.factor(usa_exp$year)
usa_exp$Age <- as.factor(usa_exp$Age)

usa_exp$Sex <- "m"

usa_M$year <- as.factor(usa_M$year)
usa_M$Age <- as.factor(usa_M$Age)

melt(usa_M,usa_exp)

TOT <- full_join(usa_exp,usa_M)



TOT$mx <- TOT$dx.tot.by.Caus/TOT$exp
head(TOT)


TOT %>% filter(Cause_Rev==3,year==1980) %>% select(mx)

rm(usa)
rm(usa_exp)
rm(usa_M)
head(TOT)

load("usa_M.RData")
head(usa_M)
usa_M%>% filter(Cause_Rev==3,year==1980) %>% select(mx.tot.by.Caus)


cbind(TOT %>% filter(Cause_Rev==7,year==2000) %>% select(mx)*1000,
      usa_M%>% filter(Cause_Rev==7,year==2000) %>% select(mx.tot.by.Caus)*1000 )
