library(tidyverse)
attach(Household_Pulse_data)

##  Question One

fact.tw <- as.numeric(TWDAYS)
fact.EEDUC <- as.numeric(EEDUC)
fact.any <- as.numeric(KINDWORK)

lm1 <- lm(fact.tw~fact.any+fact.EEDUC)
lm1

summary(lm1)


##  My null hypothesis is that TW is significantly associated with/affected by the kind of work and level of education
##  Individuals with higher education may have jobs with more flexibility (self employed, office jobs etc)
##  I ran a linear regression including the variable of KINDWORK (as.numeric)
## results showed a statistically significant result at the 0.001 level.

confint(lm1,0.98)


##  Question Two 

hdcov <- as.numeric(HADCOVIDRV)
lcov <- as.numeric(LONGCOVID)

t.test(hdcov,lcov,df=c, paired = T)

HREC <- xtabs(~ HADCOVIDRV + RECVDVACC)
HREC

LRec <- xtabs(~ LONGCOVID + RECVDVACC)
LRec

t.test(HREC,LRec,df=c, paired = T)

df <- data.frame(HREC+LRec)


summary(HREC)
summary(LRec)

lm(formula=LRec~HREC, data=df)


##  Question Three 

Inc <- subset((as.numeric)(Household_Pulse_data$INCOME) + ((as.numeric)Household_Pulse_data$ANXIOUS == drop_na() + ((as.numeric)Household_Pulse_data$RENTCUR ==drop_na()))

##  I had a bit of trouble actually running the code to create my intended subgroup, but my goal was to remove NA values in my 
#   slected variables 

INC2 <- as.numeric(INCOME)
ANX2 <- as.numeric(ANXIOUS)
REN2 <- as.numeric(RENTCUR)

lm2 <- lm(INC2~ANX2 + REN2)                                                   
lm2                                                    
summary(lm2)
