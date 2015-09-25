# do a quantile regression to get SVL vs clutch size relationship, using package quantreg
# need to force the relationship thorugh clutch size = 1 and SVL = min size at parturition

library(quantreg)
data<-read.csv("life history/N_ocellatus_clutch_size.csv")

data.orford<-subset(data,SITE.=="OR") # select Orford data

with(data.orford,plot(CSIZE~FSVLAFTER)) # plot relationship
xintercept <- 54 # x offset for SVL (1 mm less than min size at parturition)
yintercept <- 1 # y offset (minimum clutch size)
clutch.vs.svl<-summary(with(data.orford,rq(I(CSIZE-yintercept)~0+I(FSVLAFTER-xintercept),tau=0.95))) # quantile regression (95th percentile) on adjusted values

SVL2<-data.orford$FSVLAFTER-xintercept # adjusted SVL
CLUTCH2<-data.orford$CSIZE-yintercept # adjusted clutch
with(data.orford,plot(CLUTCH2~SVL2)) # plot adjusted relationship
abline(0,clutch.vs.svl$coefficients[1]) # plot estimated slope

y<-yintercept+(data.orford$FSVLAFTER-xintercept)*clutch.vs.svl$coefficients[1] # create new dataset based on above regression
x<-data.orford$FSVLAFTER/10 # convert SVL from mm to cm, because that is what the regression done in NicheMapR assumes the units are
coeffs<-summary(lm(y~x))$coefficients[,1] # estimate slope and intercept for unadjusted values

with(data.orford,plot(CSIZE~FSVLAFTER)) # replot unadjusted relationship
abline(coeffs[1],coeffs[2]/10) # plot estimated slope (converting back to mm)
