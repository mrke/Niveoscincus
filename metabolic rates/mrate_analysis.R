mratesoriginal<-read.csv("metabolic rates/mrates.csv") # read in data, including 30 deg C readings
#mratesoriginal<-read.csv("metabolic rates/mrates_no7or30degreadings.csv") # read in data, not including 7 or  30 deg C readings as there are only a few measures for these temps
mrates=mratesoriginal[which(mratesoriginal$Baseline0switchm1m2==0),] #use baseline readings only or subset to reading taken after switch, and then according to treatment.
#mratesswitch=mratesoriginal[which(mratesoriginal$Baseline0switchm1m2==1),] #use readings after exposure to a treatment
#mrates=mratesswitch[which(mratesswitch$treat1h2c==1),] #after exposure to hot treatment
#mrates=mratesswitch[which(mratesswitch$treat1h2c==2),] #after exposure to cold treatment
#mrates<-subset(mrates,N>1) # need to check what N is
mrates$mlO2.g.h<-mrates$VO2kg*60/1000 # convert from ml O2/kg/min to ml O2/g/h

MR1<-0.013 # coefficient 1 for Andews adn Pough equation
MR2<-0.8 # coefficient 2 for Andews adn Pough equation
MR3<-0.038 # coefficient 3 for Andews adn Pough equation
mrates$expect<-MR1*mrates$initialmass^MR2*10^(MR3*mrates$TaTb)/mrates$initialmass #based on Eq. 2 from Andrews & Pough 1985. Physiol. Zool. 58:214-231

species<-c('metallicus','greeni','microlepidotus','pretiosus')

TA_warm<-vector(length = 4)
TA_cool<-TA_warm
par(mfrow = c(2,2)) # set up for 4 plots in 2 columns
for(i in 1:4){
O2<-subset(mrates,speciesme1g2mi3pr4==i) # choose species, 1 is metallicus, 2 is greeni, 3 is microlepidotus, 4 is pretiosus
O2_agg<-aggregate(O2$mlO2.g.h,by=list(O2$Temprounded),FUN = function(x) quantile(x, probs = 0.1)) # get 10th percentile observed mrate
O2_exp<-aggregate(O2$expect,by=list(O2$Temprounded),mean) # get mean expected mrate
O2_agg<-cbind(O2_agg,O2_exp[,2]) # bind columns of observed and expected
colnames(O2_agg)<-c('Tair','VO2','EVO2') # label columns
O2_agg_warm<-subset(O2_agg,Tair>0) # choose warming experiment
O2_agg_cool<-subset(O2_agg,Tair<0) # choose cooling experiment
O2_agg_cool$Tair<-O2_agg_cool$Tair*-1 # make air temp positve for cooling experiment

arrhenius_warm<-as.data.frame(cbind(log(O2_agg_warm$VO2),(1/(O2_agg_warm$Tair+273)))) # compute ln rate and 1/K for arrhenius temperatur calc, warming experiment
arrhenius_cool<-as.data.frame(cbind(log(O2_agg_cool$VO2),(1/(O2_agg_cool$Tair+273)))) # compute ln rate and 1/K for arrhenius temperatur calc, cooling experiment
colnames(arrhenius_warm)<-c('lnVO2kg','invT')
colnames(arrhenius_cool)<-c('lnVO2kg','invT')

plot(O2_agg_cool$VO2~O2_agg_cool$Tair,col='blue',ylim=c(0,0.35),main=species[i]) # plot cooling experiment results
#points(O2_agg_warm$VO2~O2_agg_warm$Tair,col='red') # plot warming experiment results
points(O2_agg_warm$EVO2~O2_agg_warm$Tair,col='black',type='l') # plot expected metabolic rate

#with(arrhenius_warm,plot(lnVO2kg~invT,col='red')) # plot Arrhenius response, warming
#with(arrhenius_cool,plot(lnVO2kg~invT,col='blue')) # plot Arrhenius response, cooling

arrhen_warm<-lm(arrhenius_warm$lnVO2kg~arrhenius_warm$invT) # get Arrhenius temperature
TA_warm[i]<-arrhen_warm$coefficients[2]

arrhen_cool<-lm(arrhenius_cool$lnVO2kg~arrhenius_cool$invT)
TA_cool[i]<-arrhen_cool$coefficients[2]
text(11,0.3,paste("TA = ",round(TA_cool[i]),sep=""))
text(13,0.25,paste("mean mass (g) = ",round(mean(O2$initialmass),2),sep=""))
}

