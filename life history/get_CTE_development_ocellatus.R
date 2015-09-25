# script to compute constant temperature equivalent for development time for Niveoscincus ocellatus

data<-read.csv('life history/basking_regimes_ocellatus.csv')
data<-subset(data,regime=='b') # choose the first regime (can't remember what the different regimes are, but we used the second for lowland ocellatus development)

# reference temp and 5 parameters for the Arrhenius response curve
T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-10191 # Arrhenius temperture
TAL<-50000 # Arrhenius temperature at lower temperature threshold
TAH<-90000 # Arrhenius temperature at upper temperature threshold
TL<-273+10 # lower temperature threshold
TH<-273+37 # upper temperature threshold 

T_cor<-function(temps){ # function to compute temperature correction based on 5 parameter Arrhenius response
exp(TA*(1/(273+T_REF)-1/(273+temps)))/(1+exp(TAL*(1/(273+temps)-1/TL))+exp(TAH*(1/TH-1/(273+temps))))
}
getTb<-function(Tb){ # function finding the difference between a temperature correction factor for a specified Tb compared to the mean calculated one (aim to make this zero)
      x<-exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))-TempCorr_mean
}

data$Tcor_restricted<-T_cor(data$restricted) # apply temperature correction function to restricted basking data
data$Tcor_unrestricted<-T_cor(data$unrestricted) # apply temperature correction function to unrestricted basking data

TempCorr_mean<-mean(data$Tcor_restricted) # get mean temperature correction factor
CTE_restricted<-uniroot(f=getTb,c(TL-273,TH-273),check.conv=TRUE)$root # search for a Tb (CTE) that gives the same temperature correction factor as the mean of the simulated temperature corrections
TempCorr_mean<-mean(data$Tcor_unrestricted) # get mean temperature correction factor
CTE_unrestricted<-uniroot(f=getTb,c(TL-273,TH-273),check.conv=TRUE)$root # search for a Tb (CTE) that gives the same temperature correction factor as the mean of the simulated temperature corrections

# plot results
with(data,plot(unrestricted~hour,ylab='Tb, deg C',type='l'))
text(4,24,paste("mean Tb = ",round(mean(data$unrestricted),1),sep=""))
text(4,23,paste("mean CTE = ",round(CTE_unrestricted,1),sep=""))
text(4,22,paste("mean Tcorr = ", round(mean(data$Tcor_unrestricted),1),sep=""))

