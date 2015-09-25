# script to compute constant temperature equivalent for development time for Niveoscincus ocellatus

data<-read.csv('life history/basking_regimes_microlepidotus.csv')

# reference temp and 5 parameters for the Arrhenius response curve
T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-10191 # Arrhenius temperture
TAL<-90000 # Arrhenius temperature at lower temperature threshold
TAH<-90000 # Arrhenius temperature at upper temperature threshold
TL<-273+10 # lower temperature threshold
TH<-273+37 # upper temperature threshold 

T_cor<-function(temps){ # function to compute temperature correction based on 5 parameter Arrhenius response
exp(TA*(1/(273+T_REF)-1/(273+temps)))/(1+exp(TAL*(1/(273+temps)-1/TL))+exp(TAH*(1/TH-1/(273+temps))))
}
getTb<-function(Tb){ # function finding the difference between a temperature correction factor for a specified Tb compared to the mean calculated one (aim to make this zero)
      x<-exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))-TempCorr_mean
}

data$Tcor_temp.1<-T_cor(data$restricted) # apply temperature correction function to temp.1 basking data
data$Tcor_temp.2<-T_cor(data$unrestricted) # apply temperature correction function to temp.2 basking data

TempCorr_mean<-mean(data$Tcor_temp.1) # get mean temperature correction factor
CTE_temp.1<-uniroot(f=getTb,c(TL-273,TH-273),check.conv=TRUE)$root # search for a Tb (CTE) that gives the same temperature correction factor as the mean of the simulated temperature corrections
TempCorr_mean<-mean(data$Tcor_temp.2) # get mean temperature correction factor
CTE_temp.2<-uniroot(f=getTb,c(TL-273,TH-273),check.conv=TRUE)$root # search for a Tb (CTE) that gives the same temperature correction factor as the mean of the simulated temperature corrections

# plot results
with(data,plot(unrestricted~hour,ylab='Tb, deg C',type='l'))
text(2,28,paste("mean Tb = ",round(mean(data$unrestricted),2),sep=""))
text(2,27,paste("mean CTE = ",round(CTE_temp.2,2),sep=""))
text(2,26,paste("mean Tcorr = ", round(mean(data$Tcor_temp.2),2),sep=""))

# plot results
with(data,plot(restricted~hour,ylab='Tb, deg C',type='l'))
text(2,28,paste("mean Tb = ",round(mean(data$restricted),2),sep=""))
text(2,26,paste("mean CTE = ",round(CTE_temp.1,2),sep=""))
text(2,24,paste("mean Tcorr = ", round(mean(data$Tcor_temp.2),2),sep=""))
