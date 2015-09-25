# read sprint speed (m/s) vs. temperature (deg C) data from Yuni and compute Arrhenius temperature
data<-read.csv('sprint speed/sprint_speed_N_ocellatus_Yuni_lowland.csv')

data$invT<-1/(data$Temperature+273) # 1/K
data$lnSprint<-log(data$Speed)
with(data,plot(lnSprint~invT))

TA.data<-data[1:6,] # remove highest temperature
arrhen<-lm(TA.data$lnSprint~TA.data$invT) # get Arrhenius temperature
TA<-arrhen$coefficients[2]
list(arrhen)
