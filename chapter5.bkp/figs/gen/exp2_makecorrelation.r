vals <- read.csv("X:/data2/qspace_new\\vals.txt", header=T, sep="\t")
#threshold ADC values to reasonable values
m=mean(vals$ADCX, na.rm=T)
std=sd(vals$ADCX, na.rm=T)

vals$ADCX<-vals$ADCX[vals$ADCX<(m-2*std)]
vals$ADCX<-vals$ADCX[vals$ADCX>(m+2*std)]

m=mean(vals$ADCZ, na.rm=T)
std=sd(vals$ADCZ, na.rm=T)

vals$ADCZ<-vals$ADCZ[vals$ADCZ<(m-2*std)]
vals$ADCZ<-vals$ADCZ[vals$ADCZ>(m+2*std)]

m=mean(vals$P0X, na.rm=T)
std=sd(vals$P0X, na.rm=T)

vals$P0X<-vals$P0X[vals$P0X<(m-2*std)]
vals$P0X<-vals$P0X[vals$P0X>(m+2*std)]

m=mean(vals$FWHMX, na.rm=T)
std=sd(vals$FWHMX, na.rm=T)

vals$FWHMX<-vals$FWHMX[vals$FWHMX<(m-2*std)]
vals$FWHMX<-vals$FWHMX[vals$FWHMX>(m+2*std)]

m=mean(vals$P0Z, na.rm=T)
std=sd(vals$P0Z, na.rm=T)

vals$P0Z<-vals$P0Z[vals$P0Z<(m-2*std)]
vals$P0Z<-vals$P0Z[vals$P0Z>(m+2*std)]

m=mean(vals$FWHMZ, na.rm=T)
std=sd(vals$FWHMZ, na.rm=T)

vals$FWHMZ<-vals$FWHMZ[vals$FWHMZ<(m-2*std)]
vals$FWHMZ<-vals$FWHMZ[vals$FWHMZ>(m+2*std)]




#ADCX vs ADCZ

cr <- rcorr(cbind(vals$ADCZ, vals$ADCX, vals$P0X, vals$FWHMX, vals$P0Z, vals$FWHMZ))

rho <- cr$r[1,2]
p <- max(cr$P[1,2],0.01)

s <- sprintf("Rho: %1.2f, p< %1.2f",rho, p)

p <- ggplot(vals, aes(x=ADCX*1e9, y=ADCZ*1e9))
p <- p + geom_point() +  annotate("text", x = 1, y = 0, label = s) 
p <- p + geom_smooth(method="lm") 
p <- p + scale_x_continuous('ADCx [1e9 m^2/s]', limits=c(0, 1.2)) 
p <- p + scale_y_continuous('ADCz [1e9 m^2/s]', limits=c(0, 2.5)) 





