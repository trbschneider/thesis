require(ggplot2)
require(tikzDevice)
require(robust)

scan_labeller <- function(x,y)
{
  labels<-as.character(y)
  labels[labels=='t0']=sprintf('Scan (r=%1.3f)',cu0$cov[1,2])
  labels[labels=='t1']=sprintf('Rescan (r=%1.3f)',cu1$cov[1,2])
  print(labels)
  return(labels)
}


mm <- read.table("./allvoxels.txt", sep = "\t", header = TRUE)

plotqt=c(0.01,0.99)

# tikz('../DIAMvsFAcorr.tex', standAlone = FALSE, width=4, height=2, engine='xetex')
# 
# cu0<-covRob(mm[mm$SCAN=="t0",c("DIAM","FA")],corr=TRUE)
# cu1<-covRob(mm[mm$SCAN=="t1",c("DIAM","FA")],corr=TRUE)
# qplot(DIAM, FA, data=mm, alpha=I(1/5), xlim=quantile(mm$DIAM, probs = plotqt), ylim=quantile(mm$FA, probs = plotqt)) + facet_grid( . ~ SCAN, labeller=scan_labeller) + xlab("$a$ $[\\mu m]$") + ylab("FA")
# 
# dev.off()
# 
# tikz('../DENSvsFAcorr.tex', standAlone = FALSE, width=4, height=2, engine='xetex')
# 
# cu0<-covRob(mm[mm$SCAN=="t0",c("DENS","FA")],corr=TRUE)
# cu1<-covRob(mm[mm$SCAN=="t1",c("DENS","FA")],corr=TRUE)
# qplot(DENS, FA, data=mm, alpha=I(1/5), xlim=quantile(mm$DENS, probs = plotqt), ylim=quantile(mm$FA, probs = plotqt)) + facet_grid( . ~ SCAN, labeller=scan_labeller) + xlab("$\\rho$ $[\\mu m^{-2}]$") + ylab("FA")
# 
# dev.off()
# 
# tikz('../DIAMvsMDcorr.tex', standAlone = FALSE, width=4, height=2, engine='xetex')
# 
# 
# cu0<-covRob(mm[mm$SCAN=="t0",c("DIAM","MD")],corr=TRUE)
# cu1<-covRob(mm[mm$SCAN=="t1",c("DIAM","MD")],corr=TRUE)
# qplot(DIAM, MD, data=mm, alpha=I(1/5), xlim=quantile(mm$DIAM, probs = plotqt), ylim=quantile(mm$MD, probs = plotqt)) + facet_grid( . ~ SCAN, labeller=scan_labeller) + xlab("$a$ $[\\mu m]$") + ylab("MD $[\\times 10^{-9}mm^2/s]$")
# 
# dev.off()
# 
# tikz('../DENSvsMDcorr.tex', standAlone = FALSE, width=4, height=2, engine='xetex')
# 
# 
# cu0<-covRob(mm[mm$SCAN=="t0",c("DENS","MD")],corr=TRUE)
# cu1<-covRob(mm[mm$SCAN=="t1",c("DENS","MD")],corr=TRUE)
# qplot(DENS, MD, data=mm, alpha=I(1/5), xlim=quantile(mm$DENS, probs = plotqt), ylim=quantile(mm$MD, probs = plotqt)) + facet_grid( . ~ SCAN, labeller=scan_labeller) + xlab("$\\rho$ $[\\mu m^{-2}]$") + ylab("MD $[\\times 10^{-9}mm^2/s]$")
# 
# dev.off()
# 
# tikz('../DIAMvsADcorr.tex', standAlone = FALSE, width=4, height=2, engine='xetex')
# 
# 
# cu0<-covRob(mm[mm$SCAN=="t0",c("DIAM","L1")],corr=TRUE)
# cu1<-covRob(mm[mm$SCAN=="t1",c("DIAM","L1")],corr=TRUE)
# #qplot(DIAM, L1, data=mm, alpha=I(1/5), xlim=quantile(mm$DIAM, probs = plotqt), ylim=quantile(mm$L1, probs = plotqt)) + facet_grid( . ~ SCAN, labeller=scan_labeller) + xlab("$a$ $[\\mu m]$") + ylab("AD $[\\times 10^{-9}mm^2/s]$")
# 
# dev.off()
# 
tikz('../DENSvsADcorr.tex', standAlone = FALSE, width=4, height=2, engine='xetex')


cu0<-covRob(mm[mm$SCAN=="t0",c("DENS","L1")],corr=TRUE)
cu1<-covRob(mm[mm$SCAN=="t1",c("DENS","L1")],corr=TRUE)
qplot(DENS, L1, data=mm, alpha=I(1/5), xlim=quantile(mm$DENS, probs = plotqt), ylim=quantile(mm$L1, probs = plotqt)) + facet_grid( . ~ SCAN, labeller=scan_labeller) + xlab("$\\rho$ $[\\mu m^{-2}]$") + ylab("AD $[\\times 10^{-9}mm^2/s]$")

dev.off()
# 
# tikz('../DIAMvsRDcorr.tex', standAlone = FALSE, width=4, height=2, engine='xetex')
# 
# 
# cu0<-covRob(mm[mm$SCAN=="t0",c("DIAM","RD")],corr=TRUE)
# cu1<-covRob(mm[mm$SCAN=="t1",c("DIAM","RD")],corr=TRUE)
# qplot(DIAM, RD, data=mm, alpha=I(1/5), xlim=quantile(mm$DIAM, probs = plotqt), ylim=quantile(mm$RD, probs = plotqt)) + facet_grid( . ~ SCAN, labeller=scan_labeller) + xlab("$a$ $[\\mu m]$") + ylab("RD $[\\times 10^{-9}mm^2/s]$")
# 
# dev.off()
# 
# tikz('../DENSvsRDcorr.tex', standAlone = FALSE, width=4, height=2, engine='xetex')
# 
# 
# cu0<-covRob(mm[mm$SCAN=="t0",c("DENS","RD")],corr=TRUE)
# cu1<-covRob(mm[mm$SCAN=="t1",c("DENS","RD")],corr=TRUE)
# qplot(DENS, RD, data=mm, alpha=I(1/5), xlim=quantile(mm$DENS, probs = plotqt), ylim=quantile(mm$RD, probs = plotqt)) + facet_grid( . ~ SCAN, labeller=scan_labeller) + xlab("$\\rho$ $[\\mu m^{-2}]$") + ylab("RD $[\\times 10^{-9}mm^2/s]$")
# 
# 
# dev.off()
# 
