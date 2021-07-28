
########################################################## RR Graph Updated Jan 2017       
# install.packages('plotrix')
 library('plotrix')       #package for std.error bars


par(mar=c(4,5,1,2))
   
VentingTool <- c(1.10,1.47,1.95)      #2.5, median, 97.5 values, in that order
Needle <- c(1.14,1.52,2.02)            #2.5, median, 97.5 values, in that order
Descender <- c(1.14,1.51,2.01)          #2.5, median, 97.5 values, in that order

x=c(1,2,3)
        y=cbind(VentingTool[2], Needle[2], Descender[2])          
        
 plot(x,y,xlab="Experimental treatment", xaxt='n',
                  ylab="Mean relative survival (2.5/97.5 CI)",
     xlim=c(0.75,3.25), ylim=c(0, 2.1), cex=2, cex.lab=1.2, cex.axis=1.1, main="", cex.main=1.5,
      col=c("gray 50", "gray50", "gray50"), pch=19)
  title("", line=-1, adj=0.5, cex.main=1)

  axis(1, labels=c("Venting cannula","Venting needle", "Recompression device"), at=seq(1,3), cex.axis=1.1)
  
 arrows(1, VentingTool[2], 1, c(VentingTool[1], VentingTool[3]), angle=90, length=1/8)
 arrows(2, Needle[2], 2, c(Needle[1], Needle[3]), angle=90, length=1/8)
  arrows(3, Descender[2], 3, c(Descender[1], Descender[3]), angle=90, length=1/8)
  
  abline(h=1, col="darkgray", lty=1)
  
 