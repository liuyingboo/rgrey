X0 <- c(48.32, 80.91, 104.66, 121.83, 130.17, 144.87)
X0_Len <- length(X0)
XD_Len <- length(XD)

XD <- numeric(X0_Len)
XD2 <- numeric(X0_Len)
AGO1 <- numeric(X0_Len)

for (i in 1:X0_Len)
{
   XD[i] <- (1/(X0_Len-i+1))*sum(X0[i:X0_Len])
}
# 一阶弱化算子处理后序列
XD
for (i in 1:XD_Len)
{
   XD2[i] <- (1/(XD_Len-i+1))*sum(XD[i:XD_Len])
}
# 二阶弱化算子处理后序列
XD2
for (i in 1:XD_Len)
{
   AGO1 [i] <- sum(XD2[1:i]);
}
AGO1 
##############################################
pal <- colorRampPalette(c("yellow", "blue"))
opar <- par(no.readonly=TRUE)
plot(X0,type="b",pch=15,lty=1,col="red",xlab="k",ylab="sequence",ylim=c(-100,800) )

lines(XD,type="b",pch=16,lty=2,col="blue")
lines(XD2,type="b",pch=17,lty=2,col="green")
lines(AGO1,type="b",pch=18,lty=2,col="#ff347F")
#library(Hmisc)
#minor.tick(nx=2,ny=3,tick.ratio=0.5)

legend("topleft",inset=.05,title="Performance",
       c(expression(X^{(0)}),
       expression(X*D^{(0)}),
       expression(X*D^{(1)}),
       expression(AGO)),
       horiz=FALSE,lty=c(1,2,3,4),pch=c(15,16,17,18),
       col=c("red","blue","green","#ff347F"))
par(opar)