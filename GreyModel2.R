#######################################################
##
##    描述： GM(1,1)模型预测
##   添加人： 刘应波
##  添加时间：2014-9-28
##     备注：GM11部分参考了网上朋友内容
## 
#######################################################
rm(list=ls(all=TRUE))
#======================================================
#= 减弱算子
#= x：输入序列
#======================================================
WeakenOperator <- function(x)
{
    X0 <- x
    X0_Len <- length(X0)
    
    XD <- numeric(X0_Len)
    XD_Len <- length(XD)
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
    #----------------------------------------------
    pal <- colorRampPalette(c("yellow", "blue"))
    opar <- par(no.readonly=TRUE)
    par(mfrow=c(1,2))
    plot(X0,type="b",pch=15,lty=1,col="red",xlab="步进数",
         ylab="值")
    title("一阶和二阶减弱算子示意图",adj=1)
    lines(XD,type="b",pch=16,lty=2,col="blue")
    lines(XD2,type="b",pch=17,lty=2,col="green")    

    legend("bottomright",inset=.05,title="Performance",
           c(expression(X^{(0)}),
           expression(X*D^{(0)}),
           expression(X*D^{(1)})),
           horiz=FALSE,lty=c(1,2,3),pch=c(15,16,17),
           col=c("red","blue","green"))
    plot(AGO1,type="b",pch=15,lty=1,col="red",xlab="步进数",
         ylab="值")
    title("二阶AGO累加生成示意图",adj=1)
    par(opar)
    return(AGO1)
}

#======================================================
#= GM(1,1)模型
#= x: 待建模的输入序列
#= k: 在模型建立出来后，需要预测多少个值，k>0
#======================================================
GM11 <- function(x,k)
{
    wk_X <- WeakenOperator(x);
    n <- length(x);
    x1 <- wk_X
    z1 <- numeric(n);
    m <- n-1;
    z1 <- wk_X
    #for (j in 1:m)
    #{
    #    z1[j+1] <- (0.5*x1[j+1] + 0.5*x1[j])
    #}
    Yn <- t(t(x1[2:n]));
    
    B <- matrix(1,nrow=n-1,ncol=2);
    B[,1]<-t(t(-z1[2:n]));  
    #最小二乘法求解参数列
    u<-solve(t(B)%*%B)%*%t(B)%*%Yn;
    #
    a<-u[1];
    b<-u[2];
   
    cat("发展系统(a)-作用量(b):",a, b,'\n',
                       "预测公式：","x(k+1)=(",x[1]-b/a,
                       ") * exp (",-a," * k ) ",b/a,"\n");

    GreyPredit(x[1],a,b,6)
}
#======================================================
#= 预测模型
#= start: 序列的初始值
#=     a: 发展系数
#=     b: 灰色作用量
#=     k: 预测的数值 
#======================================================
GreyPredit <- function(start, a, b, k)
{
    #预测值
    x2<-numeric(k);
    x2[1]<-start;
    for(i in 1:k-1)
    {
       x2[1+i]=(start-b/a)*exp(-a*i)+b/a;
    } 
    cat("时间响应式计算值：",x2,"\n")   
    x2=c(0,x2);    
    #调用差分函数计算模拟数据
    y=diff(x2);
    cat("预测值：",y,"\n")   
}
X0 <- c(10155, 12588, 23480, 35388)
debug(GM11)
GM11(X0,6)
