#######################################################
##
##    ������ GM(1,1)ģ��Ԥ��
##   �����ˣ� ��Ӧ��
##  ����ʱ�䣺2014-9-28
##     ��ע��GM11���ֲο���������������
## 
#######################################################
rm(list=ls(all=TRUE))
#======================================================
#= ��������
#= x����������
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
    # һ���������Ӵ���������
    XD
    for (i in 1:XD_Len)
    {
       XD2[i] <- (1/(XD_Len-i+1))*sum(XD[i:XD_Len])
    }
    # �����������Ӵ���������
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
    plot(X0,type="b",pch=15,lty=1,col="red",xlab="������",
         ylab="ֵ")
    title("һ�׺Ͷ��׼�������ʾ��ͼ",adj=1)
    lines(XD,type="b",pch=16,lty=2,col="blue")
    lines(XD2,type="b",pch=17,lty=2,col="green")    

    legend("bottomright",inset=.05,title="Performance",
           c(expression(X^{(0)}),
           expression(X*D^{(0)}),
           expression(X*D^{(1)})),
           horiz=FALSE,lty=c(1,2,3),pch=c(15,16,17),
           col=c("red","blue","green"))
    plot(AGO1,type="b",pch=15,lty=1,col="red",xlab="������",
         ylab="ֵ")
    title("����AGO�ۼ�����ʾ��ͼ",adj=1)
    par(opar)
    return(XD2)
}

#======================================================
#= GM(1,1)ģ��
#= x: ����ģ����������
#= k: ��ģ�ͽ�����������ҪԤ����ٸ�ֵ��k>0
#======================================================
GM11 <- function(x,k)
{
    n <- length(x);
    x1 <- numeric(n);
    #��һ���ۼ�
    for (i in 1:n)
    {
        x1[i] <- sum(x[1:i]);
    }
    z1 <- numeric(n);
    m <- n-1;
    for (j in 1:m)
    {
        z1[j+1] <- (0.5*x1[j+1] + 0.5*x1[j])
    }
    Yn <- t(t(x[2:n]));
    
    B <- matrix(1,nrow=n-1,ncol=2);
    B[,1]<-t(t(-z1[2:n]));  
    #��С���˷���������
    u<-solve(t(B)%*%B)%*%t(B)%*%Yn;
    a<-u[1];
    b<-u[2];
   
    cat("��չϵͳ(a)-������(b):",a, b,'\n',
                       "Ԥ�⹫ʽ��","x(k+1)=(",x[1]-b/a,
                       ") * exp (",-a," * k ) ",b/a,"\n");

    GreyPredit(x1[1],a,b,6)
}
#======================================================
#= Ԥ��ģ��
#= start: ���еĳ�ʼֵ
#=     a: ��չϵ��
#=     b: ��ɫ������
#=     k: Ԥ�����ֵ 
#======================================================
GreyPredit <- function(start, a, b, k)
{
    #Ԥ��ֵ
    x2<-numeric(k);
    x2[1]<-start;
    for(i in 1:k-1)
    {
       x2[1+i]=(start-b/a)*exp(-a*i)+b/a;
    } 
    cat("ʱ����Ӧʽ����ֵ��",x2,"\n")   
    x2=c(0,x2);    
    #���ò�ֺ�������ģ������
    y=diff(x2);
    cat("Ԥ��ֵ��",y,"\n")   
}
X0 <- c(48.32, 80.91, 104.66, 121.83, 130.17, 144.87)
GM11(WeakenOperator(X0),6)