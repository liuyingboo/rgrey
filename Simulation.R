rm(list=ls(all=TRUE))
x<- c(27260, 56807, 89218, 124606)
count <- length(x)
Yn <- t(t( x[2:count]))
B <- matrix(1,nrow=count-1,ncol=2);
B[,1] <- t(t(x[1:count-1]));
u<-solve(t(B)%*%B)%*%t(B)%*%Yn;
u