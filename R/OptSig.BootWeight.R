OptSig.BootWeight <-
function(y,x,Rmat,rvec,p=0.5,k=1,nboot=3000,wild=FALSE,Figure=TRUE){
  set.seed(123456)
  y=as.matrix(y);x=as.matrix(x);n=nrow(y); 
  
  M=R.OLS(y,x,Rmat,rvec); n=nrow(y) 
  b0 = M$coef[,2,drop=FALSE]; e0 = M$resid[,2];  
  b = M$coef[,1,drop=FALSE]; e = M$resid[,1];
  ncp=M$ncp 
  
  xboot=cbind(rep(1,n),x); mat1=solve( t(xboot) %*% xboot )
  ncp.boot=matrix(NA,nrow=nboot,ncol=1+nrow(b))
  for(i in 1:nboot){
    
    if(wild==FALSE) ys = xboot%*%b +sample(e,size=n,replace=TRUE)
    if(wild==TRUE)  ys = xboot%*%b +wild(n)*e
    
    bs =  mat1 %*% t(xboot) %*% ys
    es=ys-xboot%*%bs
    s2s=sum(es^2)/(n-ncol(xboot)); #s2=sum(e^2)/n
    lamda1s=solve(s2s*Rmat %*% mat1 %*% t(Rmat)); 
    lamda2s=Rmat%*%bs - rvec
    ncp.boot[i,]=cbind(t(lamda2s)%*%lamda1s%*%lamda2s,t(bs))
  }
  
  tem=density(ncp.boot,n=5000); 
  tem1=cbind(tem$x,tem$y)
  tem3=quantile(ncp.boot[,1],probs=seq(0.1,0.9,0.1),type=1)
  tem4=matrix(NA,nrow=length(tem3),ncol=2)
  for (i in 1:length(tem3)){
    index=which.min(abs(tem1[,1]-tem3[i]))
    tem4[i,]=tem1[index,,drop=FALSE]}
  tem4[,2] = tem4[,2]/sum(tem4[,2])
  
  tem5=matrix(NA,nrow=length(tem3),ncol=ncol(ncp.boot))
  for (i in 1:nrow(tem4)){
    index=which.min(abs(ncp.boot[,1]-tem4[i]))
    tem5[i,]=ncp.boot[index,,drop=FALSE]}
  w = tem4[,2]
  if(Figure==TRUE) plot(tem,main="Bootstrap Distribution of Lambda",xlab="ncp",lwd=2); abline(v=ncp,col="blue")
  
  f.mat0=matrix(NA,nrow=nboot,ncol=1)
  xboot=cbind(rep(1,n),x)
  for(i in 1:nboot){
    
    if(wild==FALSE) ys0 = xboot%*%b0 +sample(e0,size=n,replace=TRUE)
    if(wild==TRUE)  ys0 = xboot%*%b0 +wild(n)*e0
    f.mat0[i,1]=R.OLS(ys0,x,Rmat,rvec)$F.stat[1] 
  }
  
  # Bootstrap Critical values
  alphavec=seq(0,1,0.00001)
  powervec=alphavec
  crvec=quantile(f.mat0,probs=1-alphavec)
  
  alphasvec=numeric()
  for (j in 1:nrow(tem5)){
    b1=t(tem5[j,2:ncol(tem5),drop=FALSE]); 
    e1=y-xboot%*%b1; e1=e1-mean(e1)
    f.mat1=matrix(NA,nrow=nboot,ncol=1)
    for(i in 1:nboot){
      
      if(wild==FALSE) ys1 = xboot%*%b1 +sample(e1,size=n,replace=TRUE)
      if(wild==TRUE)  ys1 = xboot%*%b1 +wild(n)*e1
      f.mat1[i,1]=R.OLS(ys1,x,Rmat,rvec)$F.stat[1]
    }
    
    for(i in 1:length(crvec))powervec[i]=mean(f.mat1 > crvec[i])
    betavec=1-powervec     
    dd=cbind(alphavec,betavec,abs(p*alphavec+(1-p)*k*betavec))
    dd1= dd[dd[,1] == 0.05,]
    dd2= dd[dd[,3] == min(dd[,3]),]
    alphas=dd2[1];betas=dd2[2]; names(alphas) = NULL; names(betas) = NULL
    alphasvec=c(alphasvec,alphas)
  }
  alphas=sum(alphasvec*w); cr1=quantile(f.mat0,probs=1-alphas); names(cr1) = NULL
  return(list(opt.sig=alphas,crit.opt=cr1))}
