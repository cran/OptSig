Opt.SigBoot <-
function(y,x,Rmat,rvec,p=0.5,k=1,nboot=3000,wild=FALSE,Figure=TRUE)
{
  set.seed(12345)
  y=as.matrix(y);x=as.matrix(x)
  M=R.OLS(y,x,Rmat,rvec); n=nrow(y) 
  b0 = M$coef[,2,drop=FALSE]; e0 = M$resid[,2];  
  b1 = M$coef[,1,drop=FALSE]; e1 = M$resid[,1];  
  
  f.mat0=matrix(NA,nrow=nboot,ncol=1)
  f.mat1=matrix(NA,nrow=nboot,ncol=1)
  
  xboot=cbind(rep(1,n),x)
  for(i in 1:nboot){
    
    if(wild==FALSE){
    ys0 = xboot%*%b0 +sample(e0,size=n,replace=TRUE)
    ys1 = xboot%*%b1 +sample(e1,size=n,replace=TRUE)}
    
    if(wild==TRUE){
      ys0 = xboot%*%b0 +wild(n)*e0
      ys1 = xboot%*%b1 +wild(n)*e1}
    
    f.mat0[i,1]=R.OLS(ys0,x,Rmat,rvec)$F.stat[1] 
    f.mat1[i,1]=R.OLS(ys1,x,Rmat,rvec)$F.stat[1]
  }
  
  alphavec=seq(0,1,0.00001)
  powervec=alphavec
  crvec=quantile(f.mat0,probs=1-alphavec)
  for(i in 1:length(crvec))powervec[i]=mean(f.mat1 > crvec[i])
  
  betavec=1-powervec     
  dd=cbind(alphavec,betavec,abs(p*alphavec+(1-p)*k*betavec))
  dd1= dd[dd[,1] == 0.05,]
  dd2= dd[dd[,3] == min(dd[,3]),]
  alphas=dd2[1];betas=dd2[2]; names(alphas) = NULL; names(betas) = NULL
  cr1=quantile(f.mat0,probs=1-alphas); names(cr1) = NULL
  if (Figure == TRUE) {
    par(mfrow=c(1,2))
    # Plotting densities under H0 and H1
    x=f.mat0; density=density(x)
    x1=f.mat1; density1=density(x1)
    plot(density,xlim=c(0,max(f.mat1)),lwd=2,main="Density functions under H0 and H1: Bootstrap")
    points(density1,type="l",col=2,lwd=2)
    abline(v=cr1,col="blue")
    
    plot(betavec,alphavec,type="l",xlim=c(0,1),col=1,lwd=2,ylab="alpha",xlab="beta",cex.lab=1.5,main="Optimal Level of Significance") 
    points(dd2[2],dd2[1],col=4,pch=15,cex=1.5)
    abline(v=seq(0,1,0.1), col="lightgray", lty="dotted")
    abline(h=seq(0,1,0.1), col="lightgray", lty="dotted")
    abline(h=0.05,col="red",lwd=2)
    par(mfrow=c(1,1))}
    return(list(alpha.opt=alphas,crit.opt=cr1,beta.opt=betas))}
