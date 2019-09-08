OptSig.Boot <-
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
  betavec=alphavec
  crvec=quantile(f.mat0,probs=1-alphavec)
  for(i in 1:length(crvec)) betavec[i]=1-mean(f.mat1 > crvec[i])
  
  M=E.loss(alphavec,betavec,p,k,Figure)
  alphas=M$alpha.opt
  
  cr1=quantile(f.mat0,probs=1-alphas); names(cr1) = NULL
  
    if (Figure == TRUE) {
    # Plotting densities under H0 and H1
    x=f.mat0; density=density(x)
    x1=f.mat1; density1=density(x1)
    plot(density,xlim=c(0,max(f.mat1)),lwd=2,main="Density functions under H0 and H1: Bootstrap")
    points(density1,type="l",col=2,lwd=2)
    abline(v=cr1,col="blue")
    par(mfrow=c(1,1))}
  
  return(list(alpha.opt=alphas,crit.opt=cr1,beta.opt=M$beta.opt))}
