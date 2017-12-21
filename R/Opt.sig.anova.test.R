Opt.sig.anova.test <-
function(K=NULL,n=NULL,f=NULL,p=0.5,k=1){
  alphavec=seq(0.00001,1,0.00001)
  powervec=pwr.anova.test(k=K,n=n,f=f,sig.level=alphavec,power=NULL)$power
  betavec=1-powervec     
  dd=cbind(alphavec,betavec,abs(p*alphavec+(1-p)*k*betavec))
  dd1= dd[dd[,1] == 0.05,]
  dd2= dd[dd[,3] == min(dd[,3]),]
  alphas=dd2[1];betas=dd2[2]; names(alphas) = NULL; names(betas) = NULL
  plot(betavec,alphavec,type="l",xlim=c(0,1),col=1,lwd=2,ylab="alpha",xlab="beta",cex.lab=1.5,main="Optimal Level of Significance") 
  points(dd2[2],dd2[1],col=4,pch=15,cex=1.5)
  abline(v=seq(0,1,0.1), col="lightgray", lty="dotted")
  abline(h=seq(0,1,0.1), col="lightgray", lty="dotted")
  abline(h=0.05,col="red",lwd=2)
  return(list(op.sig=alphas,beta.opt=betas))}
