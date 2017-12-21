Opt.Sig <-
function(df1,df2,ncp,p=0.5,k=1,Figure=TRUE){
  
  alphavec=seq(0,1,0.00001)
  powervec=Power.F(df1,df2,ncp,alphavec,Figure=FALSE)$Power
  betavec=1-powervec
  
  dd=cbind(alphavec,betavec,abs(p*alphavec+(1-p)*k*betavec))
  dd1= dd[dd[,1] == 0.05,]
  dd2= dd[dd[,3] == min(dd[,3]),]
  
  if (Figure == TRUE) {
    plot(betavec,alphavec,type="l",xlim=c(0,1),col=1,lwd=2,ylab="alpha",xlab="beta",cex.lab=1.5,main="Optimal Level of Significance") 
    points(dd2[2],dd2[1],col=4,pch=15,cex=1.5)
    abline(v=seq(0,1,0.1), col="lightgray", lty="dotted")
    abline(h=seq(0,1,0.1), col="lightgray", lty="dotted")
    abline(h=0.05,col="red",lwd=2)}
  alphas=dd2[1];betas=dd2[2]; names(alphas) = NULL; names(betas) = NULL
  cr1=qf(1-alphas,df1=df1,df2=df2)
  return(list(alpha.opt=alphas,crit.opt=cr1,beta.opt=betas))}
