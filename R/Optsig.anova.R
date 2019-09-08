OptSig.anova <-
function(K=NULL,n=NULL,f=NULL,p=0.5,k=1,Figure=TRUE){
  alphavec=seq(0.00001,1,0.00001)
  powervec=pwr.anova.test(k=K,n=n,f=f,sig.level=alphavec,power=NULL)$power
  betavec=1-powervec  
  
  M=E.loss(alphavec,betavec,p,k,Figure)
  alphas=M$alpha.opt
  return(list(alpha.opt = alphas, beta.opt = M$beta.opt))}
  