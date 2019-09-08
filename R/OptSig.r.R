OptSig.r <-
function(r=NULL,n=NULL,p=0.5,k=1,alternative="two.sided",Figure=TRUE){
  alphavec=seq(0.00001,1,0.00001)
  betavec = 1-pwr.r.test(r=r,n=n,sig.level=alphavec,power=NULL,alternative)$power
  
  M=E.loss(alphavec,betavec,p,k,Figure)
  alphas=M$alpha.opt
  return(list(alpha.opt=alphas,beta.opt=M$beta.opt))}
