OptSig.p <-
function(ncp=NULL,h=NULL,n=NULL,p=0.5,k=1,alternative="two.sided",Figure=TRUE){
 alphavec=seq(0.00001,1,0.00001)
  if (!is.null(ncp) & is.null(h)){
    if (alternative=="less") betavec=1-pnorm(qnorm(alphavec),mean=ncp)     
    if (alternative=="greater") betavec=pnorm(-qnorm(alphavec),mean=ncp)     
    if (alternative=="two.sided") betavec=(1-pnorm(qnorm(0.5*alphavec),mean=-abs(ncp))) + pnorm(-qnorm(0.5*alphavec),mean=abs(ncp))
  }
  if (!is.null(h) & is.null(ncp) ) betavec = 1-pwr.p.test(h=h,n=n,sig.level=alphavec,power=NULL,alternative)$power
  M=E.loss(alphavec,betavec,p,k,Figure)
  alphas=M$alpha.opt
  return(list(alpha.opt=alphas,beta.opt=M$beta.opt))}
