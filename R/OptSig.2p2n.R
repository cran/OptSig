OptSig.2p2n <-
function(ncp=NULL,h=NULL,n1=NULL,n2=NULL,p=0.5,k=1,alternative="two.sided",Figure=TRUE){
  alphavec = seq(1e-05, 1, 1e-05)
  
  if (!is.null(ncp) & is.null(h)){
    if (alternative=="less") betavec=1-pnorm(qnorm(alphavec),mean=ncp)     
    if (alternative=="greater") betavec=pnorm(-qnorm(alphavec),mean=ncp)     
    if (alternative=="two.sided") betavec=(1-pnorm(qnorm(0.5*alphavec),mean=-abs(ncp))) + pnorm(-qnorm(0.5*alphavec),mean=abs(ncp))
  }
  if (!is.null(h) & is.null(ncp) ) betavec = 1-pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = alphavec, power = NULL, alternative)$power
  
  M=E.loss(alphavec,betavec,p,k,Figure=TRUE)
  
  return(list(alpha.opt=M$alpha.opt,beta.opt=M$beta.opt))}