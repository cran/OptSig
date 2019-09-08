OptSig.t2n <-
function(ncp=NULL,d=NULL,n1=NULL,n2=NULL,p=0.5,k=1,alternative="two.sided",Figure=TRUE){
   alphavec=seq(0.00001,1,0.00001)
  n=n1+n2
  if (!is.null(ncp) & is.null(d)){
    if (alternative=="less") betavec=1-pt(qt(alphavec,df=n-2),df=n-2,ncp=ncp)     
    if (alternative=="greater") betavec=pt(-qt(alphavec,df=n-2),df=n-2,ncp=ncp)     
    if (alternative=="two.sided") betavec=(1-pt(qt(0.5*alphavec,df=n-2),df=n-2,ncp=-abs(ncp))) + pt(-qt(0.5*alphavec,df=n-2),df=n-2,ncp = abs(ncp))
  }
  if (!is.null(d) & is.null(ncp) ) betavec = 1-pwr.t2n.test(d=d,n1=n1,n2=n2,sig.level=alphavec,power=NULL,alternative)$power
  
  M=E.loss(alphavec,betavec,p,k,Figure)
  alphas=M$alpha.opt
  return(list(alpha.opt=alphas,beta.opt=M$beta.opt))}

