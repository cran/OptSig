Opt.sig.t.test <-
function(ncp=NULL,d=NULL,n=NULL,p=0.5,k=1,type="one.sample",alternative="two.sided",Figure="TRUE"){
  alphavec=seq(0.00001,1,0.00001)
  if (!is.null(ncp) & is.null(d)){
    if (alternative=="less") betavec=1-pt(qt(alphavec,df=n-1),df=n-1,ncp=ncp)     
    if (alternative=="greater") betavec=pt(-qt(alphavec,df=n-1),df=n-1,ncp=ncp)     
    if (alternative=="two.sided") betavec=(1-pt(qt(0.5*alphavec,df=n-1),df=n-1,ncp=-abs(ncp))) + pt(-qt(0.5*alphavec,df=n-1),df=n-1,ncp = abs(ncp))
  }
  if (!is.null(d) & is.null(ncp) ) betavec = 1-pwr.t.test(d=d,n=n,sig.level=alphavec,power=NULL,type,alternative)$power
  
  M=E.loss(alphavec,betavec,p,k,Figure)
  alphas=M$alpha.opt
  return(list(alpha.opt=alphas,beta.opt=M$beta.opt))}
