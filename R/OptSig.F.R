OptSig.F <-
function(df1,df2,ncp,p=0.5,k=1,Figure=TRUE){
  
  alphavec=seq(0,1,0.00001)
  betavec=1-Power.F(df1,df2,ncp,alphavec,Figure=FALSE)$Power
  
  M=E.loss(alphavec,betavec,p,k,Figure)
  alphas=M$alpha.opt
  cr1=qf(1-alphas,df1=df1,df2=df2)
  return(list(alpha.opt=alphas,crit.opt=cr1,beta.opt=M$beta.opt))}