OptSig.Weight <-
function(df1,df2,m,delta=2,p=0.5,k=1,Figure=TRUE){
  D=Folded.Normal(m,delta,Figure)
  ff1=D$x; w=D$w
  stat=numeric()
  for(i in 1:length(ff1)){
    tem=OptSig.F(df1,df2,ncp=ff1[i],p,k, Figure=FALSE)$alpha.opt
    stat=c(stat,tem)}
  alphas=sum(w*stat); cr1=qf(1-alphas,df1=df1,df2=df2)
  return(list(alpha.opt=alphas,crit.opt=cr1))}
