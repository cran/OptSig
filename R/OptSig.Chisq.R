OptSig.Chisq <-
function(w=NULL,N=NULL, ncp=NULL, df, p = 0.5, k = 1, Figure = TRUE)
{
  alphavec = seq(0, 1, 1e-05)
  if (!is.null(ncp) & is.null(w)) betavec = 1 - Power.Chisq(df, ncp, alphavec, Figure = FALSE)$Power
  if (is.null(ncp) & !is.null(w)) betavec = 1 - pwr.chisq.test(w=w,N=N,df=df,sig.level=alphavec,power=NULL)$power
  
  M=E.loss(alphavec,betavec,p,k,Figure)
  alphas=M$alpha.opt
  cr1 = qchisq(1 - alphas, df = df)
  return(list(alpha.opt = alphas, crit.opt = cr1, beta.opt = M$beta.opt))
}