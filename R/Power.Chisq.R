Power.Chisq <-
function(df,ncp,alpha,Figure=TRUE){
  cr1 = qchisq(1 - alpha, df = df)
  power1 = pchisq(cr1, df = df, ncp = ncp, lower.tail = FALSE)
  if (Figure == TRUE) {
    ec = df+ncp
    vc = 2 * (df+2*ncp) 
    xlim = ec + 3 * sqrt(vc)
    x = seq(0, xlim, 0.01)
    density = dchisq(x, df = df)
    density1 = dchisq(x, df = df, ncp = ncp)
    plot(x, density, type = "l", lwd = 3, main = "Distributions under H0 and H1")
    points(x, density1, type = "l", col = "red", lwd = 3)
    abline(v = cr1, col = "blue")
    region.x = x[x > cr1]
    region.y = density[x > cr1]
    region.x = c(cr1, region.x, tail(region.x, 1))
    region.y = c(0, region.y, 0)
    polygon(region.x, region.y, density = -1, col = "gray")
    region.x = x[x > cr1]
    region.y = density1[x > cr1]
    region.x = c(cr1, region.x, tail(region.x, 1))
    region.y = c(0, region.y, 0)
    polygon(region.x, region.y, density = 10, col = "red")
    abline(v = cr1, col = "blue", lwd = 3)
    abline(h = 0, lwd = 1)}
  return(list(Power = power1, Crit.val = cr1))}
