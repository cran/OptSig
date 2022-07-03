\name{R.OLS}
\alias{R.OLS}
\title{
Restricted OLS estimation and F-test
}
\description{
Function to calcuate the Restricted (under H0) OLS Estimators and F-test statistic
}
\usage{
R.OLS(y, x, Rmat, rvec)
}
\arguments{
  \item{y}{ a matrix of dependent variable, T by 1}
  \item{x}{ a matrix of K independent variable, T by K}
  \item{Rmat}{ a matrix for J restrictions, J by (K+1) }
  \item{rvec}{ a vector for restrictions, J by 1}
}
\details{
Rmat and rvec are the matrices for the linear restrictions, which a user should supply. 

Refer to an econometrics textbook for details. 
}
\value{

  \item{coef }{matrix of estimated coefficients, (K+1) by 2, under H1 and H0}
  \item{RSq }{R-square values under H1 and H0, 2 by 1}
  \item{resid}{residual vector under H1 and H0, T by 2}
  \item{F.stat}{F-statistic and p-value}
  \item{ncp}{non-centrality parameter, estimated by replaicing unknowns using OLS estimates}
}
\references{
Kim and Choi, 2020, Choosing the Level of Significance: A Decision-theoretic Approach, Abacus, Wiley.
<https://doi.org/10.1111/abac.12172>

}
\author{
Jae H. Kim
}
\note{
The function automatically adds an intercept, so the user need not include a vector of ones in x matrix.
}

\seealso{
Leamer, E. 1978, Specification Searches: Ad Hoc Inference with Nonexperimental Data, Wiley, New York.

Kim, JH and Ji, P. 2015, Significance Testing in Empirical Finance: A Critical Review and Assessment, Journal of Empirical Finance 34, 1-14.
<DOI:http://dx.doi.org/10.1016/j.jempfin.2015.08.006>

Kim, Jae H., 2020, Decision-theoretic hypothesis testing: A primer with R package OptSig, The American Statistician. 
<https://doi.org/10.1080/00031305.2020.1750484.>

}
\examples{
data(data1)
# Define Y and X
y=data1$lnoutput; x=cbind(data1$lncapital,data1$lnlabor)
# Restriction matrices to test for constant returns to scale
Rmat=matrix(c(0,1,1),nrow=1); rvec=matrix(1,nrow=1)
# Model Estimation and F-test
M=R.OLS(y,x,Rmat,rvec) 
}
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

