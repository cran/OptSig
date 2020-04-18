\name{OptSig.Boot}
\alias{OptSig.Boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal Significance Level for the F-test using the bootstrap
}
\description{
The function calculates the optimal level of significance for the F-test 

The bootstrap can be conducted using either iid resampling or wild bootstrap.

}
\usage{
OptSig.Boot(y,x,Rmat,rvec,p=0.5,k=1,nboot=3000,wild=FALSE,Figure=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a matrix of dependent variable, T by 1}
  \item{x}{ a matrix of K independent variable, T by K}
  \item{Rmat}{ a matrix for J restrictions, J by (K+1) }
  \item{rvec}{ a vector for restrictions, J by 1}
  \item{p}{ prior probability for H0, default is p = 0.5}
  \item{k}{ relative loss from Type I and II errors, k = L2/L1, default is k = 1}
  \item{nboot}{ the number of bootstrap iterations, the default is 3000}
  \item{wild}{ if TRUE, wild bootsrap is conducted; if FALSE (default), bootstrap is based on iid residual resampling}
  \item{Figure}{ show graph if TRUE (default). No graph otherwise }
}
\details{
See Kim and Choi (2020) 
}
\value{
\item{alpha.opt }{Optimal level of significance}
\item{crit.opt }{Critical value at the optimal level}
\item{beta.opt }{Type II error probability at the optimal level}
}
\references{
Kim and Choi, 2020, Choosing the Level of Significance: A Decision-theoretic Approach, Abacus, Wiley.
<https://doi.org/10.1111/abac.12172>

}
\author{
Jae H. Kim
}
\note{
Applicable to a linear regression model

The black curve in the figure plots the denity under H0;
The blue curve in the figure plots the denity under H1. 
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
Rmat=matrix(c(0,1,1),nrow=1); rvec=matrix(0.94,nrow=1)

OptSig.Boot(y,x,Rmat,rvec,p=0.5,k=1,nboot=1000,Figure=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

