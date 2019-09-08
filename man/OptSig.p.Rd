\name{OptSig.p}
\alias{OptSig.p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal significance level calculation for proportion tests (one sample)
}
\description{
Computes the optimal significance level for proportion tests (one sample)
}
\usage{
OptSig.p(ncp=NULL,h=NULL,n=NULL,p=0.5,k=1,alternative="two.sided",Figure=TRUE)
}
\arguments{
  \item{ncp}{Non-centraity}
  \item{h}{Effect size. Cohen's h}
  \item{n}{Number of observations (per sample)}
  \item{p}{ prior probability for H0, default is p = 0.5}
  \item{k}{ relative loss from Type I and II errors, k = L2/L1, default is k = 1}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less"
}
\item{Figure}{ show graph if TRUE (default); No graph if FALSE }
}
\details{
Refer to Kim and Choi (2019) for the details of k and p

Either ncp or h value should be given

For h, refer to Cohen (1988) or Chapmely (2017)

In a general term, if X ~ N(mu,sigma^2); let H0:mu = mu0; and H1:mu = mu1; 

ncp = sqrt(n)(mu1-mu0)/sigma


}
\value{
\item{alpha.opt }{Optimal level of significance}
\item{beta.opt }{Type II error probability at the optimal level}
}
\references{
Kim and Choi, 2019, Choosing the Level of Significance: A Decision-theoretic Approach: Abacus: a Journal of Accounting, Finance and Business Studies. Wiley. 

Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.

Stephane Champely (2017). pwr: Basic Functions for Power Analysis. R package version 1.2-1.
  https://CRAN.R-project.org/package=pwr
}
\author{
Jae H. Kim (using a function from the pwr package)
}
\note{
Also refer to the manual for the pwr package

The black curve in the figure is the line of enlightened judgement: see Kim and Choi (2019).
The red dot inticates the optimal significance level that minimizes the expected loss: (alpha.opt,beta.opt).
The blue horizontal line indicates the case of alpha = 0.05 as a reference point.        
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Kim and Choi, 2017, Choosing the Level of Significance: A Decision-theoretic Approach
}
\examples{
OptSig.p(h=0.2,n=60,alternative="two.sided")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

