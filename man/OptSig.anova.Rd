\name{OptSig.anova}
\alias{OptSig.anova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal significance level calculation for balanced one-way analysis of variance tests
}
\description{
Computes the optimal significance level for the test for balanced one-way analysis of variance tests
}
\usage{
OptSig.anova(K = NULL, n = NULL, f = NULL, p = 0.5, k = 1, Figure = TRUE)
}
\arguments{
  \item{K}{Number of groups}
  \item{n}{Number of observations (per group)}
  \item{f}{Effect size}
  \item{p}{ prior probability for H0, default is p = 0.5}
  \item{k}{ relative loss from Type I and II errors, k = L2/L1, default is k = 1}
\item{Figure}{ show graph if TRUE (default); No graph if FALSE }
}
\details{
Refer to Kim and Choi (2020) for the details of k and p

For the value of f, refer to Cohen (1988) or Champely (2017)
}
\value{
\item{alpha.opt }{Optimal level of significance}
\item{beta.opt }{Type II error probability at the optimal level}
}
\references{
Kim and Choi, 2020, Choosing the Level of Significance: A Decision-theoretic Approach: Abacus: a Journal of Accounting, Finance and Business Studies. Wiley. 
<https://doi.org/10.1111/abac.12172>

Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.

Stephane Champely (2017). pwr: Basic Functions for Power Analysis. R package version 1.2-1.
  https://CRAN.R-project.org/package=pwr
}
\author{
Jae H. Kim (using a function from the pwr package)
}
\note{
Also refer to the manual for the pwr package

The black curve in the figure is the line of enlightened judgement: see Kim and Choi (2020).
The red dot inticates the optimal significance level that minimizes the expected loss: (alpha.opt,beta.opt).
The blue horizontal line indicates the case of alpha = 0.05 as a reference point.      

}


\seealso{
Kim, Jae H., 2020, Decision-theoretic hypothesis testing: A primer with R package OptSig, The American Statistician. 
<https://doi.org/10.1080/00031305.2020.1750484.>

}
\examples{
OptSig.anova(f=0.28,K=4,n=20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

