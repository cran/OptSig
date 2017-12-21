\name{Opt.sig.anova.test}
\alias{Opt.sig.anova.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal significance level calculation for balanced one-way analysis of variance tests
}
\description{
Computes the optimal significance level for the test for balanced one-way analysis of variance tests
}
\usage{
Opt.sig.anova.test(K = NULL, n = NULL, f = NULL, p = 0.5, k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{K}{Number of groups}
  \item{n}{Number of observations (per group)}
  \item{f}{Effect size}
  \item{p}{ prior probability for H0, default is p = 0.5}
  \item{k}{ relative loss from Type I and II errors, k = L2/L1, default is k = 1}
}
\details{
Refer to Kim and Choi (2017) for the details of k and p
}
\value{
\item{alpha.opt }{Optimal level of significance}
\item{beta.opt }{Type II error probability at the optimal level}
}
\references{
Kim and Choi, 2017, Choosing the Level of Significance: A Decision-theoretic Approach: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2652773

Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.

Stephane Champely (2017). pwr: Basic Functions for Power Analysis. R package version 1.2-1.
  https://CRAN.R-project.org/package=pwr
}
\author{
Jae H. Kim (using a function from the pwr package)
}
\note{
Also refer to the manual for the pwr package
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Kim and Choi, 2017, Choosing the Level of Significance: A Decision-theoretic Approach
}
\examples{
Opt.sig.anova.test(f=0.28,K=4,n=20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

