\name{Opt.sig.r.test}
\alias{Opt.sig.r.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal significance level calculation for correlation test
}
\description{
Computes the optimal significance level for the correlation test
}

\usage{
Opt.sig.r.test(r = NULL, n = NULL, p = 0.5, k = 1, alternative = "two.sided")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{Linear correlation coefficient}
  \item{n}{ sample size }
  \item{p}{ prior probability for H0, default is p = 0.5}
  \item{k}{ relative loss from Type I and II error, k = L2/L1, default is k = 1}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less"
}
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
Opt.sig.r.test(r=0.2,n=60,alternative="two.sided")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

