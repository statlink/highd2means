\name{Power-enhanced high-dimensional tests for 2 population mean vectors}
\alias{pe.test}
\title{
Power-enhanced high-dimensional tests for 2 population mean vectors
}

\description{
Power-enhanced high-dimensional tests for 2 population mean vectors.
}

\usage{
pe.test(x1, x2, test = "clx", Rp = 1, Rb = 1, ncores = 1)
}

\arguments{
\item{x1}{
A numerical matrix with data, where the number of columns (variables) is greater than the number of rows (observations).
}
\item{x2}{
A numerical matrix with data, where the number of columns (variables) is greater than the number of rows (observations).
}
\item{test}{
The test to run, either "clx" corresponding to the test proposed by Cai, Liu and Xia (2014), "cq" corresponding to the test proposed by Chen and Qin (2010). The "pecf" combines the two previous methods using the Cauchy and the Fisher approaches. The "pecomp" updates the "cq" test via the power-enhanced component.
}
\item{Rp}{
The number of permutations to perform. This is applicable only for the "clx" and "cq" tests.
}
\item{Rb}{
The number of bootstrap samples to draw. This is applicable only for the "clx" and "cq" tests.
}
\item{ncores}{
The number of cores to use. By default is 1, so no parallel computations take place. You are advised to load the necessary libraries prior to using this argument.
}
}

\details{
These methods are designed for high-dimensional data. If the data dimension \eqn{p} is smaller than 30 this may result in an inflated Type-I error rate.

\bold{Note} that these tests were taken from the \pkg{PEtests} package. The "clx" and "cq" tests here should be the same as in the function \link{means.test} ("cailiaxiu" and "chenqin", respectively), but the authors of that package and of the package \pkg{highmean} give different formulas.
}

\value{
If the test is "clx", "cq" or "pecomp" a list including:
\item{stat}{
The test statistic value.
}
\item{pvalue}{
The asymptotic p-value of the test.
}
\item{perm.pvalue}{
The permutation based p-value will appear only if the argument Rp is greater than 1.
}
\item{boot.pvalue}{
The bootstrap based p-value will appear only if the argument Rp is greater than 1.
}
If the test is "pecf" a list including:
\item{cq}{
A list with the results of the "cq" test, the stat and the pvalue.
}
\item{clx}{
A list with the results of the "clx" test, the stat and the pvalue.
}
\item{pecauchy}{
A list with the results of the Cauchy combination, the stat and the pvalue.
}
\item{pefisher}{
A list with the results of the Fisher combination, the stat and the pvalue.
}
}

\references{
Cai T.T., Liu W., and Xia Y. (2014). Two-sample test of high dimensional means under dependence.
Journal of the Royal Statistical Society: Series B (Statistical Methodology), 76(2): 349--372.

Chen S.X. and Qin Y.L. (2010). A two-sample test for high-dimensional data with applications to
gene-set testing. The Annals of Statistics, 38(2) 808--835.

Yu X., Li D., Xue L. and Li, R. (2023). Power-enhanced simultaneous test of high-dimensional
mean vectors and covariance matrices with application to gene-set testing. Journal of the American
Statistical Association, 118(544): 2548--2561.
}

\author{
Michail Tsagris and Manos Papadakis.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and
Manos Papadakis \email{papadakm95@gmail.com}.
}

\seealso{
\code{ \link{means.test}
}
}

\examples{
n1 <- 50  ; n2 <- 100 ; p <- 500
x1 <- matrix( rnorm(n1 * p), nrow = n1, ncol = p)
x2 <- matrix( rnorm(n2 * p), nrow = n2, ncol = p)
pe.test(x1, x2, test = "pecf")
}
