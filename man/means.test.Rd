\name{High-dimensional tests for 2 population mean vectors}
\alias{means.test}
\title{
High-dimensional tests for 2 population mean vectors
}

\description{
High-dimensional tests for 2 population mean vectors.
}

\usage{
means.test(x1, x2, test = "baisara", cov.equal = TRUE, Rp = 1, Rb = 1, ncores = 1)
}

\arguments{
\item{x1}{
A numerical matrix with data, where the number of columns (variables) is greater than the number of rows (observations).
}
\item{x2}{
A numerical matrix with data, where the number of columns (variables) is greater than the number of rows (observations).
}
\item{test}{
The test to run. "baisara" corresponds to the test proposed by Bai and Saranadasa (1996), "cailiuxia" to the test proposed by Cai, Liu and Xia (2014), "chenlizho" to the test proposed by Chen, Li and Zhong (2014), "chenqin" to the test proposed by Chen and Qin (2010), and "sridu" to the test proposed by Srivastava and Du (2008). Finally, the "skk" corresponds to the test proposed by Srivastava, Katayama, and Kano(2013).
}
\item{cov.equal}{
This argument applies to the "cailiuxia", "chenlizho" and "chenqin" tests only. If this is true, it assumes that the two population covariance matrices are equal.
}
\item{Rp}{
The number of permutations to perform.
}
\item{Rb}{
The number of bootstrap samples to draw.
}
\item{ncores}{
The number of cores to use. By default is 1, so no parallel computations take place. You are advised to load the necessary libraries prior to using this argument.
}
}

\details{
These methods are designed for high-dimensional data. If the data dimension \eqn{p} is smaller than 30 this may result in an inflated Type-I error rate.

\bold{Note} that these tests were taken from the \pkg{highmean} package, except for the "skk" which was taken from the \pkg{highDmean} package. The "cailiaxiu" and "chenqin" tests here should be the same as in the function \link{pe.test} ("clx" and "cq", respectively), but the authors of that package and of the package \pkg{highmean} give different formulas.
}

\value{
A list including:
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
}

\references{
Bai Z.D. and Saranadasa H. (1996). Effect of high dimension: by an example of a two sample
problem. Statistica Sinica, 6(2): 311--329.

Cai T.T., Liu W., and Xia Y. (2014). Two-sample test of high dimensional means under dependence.
Journal of the Royal Statistical Society: Series B (Statistical Methodology), 76(2): 349--372.

Chen S.X. and Qin Y.L. (2010). A two-sample test for high-dimensional data with applications to
gene-set testing. The Annals of Statistics, 38(2) 808--835.

Chen S.X., Li J., and Zhong P.S. (2014). Two-Sample Tests for High Dimensional Means with
Thresholding and Data Transformation. arXiv preprint arXiv:1410.2848.

Srivastava M.S. and Du M. (2008). A test for the mean vector with fewer observations than the
dimension. Journal of Multivariate Analysis, 99(3): 386--402.

Srivastava, M.S., Katayama, S., and Kano, Y. (2013). A two sample test in high dimensional data.
Journal of Multivariate Analysis, 114: 349--358.
}

\author{
Michail Tsagris and Manos Papadakis.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and
Manos Papadakis \email{papadakm95@gmail.com}.
}

\seealso{
\code{ \link{pe.test}
}
}

\examples{
n1 <- 50  ; n2 <- 100 ; p <- 500
x1 <- matrix( rnorm(n1 * p), nrow = n1, ncol = p)
x2 <- matrix( rnorm(n2 * p), nrow = n2, ncol = p)
means.test(x1, x2)
}
