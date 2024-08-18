means.test <- function(x1, x2, test = "baisara", cov.equal = TRUE, Rp = 1, Rb = 1, ncores = 1) {
  if (test == "baisara") {
    res <- .baisara(x1, x2)
  } else if (test == "cailiuxia") {
    res <- .cailiuxia(x1, x2, cov.equal = cov.equal)
  } else if (test == "chenlizho") {
    res <- .chenlizho(x1, x2, cov.equal = cov.equal)
  } else if (test == "chenqin") {
    res <- .chenqin(x1, x2, cov.equal = cov.equal)
  } else if (test == "skk") {
    res <- .skk(x1, x2)
  } else if (test == "sridu") {
    res <- .sridu(x1, x2)
  }

  if ( ncores <= 1 ) {
    if (Rp > 1) {
      x <- rbind(x1, x2)
      n1 <- dim(x1)[1]
      n <- dim(x)[1]
      tp <- numeric(Rp)

      if (test == "baisara") {
        for (i in 1:Rp) {
          id <- sample(n, n1)
          tp[i] <- .baisara(x[id, ], x[-id, ])$stat
        }
      } else if (test == "cailiuxia") {
        for (i in 1:Rp) {
          id <- sample(n, n1)
          tp[i] <- .cailiuxia(x[id, ], x[-id, ])$stat
        }
      } else if (test == "chenlizho") {
        for (i in 1:Rp) {
          id <- sample(n, n1)
          tp[i] <- .chenlizho(x[id, ], x[-id, ])$stat
        }
      } else if (test == "chenqin") {
        for (i in 1:Rp) {
          id <- sample(n, n1)
          tp[i] <- .chenqin(x[id, ], x[-id, ])$stat
        }
      } else if (test == "skk") {
        for (i in 1:Rp) {
          id <- sample(n, n1)
          tp[i] <- .skk(x[id, ], x[-id, ])$stat
        }
      } else if (test == "sridu") {
        for (i in 1:Rp) {
          id <- sample(n, n1)
          tp[i] <- .sridu(x[id, ], x[-id, ])$stat
        }
      }
      res$perm.pvalue <- (sum(abs(tp) >= abs(res$stat)) + 1) / (Rp + 1)
    }

    if ( Rb > 1 ) {
      n1 <- dim(x1)[1]
      n2 <- dim(x2)[1]
      tb <- numeric(Rb)

      if ( test == "baisara" ) {
        for (i in 1:Rb) {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          tb[i] <- .baisara(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "cailiuxia") {
        for (i in 1:Rb) {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          tb[i] <- .cailiuxia(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "chenlizho") {
        for (i in 1:Rb) {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          tb[i] <- .chenlizho(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "chenqin") {
        for (i in 1:Rb) {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          tb[i] <- .chenqin(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "skk") {
        for (i in 1:Rb) {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          tb[i] <- .skk(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "sridu") {
        for (i in 1:Rb) {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          tb[i] <- .sridu(x1[id1, ], x2[id2, ])$stat
        }
      }
      res$boot.pvalue <- (sum(abs(tb) >= abs(res$stat)) + 1) / (Rb + 1)
    }

  } else {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)

    if (Rp > 1) {
      x <- rbind(x1, x2)
      n1 <- dim(x1)[1]
      n <- dim(x)[1]

      if (test == "baisara") {
        tp <- foreach::foreach(i = 1:Rp, .combine = c, .export = ".baisara") %dopar% {
          id <- sample(n, n1)
          .baisara(x[id, ], x[-id, ])$stat
        }
      } else if (test == "cailiuxia") {
        tp <- foreach::foreach(i = 1:Rp, .combine = c, .export = ".cailiuxia") %dopar% {
          id <- sample(n, n1)
          .cailiuxia(x[id, ], x[-id, ])$stat
        }
      } else if (test == "chenlizho") {
        tp <- foreach::foreach(i = 1:Rp, .combine = c, .export = ".chenlizho") %dopar% {
          id <- sample(n, n1)
          .chenlizho(x[id, ], x[-id, ])$stat
        }
      } else if (test == "chenqin") {
        tp <- foreach::foreach(i = 1:Rp, .combine = c, .export = ".chenqin") %dopar% {
          id <- sample(n, n1)
          .chenqin(x[id, ], x[-id, ])$stat
        }
      } else if (test == "skk") {
        tp <- foreach::foreach(i = 1:Rp, .combine = c, .export = ".skk") %dopar% {
          id <- sample(n, n1)
          .skk(x[id, ], x[-id, ])$stat
        }
      } else if (test == "sridu") {
        tp <- foreach::foreach(i = 1:Rp, .combine = c, .export = ".sridu") %dopar% {
          id <- sample(n, n1)
          .sridu(x[id, ], x[-id, ])$stat
        }
      }
      res$perm.pvalue <- (sum(abs(tp) >= abs(res$stat)) + 1) / (Rp + 1)
    }

    if (Rb > 1) {
      n1 <- dim(x1)[1]
      n2 <- dim(x2)[1]

      if (test == "baisara") {
        tb <- foreach::foreach(i = 1:Rb, .combine = c, .export = ".baisara") %dopar% {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          .baisara(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "cailiuxia") {
        tb <- foreach::foreach(i = 1:Rb, .combine = c, .export = ".cailiuxia") %dopar% {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          .cailiuxia(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "chenlizho") {
        tb <- foreach::foreach(i = 1:Rb, .combine = c, .export = ".chenlizho") %dopar% {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          .chenlizho(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "chenqin") {
        tb <- foreach::foreach(i = 1:Rb, .combine = c, .export = ".chenqin") %dopar% {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          .chenqin(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "skk") {
        tb <- foreach::foreach(i = 1:Rb, .combine = c, .export = ".skk") %dopar% {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          .skk(x1[id1, ], x2[id2, ])$stat
        }
      } else if (test == "sridu") {
        tb <- foreach::foreach(i = 1:Rb, .combine = c, .export = ".sridu") %dopar% {
          id1 <- sample(n1, n1, replace = TRUE)
          id2 <- sample(n2, n2, replace = TRUE)
          .sridu(x1[id1, ], x2[id2, ])$stat
        }
      }
      res$boot.pvalue <- (sum(abs(tb) >= abs(res$stat)) + 1) / (Rb + 1)
    }

    parallel::stopCluster(cl)
  }

  res
}



.co <- function(x) {
  m <- Rfast::colmeans(x)
  tcrossprod(t(x) - m) / (dim(x)[1] - 1)
}




## Modified codes taken from the highmean package

.baisara <- function(x1, x2) {
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  tau <- n1 * n2 / (n1 + n2)
  n <- n1 + n2 - 2
  p <- dim(x1)[2]
  diff <- Rfast::colmeans(x1) - Rfast::colmeans(x2)
  XX <- sum(diff^2)
  pcov <- ( (n1 - 1) * .co(x1) + (n2 - 1) * .co(x2) ) / n
  trS <- sum( diag(pcov) )
  tr.cov2 <- n^2/ ( (n + 2) * (n - 1) ) * ( sum(pcov^2) - trS^2/n )
  stat <- (tau * XX - trS) / sqrt( 2 * (n + 1)/n * tr.cov2 )
  pvalue <- 1 - pnorm(stat)
  list( stat = stat, pvalue = pvalue )
}



.cailiuxia <- function(x1, x2, cov.equal = TRUE){
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  p <- dim(x1)[2]
  b <- 2 * log(p) - log( log(p) )
  diff <- Rfast::colmeans(x1) - Rfast::colmeans(x2)

  if ( cov.equal ) {
    tau <- n1 * n2 / (n1 + n2)
    sdiag <- ( (n1-1) * Rfast::colVars(x1) + (n2 - 1) * Rfast::colVars(x2) ) / ( n1 + n2 - 2 )
    sdiag[sdiag <= 10^(-10)] <- 10^(-10)
    test.stat <- tau * max( diff^2/sdiag )

  } else {
    sdiag1 <- Rfast::colVars(x1)
    sdiag1[sdiag1 <= 10^(-10)] <- 10^(-10)
    sdiag2 <- Rfast::colVars(x2)
    sdiag2[sdiag2 <= 10^(-10)] <- 10^(-10)
    test.stat <- max( diff^2 / (sdiag1/n1 + sdiag2/n2) )
  }

  stat <- test.stat - b
  pvalue <- 1 - exp( -exp(-0.5 * stat) / sqrt(pi) )
  list( stat = stat, pvalue = pvalue )
}



.chenlizho <- function(x1, x2, cov.equal = TRUE) {
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  p <- dim(x1)[2]
  af <- sqrt( 2 * log( log(p) ) )
  bf <- 2 * log( log(p) ) + 0.5 * log( log( log(p) ) ) - 0.5 * log( 4 * 3.1416 / (1 - 0.05)^2 )
  eta <- 0.05

  if ( cov.equal ) {
    diag.sig <- ( (n1-1) * Rfast::colVars(x1) + (n2 - 1) * Rfast::colVars(x2) ) / ( n1 + n2 - 2 )
    diag.sig[diag.sig <= 10^(-10)] <- 10^(-10)
    Torig <- ( Rfast::colmeans(x1) - Rfast::colmeans(x2) )^2 / ( (1/n1 + 1/n2) * diag.sig )

  } else {
    diag1 <- Rfast::colVars(x1)
    diag1[diag1 <= 10^(-10)] <- 10^(-10)
    diag2 <- Rfast::colVars(x2)
    diag2[diag2 <= 10^(-10)] <- 10^(-10)
    Torig <- ( Rfast::colmeans(x1) - Rfast::colmeans(x2) )^2 / ( diag1/n1 + diag2/n2 )
  }

  slevel <- Torig[ sign(Torig) >= 0.01  &  Torig <= 2 * (1 - eta) * log(p) ]
  sm <- matrix(slevel, length(slevel), p)
  Tm <- matrix( Torig - 1, length(slevel), p, byrow = TRUE)
  Tm[sign(Tm + 1 - sm) == -1] <- 0
  thr <- Rfast::rowsums(Tm)
  meanthr <- 2 * sqrt( slevel) * dnorm( sqrt(slevel) ) * p
  sdthr <- sqrt( p * ( 2 * ( ( sqrt(slevel) )^3 + sqrt(slevel) ) * dnorm( sqrt(slevel) ) +
                          4 - 4 * pnorm( sqrt( slevel) ) ) - meanthr^2/p )
  stat <- max( (thr - meanthr) / sdthr ) * af - bf
  pvalue <- 1 - exp( -exp( -stat ) )
  list( stat = stat, pvalue = pvalue )
}





.sridu <- function(x1, x2) {
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  tau <- n1 * n2 / (n1 + n2)
  n <- n1 + n2 - 2
  p <- dim(x1)[2]
  sam.cov <- ( (n1 - 1) * .co(x1) + (n2 - 1) * .co(x2) ) / n
  sam.corr <- cov2cor(sam.cov)
  trR2 <- sum(sam.corr^2)
  ca <- 1 + trR2/p^1.5
  diff <- Rfast::colmeans(x1) - Rfast::colmeans(x2)
  diag.sam <- diag(sam.cov)
  diag.sam[diag.sam <= 10^(-10)] <- 10^(-10)
  XX <- sum( diff^2 / diag.sam )
  stat <- ( tau * XX - n * p/ (n - 2) )/sqrt( 2 * (trR2 - p^2/n) * ca)
  pvalue <- 1 - pnorm(stat)
  list( stat = stat, pvalue = pvalue )
}



.chenqin <- function(x1, x2, cov.equal = TRUE) {
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  p <- dim(x1)[2]
  T1 <- tcrossprod(x1)
  T2 <- tcrossprod(x2)
  P1 <- ( sum(T1) - sum( diag(T1) ) ) / ( n1 * (n1 - 1) )
  P2 <- ( sum(T2) - sum( diag(T2) ) ) / ( n2 * (n2 - 1) )
  P3 <-  - 2 * sum( tcrossprod(x1, x2) ) / ( n1 * n2 )
  TP <- P1 + P2 + P3

  if ( cov.equal ) {
    n <- n1 + n2 - 2
    sam.cov <- ( (n1 - 1) * .co(x1) + (n2 - 1) * .co(x2) ) / n
    trS <- sum( diag(sam.cov) )
    tr.cov2 <- n^2/((n + 2)*(n - 1))*(sum(sam.cov^2) - trS^2/n)
    stat <- TP/sqrt( (2/(n1*(n1 - 1)) + 2/(n2*(n2 - 1)) + 4/(n1*n2))*tr.cov2 )

  } else {
    s1 <- Rfast::colsums(x1)
    s2 <- Rfast::colsums(x2)

    tr.cov1.sq <- tr.cov2.sq <- tr.cov1.cov2 <- 0
    for ( j in 1:n1 ) {
          tempmean <- Rfast::eachrow( -x1[-j, ], s1 - x1[j, ], oper = "+" ) / (n1 - 2)
          P1 <- Rfast::eachcol.apply( t( x1[-j, ] - tempmean), x1[j, ] ) 
          P2 <- Rfast::rowsums( Rfast::eachrow(- tempmean, x1[j, ], oper = "+") * x1[-j, ] )
          tr.cov1.sq <- tr.cov1.sq + sum(P1 * P2)
    }
    tr.cov1.sq <- tr.cov1.sq / ( n1 * (n1 - 1) )

    for ( j in 1:n2 ) {
          tempmean <- Rfast::eachrow( -x2[-j, ], s2 - x2[j, ], oper = "+" ) / (n2 - 2)
          P1 <- Rfast::eachcol.apply( t( x2[-j, ] - tempmean), x2[j, ] ) 
          P2 <- Rfast::rowsums( Rfast::eachrow(- tempmean, x2[j, ], oper = "+") * x2[-j, ] )
          tr.cov2.sq <- tr.cov2.sq + sum(P1 * P2)
    }
    tr.cov2.sq <- tr.cov2.sq / ( n2 * (n2 - 1) )

    tempmean1 <- Rfast::eachrow( -x1, s1, oper = "+" ) / (n1 - 1)
    tempmean2 <- Rfast::eachrow( -x2, s2, oper = "+" ) / (n2 - 1)
    for ( j in 1:n1 ) {
      for ( k in 1:n2 ) {
        P1 <- sum( x1[j, ] * (x2[k, ] - tempmean2[k, ]) )
        P2 <- sum( x2[k, ] * (x1[j, ] - tempmean1[j, ]) )
        tr.cov1.cov2 <- tr.cov1.cov2 + P1 * P2
      }
    }
    tr.cov1.cov2 <- tr.cov1.cov2 / (n1 * n2)   

    stat <- TP / sqrt( 2 / ( n1 * (n1 - 1) ) * tr.cov1.sq + 2 / ( n2 * (n2 - 1) ) * tr.cov2.sq + 
            4 / (n1 * n2) * tr.cov1.cov2 )
  }

  pvalue <- 1 - pnorm(stat)
  list( stat = stat, pvalue = pvalue )
}





.chenqin_old <- function(x1, x2, cov.equal = TRUE) {
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  p <- dim(x1)[2]
  T1 <- tcrossprod(x1)
  T2 <- tcrossprod(x2)
  P1 <- ( sum(T1) - sum( diag(T1) ) ) / ( n1 * (n1 - 1) )
  P2 <- ( sum(T2) - sum( diag(T2) ) ) / ( n2 * (n2 - 1) )
  P3 <-  - 2 * sum( tcrossprod(x1, x2) ) / ( n1 * n2 )
  TP <- P1 + P2 + P3

  if ( cov.equal ) {
    n <- n1 + n2 - 2
    sam.cov <- ( (n1 - 1) * .co(x1) + (n2 - 1) * .co(x2) ) / n
    trS <- sum( diag(sam.cov) )
    tr.cov2 <- n^2/((n + 2) * (n - 1)) * ( sum(sam.cov^2) - trS^2/n )
    stat <- TP/sqrt( ( 2/(n1*(n1 - 1) ) + 2/( n2*(n2 - 1) ) + 4/(n1*n2) ) * tr.cov2 )

  } else {
    s1 <- Rfast::colsums(x1)
    s2 <- Rfast::colsums(x2)

    tr.cov1.sq <- tr.cov2.sq <- tr.cov1.cov2 <- 0
    for ( j in 1:n1 ) {
      for ( k in 1:n1 ) {
        if ( j != k ) {
          tempmean <- ( s1 - x1[j, ] - x1[k, ] ) / (n1 - 2)
          P1 <- sum( x1[j, ] * (x1[k, ] - tempmean) )
          P2 <- sum( x1[k, ] * (x1[j, ] - tempmean) )
          tr.cov1.sq <- tr.cov1.sq + P1 * P2
        }
      }
    }
    tr.cov1.sq <- tr.cov1.sq / ( n1 * (n1 - 1) )

    for ( j in 1:n2 ) {
      for ( k in 1:n2 ) {
        if ( j != k ) {
          tempmean <- ( s2 - x2[j, ] - x2[k, ] ) / (n2 - 2)
          P1 <- sum( x2[j, ] * (x2[k, ] - tempmean) )
          P2 <- sum( x2[k, ] * (x2[j, ] - tempmean) )
          tr.cov2.sq <- tr.cov2.sq + P1 * P2
        }
      }
    }
    tr.cov2.sq <- tr.cov2.sq / ( n2 * (n2 - 1) )

    for ( j in 1:n1 ) {
      for ( k in 1:n2 ) {
        tempmean1 <- ( s1 - x1[j, ] ) / (n1 - 1)
        tempmean2 <- ( s2 - x2[k, ] ) / (n2 - 1)
        P1 <- sum( x1[j, ] * (x2[k, ] - tempmean2) )
        P2 <- sum( x2[k, ] * (x1[j, ] - tempmean1) )
        tr.cov1.cov2 <- tr.cov1.cov2 + P1 * P2
      }
    }
    tr.cov1.cov2 <- tr.cov1.cov2 / (n1 * n2)

    stat <- TP / sqrt( 2 / ( n1 * (n1 - 1) ) * tr.cov1.sq + 2 / ( n2 * (n2 - 1) ) * tr.cov2.sq + 
            4 / (n1 * n2) * tr.cov1.cov2 )
  }

  pvalue <- 1 - pnorm(stat)
  list( stat = stat, pvalue = pvalue )
}


## modified function taken from the highDmean pakage

.skk <- function(x1, x2) {
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  p <- dim(x1)[2]
  n <- n1 + n2 - 2
  x1.bar <- Rfast::colmeans(x1)
  x2.bar <- Rfast::colmeans(x2)
  S1 <- .co(x1)    ;   S2 <- .co(x2)
  s1 <- diag(S1)   ;   s2 <- diag(S2)
  D1 <- diag(s1)   ;   D2 <- diag(s2)
  D12 <- D1 / n1 + D2 / n2
  diagD <- diag(D12)
  a <- ( S1 / n1 + S2 / n2 ) / sqrt( diagD )
  R <- t(a) / sqrt( diagD )
  trR <- sum(R^2)
  c.p.n <- 1 + trR / p ^ (3 / 2)
  var.qn <- 2 * trR / p - 2 * sum( s1 / diagD ) ^ 2 / p / n1 /
    (n1 + 1) ^ 2 - 2 * sum( s2 / diagD ) ^ 2 / p / n2 / (n2 + 1) ^ 2
  denom <- sqrt(p * c.p.n * var.qn)
  TSvalue <- ( sum( (x1.bar - x2.bar)^2 / diagD ) - p ) / denom
  pvalue <- 2 * (1 - pnorm( abs(TSvalue) ) )
  list(stat = TSvalue, pvalue = pvalue)
}
