rm(list=ls()); graphics.off(); cat("\014")

library(DiceKriging)

ucb <- function(x,gp,beta=1) {
  x = t(x)
  pred = predict( gp, data.frame(x), "UK", checkNames=F )
  ucb = pred$mean + sqrt(beta)*pred$sd
}

ackley <- function(xx, a=20, b=0.2, c=2*pi) {
  ##########################################################################
  #
  # ACKLEY FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2, ..., xd)
  # a = constant (optional), with default value 20
  # b = constant (optional), with default value 0.2
  # c = constant (optional), with default value 2*pi
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))
  
  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)
  
  y <- term1 + term2 + a + exp(1)
  return(y)
}

beale <- function(xx) {
  ##########################################################################
  #
  # BEALE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- (1.5 - x1 + x1*x2)^2
  term2 <- (2.25 - x1 + x1*x2^2)^2
  term3 <- (2.625 - x1 + x1*x2^3)^2
  
  y <- term1 + term2 + term3
  return(y)
}

branin <- function(xx, a=1, b=5.1/(4*pi^2), c=5/pi, r=6, s=10, t=1/(8*pi)) {
  ##########################################################################
  #
  # BRANIN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2)
  # a = constant (optional), with default value 1
  # b = constant (optional), with default value 5.1/(4*pi^2)
  # c = constant (optional), with default value 5/pi
  # r = constant (optional), with default value 6
  # s = constant (optional), with default value 10
  # t = constant (optional), with default value 1/(8*pi)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- a * (x2 - b*x1^2 + c*x1 - r)^2
  term2 <- s*(1-t)*cos(x1)
  
  y <- term1 + term2 + s
  return(y)
}

bukin6 <- function(xx) {
  ##########################################################################
  #
  # BUKIN FUNCTION N. 6
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- 100 * sqrt(abs(x2 - 0.01*x1^2))
  term2 <- 0.01 * abs(x1+10)
  
  y <- term1 + term2
  return(y)
}

goldprsc <- function(xx) {
  ##########################################################################
  #
  # GOLDSTEIN-PRICE FUNCTION, SCALED
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1bar <- 4*xx[1] - 2
  x2bar <- 4*xx[2] - 2
  
  fact1a <- (x1bar + x2bar + 1)^2
  fact1b <- 19 - 14*x1bar + 3*x1bar^2 - 14*x2bar + 6*x1bar*x2bar + 3*x2bar^2
  fact1 <- 1 + fact1a*fact1b
  
  fact2a <- (2*x1bar - 3*x2bar)^2
  fact2b <- 18 - 32*x1bar + 12*x1bar^2 + 48*x2bar - 36*x1bar*x2bar + 27*x2bar^2
  fact2 <- 30 + fact2a*fact2b
  
  prod <- fact1*fact2
  
  y <- (log(prod) - 8.693) / 2.427
  return(y)
}

griewank <- function(xx) {
  ##########################################################################
  #
  # GRIEWANK FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  ii <- c(1:length(xx))
  sum <- sum(xx^2/4000)
  prod <- prod(cos(xx/sqrt(ii)))
  
  y <- sum - prod + 1
  return(y)
}

levy <- function(xx) {
  ##########################################################################
  #
  # LEVY FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  w <- 1 + (xx - 1)/4
  
  term1 <- (sin(pi*w[1]))^2 
  term3 <- (w[d]-1)^2 * (1+1*(sin(2*pi*w[d]))^2)
  
  wi <- w[1:(d-1)]
  sum <- sum((wi-1)^2 * (1+10*(sin(pi*wi+1))^2))
  
  y <- term1 + sum + term3
  return(y)
}

rastr <- function(xx) {
  ##########################################################################
  #
  # RASTRIGIN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx^2 - 10*cos(2*pi*xx))
  
  y <- 10*d + sum
  return(y)
}

schwef <- function(xx) {
  ##########################################################################
  #
  # SCHWEFEL FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx*sin(sqrt(abs(xx))))
  
  y <- 418.9829*d - sum
  return(y)
}

stybtang <- function(xx) {
  ##########################################################################
  #
  # STYBLINSKI-TANG FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  sum <- sum(xx^4 - 16*xx^2 + 5*xx)
  
  y <- sum/2
  return(y)
}




# possible values:
# ackley, beale, branin, bukin6,
# goldprsc, griewank, levy, rastr,
# schwef, stybtang
test.f ="stybtang"


BO_res = SH_res = NULL


seeds = 1:30

BO_results = data.frame()


cat("*****************************************************************\n")
cat("* Run experiments with the synthetic human learner/optimizer    *\n")
cat("*****************************************************************\n")


cat("[Test problem]:",test.f,"\n")

for( seed in seeds ) {

  cat("\nSeed:",seed,"\n")
  set.seed(seed)

  
  X = lhs::maximinLHS(3,2)
  
  # function selection
  if( test.f=="ackley") {
    lower=rep(-32.768,2)
    upper=rep(32.768,2)
    f=ackley
  } else {
    if( test.f=="beale" ) {
      lower=rep(-4.5,2)
      upper=rep(4.5,2)
      f=beale
    } else {
      if( test.f=="branin" ) {
        lower=c(-5,0)
        upper=c(10,15)
        f=branin
      } else {
        if( test.f=="bukin6" ) {
          lower=c(-15,-3)
          upper=c(-5,3)
          f=bukin6
        } else {
          if( test.f=="goldprsc" ) {
            lower=rep(-2,2)
            upper=rep(2,2)
            f=goldprsc
          } else {
            if( test.f=="griewank" ) {
              lower=rep(-600,2)
              upper=rep(600,2)
              f=griewank
            } else {
              if( test.f=="levy" ) {
                lower=rep(-10,2)
                upper=rep(10,2)
                f=levy
              } else {
                if( test.f=="rastr" ) {
                  lower=rep(-5.12,2)
                  upper=rep(5.12,2)
                  f=rastr
                } else {
                  if( test.f=="schwef" ) {
                    lower=rep(-500,2)
                    upper=rep(500,2)
                    f=schwef
                  } else {
                    if( test.f=="stybtang" ) {
                      lower=rep(-5,2)
                      upper=rep(5,2)
                      f=stybtang
                    } 
                  }
                }
              }
            } 
          }
        }
      }
    }
  }
  
  
  X[,1] = X[,1] * (upper[1]-lower[1]) + lower[1]
  X[,2] = X[,2] * (upper[2]-lower[2]) + lower[2]
  
  y = -apply(X,1,f)
  
  X0=X; y0=y
  elapsed = numeric(length(y))
  
  cat("> Running Bayesian Optimization...\n")
  while( nrow(X)<20 ) {
    
    startAt = Sys.time()
    gp = km( design=data.frame(X), response=y, covtype="gauss", nugget.estim=T, control=list(trace=0) )
    
    x0 = c(runif(1,lower[1],upper[1]),runif(1,lower[2],upper[2]))
    res = optim( par=x0, fn=ucb, gr=NULL, method="L-BFGS-B", lower=lower, upper=upper, control=list(fnscale=-1), gp )
    elapsed = c(elapsed,difftime(Sys.time(),startAt,units="secs"))
    
    X = rbind(X,res$par)
    y = c(y,f(res$par))
    
  }
  
  
  cat("> Running Synthetic human learner/optimizer...\n")
  X_ = X0; y_=y0
  elapsed_ = numeric(length(y_))
  paretian.decisions = rep(NA,length(y_))
  
  while( nrow(X_)<20 ) {
    acr = sum(y_)/length(y_)
    paretian_decision = ( acr <= -490.273496 && acr > -1186.66096 ) || 
                        ( acr <= -1186.66096 && length(y_)<=6 ) ||
                        ( acr > -490.273496 && length(y_)<=3 ) ||
                        ( acr <= -6.888681 && acr > -490.273496 && length(y_)==4 ) ||
                        ( acr <= -18.730286 && acr > -22.268273 && length(y_)>6 ) ||
                        ( acr <= -0.885889 && acr > -1.340774 && length(y_)>6 )
  
    x0 = c(runif(1,-15,-5),runif(1,-3,3))
    
    if( paretian_decision ) {
      gp = km( design=data.frame(X_), response=y_, covtype="gauss", nugget.estim=T, control=list(trace=0) )
      res = optim( par=x0, fn=ucb, gr=NULL, method="L-BFGS-B", lower=lower, upper=upper, control=list(fnscale=-1), gp )
      x.next = res$par
    } else {
      x.next = x0
    }
    
    paretian.decisions = c(paretian.decisions,paretian_decision)
    
    elapsed_ = c(elapsed_,difftime(Sys.time(),startAt,units="secs"))
    
    X_ = rbind(X_,x.next)  
    y_ = c(y_,f(x.next))
  
  }
  
  BO_res = rbind( BO_res, data.frame( seed=rep(seed,20),
                                      iter=1:20,
                                      X=X,
                                      y=y,
                                      elapsed=elapsed) )
  
  SH_res = rbind( SH_res, data.frame( seed=rep(seed,20),
                                      iter=1:20,
                                      X=X_,
                                      y=y_,
                                      elapsed=elapsed_,
                                      is.paretian=paretian.decisions) )

}

cat("Saving results...\n")
if( !dir.exists("results") )
  dir.create("results")
saveRDS( object=list(BO_res=BO_res, SH_res=SH_res), file=paste0("results/",test.f,"_BOvsSH.RDS" ),  )