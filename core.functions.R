library(DiceKriging)


# function to compute the three options for bi-objective. Options are different in terms
# of the uncertainty quantification
# - Improvement vs sigma
# - Improvement vs entropy
# - Improvement vs inverse-distance
all.objectives <- function( gp, X.grid, f.min=T ) {
  
  pred <- predict( gp, data.frame(X.grid), "UK", checkNames=F )
  
  # ei 
  if( f.min ) {
    y.best <- min( gp@y )
    ei <- y.best - pred$sd
  } else {
    y.best <- max( gp@y )
    ei <- pred$sd - y.best
  }
  
  # sigma-uncertainty 
  u.sigma <- pred$sd
  
  # entropy-uncertainty
  #u.H <- 0.5 * log(2*pi*exp(1)*pred$sd^2)
  u.H <- numeric(nrow(X.grid) )
  for( j in 1:length(u.H) ) {
    K_ <- covMatrix(object=gp@covariance,X=as.matrix(c(gp@X,X.grid[j,])),noise.var=gp@noise.var)  
    u.H[j] <- 0.5 * ( log( det(K_$C) ) + ncol(gp@X) * log(2*pi*exp(1)) )
  }
  u.H[ which( is.na(u.H) | is.nan(u.H) | is.infinite(u.H) ) ] <- min(u.H,na.rm=T)
  if( is.infinite( min(u.H) ) ) {
    u.H <- numeric(nrow(X.grid) )
  }
    
  
  # inverse.distance-uncertainty
  u.z <- numeric(nrow(X.grid))
  for( j in 1:nrow(X.grid) ) {
    
    tmp <- matrix(0,ncol=2,nrow=nrow(gp@X))
    for( h in 1:nrow(tmp) )
      tmp[h,] <- X.grid[j,]
      
    dists <- sqrt( apply( (gp@X - tmp)^2, 1, sum ) )
    if( min(dists) > 10^-8 ) {
      u.z[j] <- (2/pi)*atan( 1 / sum( exp(-(dists^2)) / (dists^2) ) )
    }
  }
  
  return( list( ei=ei, u.sigma=u.sigma, u.H=u.H, u.z=u.z) )
}

# function to identify points on the Pareto efficient frontier
pareto.frontier <- function( obj1, obj2 ) {
  
  stopifnot( length(obj1)==length(obj2) )
  
  ixs <- NULL
  for( i in 1:length(obj1) ) {
    if( length( which( obj1>obj1[i] & obj2>obj2[i] ) ) == 0 )
      ixs <- c( ixs, i )
  }
  
  return( cbind(obj1[ixs],obj2[ixs]) )
}

# function to compute the distabce of a point from a Pareto frontier
pareto.dist <- function( psi_, obj1, obj2 ) {
  stopifnot( is.numeric(psi_) )
  obj1 <- c( psi_[1], obj1 )
  obj2 <- c( psi_[2], obj2 )
  front <- pareto.frontier( obj1, obj2 )
  tmp <- matrix(0,nrow=nrow(front),ncol=2)
  for( i in 1:nrow(tmp) )
    tmp[i,] <- psi_
  d <- min( sqrt( apply( (front - tmp)^2, 1, sum ) ) )
  return( d )
}

# pre-fixed grid of points where each test function used in the experiments
grid.for.function <- function( function.name ) {
  if( function.name=="ackley" ) {
    Xs1 <- seq(-32.768,32.768,length.out=30)
    Xs2 <- seq(-32.768,32.768,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )
  }
  if( function.name=="beale" ) {
    Xs1 <- seq(-4.5,4.5,length.out=30)
    Xs2 <- seq(-4.5,4.5,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
  if( function.name=="branin" ) {
    Xs1 <- seq(-5,10,length.out=30)
    Xs2 <- seq(-0,15,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
  if( function.name=="bukin6" ) {
    Xs1 <- seq(-15.5,-5,length.out=30)
    Xs2 <- seq(-3,3,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
  if( function.name=="goldpr" ) {
    Xs1 <- seq(-2,2,length.out=30)
    Xs2 <- seq(-2,2,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
  if( function.name=="griewank" ) {
    Xs1 <- seq(-600,600,length.out=30)
    Xs2 <- seq(-600,600,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
  if( function.name=="levy" ) {
    Xs1 <- seq(-10,10,length.out=30)
    Xs2 <- seq(-10,10,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
  if( function.name=="rastr" ) {
    Xs1 <- seq(-5.12,5.12,length.out=30)
    Xs2 <- seq(-5.12,5.12,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
  if( function.name=="schwef" ) {
    Xs1 <- seq(-500,500,length.out=30)
    Xs2 <- seq(-500,500,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
  if( function.name=="stybtang" ) {
    Xs1 <- seq(-5,5,length.out=30)
    Xs2 <- seq(-5,5,length.out=30)
    XX <- cbind( sort(rep(Xs1,length(Xs2))), rep(Xs2,length(Xs1)) )
    return( XX )  
  }
}

