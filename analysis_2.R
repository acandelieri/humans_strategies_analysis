rm(list=ls()); graphics.off(); cat("\014");



paretian.subsequence <- function( v ) {
  starts <- numeric(0)
  lengths <- numeric(0)
  i0 <- 1
  i1 <- 1
  print( v )
  while( i0 <= length(v) ) {
    while( i0<=length(v) & v[i0]>10^-8 )
      i0 <- i0+1
    i1 <- i0+1
    while( i1<=length(v) & v[i1]<=10^-8 )
      i1 <- i1+1
    if( i0<=length(v) ) {
      starts <- c( starts, i0 )
      lengths <- c( lengths, i1-i0 )
    }
    i0 <- i1+1
  }
  return( list(starts=starts,lengths=lengths) )
}


load("RESULTS.RData")

kernels <- c("gauss","exp","powexp","matern3_2","matern5_2")
is.min.f <- F # T in the minimization case, F otherwise
users <- unique( RESULTS$user ) # user ids
modes <- unique( RESULTS$mode ) # game modes
tfs <- unique( RESULTS$test.fun )  # test functions

# normalize <- T
graphics.off()
res <- aggregate( RESULTS$distance,
                  by=list(user=RESULTS$user,mode=RESULTS$mode,tf=RESULTS$test.fun,iter=RESULTS$iter,uq=RESULTS$uncertainty.quantification),
                  min )
users <- unique(res$user)



collected <- data.frame( user=character(),
                         tf=character(),
                         uq=character(),
                         i0=numeric(),
                         len=numeric(),
                         perc.len=numeric(),
                         stringsAsFactors=F)
for( user in users ) {
  for( tf in tfs ) {
    
    ds <- res[ which(res$user==user & res$tf==tf & res$mode=="maxPointNoValue"), ]
    ds <- ds[order(ds$iter),]
    
    v.s <- ds$x[which(ds$uq=="sd")]
    sub.seq.s <- paretian.subsequence(v.s)
    if( length(sub.seq.s$starts)>0 )
      for( i in 1:length(sub.seq.s$starts) )
        collected <- rbind( collected, data.frame( user=user,
                                                   tf=tf,
                                                   uq="sd",
                                                   i0=sub.seq.s$starts[i],
                                                   len=sub.seq.s$lengths[i],
                                                   perc.len=round(100*sub.seq.s$lengths[i]/length(v.s),2),
                                                   stringsAsFactors=F))
        
    v.h <- ds$x[which(ds$uq=="entropy")]
    sub.seq.h <- paretian.subsequence(v.h)
    if( length(sub.seq.h$starts)>0 )
      for( i in 1:length(sub.seq.h$starts) )
        collected <- rbind( collected, data.frame( user=user,
                                                   tf=tf,
                                                   uq="h",
                                                   i0=sub.seq.h$starts[i],
                                                   len=sub.seq.h$lengths[i],
                                                   perc.len=round(100*sub.seq.h$lengths[i]/length(v.s),2),
                                                   stringsAsFactors=F))
    
    v.z <- ds$x[which(ds$uq=="inv.dist")]
    sub.seq.z <- paretian.subsequence(v.z)
    if( length(sub.seq.z$starts)>0 )
      for( i in 1:length(sub.seq.z$starts) )
        collected <- rbind( collected, data.frame( user=user,
                                                   tf=tf,
                                                   uq="inv.dist",
                                                   i0=sub.seq.z$starts[i],
                                                   len=sub.seq.z$lengths[i],
                                                   perc.len=round(100*sub.seq.z$lengths[i]/length(v.s),2),
                                                   stringsAsFactors=F))
    
  }
}


analysis_2 <- aggregate( collected$len, by=list(collected$user,collected$tf,collected$uq), "max" )
colnames( analysis_2) <- c( "user", "tf", "uq", "len" )  

curr.mar <- par("mar")
par( mfrow=c(3,4) )
par( mar=c(4.1,4.6,2.1,1.1) )
for( tf in tfs ) {
  num.s <- analysis_2[which(analysis_2$tf==tf & analysis_2$uq=="sd"),]$len
  num.h <- analysis_2[which(analysis_2$tf==tf & analysis_2$uq=="h"),]$len
  num.z <- analysis_2[which(analysis_2$tf==tf & analysis_2$uq=="inv.dist"),]$len
  
  hist.s <- hist( num.s, breaks=seq(0,17,by=1), plot=F )
  hist.h <- hist( num.h, breaks=seq(0,17,by=1), plot=F )
  hist.z <- hist( num.z, breaks=seq(0,17,by=1), plot=F )
  
  barplot( rbind(hist.s$counts,hist.h$counts,hist.z$counts),
           main=tf, border=NA, beside=F, axis.lty=1,
           ylab="Frequency", cex.axis=2, cex.lab=2, cex.main=2, cex.names=1.3, 
           xlab="consecutive Pareto decisions", names.arg=1:17, 
           col=c( adjustcolor("blue",alpha.f=0.8),
                  adjustcolor("red",alpha.f=0.8),
                  adjustcolor("green3",alpha.f=0.8)) ) 
  
}
plot.new()
legend("left",
       legend=c(expression(u(x)==sigma(x)),expression(u(x)==h(x)),expression(u(x)==z(x))),
       fill=adjustcolor( c("blue","red","green3"), alpha.f=0.8), bty="n", cex=2.0, border=NA )

par( mar=curr.mar )
par( mfrow=c(1,1) )


curr.mar <- par("mar")
par( mfrow=c(3,5) )
par( mar=c(4.1,4.6,2.1,1.1) )
for( user in users ) {
  num.s <- analysis_2[which(analysis_2$user==user & analysis_2$uq=="sd"),]$len
  num.h <- analysis_2[which(analysis_2$user==user & analysis_2$uq=="h"),]$len
  num.z <- analysis_2[which(analysis_2$user==user & analysis_2$uq=="inv.dist"),]$len
  
  hist.s <- hist( num.s, breaks=seq(0,17,by=1), plot=F )
  hist.h <- hist( num.h, breaks=seq(0,17,by=1), plot=F )
  hist.z <- hist( num.z, breaks=seq(0,17,by=1), plot=F )
  
  barplot( rbind(hist.s$counts,hist.h$counts,hist.z$counts),
           main=user, border=NA, beside=F, axis.lty=1,
           ylab="Frequency", cex.axis=2, cex.lab=2, cex.main=2, cex.names=1.3, 
           xlab="consecutive Pareto decisions", names.arg=1:17, 
           col=c( adjustcolor("blue",alpha.f=0.8),
                  adjustcolor("red",alpha.f=0.8),
                  adjustcolor("green3",alpha.f=0.8)) ) 
  
}
plot.new()
legend("left",
       legend=c(expression(u(x)==sigma(x)),expression(u(x)==h(x)),expression(u(x)==z(x))),
       fill=adjustcolor( c("blue","red","green3"), alpha.f=0.8), bty="n", cex=2.0, border=NA )

par( mar=curr.mar )
par( mfrow=c(1,1) )