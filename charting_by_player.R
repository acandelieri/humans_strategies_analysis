rm(list=ls()); graphics.off(); cat("\014");

load("RESULTS.RData")

kernels <- c("gauss","exp","powexp","matern3_2","matern5_2")
is.min.f <- F # T in the minimization case, F otherwise
users <- unique( RESULTS$user ) # user ids
modes <- unique( RESULTS$mode ) # game modes
tfs <- unique( RESULTS$test.fun )  # test functions

# normalize <- T
graphics.off()
curr.mar <- par("mar")
par( mar=c(2.1,5.1,2.1,1.1) )
par( mfrow=c(3,4) )
res <- aggregate( RESULTS$distance,
                  by=list(user=RESULTS$user,mode=RESULTS$mode,tf=RESULTS$test.fun,iter=RESULTS$iter,uq=RESULTS$uncertainty.quantification),
                  min )
users <- unique(res$user)

for( user in users ){
  
  plot.new()
  legend( "left", legend=c( expression(psi[i]~"=("~zeta[i]~";"~sigma[i]~")"),
                            expression(psi[i]~"=("~zeta[i]~";"~h[i]~")"),
                            expression(psi[i]~"=("~zeta[i]~";"~z[i]~")") ),
          lwd=2, lty=c(1,2,3), pch=c(19,17,4), col=c("blue","green3","red"), cex=2, bty="n" )
  
  text( 0.5, 0.9, paste("userid:",user), cex=1.5 )
  
  for( tf in tfs ) {
    
    cat("> Charting USER:",user,"FUNCTION:",tf,"...\n")
    
    s1 <- res$x[which(res$user==user & res$mode=="maxPointNoValue" & res$tf==tf & res$uq=="sd" )]
    s2 <- res$x[which(res$user==user & res$mode=="maxPointNoValue" & res$tf==tf & res$uq=="entropy" )]
    s3 <- res$x[which(res$user==user & res$mode=="maxPointNoValue" & res$tf==tf & res$uq=="inv.dist" )]
    
    # if( normalize ) {
    #   if( max(s1)-min(s1) > 0 )
    #     s1 <- ( s1-min(s1) )/( max(s1)-min(s1) )
    #   else
    #     s1 <- numeric(length(s1))
    #   if( max(s2)-min(s2) > 0 )
    #     s2 <- ( s2-min(s2) )/( max(s2)-min(s2) )
    #   else
    #     s2 <- numeric(length(s2))
    #   if( max(s3)-min(s3) > 0 )
    #     s3 <- ( s3-min(s3) )/( max(s3)-min(s3) )
    #   else
    #     s3 <- numeric(length(s3))
    # }
    
    plot( 1:length(s1)+2, s1, type="l", col="blue", ylab=expression(dist(psi[i],P)), xlab=NA,
          lwd=2, ylim=c(0,1.3*max(s1,s2,s3)), cex.axis=2, cex.lab=2, main=tf, cex.main=2 )
    lines( 1:length(s1)+2, s2, col="green3", ylab="H(x)", xlab="iter", lwd=2, lty=2)
    lines( 1:length(s1)+2, s3, col="red", ylab="sigma", xlab="iter", lwd=2, lty=3)
    
    points( 1:length(s1)+2, s1, pch=19, col="blue", lwd=2, cex=1.5 ) 
    points( 1:length(s2)+2, s2, pch=17, col="green3", lwd=2, cex=1.5 ) 
    points( 1:length(s3)+2, s3, pch=4, col="red", lwd=2, cex=1.5 ) 
    
    # legend( "topleft", legend=c( expression(zeta-sigma), expression(zeta-H), expression(zeta-z) ),
    #         lwd=2, lty=c(1,2,3), pch=c(19,17,4), col=c("blue","green3","red"), cex=2, bty="n" )
  }
  
  
  invisible(readline("Push a button to plot the next user..."))
  graphics.off()
  par( mar=c(2.1,5.1,2.1,1.1) )
  par( mfrow=c(3,4) )
}
par( mar=curr.mar )
