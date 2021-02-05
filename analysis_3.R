rm(list=ls()); graphics.off(); cat("\014")

load("RESULTS.RData")

# loading games data
games <- read.table("Dataset_Amazon_Turk_complete_game.csv",header=T,sep=";",dec="," )


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



graphics.off()
curr.mar <- par("mar")
par( mar=c(2.1,5.1,2.1,1.1) )
par( mfrow=c(3,4) )


for( user in users ) {
  
  plot.new()
  legend( "left", legend=c("Pareto decision","Not-Pareto decision"),
          fill=c("skyblue","pink"), cex=2, bty="n" )
  
  text( 0.5, 0.9, paste("userid:",user), cex=1.5 )
  
  for( tf in tfs ) {
    
    rationality <- res[ which(res$user==user & res$tf==tf & res$mode=="maxPointNoValue"), ]
    #s.rationality <- rationality[which(rationality$uq=="sd",)]
    #s.rationality <- s.rationality[order(s.rationality$iter),]
    #h.rationality <- rationality[which(rationality$uq=="entropy"),]
    #h.rationality <- h.rationality[order(h.rationality$iter),]
    z.rationality <- rationality[which(rationality$uq=="inv.dist"),]
    z.rationality <- z.rationality[order(z.rationality$iter),]
    
    game <- games[ which(games$username==user & games$function.==tf & games$gameMode=="maxPointNoValue"),]
    game <- game[order(game$iteration),]

    ix <- which( z.rationality$x<=10^-8 )
    if(length(ix)==0) {
      pareto.iters <- NA
      not.pareto.iters <- z.rationality$iter
    } else {
      if(length(ix)==nrow(z.rationality)) {
        pareto.iters <- z.rationality$iter
        not.pareto.iters <- NA
      } else {
        pareto.iters <- z.rationality$iter[ix]
        not.pareto.iters <- z.rationality$iter[-ix]
      }
    }
    
    # avg cum reward over iterations
    cum.reward <- cumsum(game$y)
    boxplot(cum.reward[pareto.iters], cum.reward[not.pareto.iters], col=c("skyblue","pink"),
            main=tf, ylab="ACR", names=NA, cex.lab=2.0, cex.axis=2.0, cex.main=2.0 )
    

  }
  
  #invisible(readline("Push a button to plot the next user..."))
  graphics.off()
  par( mar=c(2.1,5.1,2.1,1.1) )
  par( mfrow=c(3,4) )
}
par( mar=curr.mar )
par( mfrow=c(1,1) )






curr.mar <- par("mar")
par( mar=c(2.1,5.1,2.1,1.1) )
par( mfrow=c(3,4) )

plot.new()
legend( "left", legend=c("Pareto","Not-Pareto"), title="Type of decisions",
        fill=c("skyblue","pink"), cex=2, bty="n" )


for( tf in tfs ) {

  aggr.pareto <- numeric()
  aggr.no.pareto <- numeric()
  
  for( user in users ) {  
    
    rationality <- res[ which(res$user==user & res$tf==tf & res$mode=="maxPointNoValue"), ]
    #s.rationality <- rationality[which(rationality$uq=="sd",)]
    #s.rationality <- s.rationality[order(s.rationality$iter),]
    #h.rationality <- rationality[which(rationality$uq=="entropy"),]
    #h.rationality <- h.rationality[order(h.rationality$iter),]
    z.rationality <- rationality[which(rationality$uq=="inv.dist"),]
    z.rationality <- z.rationality[order(z.rationality$iter),]
    
    game <- games[ which(games$username==user & games$function.==tf & games$gameMode=="maxPointNoValue"),]
    game <- game[order(game$iteration),]
    
    ix <- which( z.rationality$x<=10^-8 )
    if(length(ix)==0) {
      pareto.iters <- NA
      not.pareto.iters <- z.rationality$iter
    } else {
      if(length(ix)==nrow(z.rationality)) {
        pareto.iters <- z.rationality$iter
        not.pareto.iters <- NA
      } else {
        pareto.iters <- z.rationality$iter[ix]
        not.pareto.iters <- z.rationality$iter[-ix]
      }
    }
    
    # avg cum reward over iterations
    cum.reward <- cumsum(game$y)
    
    aggr.pareto <- c( aggr.pareto, cum.reward[pareto.iters] )
    aggr.no.pareto <- c( aggr.no.pareto, cum.reward[not.pareto.iters] )
    
  }
  
  boxplot( aggr.pareto, aggr.no.pareto, col=c("skyblue","pink"),
           main=tf, ylab="ACR", names=NA, cex.lab=2.0, cex.axis=2.0, cex.main=2.0 )
  
  
  w.t <- wilcox.test( aggr.pareto, aggr.no.pareto, paired=F )
  #print(w.t)
  cat(tf,"\t",mean(aggr.pareto,na.rm=T),"\t(",sd(aggr.pareto,na.rm=T),")\t",
      mean(aggr.no.pareto,na.rm=T),"\t(",sd(aggr.no.pareto,na.rm=T),")\t",w.t$p.value,"\n")
  
}

par( mar=curr.mar )
par( mfrow=c(1,1) )
