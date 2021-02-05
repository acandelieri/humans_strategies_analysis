rm(list=ls()); graphics.off(); cat("\014");

# loading core functions
source("core.functions.R")

# kernels to consider for the probabilistic surrogate model
kernels <- c("gauss","exp","powexp","matern3_2","matern5_2")
is.min.f <- F # T in the minimization case, F otherwise

# loading games data
games <- read.table("Dataset_Amazon_Turk_complete_game.csv",header=T,sep=";",dec="," )


users <- unique( games$username ) # user ids
modes <- unique( games$gameMode ) # game modes
tfs <- unique( games$function. )  # test functions


RESULTS <- NULL
for( user in users ) {
  cat("> Analysing user '",user,"'...\n",sep="")
  for( mode in modes ) {
    cat("  - Game mode: '",mode,"'\n",sep="")
    for( tf in tfs ) {
      cat("  - Test function: '",tf,"'\n",sep="")
    
      # grid of points on which generating the Pareto frontier
      X.grid <- grid.for.function( tf )
      
      # selecting games data for the current (user, game mode, test function)
      ds <- games[which(games$username==user & games$gameMode==mode & games$function.==tf ),]
      ds <- ds[order(ds$iteration),] # sorting by iteration, just to be sure!
  
      
      # sequential analysis for each player's decision
      
      for( i in 3:(nrow(ds)-1) ) {
        
        cat("> Iteration ",i,"...\n", sep="")
  
        for( j in 1:length(kernels) ) {
          cat("  - kernel: '",kernels[j],"'...\n",sep="")
          cat("    * (fitting GP...)\n")
          
          options(show.error.messages = F)
          gp <- try( km( design=round(ds[1:i,c("x1","x2")],4), response=round(ds$y[1:i],4), covtype=kernels[j], nugget.estim=T, control=list(trace=0) ) )
          options(show.error.messages = T)
          
          if( class(gp)=="try-error" ) {
            warning("*** Skipped! *** User:",user,"Game-mode:",mode,"Test:",tf,"iter:",i,"kernel:",kernels[j])
          } else {
            cat("    * (computing objectives on a",nrow(X.grid),"possible decisions...)\n")
            objs <- all.objectives( gp=gp, X.grid=X.grid, f.min=is.min.f )
            # Pareto.sigma <- pareto.frontier( objs$ei, objs$u.sigma ) 
            # Pareto.H <- pareto.frontier( objs$ei, objs$u.H ) 
            # Pareto.z <- pareto.frontier( objs$ei, objs$u.z )
            # cat("    * (analysing next decision...)\n")
            x.next <- ds[i+1,c("x1","x2")]
            all.psi_ <- all.objectives( gp=gp, X.grid=as.matrix(x.next), f.min=is.min.f )
            # cat("    * (distance from 'sigma' Pareto...)\n")
            Pareto.sigma.dist <- pareto.dist( psi_=c(all.psi_$ei,all.psi_$u.sigma), obj1=objs$ei, obj2=objs$u.sigma )
            # cat("    * (distance from 'entropy' Pareto...)\n")
            Pareto.H.dist <- pareto.dist( psi_=c(all.psi_$ei,all.psi_$u.H), obj1=objs$ei, obj2=objs$u.H )
            # cat("    * (distance from 'inverse distance' Pareto...)\n")
            Pareto.z.dist <- pareto.dist( psi_=c(all.psi_$ei,all.psi_$u.z), obj1=objs$ei, obj2=objs$u.z )
            
            # cat("    * (appending results...)\n")
            RESULTS <- rbind( RESULTS, data.frame( user=user,
                                                   mode=mode,
                                                   test.fun=tf,
                                                   iter=i,
                                                   kernel=kernels[j],
                                                   uncertainty.quantification="sd",
                                                   distance=Pareto.sigma.dist,
                                                   stringsAsFactors=F) )
            
            RESULTS <- rbind( RESULTS, data.frame( user=user,
                                                   mode=mode,
                                                   test.fun=tf,
                                                   iter=i,
                                                   kernel=kernels[j],
                                                   uncertainty.quantification="entropy",
                                                   distance=Pareto.H.dist,
                                                   stringsAsFactors=F) )
            
            RESULTS <- rbind( RESULTS, data.frame( user=user,
                                                   mode=mode,
                                                   test.fun=tf,
                                                   iter=i,
                                                   kernel=kernels[j],
                                                   uncertainty.quantification="inv.dist",
                                                   distance=Pareto.z.dist,
                                                   stringsAsFactors=F) )
          }
        }
      }
    }
  }
  cat("\014")
}


cat("> Saving results from the analysis...\n")
print( warnings() )
save( file="RESULTS.RData", list="RESULTS" )
