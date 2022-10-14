rm(list=ls()); graphics.off(); cat("\014")


files = list.files("results")


for( f in files ) {
  
  f.name = unlist(strsplit(f,"_",T))[1]
  
  res = readRDS(paste0("results/",f))
  
  # organizing BO results 
  BO = res$BO_res
  BO.y = NULL
  for( seed in 1:max(res$BO_res$seed) ) {
    ys = BO$y[which(BO$seed==seed)]
    y = cummax( c(max(ys[1:3]), ys[-c(1:3)]) )
    BO.y = rbind(BO.y,y)
  }
  BO.m = apply(BO.y,2,mean)
  if( nrow(BO.y)==1 ) {
    BO.s = numeric(length(BO.m))
  } else {
    BO.s = apply(BO.y,2,sd)
  }
  BO.up = BO.m + BO.s
  BO.dw = BO.m - BO.s
  
  
  # organizing synthetic human results
  SH = res$SH_res
  SH.y = NULL
  for( seed in 1:max(res$SH_res$seed) ) {
    ys = SH$y[which(SH$seed==seed)]
    y = cummax( c(max(ys[1:3]), ys[-c(1:3)]) )
    SH.y = rbind(SH.y,y)
  }
  SH.m = apply(SH.y,2,mean)
  if( nrow(SH.y)==1 ) {
    SH.s = numeric(length(SH.m))
  } else {
    SH.s = apply(SH.y,2,sd)
  }
  SH.up = SH.m + SH.s
  SH.dw = SH.m - SH.s
  
  
  # visualization
  curr.mar = par("mar")
  par(mar=c(4.1,4.6,2.1,1.1) )
  plot( 2+(1:max(length(BO.m),length(SH.m))), rep(NA,max(length(BO.m),length(SH.m))),
        type="l", ylim=c(min(BO.dw,SH.dw),max(BO.up,SH.up)),
        xlab="sequential decisions", ylab="max score collected", 
        main=f.name, cex.lab=1.5, cex.axis=1.5, cex.main=1.5 )
  polygon( c(2+(1:length(BO.m)),rev(2+(1:length(BO.m)))),
           c(BO.m+BO.s,rev(BO.m-BO.s)),
           col=adjustcolor("red",alpha.f=0.25), border="firebrick"  )
  polygon( c(2+(1:length(SH.m)),rev(2+(1:length(SH.m)))),
           c(SH.m+SH.s,rev(SH.m-SH.s)),
           col=adjustcolor("blue",alpha.f=0.25), border="blue"  )
  lines( 2+(1:length(BO.m)), BO.m, lwd=4, col="firebrick" )
  lines( 2+(1:length(SH.m)), SH.m, lwd=4, col="blue" )
  
  legend.pos = ifelse(f.name %in% c("branin","stybtang"), "right","bottomright" )
  legend( legend.pos, legend=c("Bayesian Optimization","Synthetic Human"),
          lwd=3, col=c("firebrick","blue"), cex=1.5 )
  par(mar=curr.mar)

}