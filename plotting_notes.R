
# PLOTTING  RESULTS #

BSB_base <- readRDS("MSE_obj/MSE_BlackSeaBass_base.rds")
BSB_age0M_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_age0M_hi.rds")
BSB_age0M_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_age0M_lo.rds")
BSB_epiM <- readRDS("MSE_obj/MSE_BlackSeaBass_epiM.rds")
BSB_recdev <- readRDS("MSE_obj/MSE_BlackSeaBass_recdev.rds")
BSB_recns <- readRDS("MSE_obj/MSE_BlackSeaBass_recns.rds")
BSB_refbias_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_refbias_hi.rds")
BSB_refbias_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_refbias_lo.rds")
BSB_uobs_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_uobs_hi.rds")
BSB_uobs_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_uobs_lo.rds")
# BSB_ <- readRDS("MSE_obj/MSE_BlackSeaBass_.rds")
# BSB_ <- readRDS("MSE_obj/MSE_BlackSeaBass_.rds")


library(vioplot)
quart1<-function(y, type="1st Qu."){
  return(summary(y)[type])
}
quart3<-function(y, type="3rd Qu."){
  return(summary(y)[type])
}
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  ## Save the color
  invisible(t.col)
}


for(res in c("BSB_base","BSB_age0M_hi","BSB_age0M_lo","BSB_epiM","BSB_recdev","BSB_recns","BSB_refbias_hi","BSB_refbias_lo","BSB_uobs_hi","BSB_uobs_lo")){
  # results<-BSB_base
  results<-res

  if (res==BSB_epiM){
    proj_dynSSB0<-results@RefPoint$Dynamic_Unfished$SSB0[,40:89]
    for(i in 1:dim(results@SB_SBMSY)[2]){
      # print("before = "); print(summary(c(results@SB_SBMSY[,i,])))
      results@SB_SBMSY[,i,][which(results@SB_SBMSY[,i,]=="Inf")] <-
        results@SSB[which(results@SB_SBMSY[,i,]=="Inf")] /
        proj_dynSSB0[which(results@SB_SBMSY[,i,]=="Inf")]
      # print("after = "); print(summary(c(results@SB_SBMSY[,i,])))
    }

    # test<-cbind("SB_SBMSY"=BSB_epiM@SB_SBMSY[,1,50],
    #             "SSB/SSBMSY"= BSB_epiM@SSB[,1,50]/results@RefPoint$SSBMSY[,1,89],
    #             "Corrected SSB/SSBMSY" = results@SB_SBMSY[,1,50],
    #             "SSB"= BSB_epiM@SSB[,1,50],
    #             "SSBMSY"=results@RefPoint$SSBMSY[,1,89],
    #             "DynSSB0"=results@RefPoint$Dynamic_Unfished$SSB0[,89])
    # View(test)
  }

  png(
    filename = paste0("Plots/", OM_Name, "_SSBSSBMSY.png"),
    type = "cairo",
    units = "mm",
    width = 300,
    height = 225,
    pointsize = 16,
    res = 300
  )
  plot(apply(results@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, 5), lwd=2)
  abline(h=1)
  for(i in 2:dim(results@SB_SBMSY)[2]){
    lines(apply(results@SB_SBMSY[,i,], 2, median), type='l', lwd=2, lty=i, col=i)
  }
  legend("bottom", c(results@MPs), lwd=2, lty=1:dim(results@SB_SBMSY)[2], col=1:dim(results@SB_SBMSY)[2], bty='n', ncol=4, cex=0.75)
  mtext(3, paste0(res, " SSB/SSBMSY"), line=-1)
  dev.off()

} # end for res


# quart1((results@SB_SBMSY[,1,5]))
# quart3((results@SB_SBMSY[,1,5]))



df<-data.frame(yrs = 1:results@proyears,
               mean = apply(results@SB_SBMSY[,i,], 2, mean),
               quart1 = apply(results@SB_SBMSY[,i,], 2, quart1),
               quart3 = apply(results@SB_SBMSY[,i,], 2, quart3),
               med = apply(results@SB_SBMSY[,i,], 2, median))
df1<-data.frame(yrs = 1:results@proyears,
               mean = apply(results@SB_SBMSY[,1,], 2, mean),
               quart1 = apply(results@SB_SBMSY[,1,], 2, quart1),
               quart3 = apply(results@SB_SBMSY[,1,], 2, quart3),
               med = apply(results@SB_SBMSY[,1,], 2, median))
with(df, plot(mean~yrs, type='n', xlab="projection year", ylab="SB_SBMSY", ylim=c(0, 5)))
yr<-as.numeric(df$yrs)
polygon(c(yr, rev(yr)), c(df$quart1, rev(df$quart3)), col=t_col('grey'), border=NA)
polygon(c(yr, rev(yr)), c(df1$quart1, rev(df1$quart3)), col=t_col('deepskyblue'), border=NA)
abline(h=1)
lines(df$mean, lwd=2, col='black')
lines(df$med, lwd=1, col='black')

lines(df1$mean, lwd=2, col='deepskyblue4')
lines(df1$med, lwd=1, col='deepskyblue4')


apply(results@SB_SBMSY[,i,], 2, quart1)
apply(results@SB_SBMSY[,i,], 2, quart3)



vioplot(results@SB_SBMSY[,1,50],results@SB_SBMSY[,2,50],results@SB_SBMSY[,3,50],
        results@SB_SBMSY[,4,50],results@SB_SBMSY[,5,50],results@SB_SBMSY[,6,50],
        results@SB_SBMSY[,7,50],results@SB_SBMSY[,8,50],results@SB_SBMSY[,9,50],
        results@SB_SBMSY[,10,50],results@SB_SBMSY[,11,50],results@SB_SBMSY[,12,50],
        results@SB_SBMSY[,13,50],results@SB_SBMSY[,14,50],
        names=c(results@MPs))
abline(h=1)


plot(apply(BSB_uobs_hi@SB_SBMSY[,1,], 2, mean), type='l', ylim=c(0, 5), lwd=2)
abline(h=1)
  lines(apply(BSB_uobs_hi@SB_SBMSY[,1,], 2, mean), type='l', lwd=2, lty=2, col=2)

