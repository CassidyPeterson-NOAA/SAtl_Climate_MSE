## LOAD PACKAGES ##

library(openMSE)
avail("PM")

library(vioplot)
quart1<-function(y, type="1st Qu."){
  return(summary(c(y))[type])
}
quart3<-function(y, type="3rd Qu."){
  return(summary(c(y))[type])
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




EpiMDeal<-function(data){
  proj_dynSSB0<-data$epiM@RefPoint$Dynamic_Unfished$SSB0[,40:89]
  for(i in 1:dim(data$epiM@SB_SBMSY)[2]){
    # print("before = "); print(summary(c(results@SB_SBMSY[,i,])))
    data$epiM@SB_SBMSY[,i,][which(data$epiM@SB_SBMSY[,i,]=="Inf")] <-
      data$epiM@SSB[which(data$epiM@SB_SBMSY[,i,]=="Inf")] /
      proj_dynSSB0[which(data$epiM@SB_SBMSY[,i,]=="Inf")]
    # print("after = "); print(summary(c(results@SB_SBMSY[,i,])))
  }
  return(data)
}



source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/merge_MSE.R")



# PLOTTING  RESULTS #

# BSB
BSB_base <- readRDS("MSE_obj/MSE_BlackSeaBass_base.rds")
BSB_age0M_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_age0M_hi.rds")
BSB_age0M_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_age0M_lo.rds")
BSB_epiM <- readRDS("MSE_obj/MSE_BlackSeaBass_epiM.rds")
BSB_recdev_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_recdev_hi.rds")
BSB_recdev_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_recdev_lo.rds")
BSB_recns <- readRDS("MSE_obj/MSE_BlackSeaBass_recns.rds")
BSB_refbias_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_refbias_hi.rds")
BSB_refbias_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_refbias_lo.rds")
BSB_uobs_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_uobs_hi.rds")
BSB_uobs_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_uobs_lo.rds")
# BSB_ <- readRDS("MSE_obj/MSE_BlackSeaBass_.rds")
# BSB_ <- readRDS("MSE_obj/MSE_BlackSeaBass_.rds")

BSB<-list(base = BSB_base,
          age0M_hi = BSB_age0M_hi,
          age0M_lo = BSB_age0M_lo,
          epiM = BSB_epiM,
          recdev_hi = BSB_recdev_hi,
          recdev_lo = BSB_recdev_lo,
          recns = BSB_recns,
          refbias_hi = BSB_refbias_hi,
          refbias_lo = BSB_refbias_lo,
          uobs_hi = BSB_uobs_hi,
          uobs_lo = BSB_uobs_lo
)


summary(c(BSB$epiM@SB_SBMSY))
BSB<-EpiMDeal(BSB)
summary(c(BSB$epiM@SB_SBMSY))


#VS
VS_base <- readRDS("MSE_obj/MSE_VermilionSnapper_base.rds")
VS_age0M_hi <- readRDS("MSE_obj/MSE_VermilionSnapper_age0M_hi.rds")
VS_age0M_lo <- readRDS("MSE_obj/MSE_VermilionSnapper_age0M_lo.rds")
VS_epiM <- readRDS("MSE_obj/MSE_VermilionSnapper_epiM.rds")
VS_recdev_hi <- readRDS("MSE_obj/MSE_VermilionSnapper_recdev_hi.rds")
VS_recdev_lo <- readRDS("MSE_obj/MSE_VermilionSnapper_recdev_lo.rds")
VS_recns <- readRDS("MSE_obj/MSE_VermilionSnapper_recns.rds")
VS_refbias_hi <- readRDS("MSE_obj/MSE_VermilionSnapper_refbias_hi.rds")
VS_refbias_lo <- readRDS("MSE_obj/MSE_VermilionSnapper_refbias_lo.rds")
VS_uobs_hi <- readRDS("MSE_obj/MSE_VermilionSnapper_uobs_hi.rds")
VS_uobs_lo <- readRDS("MSE_obj/MSE_VermilionSnapper_uobs_lo.rds")

VS<-list(base = VS_base,
          age0M_hi = VS_age0M_hi,
          age0M_lo = VS_age0M_lo,
          epiM = VS_epiM,
          recdev_hi = VS_recdev_hi,
          recdev_lo = VS_recdev_lo,
          recns = VS_recns,
          refbias_hi = VS_refbias_hi,
          refbias_lo = VS_refbias_lo,
          uobs_hi = VS_uobs_hi,
          uobs_lo = VS_uobs_lo
)

summary(c(VS$epiM@SB_SBMSY))
VS<-EpiMDeal(VS)
summary(c(VS$epiM@SB_SBMSY))



# RP
RP_base <- readRDS("MSE_obj/MSE_RedPorgy_base.rds")
RP_age0M_hi <- readRDS("MSE_obj/MSE_RedPorgy_age0M_hi.rds")
RP_age0M_lo <- readRDS("MSE_obj/MSE_RedPorgy_age0M_lo.rds")
RP_epiM <- readRDS("MSE_obj/MSE_RedPorgy_epiM.rds")
RP_recdev_hi <- readRDS("MSE_obj/MSE_RedPorgy_recdev_hi.rds")
RP_recdev_lo <- readRDS("MSE_obj/MSE_RedPorgy_recdev_lo.rds")
RP_recns <- readRDS("MSE_obj/MSE_RedPorgy_recns.rds")
RP_refbias_hi <- readRDS("MSE_obj/MSE_RedPorgy_refbias_hi.rds")
RP_refbias_lo <- readRDS("MSE_obj/MSE_RedPorgy_refbias_lo.rds")
RP_uobs_hi <- readRDS("MSE_obj/MSE_RedPorgy_uobs_hi.rds")
RP_uobs_lo <- readRDS("MSE_obj/MSE_RedPorgy_uobs_lo.rds")
# RP_ <- readRDS("MSE_obj/MSE_RedPorgy_.rds")
# RP_ <- readRDS("MSE_obj/MSE_RedPorgy_.rds")

RP<-list(base = RP_base,
          age0M_hi = RP_age0M_hi,
          age0M_lo = RP_age0M_lo,
          epiM = RP_epiM, ########### NEEDS REPLACING ###########
          recdev_hi = RP_recdev_hi,
          recdev_lo = RP_recdev_lo,
          recns = RP_recns,
          refbias_hi = RP_refbias_hi,
          refbias_lo = RP_refbias_lo,
          uobs_hi = RP_uobs_hi,
          uobs_lo = RP_uobs_lo
)

summary(c(RP$epiM@SB_SBMSY))
RP<-EpiMDeal(RP)
summary(c(RP$epiM@SB_SBMSY))








# test<-cbind("SB_SBMSY"=BSB_epiM@SB_SBMSY[,1,50],
#             "SSB/SSBMSY"= BSB_epiM@SSB[,1,50]/results@RefPoint$SSBMSY[,1,89],
#             "Corrected SSB/SSBMSY" = results@SB_SBMSY[,1,50],
#             "SSB"= BSB_epiM@SSB[,1,50],
#             "SSBMSY"=results@RefPoint$SSBMSY[,1,89],
#             "DynSSB0"=results@RefPoint$Dynamic_Unfished$SSB0[,89])
# View(test)


### Read in


scenarios<-c("base","age0M_hi","age0M_lo","epiM","recdev_hi","recdev_lo",
             "recns","refbias_hi","refbias_lo","uobs_hi","uobs_lo")


MP_names<-BSB[[1]]@MPs
MP_namesR<-c("SCA_1","SCA_5","SCA_10", "pMP_5", "pMP_10","GB_slope1", "GB_target1", "ICI2", "myIratio", "myIT10", "myItarget", "L95target")

MP_colR<-c("deepskyblue","deepskyblue3","deepskyblue4", "lightskyblue","lightskyblue1", "orange","slateblue","red", "violetred","darkolivegreen1","turquoise1","darkorchid")



# merge_MSE(BSB_age0M_hi, BSB_age0M_lo)




### PLOT TRAJECTORIES
# for(res in c("BSB_base","BSB_age0M_hi","BSB_age0M_lo","BSB_epiM","BSB_recdev","BSB_recns","BSB_refbias_hi","BSB_refbias_lo","BSB_uobs_hi","BSB_uobs_lo")){
for(fsh in c('BSB','VS','RP')){
  for(res in scenarios){
    # results<-BSB_base
    # results<-get(res)
    data<-get(fsh)
    results<-get(res,data)


    ## SSB/SSBMSY
    png(
      filename = paste0("Plots/", fsh,"_",res, "_SSBSSBMSY.png"),
      type = "cairo",
      units = "mm",
      width = 300,
      height = 225,
      pointsize = 16,
      res = 300
    )
    plot(apply(results@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, 4), lwd=2)
    abline(h=1)
    for(i in 2:dim(results@SB_SBMSY)[2]){
      lines(apply(results@SB_SBMSY[,i,], 2, median), type='l', lwd=2, lty=i, col=i)
    }
    legend("bottom", c(results@MPs), lwd=2, lty=1:dim(results@SB_SBMSY)[2], col=1:dim(results@SB_SBMSY)[2], bty='n', ncol=4, cex=0.75)
    mtext( paste0(fsh, " ", res, " SSB/SSBMSY"), side=3, line=-1)
    dev.off()

    ## Catch
    png(
      filename = paste0("Plots/", fsh,"_",res, "_Catch.png"),
      type = "cairo",
      units = "mm",
      width = 300,
      height = 225,
      pointsize = 16,
      res = 300
    )
    plot(apply(results@Catch[,1,], 2, median), type='l',  lwd=2, ylim=c(0, 4000))
    abline(h=1)
    for(i in 2:dim(results@Catch)[2]){
      lines(apply(results@Catch[,i,], 2, median), type='l', lwd=2, lty=i, col=i)
    }
    legend("bottom", c(results@MPs), lwd=2, lty=1:dim(results@Catch)[2], col=1:dim(results@Catch)[2], bty='n', ncol=4, cex=0.75)
    mtext( paste0(fsh, " ", res, " Catch"), side=3, line=-1)
    dev.off()

  } # end for res

} # end for fsh


# quart1((results@SB_SBMSY[,1,5]))
# quart3((results@SB_SBMSY[,1,5]))

for(cMP in 1:length(BSB$base@MPs)){
  png(
    filename = paste0("Plots/", BSB$base@MPs[cMP], "_SSBSSBMSY.png"),
    type = "cairo",
    units = "mm",
    width = 300,
    height = 225,
    pointsize = 16,
    res = 300
  )
  plot(apply(BSB$base@SB_SBMSY[,cMP,], 2, median), type='l', ylim=c(0, 5), lwd=2)
  abline(h=1)
  for(res in scenarios[2:length(scenarios)]){
    results<-get(res, BSB)
    lines(apply(results@SB_SBMSY[,cMP,], 2, median), type='l', lwd=2, lty=which(scenarios==res), col=which(scenarios==res))
  }
  legend("topright", scenarios, lwd=2, lty=1:length(scenarios), col=1:length(scenarios), bty='n', ncol=2, cex=0.75)
  mtext( paste0(BSB$base@MPs[cMP], " SSB/SSBMSY"), side=3, line=-1)
  dev.off()
}





dim(BSB[[1]]@SB_SBMSY) # 100, 12, 50 [iterations, MPs, yrs]




# SB_SBMSY_SCA1 -- dimensions [iterations, nyears, scenario]
for(i in 1:length(MP_names)){
  x<-array(data=c(BSB[[1]]@SB_SBMSY[,i,],
                  BSB[[2]]@SB_SBMSY[,i,],
                  BSB[[3]]@SB_SBMSY[,i,],
                  BSB[[4]]@SB_SBMSY[,i,],
                  BSB[[5]]@SB_SBMSY[,i,],
                  BSB[[6]]@SB_SBMSY[,i,],
                  BSB[[7]]@SB_SBMSY[,i,],
                  BSB[[8]]@SB_SBMSY[,i,],
                  BSB[[9]]@SB_SBMSY[,i,],
                  BSB[[10]]@SB_SBMSY[,i,]#,
                  # BSB[[11]]@SB_SBMSY[,1,]
  ),
  dim=c(dim(BSB[[1]]@SB_SBMSY)[1],dim(BSB[[1]]@SB_SBMSY)[3],length(scenarios)))
  assign(paste0("SB_SBMSY_",MP_names[i]), x)
}

# SB_SBMSY_SCA1<-array(data=c(BSB[[1]]@SB_SBMSY[,1,],
#                             BSB[[2]]@SB_SBMSY[,1,],
#                             BSB[[3]]@SB_SBMSY[,1,],
#                             BSB[[4]]@SB_SBMSY[,1,],
#                             BSB[[5]]@SB_SBMSY[,1,],
#                             BSB[[6]]@SB_SBMSY[,1,],
#                             BSB[[7]]@SB_SBMSY[,1,],
#                             BSB[[8]]@SB_SBMSY[,1,],
#                             BSB[[9]]@SB_SBMSY[,1,],
#                             BSB[[10]]@SB_SBMSY[,1,]#,
#                             # BSB[[11]]@SB_SBMSY[,1,]
# ),
# dim=c(dim(BSB[[1]]@SB_SBMSY)[1],dim(BSB[[1]]@SB_SBMSY)[3],length(scenarios)))


### BOXPLOT
MP_namesR<-c("SCA_1","SCA_5","SCA_10", "pMP_5", "pMP_10","GB_slope1", "GB_target1", "ICI2", "myIratio", "myIT10", "myItarget", "L95target")
MP_colR<-c("deepskyblue","deepskyblue3","deepskyblue4", "lightskyblue","lightskyblue1", "orange","slateblue","red", "violetred","darkolivegreen1","turquoise1","darkorchid")

boxplot(c(SB_SBMSY_SCA_1[,50,]), c(SB_SBMSY_SCA_5[,50,]), c(SB_SBMSY_SCA_10[,50,]),
        c(SB_SBMSY_pMP_5[,50,]), c(SB_SBMSY_pMP_10[,50,]),
        c(SB_SBMSY_GB_slope1[,50,]),c(SB_SBMSY_GB_target1[,50,])
        ,c(SB_SBMSY_ICI2[,50,]), c(SB_SBMSY_myIratio[,50,]),
        c(SB_SBMSY_myIT10[,50,]), c(SB_SBMSY_myItarget[,50,]),
        c(SB_SBMSY_L95target[,50,]), ylim=c(0,5), col=MP_colR, names=MP_namesR, cex.axis=0.7) #col=c(),
abline(h=1)
mtext("SSB/SSBMSY", side=2, line=1.5)
mtext("Black Sea Bass", side=3, line=1.0)






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
# lines(df$mean, lwd=2, col='black')
lines(df$med, lwd=1, col='black')
# lines(df1$mean, lwd=2, col='deepskyblue4')
lines(df1$med, lwd=1, col='deepskyblue4')


apply(results@SB_SBMSY[,i,], 2, quart1)
apply(results@SB_SBMSY[,i,], 2, quart3)


dim(SB_SBMSY_SCA1) #[1] 100  50  10 - [iters, yrs, OM]


median(SB_SBMSY_SCA1[,1,])
# sd(SB_SBMSY_SCA1[,1,])
# mean(SB_SBMSY_SCA1[,1,])


Uncert_plot<-function(stat="SB_SBMSY", mps, cols=1:10, quant1=0.1, quant3=0.9){
  for(i in 1:length(mps)){
    results<-get(paste(stat,mps[i],sep="_"))

    df<-data.frame(yrs = 1:dim(results)[2],
                   med = apply(results, 2, median),
                   quant1 = apply(results, 2, quantile, probs=quant1),
                   quant3 = apply(results, 2, quantile, probs=quant3))

    if(i==1){
      with(df, plot(med~yrs, type='n', xlab="projection year", ylab=stat, ylim=c(0, 5)))
      abline(h=1)
    }
    yr<-as.numeric(df$yrs)
    polygon(c(yr, rev(yr)), c(df$quant1, rev(df$quant3)), col=t_col(cols[i]), border=NA)
    lines(df$med, lwd=2, col=cols[i])
    # polygon(c(yr, rev(yr)), c(df$quant1, rev(df$quant3)), col=t_col(MP_colR[which(MP_namesR==mps[i])]), border=NA)
    # lines(df$med, lwd=2, col=MP_colR[which(MP_namesR==mps[i])])
  } # end i loop
} # end Uncert_plot function

Uncert_plot(mps=c('SCA_1','SCA_5','SCA_10'))
Uncert_plot(mps=c('SCA_1','pMP_5','pMP_10'))

Uncert_plot(mps=c('SCA_1','SCA_5','GB_target1'))
Uncert_plot(mps=c('SCA_1','myIT10','myItarget'))
Uncert_plot(mps=c('SCA_1','SCA_5','myItarget'))



AAVY(BSB$base)@Stat

par(mfrow=c(4,3), mar=c(0.3, 0.3, 0.3, 0.3), oma=c(2, 2, 1,1))
for(mps in 1:length(MP_names)){

  boxplot(AAVY(BSB[[1]])@Stat[,mps], AAVY(BSB[[2]])@Stat[,mps],AAVY(BSB[[3]])@Stat[,mps],
          AAVY(BSB[[4]])@Stat[,mps],AAVY(BSB[[5]])@Stat[,mps],AAVY(BSB[[6]])@Stat[,mps],
          AAVY(BSB[[7]])@Stat[,mps],AAVY(BSB[[8]])@Stat[,mps],AAVY(BSB[[9]])@Stat[,mps],
          AAVY(BSB[[10]])@Stat[,mps],AAVY(BSB[[11]])@Stat[,mps], names=scenarios, cex.axis=0.7, ylim=c(0,1))
  mtext(MP_names[mps], line=-1)

}
# AAVY(BSB[[1]])@Stat[, which(MP_names=="SCA_1")]

par(mfrow=c(1,1))

boxplot(c(AAVY(BSB[[1]])@Stat[,1], AAVY(BSB[[2]])@Stat[,1],AAVY(BSB[[3]])@Stat[,1],
          AAVY(BSB[[4]])@Stat[,1],AAVY(BSB[[5]])@Stat[,1],AAVY(BSB[[6]])@Stat[,1],
          AAVY(BSB[[7]])@Stat[,1],AAVY(BSB[[8]])@Stat[,1],AAVY(BSB[[9]])@Stat[,1],
          AAVY(BSB[[10]])@Stat[,1],AAVY(BSB[[11]])@Stat[,1]),
        c(AAVY(BSB[[1]])@Stat[,2], AAVY(BSB[[2]])@Stat[,2],AAVY(BSB[[3]])@Stat[,2],
          AAVY(BSB[[4]])@Stat[,2],AAVY(BSB[[5]])@Stat[,2],AAVY(BSB[[6]])@Stat[,2],
          AAVY(BSB[[7]])@Stat[,2],AAVY(BSB[[8]])@Stat[,2],AAVY(BSB[[9]])@Stat[,2],
          AAVY(BSB[[10]])@Stat[,2],AAVY(BSB[[11]])@Stat[,2]),
        c(AAVY(BSB[[1]])@Stat[,3], AAVY(BSB[[2]])@Stat[,3],AAVY(BSB[[3]])@Stat[,3],
          AAVY(BSB[[4]])@Stat[,3],AAVY(BSB[[5]])@Stat[,3],AAVY(BSB[[6]])@Stat[,3],
          AAVY(BSB[[7]])@Stat[,3],AAVY(BSB[[8]])@Stat[,3],AAVY(BSB[[9]])@Stat[,3],
          AAVY(BSB[[10]])@Stat[,3],AAVY(BSB[[11]])@Stat[,3]),
        c(AAVY(BSB[[1]])@Stat[,4], AAVY(BSB[[2]])@Stat[,4],AAVY(BSB[[3]])@Stat[,4],
          AAVY(BSB[[4]])@Stat[,4],AAVY(BSB[[5]])@Stat[,4],AAVY(BSB[[6]])@Stat[,4],
          AAVY(BSB[[7]])@Stat[,4],AAVY(BSB[[8]])@Stat[,4],AAVY(BSB[[9]])@Stat[,4],
          AAVY(BSB[[10]])@Stat[,4],AAVY(BSB[[11]])@Stat[,4]),
        c(AAVY(BSB[[1]])@Stat[,5], AAVY(BSB[[2]])@Stat[,5],AAVY(BSB[[3]])@Stat[,5],
          AAVY(BSB[[4]])@Stat[,5],AAVY(BSB[[5]])@Stat[,5],AAVY(BSB[[6]])@Stat[,5],
          AAVY(BSB[[7]])@Stat[,5],AAVY(BSB[[8]])@Stat[,5],AAVY(BSB[[9]])@Stat[,5],
          AAVY(BSB[[10]])@Stat[,5],AAVY(BSB[[11]])@Stat[,5]),
        c(AAVY(BSB[[1]])@Stat[,6], AAVY(BSB[[2]])@Stat[,6],AAVY(BSB[[3]])@Stat[,6],
          AAVY(BSB[[4]])@Stat[,6],AAVY(BSB[[5]])@Stat[,6],AAVY(BSB[[6]])@Stat[,6],
          AAVY(BSB[[7]])@Stat[,6],AAVY(BSB[[8]])@Stat[,6],AAVY(BSB[[9]])@Stat[,6],
          AAVY(BSB[[10]])@Stat[,6],AAVY(BSB[[11]])@Stat[,6]),
        c(AAVY(BSB[[1]])@Stat[,7], AAVY(BSB[[2]])@Stat[,7],AAVY(BSB[[3]])@Stat[,7],
          AAVY(BSB[[4]])@Stat[,7],AAVY(BSB[[5]])@Stat[,7],AAVY(BSB[[6]])@Stat[,7],
          AAVY(BSB[[7]])@Stat[,7],AAVY(BSB[[8]])@Stat[,7],AAVY(BSB[[9]])@Stat[,7],
          AAVY(BSB[[10]])@Stat[,7],AAVY(BSB[[11]])@Stat[,7]),
        c(AAVY(BSB[[1]])@Stat[,8], AAVY(BSB[[2]])@Stat[,8],AAVY(BSB[[3]])@Stat[,8],
          AAVY(BSB[[4]])@Stat[,8],AAVY(BSB[[5]])@Stat[,8],AAVY(BSB[[6]])@Stat[,8],
          AAVY(BSB[[7]])@Stat[,8],AAVY(BSB[[8]])@Stat[,8],AAVY(BSB[[9]])@Stat[,8],
          AAVY(BSB[[10]])@Stat[,8],AAVY(BSB[[11]])@Stat[,8]),
        c(AAVY(BSB[[1]])@Stat[,9], AAVY(BSB[[2]])@Stat[,9],AAVY(BSB[[3]])@Stat[,9],
          AAVY(BSB[[4]])@Stat[,9],AAVY(BSB[[5]])@Stat[,9],AAVY(BSB[[6]])@Stat[,9],
          AAVY(BSB[[7]])@Stat[,9],AAVY(BSB[[8]])@Stat[,9],AAVY(BSB[[9]])@Stat[,9],
          AAVY(BSB[[10]])@Stat[,9],AAVY(BSB[[11]])@Stat[,9]),
        c(AAVY(BSB[[1]])@Stat[,10], AAVY(BSB[[2]])@Stat[,10],AAVY(BSB[[3]])@Stat[,10],
          AAVY(BSB[[4]])@Stat[,10],AAVY(BSB[[5]])@Stat[,10],AAVY(BSB[[6]])@Stat[,10],
          AAVY(BSB[[7]])@Stat[,10],AAVY(BSB[[8]])@Stat[,10],AAVY(BSB[[9]])@Stat[,10],
          AAVY(BSB[[10]])@Stat[,10],AAVY(BSB[[11]])@Stat[,10]),
        c(AAVY(BSB[[1]])@Stat[,11], AAVY(BSB[[2]])@Stat[,11],AAVY(BSB[[3]])@Stat[,11],
          AAVY(BSB[[4]])@Stat[,11],AAVY(BSB[[5]])@Stat[,11],AAVY(BSB[[6]])@Stat[,11],
          AAVY(BSB[[7]])@Stat[,11],AAVY(BSB[[8]])@Stat[,11],AAVY(BSB[[9]])@Stat[,11],
          AAVY(BSB[[10]])@Stat[,11],AAVY(BSB[[11]])@Stat[,11]),
        c(AAVY(BSB[[1]])@Stat[,12], AAVY(BSB[[2]])@Stat[,12],AAVY(BSB[[3]])@Stat[,12],
          AAVY(BSB[[4]])@Stat[,12],AAVY(BSB[[5]])@Stat[,12],AAVY(BSB[[6]])@Stat[,12],
          AAVY(BSB[[7]])@Stat[,12],AAVY(BSB[[8]])@Stat[,12],AAVY(BSB[[9]])@Stat[,12],
          AAVY(BSB[[10]])@Stat[,12],AAVY(BSB[[11]])@Stat[,12]),
        names=MP_names, cex.axis=0.7, ylim=c(0,1))
mtext("BSB",3, line=-1)
mtext("AAVY", 2, line=1)




boxplot(c(AAVY(BSB[[1]])@Stat[,1], AAVY(BSB[[2]])@Stat[,1],AAVY(BSB[[3]])@Stat[,1],
          AAVY(BSB[[4]])@Stat[,1],AAVY(BSB[[5]])@Stat[,1],AAVY(BSB[[6]])@Stat[,1],
          AAVY(BSB[[7]])@Stat[,1],AAVY(BSB[[8]])@Stat[,1],AAVY(BSB[[9]])@Stat[,1],
          AAVY(BSB[[10]])@Stat[,1],AAVY(BSB[[11]])@Stat[,1]),
        c(AAVY(BSB[[1]])@Stat[,2], AAVY(BSB[[2]])@Stat[,2],AAVY(BSB[[3]])@Stat[,2],
          AAVY(BSB[[4]])@Stat[,2],AAVY(BSB[[5]])@Stat[,2],AAVY(BSB[[6]])@Stat[,2],
          AAVY(BSB[[7]])@Stat[,2],AAVY(BSB[[8]])@Stat[,2],AAVY(BSB[[9]])@Stat[,2],
          AAVY(BSB[[10]])@Stat[,2],AAVY(BSB[[11]])@Stat[,2]),
        c(AAVY(BSB[[1]])@Stat[,3], AAVY(BSB[[2]])@Stat[,3],AAVY(BSB[[3]])@Stat[,3],
          AAVY(BSB[[4]])@Stat[,3],AAVY(BSB[[5]])@Stat[,3],AAVY(BSB[[6]])@Stat[,3],
          AAVY(BSB[[7]])@Stat[,3],AAVY(BSB[[8]])@Stat[,3],AAVY(BSB[[9]])@Stat[,3],
          AAVY(BSB[[10]])@Stat[,3],AAVY(BSB[[11]])@Stat[,3]),
        c(AAVY(BSB[[1]])@Stat[,4], AAVY(BSB[[2]])@Stat[,4],AAVY(BSB[[3]])@Stat[,4],
          AAVY(BSB[[4]])@Stat[,4],AAVY(BSB[[5]])@Stat[,4],AAVY(BSB[[6]])@Stat[,4],
          AAVY(BSB[[7]])@Stat[,4],AAVY(BSB[[8]])@Stat[,4],AAVY(BSB[[9]])@Stat[,4],
          AAVY(BSB[[10]])@Stat[,4],AAVY(BSB[[11]])@Stat[,4]),
        c(AAVY(BSB[[1]])@Stat[,5], AAVY(BSB[[2]])@Stat[,5],AAVY(BSB[[3]])@Stat[,5],
          AAVY(BSB[[4]])@Stat[,5],AAVY(BSB[[5]])@Stat[,5],AAVY(BSB[[6]])@Stat[,5],
          AAVY(BSB[[7]])@Stat[,5],AAVY(BSB[[8]])@Stat[,5],AAVY(BSB[[9]])@Stat[,5],
          AAVY(BSB[[10]])@Stat[,5],AAVY(BSB[[11]])@Stat[,5]),
        c(AAVY(BSB[[1]])@Stat[,6], AAVY(BSB[[2]])@Stat[,6],AAVY(BSB[[3]])@Stat[,6],
          AAVY(BSB[[4]])@Stat[,6],AAVY(BSB[[5]])@Stat[,6],AAVY(BSB[[6]])@Stat[,6],
          AAVY(BSB[[7]])@Stat[,6],AAVY(BSB[[8]])@Stat[,6],AAVY(BSB[[9]])@Stat[,6],
          AAVY(BSB[[10]])@Stat[,6],AAVY(BSB[[11]])@Stat[,6]),
        c(AAVY(BSB[[1]])@Stat[,7], AAVY(BSB[[2]])@Stat[,7],AAVY(BSB[[3]])@Stat[,7],
          AAVY(BSB[[4]])@Stat[,7],AAVY(BSB[[5]])@Stat[,7],AAVY(BSB[[6]])@Stat[,7],
          AAVY(BSB[[7]])@Stat[,7],AAVY(BSB[[8]])@Stat[,7],AAVY(BSB[[9]])@Stat[,7],
          AAVY(BSB[[10]])@Stat[,7],AAVY(BSB[[11]])@Stat[,7]),
        c(AAVY(BSB[[1]])@Stat[,8], AAVY(BSB[[2]])@Stat[,8],AAVY(BSB[[3]])@Stat[,8],
          AAVY(BSB[[4]])@Stat[,8],AAVY(BSB[[5]])@Stat[,8],AAVY(BSB[[6]])@Stat[,8],
          AAVY(BSB[[7]])@Stat[,8],AAVY(BSB[[8]])@Stat[,8],AAVY(BSB[[9]])@Stat[,8],
          AAVY(BSB[[10]])@Stat[,8],AAVY(BSB[[11]])@Stat[,8]),
        c(AAVY(BSB[[1]])@Stat[,9], AAVY(BSB[[2]])@Stat[,9],AAVY(BSB[[3]])@Stat[,9],
          AAVY(BSB[[4]])@Stat[,9],AAVY(BSB[[5]])@Stat[,9],AAVY(BSB[[6]])@Stat[,9],
          AAVY(BSB[[7]])@Stat[,9],AAVY(BSB[[8]])@Stat[,9],AAVY(BSB[[9]])@Stat[,9],
          AAVY(BSB[[10]])@Stat[,9],AAVY(BSB[[11]])@Stat[,9]),
        c(AAVY(BSB[[1]])@Stat[,10], AAVY(BSB[[2]])@Stat[,10],AAVY(BSB[[3]])@Stat[,10],
          AAVY(BSB[[4]])@Stat[,10],AAVY(BSB[[5]])@Stat[,10],AAVY(BSB[[6]])@Stat[,10],
          AAVY(BSB[[7]])@Stat[,10],AAVY(BSB[[8]])@Stat[,10],AAVY(BSB[[9]])@Stat[,10],
          AAVY(BSB[[10]])@Stat[,10],AAVY(BSB[[11]])@Stat[,10]),
        c(AAVY(BSB[[1]])@Stat[,11], AAVY(BSB[[2]])@Stat[,11],AAVY(BSB[[3]])@Stat[,11],
          AAVY(BSB[[4]])@Stat[,11],AAVY(BSB[[5]])@Stat[,11],AAVY(BSB[[6]])@Stat[,11],
          AAVY(BSB[[7]])@Stat[,11],AAVY(BSB[[8]])@Stat[,11],AAVY(BSB[[9]])@Stat[,11],
          AAVY(BSB[[10]])@Stat[,11],AAVY(BSB[[11]])@Stat[,11]),
        c(AAVY(BSB[[1]])@Stat[,12], AAVY(BSB[[2]])@Stat[,12],AAVY(BSB[[3]])@Stat[,12],
          AAVY(BSB[[4]])@Stat[,12],AAVY(BSB[[5]])@Stat[,12],AAVY(BSB[[6]])@Stat[,12],
          AAVY(BSB[[7]])@Stat[,12],AAVY(BSB[[8]])@Stat[,12],AAVY(BSB[[9]])@Stat[,12],
          AAVY(BSB[[10]])@Stat[,12],AAVY(BSB[[11]])@Stat[,12]),
        names=MP_names, cex.axis=0.7, ylim=c(0,1))
mtext("BSB",3, line=-1)
mtext("Average Yield", 2, line=1)





boxplot(c(AAVY(BSB[[1]])@Stat[,1], AAVY(BSB[[2]])@Stat[,1],AAVY(BSB[[3]])@Stat[,1],
          AAVY(BSB[[4]])@Stat[,1],AAVY(BSB[[5]])@Stat[,1],AAVY(BSB[[6]])@Stat[,1],
          AAVY(BSB[[7]])@Stat[,1],AAVY(BSB[[8]])@Stat[,1],AAVY(BSB[[9]])@Stat[,1],
          AAVY(BSB[[10]])@Stat[,1],AAVY(BSB[[11]])@Stat[,1]),
        c(AAVY(BSB[[1]])@Stat[,2], AAVY(BSB[[2]])@Stat[,2],AAVY(BSB[[3]])@Stat[,2],
          AAVY(BSB[[4]])@Stat[,2],AAVY(BSB[[5]])@Stat[,2],AAVY(BSB[[6]])@Stat[,2],
          AAVY(BSB[[7]])@Stat[,2],AAVY(BSB[[8]])@Stat[,2],AAVY(BSB[[9]])@Stat[,2],
          AAVY(BSB[[10]])@Stat[,2],AAVY(BSB[[11]])@Stat[,2]),
        c(AAVY(BSB[[1]])@Stat[,3], AAVY(BSB[[2]])@Stat[,3],AAVY(BSB[[3]])@Stat[,3],
          AAVY(BSB[[4]])@Stat[,3],AAVY(BSB[[5]])@Stat[,3],AAVY(BSB[[6]])@Stat[,3],
          AAVY(BSB[[7]])@Stat[,3],AAVY(BSB[[8]])@Stat[,3],AAVY(BSB[[9]])@Stat[,3],
          AAVY(BSB[[10]])@Stat[,3],AAVY(BSB[[11]])@Stat[,3]),
        c(AAVY(BSB[[1]])@Stat[,4], AAVY(BSB[[2]])@Stat[,4],AAVY(BSB[[3]])@Stat[,4],
          AAVY(BSB[[4]])@Stat[,4],AAVY(BSB[[5]])@Stat[,4],AAVY(BSB[[6]])@Stat[,4],
          AAVY(BSB[[7]])@Stat[,4],AAVY(BSB[[8]])@Stat[,4],AAVY(BSB[[9]])@Stat[,4],
          AAVY(BSB[[10]])@Stat[,4],AAVY(BSB[[11]])@Stat[,4]),
        c(AAVY(BSB[[1]])@Stat[,5], AAVY(BSB[[2]])@Stat[,5],AAVY(BSB[[3]])@Stat[,5],
          AAVY(BSB[[4]])@Stat[,5],AAVY(BSB[[5]])@Stat[,5],AAVY(BSB[[6]])@Stat[,5],
          AAVY(BSB[[7]])@Stat[,5],AAVY(BSB[[8]])@Stat[,5],AAVY(BSB[[9]])@Stat[,5],
          AAVY(BSB[[10]])@Stat[,5],AAVY(BSB[[11]])@Stat[,5]),
        c(AAVY(BSB[[1]])@Stat[,6], AAVY(BSB[[2]])@Stat[,6],AAVY(BSB[[3]])@Stat[,6],
          AAVY(BSB[[4]])@Stat[,6],AAVY(BSB[[5]])@Stat[,6],AAVY(BSB[[6]])@Stat[,6],
          AAVY(BSB[[7]])@Stat[,6],AAVY(BSB[[8]])@Stat[,6],AAVY(BSB[[9]])@Stat[,6],
          AAVY(BSB[[10]])@Stat[,6],AAVY(BSB[[11]])@Stat[,6]),
        c(AAVY(BSB[[1]])@Stat[,7], AAVY(BSB[[2]])@Stat[,7],AAVY(BSB[[3]])@Stat[,7],
          AAVY(BSB[[4]])@Stat[,7],AAVY(BSB[[5]])@Stat[,7],AAVY(BSB[[6]])@Stat[,7],
          AAVY(BSB[[7]])@Stat[,7],AAVY(BSB[[8]])@Stat[,7],AAVY(BSB[[9]])@Stat[,7],
          AAVY(BSB[[10]])@Stat[,7],AAVY(BSB[[11]])@Stat[,7]),
        c(AAVY(BSB[[1]])@Stat[,8], AAVY(BSB[[2]])@Stat[,8],AAVY(BSB[[3]])@Stat[,8],
          AAVY(BSB[[4]])@Stat[,8],AAVY(BSB[[5]])@Stat[,8],AAVY(BSB[[6]])@Stat[,8],
          AAVY(BSB[[7]])@Stat[,8],AAVY(BSB[[8]])@Stat[,8],AAVY(BSB[[9]])@Stat[,8],
          AAVY(BSB[[10]])@Stat[,8],AAVY(BSB[[11]])@Stat[,8]),
        c(AAVY(BSB[[1]])@Stat[,9], AAVY(BSB[[2]])@Stat[,9],AAVY(BSB[[3]])@Stat[,9],
          AAVY(BSB[[4]])@Stat[,9],AAVY(BSB[[5]])@Stat[,9],AAVY(BSB[[6]])@Stat[,9],
          AAVY(BSB[[7]])@Stat[,9],AAVY(BSB[[8]])@Stat[,9],AAVY(BSB[[9]])@Stat[,9],
          AAVY(BSB[[10]])@Stat[,9],AAVY(BSB[[11]])@Stat[,9]),
        c(AAVY(BSB[[1]])@Stat[,10], AAVY(BSB[[2]])@Stat[,10],AAVY(BSB[[3]])@Stat[,10],
          AAVY(BSB[[4]])@Stat[,10],AAVY(BSB[[5]])@Stat[,10],AAVY(BSB[[6]])@Stat[,10],
          AAVY(BSB[[7]])@Stat[,10],AAVY(BSB[[8]])@Stat[,10],AAVY(BSB[[9]])@Stat[,10],
          AAVY(BSB[[10]])@Stat[,10],AAVY(BSB[[11]])@Stat[,10]),
        c(AAVY(BSB[[1]])@Stat[,11], AAVY(BSB[[2]])@Stat[,11],AAVY(BSB[[3]])@Stat[,11],
          AAVY(BSB[[4]])@Stat[,11],AAVY(BSB[[5]])@Stat[,11],AAVY(BSB[[6]])@Stat[,11],
          AAVY(BSB[[7]])@Stat[,11],AAVY(BSB[[8]])@Stat[,11],AAVY(BSB[[9]])@Stat[,11],
          AAVY(BSB[[10]])@Stat[,11],AAVY(BSB[[11]])@Stat[,11]),
        c(AAVY(BSB[[1]])@Stat[,12], AAVY(BSB[[2]])@Stat[,12],AAVY(BSB[[3]])@Stat[,12],
          AAVY(BSB[[4]])@Stat[,12],AAVY(BSB[[5]])@Stat[,12],AAVY(BSB[[6]])@Stat[,12],
          AAVY(BSB[[7]])@Stat[,12],AAVY(BSB[[8]])@Stat[,12],AAVY(BSB[[9]])@Stat[,12],
          AAVY(BSB[[10]])@Stat[,12],AAVY(BSB[[11]])@Stat[,12]),
        names=MP_names, cex.axis=0.7, ylim=c(0,1))
mtext("BSB",3, line=-1)
mtext("AAVY", 2, line=1)
scenarios
MPs
