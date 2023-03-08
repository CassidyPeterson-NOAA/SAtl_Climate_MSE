## LOAD PACKAGES ##

library(openMSE)
avail("PM")
library(RColorBrewer)
display.brewer.all()
library(paletteer)

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


## TEST IF REF POINTS ARE DYNAMIC -- INFO IN OM@RefPoint$SSBMSY MATCHES OM@SB_SBMSY
SSB_SSBMSY <- VS$age0M_hi@SB_SBMSY[1,1,]

SSB <- VS$age0M_hi@SSB[1,1,]

SSB/SSB_SSBMSY

VS$age0M_hi@OM$SSBMSY[1]
VS$age0M_hi@RefPoint$SSBMSY[1,1,(length(VS$age0M_hi@RefPoint$SSBMSY[1,1,])-49):length(VS$age0M_hi@RefPoint$SSBMSY[1,1,])]



# SSB / SSBMSY = SSB_SSBMSY
# SSB / SSB_SSBMSY = SSBMSY

# PLOTTING  RESULTS #
############# read in data ####################
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

BSB_base@PPD$SCA_1@CV_AddInd[,1,] # projected CV = 0.199
names(BSB_age0M_hi@RefPoint$Dynamic_Unfished)
SSB0dyn<-BSB_age0M_hi@RefPoint$Dynamic_Unfished$SSB0
BSB_age0M_hi@RefPoint$SSBMSY[,1,]/BSB_age0M_hi@RefPoint$Dynamic_Unfished$SSB0
SSBtemp<-cbind(BSB_age0M_hi@SSB_hist, BSB_age0M_hi@SSB[,1,])
SSBtempdyn<-SSBtemp/SSB0dyn
SSBtempdyn_med<-apply(SSBtempdyn,2,median)
plot(SSBtempdyn_med, type='l', ylim=c(0,2))
# lines(y=apply(BSB_age0M_hi@SB_SBMSY[,1,],2,median),x=c(40:89), type='l', lty=2, col="red")
lines(apply(((cbind(BSB_age0M_hi@SSB_hist, BSB_age0M_hi@SSB[,6,]))/BSB_age0M_hi@RefPoint$Dynamic_Unfished$SSB0),2,median), type='l', lty=2, col="red")

plot(apply(SSBtemp, 2, median) , type='l')
lines(apply(cbind(BSB_age0M_hi@SSB_hist, BSB_age0M_hi@SSB[,6,]), 2, median), type='l', lty=2, col="red")
BSB_age0M_hi@MPs

BSB$recdev_lo@F_FMSY
BSB$recdev_hi@PPD$SCA_1@OM$BMSY_B0
VS$recdev_hi@PPD$SCA_1@OM$BMSY_B0
RP$recdev_hi@PPD$SCA_1@OM$BMSY_B0


dim(BSB_age0M_hi@SB_SBMSY[,1,])

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


VS_base@PPD$SCA_1@CV_AddInd[,1,] # projected CV = 0.276

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


RP_base@PPD$SCA_1@CV_AddInd[,1,] # projected CV = 0.12

RP<-list(base = RP_base,
          age0M_hi = RP_age0M_hi,
          age0M_lo = RP_age0M_lo,
          epiM = RP_epiM,
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






# BSB_Overages
BSB_O_base <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_base.rds")
BSB_O_age0M_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_age0M_hi.rds")
BSB_O_age0M_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_age0M_lo.rds")
BSB_O_epiM <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_epiM.rds")
BSB_O_recdev_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_recdev_hi.rds")
BSB_O_recdev_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_recdev_lo.rds")
BSB_O_recns <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_recns.rds")
BSB_O_refbias_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_refbias_hi.rds")
BSB_O_refbias_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_refbias_lo.rds")
BSB_O_uobs_hi <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_uobs_hi.rds")
BSB_O_uobs_lo <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_uobs_lo.rds")
# BSB_O_ <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_.rds")
# BSB_O_ <- readRDS("MSE_obj/MSE_BlackSeaBass_Over_.rds")

BSB_O<-list(base = BSB_O_base,
          age0M_hi = BSB_O_age0M_hi,
          age0M_lo = BSB_O_age0M_lo,
          epiM = BSB_O_epiM,
          recdev_hi = BSB_O_recdev_hi,
          recdev_lo = BSB_O_recdev_lo,
          recns = BSB_O_recns,
          refbias_hi = BSB_O_refbias_hi,
          refbias_lo = BSB_O_refbias_lo,
          uobs_hi = BSB_O_uobs_hi,
          uobs_lo = BSB_O_uobs_lo
)


summary(c(BSB_O$epiM@SB_SBMSY))
BSB_O<-EpiMDeal(BSB_O)
summary(c(BSB_O$epiM@SB_SBMSY))


#VS_Over
VS_O_base <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_base.rds")
VS_O_age0M_hi <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_age0M_hi.rds")
VS_O_age0M_lo <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_age0M_lo.rds")
VS_O_epiM <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_epiM.rds")
VS_O_recdev_hi <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_recdev_hi.rds")
VS_O_recdev_lo <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_recdev_lo.rds")
VS_O_recns <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_recns.rds")
VS_O_refbias_hi <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_refbias_hi.rds")
VS_O_refbias_lo <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_refbias_lo.rds")
VS_O_uobs_hi <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_uobs_hi.rds")
VS_O_uobs_lo <- readRDS("MSE_obj/MSE_VermilionSnapper_Over_uobs_lo.rds")

VS_O<-list(base = VS_O_base,
         age0M_hi = VS_O_age0M_hi,
         age0M_lo = VS_O_age0M_lo,
         epiM = VS_O_epiM,
         recdev_hi = VS_O_recdev_hi,
         recdev_lo = VS_O_recdev_lo,
         recns = VS_O_recns,
         refbias_hi = VS_O_refbias_hi,
         refbias_lo = VS_O_refbias_lo,
         uobs_hi = VS_O_uobs_hi,
         uobs_lo = VS_O_uobs_lo
)

summary(c(VS_O$epiM@SB_SBMSY))
VS_O<-EpiMDeal(VS_O)
summary(c(VS_O$epiM@SB_SBMSY))



# RP_Over
RP_O_base <- readRDS("MSE_obj/MSE_RedPorgy_Over_base.rds")
RP_O_age0M_hi <- readRDS("MSE_obj/MSE_RedPorgy_Over_age0M_hi.rds")
RP_O_age0M_lo <- readRDS("MSE_obj/MSE_RedPorgy_Over_age0M_lo.rds")
RP_O_epiM <- readRDS("MSE_obj/MSE_RedPorgy_Over_epiM.rds")
RP_O_recdev_hi <- readRDS("MSE_obj/MSE_RedPorgy_Over_recdev_hi.rds")
RP_O_recdev_lo <- readRDS("MSE_obj/MSE_RedPorgy_Over_recdev_lo.rds")
RP_O_recns <- readRDS("MSE_obj/MSE_RedPorgy_Over_recns.rds")
RP_O_refbias_hi <- readRDS("MSE_obj/MSE_RedPorgy_Over_refbias_hi.rds")
RP_O_refbias_lo <- readRDS("MSE_obj/MSE_RedPorgy_Over_refbias_lo.rds")
RP_O_uobs_hi <- readRDS("MSE_obj/MSE_RedPorgy_Over_uobs_hi.rds")
RP_O_uobs_lo <- readRDS("MSE_obj/MSE_RedPorgy_Over_uobs_lo.rds")
# RP_O_ <- readRDS("MSE_obj/MSE_RedPorgy_.rds")
# RP_O_ <- readRDS("MSE_obj/MSE_RedPorgy_.rds")

RP_O<-list(base = RP_O_base,
         age0M_hi = RP_O_age0M_hi,
         age0M_lo = RP_O_age0M_lo,
         epiM = RP_O_epiM,
         recdev_hi = RP_O_recdev_hi,
         recdev_lo = RP_O_recdev_lo,
         recns = RP_O_recns,
         refbias_hi = RP_O_refbias_hi,
         refbias_lo = RP_O_refbias_lo,
         uobs_hi = RP_O_uobs_hi,
         uobs_lo = RP_O_uobs_lo
)

summary(c(RP_O$epiM@SB_SBMSY))
RP_O<-EpiMDeal(RP_O)
summary(c(RP_O$epiM@SB_SBMSY))
##########################


aaa<- paletteer::scale_color_paletteer_d("dichromat::BluetoGray_8")
paletteer::scale_colour_paletteer_d("dichromat::DarkRedtoBlue_18")



### Read in


scenarios<-c("base","age0M_hi","age0M_lo","epiM","recdev_hi","recdev_lo",
             "recns","refbias_hi","refbias_lo","uobs_hi","uobs_lo")


MP_names<-BSB[[1]]@MPs
MP_namesR<-c(MP_names[c(1,9,10,2:8)])
# MP_namesR<-c("SCA_1","SCA_5","SCA_10", "pMP_5", "pMP_10","GB_slope1", "GB_target1", "ICI2", "myIratio", "myIT10", "myItarget", "L95target")
c_emps<-c(paletteer_d("colorBlindness::LightBlue2DarkBlue7Steps"))[3:7]
# c_emps<-c(paletteer::paletteer_d("ggthemes::excel_Green"))[c(1,2,3,5,6)]
# c_emps<-c(paletteer::paletteer_d("vapoRwave::jazzCup"))
c_mods<-c(paletteer_d("ggthemes::Seattle_Grays"))

MP_colR<-c("deepskyblue","deepskyblue3","deepskyblue4", "lightskyblue","lightskyblue1", "orange","slateblue","red", "violetred","darkolivegreen1","turquoise1","darkorchid")
MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p", "GBtarg","ICI","Irat","IT10","Itarg")
MP_colR2<-c('black',c_mods[2:5], c_emps)



# merge_MSE(BSB_age0M_hi, BSB_age0M_lo)




### PLOT TRAJECTORIES
# for(res in c("BSB_base","BSB_age0M_hi","BSB_age0M_lo","BSB_epiM","BSB_recdev","BSB_recns","BSB_refbias_hi","BSB_refbias_lo","BSB_uobs_hi","BSB_uobs_lo")){
# for(fsh in c('BSB','VS','RP', 'BSB_O','VS_O')){
  for(fsh in c('BSB','BSB_O')){

  for(res in scenarios){
    # results<-BSB_base
    # results<-get(res)
    data<-get(fsh)
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[c(1,9,10,2:8)])


    ## SSB/SSBMSY
    png(
      filename = paste0("Plots/", fsh,"_",res, "_SSBSSBMSY.png"),
      type = "cairo",
      units = "mm",
      width = 300,
      height = 225,
      pointsize = 18,
      res = 300
    )

    par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    max_y<-0
    for(ii in 1:dim(results@SB_SBMSY)[2]){
      max_y<-max(max_y, apply(results@SB_SBMSY[,ii,], 2, median) )
    }
    plot(apply(results@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, max(max_y*1.1, 1.1)), lwd=2, ylab="SSB/SSBMSY", xlab="Projected Years")
    abline(h=1)
    for(i in 2:dim(results@SB_SBMSY)[2]){
      lines(apply(results@SB_SBMSY[,i,], 2, median), type='l', lwd=2, lty=i, col=i)
    }
    legend("bottom", c(results@MPs), lwd=2, lty=1:dim(results@SB_SBMSY)[2], col=1:dim(results@SB_SBMSY)[2], bty='n', ncol=4, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1)
    dev.off()

    ## Catch
    png(
      filename = paste0("Plots/", fsh,"_",res, "_Catch.png"),
      type = "cairo",
      units = "mm",
      width = 300,
      height = 225,
      pointsize = 18,
      res = 300
    )
    par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    max_y<-0
    for(ii in 1:dim(results@Catch)[2]){
      max_y<-max(max_y, apply(results@Catch[,ii,], 2, median) )
    }
    plot(apply(results@Catch[,1,], 2, median), type='l',  lwd=2, ylim=c(0, max_y*1.02), ylab="Catch", xlab="Projected Years")
    abline(h=1)
    for(i in 2:dim(results@Catch)[2]){
      lines(apply(results@Catch[,i,], 2, median), type='l', lwd=2, lty=i, col=i)
    }
    legend("bottom", c(results@MPs), lwd=2, lty=1:dim(results@Catch)[2], col=1:dim(results@Catch)[2], bty='n', ncol=4, cex=0.75)
    mtext( paste0(fsh, " ", res, " Catch"), side=3, line=-1)
    dev.off()

  } # end for res

} # end for fsh

## one plot each -- new color
for(fsh in c('BSB','VS','RP')){
  # for(fsh in c('RP','RP_O')){

  for(res in scenarios){
    # results<-BSB_base
    # results<-get(res)
    data<-get(fsh)
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[c(1,9,10,2:8)])


    ## SSB/SSBMSY
    png(
      filename = paste0("Plots/", fsh,"_",res, "_SSBSSBMSY2.png"),
      type = "cairo",
      units = "mm",
      width = 300,
      height = 225,
      pointsize = 24,
      res = 300
    )

    par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    max_y<-0
    for(ii in 1:dim(results@SB_SBMSY)[2]){
      max_y<-max(max_y, apply(results@SB_SBMSY[,ii,], 2, median) )
    }
    plot(apply(results@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, max(max_y*1.1, 1.1)), lwd=2, ylab="SSB/SSBMSY", xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@SB_SBMSY[,i,], 2, median), type='l', lwd=2, lty=mylty[iorder], col=MP_colR2[iorder])
    }
    legend("bottom", MP_namesR_leg, lwd=2, lty=mylty, col=MP_colR2, bty='n', ncol=4, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1)
    dev.off()

    ## Catch
    png(
      filename = paste0("Plots/", fsh,"_",res, "_Catch2.png"),
      type = "cairo",
      units = "mm",
      width = 300,
      height = 225,
      pointsize = 24,
      res = 300
    )
    par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    max_y<-0
    for(ii in 1:dim(results@Catch)[2]){
      max_y<-max(max_y, apply(results@Catch[,ii,], 2, median) )
    }
    plot(apply(results@Catch[,1,], 2, median), type='l',  lwd=2, ylim=c(0, max_y*1.02), ylab="Catch", xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@Catch[,i,], 2, median), type='l', lwd=2, lty=mylty[iorder], col=MP_colR2[iorder])
    }
    legend("bottom", MP_namesR_leg, lwd=2, lty=mylty, col=MP_colR2, bty='n', ncol=4, cex=0.75)
    mtext( paste0(fsh, " ", res, " Catch"), side=3, line=-1)
    dev.off()

  } # end for res

} # end for fsh



# all on same plot -- SSB - new color
for(fsh in c('BSB','VS','RP')){
  # for(fsh in c('RP','RP_O')){
  ## SSB/SSBMSY
  png(
    filename = paste0("Plots/", fsh, "_ALL_SSBSSBMSY2.png"),
    type = "cairo",
    units = "mm",
    width = 300,
    height = 225,
    pointsize = 18,
    res = 300
  )

  par(mfrow=c(3,4), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)

  for(res in scenarios){
    # results<-BSB_base
    # results<-get(res)
    data<-get(fsh)
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[c(1,9,10,2:8)])
    mylty<-c(1:5, 1:5)

    ## PLOT ###
    max_y<-0
    for(ii in 1:dim(results@SB_SBMSY)[2]){
      max_y<-max(max_y, apply(results@SB_SBMSY[,ii,], 2, median) )
    }
    plot(apply(results@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, max(max_y*1.1, 1.1)), lwd=2, ylab="SSB/SSBMSY", xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@SB_SBMSY[,i,], 2, median), type='l', lwd=2, lty=mylty[iorder], col=MP_colR2[iorder])
    }

    mtext( paste0(fsh, " ", res), side=3, line=-1.1, cex=0.9)

  } # end for res
  plot.new()
  legend("center", MP_namesR_leg, lwd=2, lty=mylty, col=MP_colR2, bty='n', ncol=2, cex=1)

  dev.off()


} # end for fsh


# all on same plot - old SSB
for(fsh in c('BSB','VS','RP', 'BSB_O','VS_O')){
  # for(fsh in c('RP','RP_O')){
  ## SSB/SSBMSY
  png(
    filename = paste0("Plots/", fsh, "_ALL_SSBSSBMSY.png"),
    type = "cairo",
    units = "mm",
    width = 300,
    height = 225,
    pointsize = 18,
    res = 300
  )

  par(mfrow=c(3,4), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)

  for(res in scenarios){
    # results<-BSB_base
    # results<-get(res)
    data<-get(fsh)
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[c(1,9,10,2:8)])

    ## PLOT ###
    max_y<-0
    for(ii in 1:dim(results@SB_SBMSY)[2]){
      max_y<-max(max_y, apply(results@SB_SBMSY[,ii,], 2, median) )
    }
    plot(apply(results@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, max(max_y*1.1, 1.1)), lwd=2, ylab="SSB/SSBMSY", xlab="Projected Years")
    abline(h=1)
    for(i in 2:dim(results@SB_SBMSY)[2]){
      lines(apply(results@SB_SBMSY[,i,], 2, median), type='l', lwd=2, lty=i, col=i)
    }

    mtext( paste0(fsh, " ", res), side=3, line=-1.1, cex=0.9)

  } # end for res
  plot.new()
  legend("center", c(results@MPs), lwd=2, lty=1:dim(results@SB_SBMSY)[2], col=1:dim(results@SB_SBMSY)[2], bty='n', ncol=2, cex=0.75)

  dev.off()


} # end for fsh


# all on same plot - old Catch
for(fsh in c('BSB','VS','RP', 'BSB_O','VS_O')){
  # for(fsh in c('RP','RP_O')){
  ## catch
  png(
    filename = paste0("Plots/", fsh, "_ALL_Catch.png"),
    type = "cairo",
    units = "mm",
    width = 300,
    height = 225,
    pointsize = 18,
    res = 300
  )

  par(mfrow=c(3,4), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)

  for(res in scenarios){
    # results<-BSB_base
    # results<-get(res)
    data<-get(fsh)
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[c(1,9,10,2:8)])

    ## PLOT ###
    max_y<-0
    for(ii in 1:dim(results@Catch)[2]){
      max_y<-max(max_y, apply(results@Catch[,ii,], 2, median) )
    }
    plot(apply(results@Catch[,1,], 2, median), type='l',  lwd=2, ylim=c(0, max_y*1.02), ylab="Catch", xlab="Projected Years")
    abline(h=1)
    for(i in 2:dim(results@Catch)[2]){
      lines(apply(results@Catch[,i,], 2, median), type='l', lwd=2, lty=i, col=i)
    }

    mtext( paste0(fsh, " ", res, " Catch"), side=3, line=-1.1, cex=0.9)

  } # end for res
  plot.new()
  legend("center", c(results@MPs), lwd=2, lty=1:dim(results@SB_SBMSY)[2], col=1:dim(results@SB_SBMSY)[2], bty='n', ncol=2, cex=0.75)
  dev.off()


} # end for fsh


# all on same plot - new color Catch
for(fsh in c('BSB','VS','RP', 'BSB_O','VS_O')){
  # for(fsh in c('RP','RP_O')){
  ## catch
  png(
    filename = paste0("Plots/", fsh, "_ALL_Catch2.png"),
    type = "cairo",
    units = "mm",
    width = 300,
    height = 225,
    pointsize = 18,
    res = 300
  )

  par(mfrow=c(3,4), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)

  for(res in scenarios){
    # results<-BSB_base
    # results<-get(res)
    data<-get(fsh)
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[c(1,9,10,2:8)])

    ## PLOT ###
    max_y<-0
    for(ii in 1:dim(results@Catch)[2]){
      max_y<-max(max_y, apply(results@Catch[,ii,], 2, median) )
    }
    plot(apply(results@Catch[,1,], 2, median), type='l',  lwd=2, ylim=c(0, max_y*1.02), ylab="Catch", xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@Catch[,i,], 2, median), type='l', lwd=2, lty=mylty[iorder], col=MP_colR2[iorder])
    }


    mtext( paste0(fsh, " ", res, " Catch"), side=3, line=-1.1, cex=0.9)

  } # end for res
  plot.new()
  legend("center", MP_namesR_leg, lwd=2, lty=mylty, col=MP_colR2, bty='n', ncol=2, cex=1)
  # legend("center", c(results@MPs), lwd=2, lty=1:dim(results@SB_SBMSY)[2], col=1:dim(results@SB_SBMSY)[2], bty='n', ncol=2, cex=0.75)
  dev.off()


} # end for fsh




# quart1((results@SB_SBMSY[,1,5]))
# quart3((results@SB_SBMSY[,1,5]))
#####
# for(cMP in 1:length(BSB$base@MPs)){
#   png(
#     filename = paste0("Plots/", BSB$base@MPs[cMP], "_SSBSSBMSY.png"),
#     type = "cairo",
#     units = "mm",
#     width = 300,
#     height = 225,
#     pointsize = 16,
#     res = 300
#   )
#   plot(apply(BSB$base@SB_SBMSY[,cMP,], 2, median), type='l', ylim=c(0, 5), lwd=2)
#   abline(h=1)
#   for(res in scenarios[2:length(scenarios)]){
#     results<-get(res, BSB)
#     lines(apply(results@SB_SBMSY[,cMP,], 2, median), type='l', lwd=2, lty=which(scenarios==res), col=which(scenarios==res))
#   }
#   legend("topright", scenarios, lwd=2, lty=1:length(scenarios), col=1:length(scenarios), bty='n', ncol=2, cex=0.75)
#   mtext( paste0(BSB$base@MPs[cMP], " SSB/SSBMSY"), side=3, line=-1)
#   dev.off()
# }

###########



dim(BSB[[1]]@SB_SBMSY) # 100, 12, 50 [iterations, MPs, yrs]


P50(BSB$base, Yrs=c(40,50))
PNOF(BSB$base, Yrs=c(40,50))
AAVY(BSB$base)@MPs
AAVY(BSB$base)@Stat
summary(AAVY(BSB$base)@Stat)
slotNames(AAVY(BSB$base))

for(om in names(BSB)){
  msemod<-get(om,BSB)

  vioplot(AAVY(msemod)@Stat, names=AAVY(msemod)@MPs, ylim=c(0,2))
}
which(AAVY(msemod)@Stat[,8]>1000)
msemod@SSB[which(AAVY(msemod)@Stat[,8]>1000),8,]
msemod@Catch[which(AAVY(msemod)@Stat[,8]>1000),8,] #-- NEED TO BUILD IN MIN CATCH VALUE INTO myItarget_BSB -- catches <<1, which is blowing up AAVY calcs

boxplot(AAVY(BSB$base)@Stat)



dim(Yield(BSB$base)@Stat)
boxplot()


boxplot(c(Yield(BSB$base)@Stat[,1,]), c(Yield(BSB$base)@Stat[,2,]), c(Yield(BSB$base)@Stat[,3,]),
        c(Yield(BSB$base)@Stat[,4,]), c(Yield(BSB$base)@Stat[,5,]), c(Yield(BSB$base)@Stat[,6,]),
        c(Yield(BSB$base)@Stat[,7,]), c(Yield(BSB$base)@Stat[,8,]), c(Yield(BSB$base)@Stat[,9,]),
        c(Yield(BSB$base)@Stat[,10,]) )

# SB_SBMSY_SCA1 -- dimensions [iterations, nyears, scenario]

## GET SB_SBMSY, F_FMSY,  RESULTS COLLATED ACROSS MPs.
CollateMPResults<-function(dataN='BSB', stat=c('SB_SBMSY','F_FMSY', 'Catch')){
  data<-get(dataN)
  MP_names<-data[[1]]@MPs

  if('SB_SBMSY' %in% stat){
    for(i in 1:length(MP_names)){
      x<-array(data=c(data[[1]]@SB_SBMSY[,i,],
                      data[[2]]@SB_SBMSY[,i,],
                      data[[3]]@SB_SBMSY[,i,],
                      data[[4]]@SB_SBMSY[,i,],
                      data[[5]]@SB_SBMSY[,i,],
                      data[[6]]@SB_SBMSY[,i,],
                      data[[7]]@SB_SBMSY[,i,],
                      data[[8]]@SB_SBMSY[,i,],
                      data[[9]]@SB_SBMSY[,i,],
                      data[[10]]@SB_SBMSY[,i,],
                      data[[11]]@SB_SBMSY[,i,]
      ),
      dim=c(dim(data[[1]]@SB_SBMSY)[1],dim(data[[1]]@SB_SBMSY)[3],length(scenarios)))
      assign(paste0(dataN, "_SB_SBMSY_",MP_names[i]), x, envir=globalenv())
    } # end for loop
  } # end SB_SBMSY


  if('F_FMSY' %in% stat){
    for(i in 1:length(MP_names)){
      x<-array(data=c(data[[1]]@F_FMSY[,i,],
                      data[[2]]@F_FMSY[,i,],
                      data[[3]]@F_FMSY[,i,],
                      data[[4]]@F_FMSY[,i,],
                      data[[5]]@F_FMSY[,i,],
                      data[[6]]@F_FMSY[,i,],
                      data[[7]]@F_FMSY[,i,],
                      data[[8]]@F_FMSY[,i,],
                      data[[9]]@F_FMSY[,i,],
                      data[[10]]@F_FMSY[,i,],
                      data[[11]]@F_FMSY[,i,]
      ),
      dim=c(dim(data[[1]]@F_FMSY)[1],dim(data[[1]]@F_FMSY)[3],length(scenarios)))
      assign(paste0(dataN, "_F_FMSY_",MP_names[i]), x, envir=globalenv())
    } # end for loop
  } # end F_FMSY



  if('Catch' %in% stat){
    for(i in 1:length(MP_names)){
      x<-array(data=c(data[[1]]@Catch[,i,],
                      data[[2]]@Catch[,i,],
                      data[[3]]@Catch[,i,],
                      data[[4]]@Catch[,i,],
                      data[[5]]@Catch[,i,],
                      data[[6]]@Catch[,i,],
                      data[[7]]@Catch[,i,],
                      data[[8]]@Catch[,i,],
                      data[[9]]@Catch[,i,],
                      data[[10]]@Catch[,i,],
                      data[[11]]@Catch[,i,]
      ),
      dim=c(dim(data[[1]]@Catch)[1],dim(data[[1]]@Catch)[3],length(scenarios)))
      assign(paste0(dataN, "_Catch_",MP_names[i]), x, envir=globalenv())
    } # end for loop
  } # end Catch



} # end function

CollateMPResults(dataN='BSB')
CollateMPResults(dataN='BSB_O')
CollateMPResults(dataN='VS')
CollateMPResults(dataN='VS_O')
CollateMPResults(dataN='RP')
CollateMPResults(dataN='RP_O')




### COLLATE AAVY, terminal relative SSB ratio, terminal relative F ratio, terminal yield, cumulative yield
CollatePMs <- function(dataN='BSB', stat=c('AAVY','trelSSB','trelF','tyield','cyield', 'PNOF', 'P100')){
  data<-get(dataN)
  MP_names<-data[[1]]@MPs
  returnlist<-list()

  #AAVY
  if('AAVY' %in% stat){
    AAVYs<-AAVY(data[[1]])@Stat
    for(i in 2:length(data)){
      AAVYs<-rbind(AAVYs, AAVY(data[[i]])@Stat)
    } # end for loop

    colnames(AAVYs)<-MP_names
    returnlist$AAVY<-AAVYs
  } # end AAVY


  #terminal relative SSB ratio
  if('trelSSB' %in% stat){
    trelSSB<-P100(data[[1]], Yrs=-1)@Stat
    for(i in 2:length(data)){
      trelSSB<-rbind(trelSSB, P100(data[[i]], Yrs=-1)@Stat)
    } # end for loop

    colnames(trelSSB)<-MP_names
    returnlist$trelSSB<-trelSSB
  } # end trelSSB


  #terminal F ratio
  if('trelF' %in% stat){
    trelF<-PNOF(data[[1]], Yrs=-1)@Stat
    for(i in 2:length(data)){
      trelF<-rbind(trelF, PNOF(data[[i]], Yrs=-1)@Stat)
    } # end for loop

    colnames(trelF)<-MP_names
    returnlist$trelF<-trelF
  } # end trelF


  # terminal yield
  if('tyield' %in% stat){
    tyield<-Yield(data[[1]], Yrs=-1)@Stat
    for(i in 2:length(data)){
      tyield<-rbind(tyield, Yield(data[[i]], Yrs=-1)@Stat)
    } # end for loop

    colnames(tyield)<-MP_names
    returnlist$tyield<-tyield
  } # end tyield


  # cumulative yield
  if('cyield' %in% stat){

    # build function to total yield for each iteration
    SumYieldMP<-function(dataMSE){
      SumY<-apply(Yield(dataMSE)@Stat[,1,],1,sum)
      for(j in 2:dataMSE@nMPs){
        SumY<-cbind(SumY, apply(Yield(dataMSE)@Stat[,j,],1,sum) )
      } # end j for loop
      return(SumY)
    }# end SumYieldMP

    cyield<-SumYieldMP(data[[1]])
    for(i in 2:length(data)){
      cyield<-rbind(cyield, SumYieldMP(data[[1]]))
    } # end for loop

    colnames(cyield)<-MP_names
    returnlist$cyield<-cyield
  } # end cyield



  #PNOF
  if('PNOF' %in% stat){
    PNOFs<-PNOF(data[[1]])@Prob
    for(i in 2:length(data)){
      PNOFs<-rbind(PNOFs, PNOF(data[[i]])@Prob)
    } # end for loop

    colnames(PNOFs)<-MP_names
    returnlist$PNOF<-PNOFs
  } # end PNOF



  #P100
  if('P100' %in% stat){
    P100s<-P100(data[[1]])@Prob
    for(i in 2:length(data)){
      P100s<-rbind(P100s, P100(data[[i]])@Prob)
    } # end for loop

    colnames(P100s)<-MP_names
    returnlist$P100<-P100s
  } # end P100



  return(returnlist)
}
BSB_PMs<-CollatePMs('BSB')

BSB_AAVY[which(BSB_AAVY>10)]


# Build function to reorder list of CMPs
# orderedMPs<-c(1, 9:10, 2:8)
Reorder_MPs<-function(PMs_obj, orderedMPs=c(1, 9:10, 2:8)){
  for(l in 1:length(PMs_obj)){
    PMs_obj[[l]]<-PMs_obj[[l]][,orderedMPs]
  } # end l loop
  return(PMs_obj)
}

### HAVE TO GO BACK AND CORRECT FOR F==INF

BSB_PM<-Reorder_MPs(BSB_PMs)

par(mfrow=c(3,2))

vioplot(BSB_PM$AAVY, ylim=c(0,2), col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'))
vioplot(BSB_PM$trelSSB, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'))
vioplot(BSB_PM$trelSSB, ylim=c(0,5), col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'))
abline(h=1)
# vioplot(BSB_PM$trelF, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'))
vioplot(BSB_PM$tyield,  col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'))
vioplot(BSB_PM$cyield,  col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'))


vioplot(AAVY(BSB$base)@Stat, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'))


## PLOT VIOLIN PLOTS OF PMs###########################################
SPP<-BSB
par(mfrow=c(4,3), mar=c(2.1,2.3,0.6,0.6), mgp=c(1, 0.3, 0), tck=-0.01)
Plot_trelSSB<-function(SPP, ylims=c(NULL)){
  par(mfrow=c(4,3))
  for(ii in 1:length(names(SPP))){
    vioplot(P100(SPP[[ii]], Yrs=-1)@Stat, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'), names=MP_namesR_leg, ylim=ylims); abline(h=1)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("SSB ratio", 2, line=1.1)
  }# end for loop
}
Plot_PNOF<-function(SPP, ylims=c(NULL), refline=NULL){
  par(mfrow=c(4,3))
  for(ii in 1:length(names(SPP))){
    vioplot(PNOF(SPP[[ii]])@Prob, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'), names=MP_namesR_leg, ylim=ylims); abline(h=1); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("PNOF", 2, line=1.1)
  }# end for loop
}
Plot_P100<-function(SPP, ylims=c(NULL), refline=NULL){
  par(mfrow=c(4,3))
  for(ii in 1:length(names(SPP))){
    vioplot(P100(SPP[[ii]],Yrs=-10)@Prob, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'), names=MP_namesR_leg, ylim=ylims); abline(h=1); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("Prob SSB_40-50>SSBMSY", 2, line=1.1)
  }# end for loop
}


Plot_trelSSB(BSB)
Plot_trelSSB(BSB, ylims=c(0, 6))

Plot_PNOF(BSB, refline=0.5, ylims=c(0,1.1))

Plot_P100(BSB, refline=0.5, ylims=c(0,1.1))


#### CUMULATIVE PLTOS ACROSS OMs ####
Plot_cumul_trelSSB<-function(SPP_PM, ylims=c(NULL)){
  vioplot(SPP_PM$trelSSB, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'), names=MP_namesR_leg, ylim=ylims); abline(h=1)
    mtext("SSB ratio", 2, line=1.1)
}

Plot_cumul_PNOF<-function(SPP_PM, ylims=c(NULL), refline=NULL){
    vioplot(SPP_PM$PNOF, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'), names=MP_namesR_leg, ylim=ylims); abline(h=1); abline(h=refline, lty=2)
    mtext("PNOF", 2, line=1.1)
}
Plot_cumul_P100<-function(SPP_PM, ylims=c(NULL), refline=NULL){
  vioplot(SPP_PM$P100, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'), names=MP_namesR_leg, ylim=ylims); abline(h=1); abline(h=refline, lty=2)
  mtext("Prob SSB_40-50>SSBMSY", 2, line=1.1)
}

Plot_cumul_AAVY<-function(SPP_PM, ylims=c(NULL), refline=NULL){
  vioplot(SPP_PM$AAVY, col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue'), names=MP_namesR_leg, ylim=ylims); abline(h=1); abline(h=refline, lty=2)
  mtext("AAVY", 2, line=1.1)
}

Plot_cumul_trelSSB(BSB_PM, ylims=c(0,5))
Plot_cumul_PNOF(BSB_PM, refline=0.5)
Plot_cumul_P100(BSB_PM, refline=0.5)
Plot_cumul_AAVY(BSB_PM, ylims=c(0, 1))


TradePlot(BSB$base) #regresses c("STY", "LTY", "P10", "AAVY")

avail(PM)


plot(BSB$base)


############ TRADEOFF PLOTS FUNCTION ####

names(BSB_PM)
MP_namesR_leg
MP_namesR_col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue')

as.numeric(summary(BSB_PM$trelSSB)[3,])

apply(BSB_PM$trelSSB, 2, median)
apply(BSB_PM$trelSSB, 2, function(x) quantile(x, probs=0.1))
apply(BSB_PM$trelSSB, 2, function(x) quantile(x, probs=0.9))

par(mfrow=c(1,1))
SPP_PM<-BSB_PM
PMx<-'trelSSB'
PMy<-'cyield'

TOff_Plot<- function(SPP_PM, PMx, PMy){
  xx<-get(PMx, SPP_PM)
  yy<-get(PMy, SPP_PM)
  plot(apply(xx, 2, median), apply(yy, 2, median), pch=16, col=MP_namesR_col,
       ylim=c( (min(apply(yy, 2, median))-0.025*min(apply(yy, 2, median))),
               (max(apply(yy, 2, median))+0.02*max(apply(yy, 2, median))) ),
       xlim=c( (min(apply(xx, 2, median))-0.02*min(apply(xx, 2, median))),
               (max(apply(xx, 2, median))+0.025*max(apply(xx, 2, median))) ),
       ylab=PMy, xlab=PMx)
  text((apply(xx, 2, median) + 0.01*apply(xx, 2, median)),
       (apply(yy, 2, median) - 0.01*apply(yy, 2, median)), labels=MP_namesR_leg, col=MP_namesR_col)

}

TOff_Plot(SPP_PM=BSB_PM, PMy='trelSSB', PMx='cyield'); abline(h=1)
TOff_Plot(SPP_PM=BSB_PM, PMy='AAVY', PMx='cyield')



###########
# for(i in 1:length(MP_names)){
#   x<-array(data=c(BSB[[1]]@SB_SBMSY[,i,],
#                   BSB[[2]]@SB_SBMSY[,i,],
#                   BSB[[3]]@SB_SBMSY[,i,],
#                   BSB[[4]]@SB_SBMSY[,i,],
#                   BSB[[5]]@SB_SBMSY[,i,],
#                   BSB[[6]]@SB_SBMSY[,i,],
#                   BSB[[7]]@SB_SBMSY[,i,],
#                   BSB[[8]]@SB_SBMSY[,i,],
#                   BSB[[9]]@SB_SBMSY[,i,],
#                   BSB[[10]]@SB_SBMSY[,i,],
#                   BSB[[11]]@SB_SBMSY[,i,]
#   ),
#   dim=c(dim(BSB[[1]]@SB_SBMSY)[1],dim(BSB[[1]]@SB_SBMSY)[3],length(scenarios)))
#   assign(paste0("SB_SBMSY_",MP_names[i]), x)
# }

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
##########

### BOXPLOT

# MP_names<-BSB[[1]]@MPs
# MP_namesR<-c(MP_names[c(1,9,10,2:8)])

## BOXPLOT OF COLLATED SB_SBMSY RESULTS
bxplt<-function(dataN="BSB", save=F, col_scheme=1){
  data<-get(dataN)
  MP_names<-data[[1]]@MPs
  MP_namesR<-c(MP_names[c(1,9,10,2:8)])
  MP_namesR_Gen<-c("SCA_1","SCA_5","SCA_10", "pMP_5","pMP_10", "GB_target","ICI","Iratio","IT10","Itarget")
  MP_colR<-c("deepskyblue","deepskyblue3","deepskyblue4", "lightskyblue","lightskyblue1",
             "orange","red", "violetred","darkolivegreen1","darkorchid")
  if(col_scheme==2){MP_colR<-c('white',c_mods[2:5], c_emps)  }

  if(save==T){
    ## SSB/SSBMSY
    png(
      filename = paste0("Plots/BOXPLOT_", dataN,"_SSBSSBMSY.png"),
      type = "cairo",
      units = "mm",
      width = 300,
      height = 225,
      pointsize = 16,
      res = 300
    )
  }

  par(mar=c(2.1, 2.6, 1.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
  boxplot(c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[1]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[2]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[3]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[4]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[5]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[6]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[7]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[8]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[9]))[,50,]),
          c(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[10]))[,50,]),
          ylim=c(0,5),  col=MP_colR, names=MP_namesR_Gen, cex.axis=0.7)  #
  abline(h=1)
  mtext("SSB/SSBMSY", side=2, line=1.5)
  mtext(dataN, side=3, line=0.25)

  if(save==T){ dev.off() }


}
bxplt("BSB", save=T, col_scheme=2)
bxplt("BSB_O", save=T)
bxplt("VS", save=T)
bxplt("VS_O", save=T)
bxplt("RP", save=T)
bxplt("RP_O", save=T)

bxplt("RP")

bxplt_sep<-function(dataN="BSB", save=F){
  data<-get(dataN)
  MP_names<-data[[1]]@MPs
  MP_namesR<-c(MP_names[c(1,9,10,2:8)])
  MP_namesR_Gen<-c("SCA_1","SCA_5","SCA_10", "pMP_5","pMP_10", "GB_target","ICI","Iratio","IT10","Itarget")
  MP_colR<-c("deepskyblue","deepskyblue3","deepskyblue4", "lightskyblue","lightskyblue1",
             "orange","red", "violetred","darkolivegreen1","darkorchid")

  if(save==T){
    ## SSB/SSBMSY
    png(
      filename = paste0("Plots/BOXPLOT_all_", dataN,"_SSBSSBMSY.png"),
      type = "cairo",
      units = "mm",
      width = 600,
      height = 225,
      pointsize = 16,
      res = 300
    )
  }
  OMs <- c('base',  'age0M_hi' ,   'age0M_lo' ,  'epiM' ,  'recdev_hi' ,  'recdev_lo' ,  'recns' ,  'refbias_hi' ,  'refbias_lo' ,  'uobs_hi' ,  'uobs_lo' )
  OM_cols <- c('white','darkolivegreen1','darkolivegreen','red','deepskyblue','deepskyblue4','royalblue','darkorchid1','darkorchid4','orange','darkorange3')

  par(mfrow=c(2, 5), mar=c(2.1, 2.6, 1.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
  for(i in 1:length(MP_namesR)){
    boxplot(get(paste0(dataN,"_SB_SBMSY_", MP_namesR[i]))[,50,],
            ylim=c(0,5),  col=OM_cols, names=OMs, cex.axis=0.7)  #
    abline(h=1)
    mtext("SSB/SSBMSY", side=2, line=1.5)
    mtext(MP_namesR[i], side=3, line=0.25)
  }


  if(save==T){ dev.off() }


}
bxplt_sep("BSB", save=T)
bxplt_sep("BSB_O", save=T)
bxplt_sep("VS", save=T)
bxplt_sep("VS_O", save=T)
bxplt_sep("RP", save=T)
bxplt_sep("RP_O", save=T)

### merge BSB w BSB_O


# MP_namesR<-c("SCA_1","SCA_5","SCA_10", "pMP_5", "pMP_10","GB_slope1", "GB_target1", "ICI2", "myIratio", "myIT10", "myItarget", "L95target")
# MP_colR<-c("deepskyblue","deepskyblue3","deepskyblue4", "lightskyblue","lightskyblue1", "orange","slateblue","red", "violetred","darkolivegreen1","turquoise1","darkorchid")
#
# boxplot(c(SB_SBMSY_SCA_1[,50,]), c(SB_SBMSY_SCA_5[,50,]), c(SB_SBMSY_SCA_10[,50,]),
#         c(SB_SBMSY_pMP_5[,50,]), c(SB_SBMSY_pMP_10[,50,]),
#         c(SB_SBMSY_GB_slope1[,50,]),c(SB_SBMSY_GB_target1[,50,])
#         ,c(SB_SBMSY_ICI2[,50,]), c(SB_SBMSY_myIratio[,50,]),
#         c(SB_SBMSY_myIT10[,50,]), c(SB_SBMSY_myItarget[,50,]),
#         c(SB_SBMSY_L95target[,50,]), ylim=c(0,5), col=MP_colR, names=MP_namesR, cex.axis=0.7) #col=c(),
# abline(h=1)
# mtext("SSB/SSBMSY", side=2, line=1.5)
# mtext("Black Sea Bass", side=3, line=1.0)






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


Uncert_plot<-function(dataN="BSB", stat="SB_SBMSY", mps, cols=1:10, quant1=0.1, quant3=0.9){
  for(i in 1:length(mps)){
    results<-get(paste(dataN, stat,mps[i],sep="_"))

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

Uncert_plot(mps=c('SCA_1','SCA_5','GB_target_BSB'))
Uncert_plot(mps=c('SCA_1','myIT10_BSB','myICI_BSB'))
Uncert_plot(mps=c('SCA_1','SCA_5','myItarget'))

AAVY(BSB)

AAVY(BSB$base)@Stat




AAVY_plot<-function(dataN){
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
}

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


##################################### NOTE
# OLD PlotRelPM
PlotRelPM<-function(sp, calc='rd', title=NULL,
                    Pstat=c('AAVY','trelSSB','treldSSB0','trelF','t10relSSB','t10reldSSB0',
                            't10relF','tyield','cyield','PNOF','P100','P90'),
                    ylims=c(NULL), baserefline=NULL, refline=NULL, MPnam=MP_namesR_abbrev,
                    MPcol=MP_R_col, mf=c(4,3), oMPs=orderedMPs, minylim=NULL, maxylim=NULL,
                    xlab.cex=0.79){
  # par(cex.axis=1)
  dat<-get(paste(sp,'RelPM',calc, sep="_"))
  datStat<-get(Pstat, dat)
  par(mfrow=mf, oma=c(0,0,2.2,0))


  # BaseRes<-get(paste0(sp,"_PM"))$'t10reldSSB0'

  ## get Base Results
  if(Pstat=="trelSSB"){
    BaseRes<-P100(get(sp)$base, Yrs=-1)@Stat
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  } #trelSSB
  if(Pstat=="trelF"){
    BaseRes<-PNOF(get(sp)$base, Yrs=-1)@Stat
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  } #trelF
  if(Pstat=="t10relSSB"){
    BaseRes<-apply(P100(get(sp)$base, Yrs=-10)@Stat, c(1,2), mean)
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  } #t10relSSB

  if(Pstat=="t10reldSSB0"){
    BaseRes<-get(paste0(sp,"_PM"))$'t10reldSSB0'
  }
  if(Pstat=="t10reldSSB0"){
    BaseRes<-get(paste0(sp,"_PM"))$'t10reldSSB0'
  }

  if(Pstat=="t10relF"){
    BaseRes<-apply(PNOF(get(sp)$base, Yrs=-10)@Stat, c(1,2), mean)
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  } #t10relF
  if(Pstat=="PNOF"){
    BaseRes<-PNOF(get(sp)$base)@Prob
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  } #PNOF
  if(Pstat=="P100"){
    BaseRes<-P100(get(sp)$base,Yrs=-10)@Prob
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  } #P100
  if(Pstat=="P90"){
    BaseRes<-P100(get(sp)$base, Ref=0.9,Yrs=-10)@Prob
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  } #P100
  if(Pstat=="AAVY"){
    BaseRes<-P100(get(sp)$base)@Prob
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  }#AAVY
  if(Pstat=="tyield"){
    BaseRes<-Yield(get(sp)$base, Yrs=-1)
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  }#tyield
  if(Pstat=="cyield"){
    BaseRes<-SumYieldMP(get(sp)$base)
    colnames(BaseRes)<-get(sp)$base@MPs
    BaseRes<-BaseRes[,oMPs]
  }#cyield

  # Plot BaseRes
  ylims1=c(minylim[1],maxylim[1])    # get ylims
  # boxplot(BaseRes, col=MPcol, names=MPnam, ylim=ylims1)
  boxplot(BaseRes, col=MPcol, names=MPnam, ylim=ylims1, axes=F)
  axis(2)
  axis(1, at=1:dim(BaseRes)[2], labels=MPnam, cex.axis=xlab.cex)
  # axis(1, at=1:dim(BaseRes)[2], labels=F)
  # text(x=1:dim(BaseRes)[2]-0.1, y=par("usr")[3]-0.25, xpd=NA, labels=MPnam, srt=35, cex=xlab.cex)
  box()
  abline(h=baserefline)
  mtext("Base Results", 3, line=-1.1)
  mtext(Pstat, 2, line=1.1)

  # plot differences for each
  for(ss in names(datStat)){

    if(is.null(ylims)){                              # get ylims
      if(!is.null(minylim) && !is.null(maxylim)){
        ylims1<-c(minylim[which(names(datStat)==ss)+1], maxylim[which(names(datStat)==ss)+1])
      } else{ylims<-NULL} # end nested if-else
    }else{ ylims1<-ylims} # end outer if-else

    # boxplot(get(ss,datStat), col=MPcol, names=MPnam, ylim=ylims1, na.rm=T)
    boxplot(get(ss,datStat), col=MPcol, names=MPnam, ylim=ylims1, na.rm=T, axes=F)
    axis(2)
    axis(1, at=1:dim(BaseRes)[2], labels=MPnam, cex.axis=xlab.cex)
    # axis(1, at=1:dim(BaseRes)[2], labels=F)
    # text(x=1:dim(BaseRes)[2]-0.1, y=par("usr")[3]-0.25, xpd=NA, labels=MPnam, srt=35, cex=xlab.cex)
    box()
    mtext("Difference", 2, line=1)
    mtext(ss, 3, line=-1.1)
    abline(h=refline)
  }# end scenario / OM loop
  if(is.null(title)) title<-sp
  mtext(title, 3, line=1, outer=T)
  mtext(Pstat, 3, line=0, outer=T)
}


# stat-specific vioplots
Plot_trelSSB<-function(SPP, ylims=c(NULL), oMPs=orderedMPs, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                       mf=c(4,3), xlab.cex=0.79){
  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    if(ii==4){ylims1=c(0,5)} ## for epiM
    if(ii!=4){ylims1=ylims}
    vioplot(P100(SPP[[ii]], Yrs=-1)@Stat[,oMPs], col=MPcol, names=NA, ylim=ylims1, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=1)

    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("SSB ratio", 2, line=1.1)
  }# end for loop
}
Plot_treldSSB0<-function(SPP, ylims=c(NULL), oMPs=orderedMPs, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                         mf=c(4,3), xlab.cex=0.79){
  par(mfrow=mf)
  l1<-dim(SPP[[1]]@SSB[,1,])[2]
  l2<-dim(SPP[[1]]@RefPoint$Dynamic_Unfished$SSB0)[2]

  for(ii in 1:length(names(SPP))){
    ylims1=ylims

    tSSBtmp<-c()
    for(c in 1:dim(SPP[[ii]]@SSB)[2]){
      tSSBtmp<-cbind(tSSBtmp, SPP[[ii]]@SSB[,c,l1] / SPP[[ii]]@RefPoint$Dynamic_Unfished$SSB0[,l2])
    }
    tSSBtmp_r <- tSSBtmp[,oMPs]

    vioplot(tSSBtmp_r, col=MPcol, names=NA, ylim=ylims1, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=1); abline(h=0.5, lty=2)

    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("SSB ratio", 2, line=1.1)
  }# end for loop
}

Plot_t10reldSSB0<-function(SPP, ylims=c(NULL),  MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                           mf=c(4,3), xlab.cex=0.79){
  par(mfrow=mf)
  l1<-dim(SPP[[1]]@SSB[,1,])[2]
  l2<-dim(SPP[[1]]@RefPoint$Dynamic_Unfished$SSB0)[2]

  for(ii in 1:length(names(SPP))){
    ylims1=ylims

    tSSBtmp<-c()
    for(c in 1:dim(SPP[[ii]]@SSB)[2]){
      tSSBtmp<-cbind(tSSBtmp, SPP[[ii]]@SSB[,c,l1] / SPP[[ii]]@RefPoint$Dynamic_Unfished$SSB0[,l2])
    }
    tSSBtmp_r <- tSSBtmp[,oMPs]

    vioplot(tSSBtmp_r, col=MPcol, names=NA, ylim=ylims1, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=1); abline(h=0.5, lty=2)

    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("SSB ratio", 2, line=1.1)
  }# end for loop
}

Plot_trawSSB<-function(SPP, ylims=c(NULL), oMPs=orderedMPs, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                       mf=c(4,3), xlab.cex=0.79, refMSY=T){
  par(mfrow=mf)
  l1<-dim(SPP[[1]]@SSB[,1,])[2]

  for(ii in 1:length(names(SPP))){
    if(ii!=4){ylims1=ylims}

    tSSBtmp<-c()
    for(c in 1:dim(SPP[[ii]]@SSB)[2]){
      tSSBtmp<-cbind(tSSBtmp, SPP[[ii]]@SSB[,c,l1])
    }
    tSSBtmp_r <- tSSBtmp[,oMPs]

    vioplot(tSSBtmp_r, col=MPcol, names=NA, ylim=ylims1, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box()#; abline(h=1); abline(h=0.5, lty=2)
    dim(SPP[[ii]]@RefPoint$MSY)
    if(refMSY==T){
      abline(h=SPP[[ii]]@RefPoint$SSBMSY[1,1, dim(SPP[[ii]]@RefPoint$SSBMSY)[3] ], lty=2)
    }
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("SSB", 2, line=1.1)
  }# end for loop
}
Plot_trelF<-function(SPP, ylims=c(NULL), oMPs=orderedMPs, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                     mf=c(4,3), xlab.cex=0.79){


  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    yyy<-PNOF(SPP[[ii]], Yrs=-1)@Stat # get observed values for EpiM
    yyy[which(yyy==Inf)]<-100            # remove Inf values with very large F/FMSY values ==100
    if(ii==4){ ylims1<-c(0,10) }
    if(ii!=4){ ylims1<-ylims }
    vioplot(yyy[,oMPs], col=MPcol, names=MPnam, ylim=ylims1, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=NA, cex.axis=xlab.cex)
    box(); abline(h=1)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("F ratio", 2, line=1.1)
  }# end for loop
}
Plot_t10relSSB<-function(SPP, ylims=c(NULL), oMPs=orderedMPs, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                         mf=c(4,3), xlab.cex=0.79){
  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    if(ii==4){ylims1=c(0,5)} ## for epiM
    if(ii!=4){ylims1=ylims}
    vioplot(apply(P100(SPP[[ii]], Yrs=-10)@Stat, c(1,2), mean)[,oMPs], col=MPcol, names=NA, ylim=ylims1, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=1)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("SSB ratio", 2, line=1.1)
  }# end for loop
}
Plot_t10relF<-function(SPP, ylims=c(NULL), oMPs=orderedMPs, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                       mf=c(4,3), xlab.cex=0.79){
  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    yyy<-PNOF(SPP[[ii]], Yrs=-10)@Stat # get observed values for EpiM
    yyy[which(yyy==Inf)]<-100            # remove Inf values with very large F/FMSY values ==100
    if(ii==4){ ylims1<-c(0,10) }        # limit yaxes for EpiM scenario
    if(ii!=4){ ylims1<-ylims }
    vioplot(apply(yyy, c(1,2), mean)[,oMPs], col=MPcol, names=NA, ylim=ylims, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=1)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("F ratio", 2, line=1.1)
  }# end for loop
}
Plot_PNOF<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                    mf=c(4,3), xlab.cex=0.79, oMPs=orderedMPs){
  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    vioplot(PNOF(SPP[[ii]])@Prob[,oMPs], col=MPcol, names=NA, ylim=ylims, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=1); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("PNOF", 2, line=1.1)
  }# end for loop
}
Plot_P100<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                    mf=c(4,3), xlab.cex=0.79, oMPs=orderedMPs, rref=1){
  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    vioplot(P100(SPP[[ii]], Ref=rref,Yrs=-10)@Prob[,oMPs], col=MPcol, names=NA, ylim=ylims, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=1); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext(expression("Prob SSB"['40-50']~">SSB"['MSY']), 2, line=1.1, cex=0.7)
  }# end for loop
}
Plot_AAVY<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                    mf=c(4,3), xlab.cex=0.79, oMPs=orderedMPs){
  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    vioplot(AAVY(SPP[[ii]])@Stat[,oMPs], col=MPcol, names=NA, ylim=ylims, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("AAVY", 2, line=1.1)
  }# end for loop
}
Plot_tyield<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                      mf=c(4,3), xlab.cex=0.79, oMPs=orderedMPs){
  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    vioplot(Yield(SPP[[ii]], Yrs=-1)@Stat[,oMPs], col=MPcol, names=NA, ylim=ylims, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("tYield", 2, line=1.1)
  }# end for loop
}
Plot_cyield<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                      oMPs=orderedMPs, mf=c(4,3), xlab.cex=0.79){
  par(mfrow=mf)
  dat<-lapply(SPP, SumYieldMP)
  for(ii in 1:length(names(SPP))){
    vioplot(dat[[ii]][,oMPs], col=MPcol, names=NA, ylim=ylims, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("tYield", 2, line=1.1)
  }# end for loop
}


Plot_trelSSB(SPP=spec) # terminal year relSSB
Plot_treldSSB0(SPP=spec, ylims=c(0,1.1))
Plot_trawSSB(SPP=spec)
Plot_trelF(SPP=spec)
Plot_t10relSSB(SPP=spec) # terminal 10 years mean relSSB
# Plot_t10reldSSB10(SPP=spec) # terminal 10 years mean relSSB
Plot_t10relF(SPP=spec)
Plot_PNOF(SPP=spec, refline=0.5, ylims=c(0,1.2))
Plot_P100(SPP=spec, refline=0.5, ylims=c(0,1.2))
Plot_P100(SPP=spec, rref=0.9, refline=0.5, ylims=c(0,1.2))
Plot_AAVY(SPP=spec, refline=0.30, ylims=c(0,1.5))
Plot_AAVY(SPP=spec, refline=0.30)

