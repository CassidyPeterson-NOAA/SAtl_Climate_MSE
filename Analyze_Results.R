## CLEANED MSE PLOTTING AND ANALYSIS CODE ###

#---------------------------------------------------- LOAD PACKAGES -------------------------------------------------

library(openMSE)
library(vioplot)
# library(RColorBrewer)
# display.brewer.all()
# library(paletteer)

avail("PM")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/merge_MSE.R")



# ---------------------------------------------- Homemade functions ----------------------------------------------------
quart1<-function(y, type="1st Qu."){
  return(summary(c(y))[type])
}
quart3<-function(y, type="3rd Qu."){
  return(summary(c(y))[type])
}

#make a function transparent
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


# Deal with nonsensical ref points related to high (episodic) values (where SSB_SSBMSY == Inf, set SSBMSY == dynamic SSB0)
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

## BUILD FUNCTIONS ##
# Get Results collated into one list (BSB)
GetResults<-function(species=species, sp=sp, oScenarios=orderedScenarios){
  if(grepl(species, pattern="Over")){ # get files of matching species name (correcting for Over results)
    files1<-list.files(set.file, pattern=species)
  } else {
    files1<-grep(list.files(set.file, pattern=species), pattern="Over", invert=TRUE, value=TRUE) #
  }

  temp_res<-list() # create empty list with sp title.
  for(f in 1:length(files1)){ # get results for each OM
    lab1 <- gsub("MSE_|\\..*", "", files1[f]); lab2<- gsub(species, sp, lab1); lab3<-gsub(paste0(sp,"_"),"",lab2) # get rds label
    assign(lab2, readRDS(file.path(set.file,files1[f]))) # get results for each OM
    temp_res[lab3]<-get(lab2) # # save OM results in a single object titled sp
  } # end for loop
  temp_res<-temp_res[oScenarios]
  # assign(sp, temp_res)
  return(temp_res)
}

# COLLATE AAVY, terminal relative SSB ratio, terminal relative F ratio, terminal yield, cumulative yield
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

# Build function to reorder list of CMPs
Reorder_MPs<-function(PMs_obj, oMPs=orderedMPs){
  for(l in 1:length(PMs_obj)){
    PMs_obj[[l]]<-PMs_obj[[l]][,oMPs]
  } # end l loop
  return(PMs_obj)
}



#### PLOTTING FUNCTIONS ####
### trajectories ###
par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
## one plot each -- new color
Plot_SSBtraj<-function( fsh, scenarios=NULL, save.png=F,
                        oMPs=orderedMPs, oScenarios=orderedScenarios,
                        colsR=MP_R_col, namesR=MP_namesR_leg){

  data<<-get(fsh)
  if(is.null(scenarios)){
    scenarios<-names(data)[oScenarios]
  } # end get scenarios

  for(res in scenarios){
    results<<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[oMPs])


    ## SSB/SSBMSY
    if(save.png==T){
      png(
        filename = paste0("Plots/", fsh,"_",res, "_SSBSSBMSY.png"),
        type = "cairo",
        units = "mm",
        width = 300,
        height = 225,
        pointsize = 24,
        res = 300
      )
    } # end save.png

    # par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    max_y<-0
    for(ii in 1:dim(results@SB_SBMSY)[2]){
      max_y<-max(max_y, apply(results@SB_SBMSY[,ii,], 2, median) )
    }
    plot(apply(results@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, max(max_y*1.1, 1.1)), lwd=2, ylab="SSB/SSBMSY", xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@SB_SBMSY[,i,], 2, median), type='l', lwd=2, lty=iorder, col=colsR[iorder])
    }
    legend("bottom", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=5, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1)

    if(save.png==T){
      dev.off()
    } # end save.png

  } # end for res
} # end function



Plot_Catchtraj<-function( fsh, scenarios=NULL, save.png=F,
                          oMPs=orderedMPs, oScenarios=orderedScenarios,
                          colsR=MP_R_col, namesR=MP_namesR_leg){

  data<<-get(fsh)
  if(is.null(scenarios)){
    scenarios<-names(data)[oScenarios]
  } # end get scenarios

  for(res in scenarios){
    results<<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[oMPs])


    ## SSB/SSBMSY
    if(save.png==T){
      png(
        filename = paste0("Plots/", fsh,"_",res, "_Catch.png"),
        type = "cairo",
        units = "mm",
        width = 300,
        height = 225,
        pointsize = 24,
        res = 300
      )
    } # end save.png

    # par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    max_y<-0
    for(ii in 1:dim(results@Catch)[2]){
      max_y<-max(max_y, apply(results@Catch[,ii,], 2, median) )
    }
    plot(apply(results@Catch[,1,], 2, median), type='l', ylim=c(0, max(max_y*1.1, 1.1)), lwd=2, ylab="Catch", xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@Catch[,i,], 2, median), type='l', lwd=2, lty=iorder, col=colsR[iorder])
    }
    legend("bottom", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=5, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1)

    if(save.png==T){
      dev.off()
    } # end save.png

  } # end for res
} # end function


## VIOLIN PLOTS ##
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

# -------------------------------------------------- GET DATA ------------------------------------------------------

## INPUTS ##
set.file<-"MSE_obj/"
species<-"BlackSeaBass"; sp<-"BSB"
## DATA AND PLOTTING INPUTS
orderedMPs<-c(1, 9:10, 2:8)
orderedScenarios<-c(3,5:6,4,1:2,7:11)
scenarios<-c("base","age0M_hi","age0M_lo","epiM","recdev_hi","recdev_lo",
             "recns","refbias_hi","refbias_lo","uobs_hi","uobs_lo")
MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p", "GBtarg","ICI","Irat","IT10","Itarg")
MP_R_col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue')


### Run FUNCTIONS for input species ##
assign(sp, GetResults(species=species,sp=sp)) # save GetResults with sp

# correct results for EpiM SSB nonsensical results
sptemp<-get(sp)
summary(sptemp$epiM@SB_SBMSY)
assign(sp, EpiMDeal(sptemp))
summary(get(sp)$epiM@SB_SBMSY)

# Get performance metrics and reorder
assign(paste(sp,"PMs",sep='_'), CollatePMs(sp)) #BSB_PMs
assign(paste(sp, "PM", sep="_"), Reorder_MPs(get(paste0(sp,"_PMs"))) )



############## -----------------------------------------------------------------



# --------------------------------------------------- PLOTTING -------------------------------------------------


# Plot median Trajectories
par(mfrow=c(4,3))
Plot_SSBtraj(fsh=sp)
par(mfrow=c(4,3))
Plot_Catchtraj(fsh=sp)


spec<-get(sp)
# plot violin plots
Plot_trelSSB(SPP=spec)


### NEED TO BUILD A FUNCTION TO REORDER BSB

