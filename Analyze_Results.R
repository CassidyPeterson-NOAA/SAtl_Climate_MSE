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
GetResults<-function(species1=species, sp1=sp, oScenarios=orderedScenarios){
  if(grepl(species1, pattern="Over")){ # get files of matching species name (correcting for Over results)
    files1<-list.files(set.file, pattern=species1)
  } else {
    files1<-grep(list.files(set.file, pattern=species1), pattern="Over", invert=TRUE, value=TRUE) #
  }

  temp_res<-list() # create empty list with sp title.
  for(f in 1:length(files1)){ # get results for each OM
    lab1 <- gsub("MSE_|\\..*", "", files1[f]); lab2<- gsub(species1, sp1, lab1); lab3<-gsub(paste0(sp1,"_"),"",lab2) # get rds label
    assign(lab2, readRDS(file.path(set.file,files1[f]))) # get results for each OM
    temp_res[[lab3]]<-get(lab2) # # save OM results in a single object titled sp
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
# par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
## one plot each -- new color
Plot_SSBtraj<-function( fsh, scenarios=NULL, save.png=F,
                        oMPs=orderedMPs, oScenarios=NULL,
                        colsR=MP_R_col, namesR=MP_namesR_leg){

  data<-get(fsh)
  if(is.null(scenarios)){
    if(!is.null(oScenarios)){
      scenarios<-names(data)[oScenarios]
    } else {
        scenarios<-names(data)
        } # end if-else ; define scenarios
  } # end get scenarios

  for(res in scenarios){
    results<-get(res,data)

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
      par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    } # end save.png

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
    if(save.png==T) legend("bottom", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=5, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1.2)

    if(save.png==T){
      dev.off()
    } # end save.png

  } # end for res

  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  legend("center", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=2, cex=0.75)

} # end function



Plot_Catchtraj<-function( fsh, scenarios=NULL, save.png=F,
                          oMPs=orderedMPs, oScenarios=NULL,
                          colsR=MP_R_col, namesR=MP_namesR_leg){

  data<-get(fsh)
  if(is.null(scenarios)){
    if(!is.null(oScenarios)){
      scenarios<-names(data)[oScenarios]
    } else {
      scenarios<-names(data)
    } # end if-else ; define scenarios
  } # end get scenarios

  for(res in scenarios){
    results<-get(res,data)

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
      par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    } # end save.png

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
    if(save.png==T) legend("bottom", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=5, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1.2)

    if(save.png==T){
      dev.off()
    } # end save.png

  } # end for res

  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  legend("center", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=2, cex=0.75)

} # end function


## VIOLIN PLOTS ##
Plot_trelSSB<-function(SPP, ylims=c(NULL), oMPs=orderedMPs, MPnam=MP_namesR_leg, MPcol=MP_R_col){
  par(mfrow=c(4,3))
  for(ii in 1:length(names(SPP))){
    vioplot(P100(SPP[[ii]], Yrs=-1)@Stat[,oMPs], col=MPcol, names=MPnam, ylim=ylims); abline(h=1)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("SSB ratio", 2, line=1.1)
  }# end for loop
}
Plot_PNOF<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_leg, MPcol=MP_R_col){
  par(mfrow=c(4,3))
  for(ii in 1:length(names(SPP))){
    vioplot(PNOF(SPP[[ii]])@Prob[,oMPs], col=MPcol, names=MPnam, ylim=ylims); abline(h=1); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("PNOF", 2, line=1.1)
  }# end for loop
}
Plot_P100<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_leg, MPcol=MP_R_col){
  par(mfrow=c(4,3))
  for(ii in 1:length(names(SPP))){
    vioplot(P100(SPP[[ii]],Yrs=-10)@Prob[,oMPs], col=MPcol, names=MPnam, ylim=ylims); abline(h=1); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("Prob SSB_40-50>SSBMSY", 2, line=1.1)
  }# end for loop
}
Plot_AAVY<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_leg, MPcol=MP_R_col){
  par(mfrow=c(4,3))
  for(ii in 1:length(names(SPP))){
    vioplot(AAVY(SPP[[ii]],Yrs=-10)@Stat[,oMPs], col=MPcol, names=MPnam, ylim=ylims); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("AAVY", 2, line=1.1)
  }# end for loop
}

# cumulative violin plots (over all OMs)
PlotCumPM<-function(sp, title=NULL, Pstat=c('AAVY','trelSSB','trelF','tyield','cyield','PNOF','P100'),
                    ylims=c(NULL), refline=NULL, MPnam=MP_namesR_leg, MPcol=MP_R_col){
  dat<-get(paste0(sp,"_PM"))
  datStat<-get(Pstat, dat)

  vioplot(datStat, col=MPcol, names=MPnam, ylim=ylims); abline(h=refline, lty=2)
  if(is.null(title)) title<-sp
  mtext(title, 3, line=-1.2)
  mtext(Pstat, 2, line=1.1)
}


# -------------------------------------------------- GET DATA ------------------------------------------------------

# BSB<-loadRDS("MSE_obj/MSE_BlackSeaBass_base.rds")

##### INPUTS ######
set.file<-"MSE_obj/"
species<-"VermilionSnapper"; sp<-"VS"
# species<-"BlackSeaBass"; sp<-"BSB"
## DATA AND PLOTTING INPUTS
orderedMPs<- c(1, 10:11, 2:9, 12:13) #c(1, 9:10, 2:8)
orderedScenarios<-c(3, 5,6,4,1,2,7,10,11,8,9) #c(3,5:6,4,1:2,7:11)
scenarios<-c("base","recdev_hi", "recdev_lo", "epiM", "age0M_hi", "age0M_lo",
             "recns", "uobs_hi", "uobs_lo", "refbias_hi",  "refbias_lo")
MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p", "GBtarg","GBtarg2","ICI","Irat","IT10","Itarg","GBslope","Islope")
MP_R_col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue', 'lightseagreen', 'cadetblue','cadetblue1')
par.args<-list(mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
par(par.args)




### Run FUNCTIONS for input species ##
assign(sp, GetResults(species1=species,sp1=sp)) # save GetResults with sp | BSB

# correct results for EpiM SSB nonsensical results
sptemp<-get(sp)                       # save BSB as sptemp
summary(sptemp$epiM@SB_SBMSY)         # see nonsensical sptemp results
assign(sp, EpiMDeal(sptemp))          # deal with nonsensical results -- resave as BSB
summary(get(sp)$epiM@SB_SBMSY)        # see updated results

# Get performance metrics and reorder
assign(paste(sp,"PMs",sep='_'), CollatePMs(sp)) #BSB_PMs
assign(paste(sp, "PM", sep="_"), Reorder_MPs(get(paste0(sp,"_PMs"))) ) #BSB_PM ## REORDERED PERF METRICS








# --------------------------------------------------- PLOTTING -------------------------------------------------

# Plot median Trajectories
par(mfrow=c(4,3), par.args)
Plot_SSBtraj(fsh=sp)
par(mfrow=c(4,3), par.args)
Plot_Catchtraj(fsh=sp)


spec<-get(sp)
# plot violin plots
Plot_trelSSB(SPP=spec)
Plot_PNOF(SPP=spec, refline=0.5, ylims=c(0,1.2))
Plot_P100(SPP=spec, refline=0.5, ylims=c(0,1.2))
Plot_AAVY(SPP=spec, refline=0.30, ylims=c(0,1.5))
Plot_AAVY(SPP=spec, refline=0.30)





## BUILD cumulative violins / boxplots


par(mfrow=c(1,1))
PlotCumPM(sp, Pstat="AAVY", refline=c(0.3), title=species)
PlotCumPM(sp, Pstat="PNOF", refline=c(0.5), title=species)
PlotCumPM(sp, Pstat="P100", refline=c(0.5), title=species)
PlotCumPM(sp, Pstat="trelSSB", ylim=c(0,10), refline=c(1), title=species)
PlotCumPM(sp, Pstat="cyield",  refline=c(45), title=species)





## BUILD wormplot
get(sp)$base@MPs
MPs=c("SCA_1","GB_target_BSB","myICI_BSB")
myWorm<-function(sp, OM="base", MPs=NULL,                 ### NOTE: MPs names should match sp$OM@MPs names.
                 metric=c("SB_SBMSY","F_FMSY","AddInd", "Catch"),
                 nworms=4, iters=NULL, seed=NULL , byMP=FALSE, ... ){
  dat<-get(OM, get(sp))
  if(is.null(seed)) seed<-8675309
  set.seed(seed)
  if(is.null(iters)) iters<-sample(1:dat@nsim, nworms, replace=FALSE)


  if(metric=="SB_SBMSY" ){ datmet<-dat@SB_SBMSY}
  if(metric=="F_FMSY"){ datmet<-dat@F_FMSY}
  if(metric=="Catch"){ datmet<-dat@Catch}

  if(metric=="SB_SBMSY" | metric=="F_FMSY" | metric=="Catch"){

    if(byMP==F){
      for(i in 1:nworms){
        datplot<-datmet[iters[i], , ]

        plot(datplot[which(dat@MPs==MPs[1]),], type='l', lty=1, col=1, lwd=2, #col=which(MPs==m)
             ylim=c(min(datplot)-0.1*(min(datplot)), max(datplot)+0.1*(max(datplot))),
             ylab=metric, xlab="Projected Year" ); abline(h=1)
        for(m in 2:length(MPs) ){
          lines(datplot[which(dat@MPs==MPs[m]),], lty=m,col=m ,lwd=2)
        } # end m loop

      } # end i for loop
      legend("topright",MPs,lty=1:length(MPs), col=1:length(MPs), bty='n')
    } # end if byMP==F



    if(byMP==T){
      # separate by MP
      for(m in MPs){
        datplot<-datmet[iters, which(dat@MPs==m), ]
        plot(datplot[1,], type='l', lty=1, col=1, lwd=2, #col=which(MPs==m)
             ylim=c(min(datplot)-0.1*(min(datplot)), max(datplot)+0.1*(max(datplot))),
             ylab=metric, xlab="Projected Year" ); abline(h=1); mtext(m, 3, line=-1.2)
        for(i in 2:nworms){
          lines(datplot[i,], lty=i,col=i ,lwd=2)
        } # end i loop

      } # end m for loop
    } # if byMP==T



  } # if metric == SBSBMSY, FFMSY, Catch



  ### IF metric=="AddInd" UNDER CONSTRUCTION
  # if(metric=="AddInd"){
  #   datmet<-c()
  #   for(m in MPs){
  #     datmet<-rbind(datmet, get(m, dat@PPD)@AddInd[iters,1,])
  #   }# end for m loop
  # }# end if metric=="AddInd"

  return(list(seed=seed, iters=iters))
} # end function

par(mfrow=c(1,4))
myWorm(sp, OM="base", MPs=c("SCA_1","GB_target_BSB","myICI_BSB"),
       metric="SB_SBMSY")
par(mfrow=c(1,3))
myWorm(sp, OM="base", MPs=c("SCA_1","GB_target_BSB","myICI_BSB"),
       metric="SB_SBMSY", byMP=T)
