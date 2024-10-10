library(openMSE)

##### INPUTS ######

source("Analyze_Results_Setup.R")

set.file<-"MSE_obj/"

species_list<-list()
species_list[[1]]<-c("VermilionSnapper","VS")
species_list[[2]]<-c("BlackSeaBass","BSB")
species_list[[3]]<-c("RedPorgy","RP")

# species<-"RedPorgy_Over"; sp<-"RP_O"
# species<-"VermilionSnapper_Over"; sp<-"VS_O"
# species<-"BlackSeaBass_Over"; sp<-"BSB_O"

orderedMPs<- c(1:2, 14,15, 5:13) #c(1:2, 12:13, 3:11) #c(1, 10:11, 2:9, 12:13) #c(1, 9:10, 2:8)
orderedScenarios<-c(3, 5:6, 1:2, 7, 4, 8:9) #c(3, 5,6,4,1,2,7,10,11,8,9) #c(3,5:6,4,1:2,7:11)
scenarios<-c("base","recdev_hi", "recdev_lo", "epiM", "age0M_hi", "age0M_lo",
             "recns", "uobs_hi", "uobs_lo") #, "refbias_hi",  "refbias_lo")
MP_col=c('grey30','gray40','gray50','gray60','gray70') #R0 #h #MSY #FMSY #SSBMSY #SSB_SSBMSY #F_FMSY
scen_col=c('black','deepskyblue','deepskyblue4','firebrick','darkorchid','darkorchid4','darkorange','darkolivegreen2','darkolivegreen') #,'slateblue','slateblue4'



##### Define Functions ####
# function to get final EM results
FinalEMResults<-function(MSE, mp){ #MPs = c(2,12,13,3,4)
  results<-list()
  #R0 #h #MSY #FMSY #SSBMSY #SSB_SSBMSY #F_FMSY
  em_R0<-om_R0<-em_h<-om_h<-em_MSY<-om_MSY<-em_FMSY<-om_FMSY<-em_SSBMSY<-om_SSBMSY<-em_SSB_SSBMSY<-om_SSB_SSBMSY<-em_F_FMSY<-om_F_FMSY<-vector(mode="integer", length=MSE@nsim)


  for(i in 1:MSE@nsim){
    l<-length(MSE@PPD[[mp]]@Misc[[i]]$diagnostic)
    yr<-MSE@PPD[[mp]]@Misc[[i]]$diagnostic[[l]]$Year # year for EM results
    resyr<-yr - MSE@OM$CurrentYr[1] # Projection year for EM results
    omyr<-MSE@nyears + resyr # OM year for EM results (hist + projection year)

    #R0
    x<-MSE@PPD[[mp]]@Misc[[i]]$diagnostic[[l]]$R0
    em_R0[i]<- ifelse(length(x)==0,NA,x)
    om_res_R0<-MSE@RefPoint$ByYear$R0
    om_R0[i]<- om_res_R0[i,omyr]

    #h
    x<-MSE@PPD[[mp]]@Misc[[i]]$diagnostic[[l]]$h
    em_h[i]<-ifelse(length(x)==0,NA,x)
    om_res_h<-MSE@RefPoint$ByYear$h
    om_h[i]<- om_res_h[i,omyr]

    #MSY
    x<-MSE@PPD[[mp]]@Misc[[i]]$diagnostic[[l]]$MSY
    em_MSY[i]<- ifelse(length(x)==0,NA,x)
    om_res_MSY<-MSE@RefPoint$ByYear$MSY
    om_MSY[i]<- om_res_MSY[i,omyr]

    #FMSY
    x<-MSE@PPD[[mp]]@Misc[[i]]$diagnostic[[l]]$FSY
    em_FMSY[i]<-ifelse(length(x)==0,NA,x)
    om_res_FMSY<-MSE@RefPoint$ByYear$FMSY
    om_FMSY[i]<-om_res_FMSY[i,omyr]

    #SSBMSY
    x<-MSE@PPD[[mp]]@Misc[[i]]$diagnostic[[l]]$SSBMSY
    em_SSBMSY[i]<-ifelse(length(x)==0,NA,x)
    om_res_SSBMSY<-MSE@RefPoint$ByYear$SSBMSY
    om_SSBMSY[i]<-om_res_SSBMSY[i,omyr]

    #SSB_SSBMSY
    x<-MSE@PPD[[mp]]@Misc[[i]]$diagnostic[[l]]$SSB_SSBMSY
    em_SSB_SSBMSY[i]<- ifelse(length(x)==0,NA,x)
    om_res_SSB_SSBMSY<-MSE@SB_SBMSY
    om_SSB_SSBMSY[i]<- om_res_SSB_SSBMSY[i,mp,(resyr-1)]

    #F_FMSY
    x<-MSE@PPD[[mp]]@Misc[[i]]$diagnostic[[l]]$F_FMSY
    em_F_FMSY[i]<- ifelse(length(x)==0,NA,x)
    om_res_F_FMSY<-MSE@F_FMSY
    om_F_FMSY[i]<-om_res_F_FMSY[i,mp,(resyr-2)]

  }# end i loop

  results[["R0"]]<- (em_R0-om_R0)/om_R0
  results[["h"]]<- (em_h-om_h)/om_h
  results[["MSY"]]<- (em_MSY-om_MSY)/om_MSY
  results[["FMSY"]]<- (em_FMSY-om_FMSY)/om_FMSY
  results[["SSBMSY"]]<- as.vector( (em_SSBMSY-om_SSBMSY)/om_SSBMSY )
  results[["SSB_SSBMSY"]]<- as.vector( (em_SSB_SSBMSY-om_SSB_SSBMSY)/om_SSB_SSBMSY )
  results[["F_FMSY"]]<- as.vector( (em_F_FMSY-om_F_FMSY)/om_F_FMSY )

  return(results)
}

# function to plot final EM results
Plot_tEMres<- function(tEMres, p){
  par(mfrow=c(11,5), mar=c(1, 1, 0.1, 0.1), oma=c(0.3,0.3,1.1,0.1), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
  for(s in names(mse)){
  for(mp in names(tEMres[[1]])){
      hist(tEMres[[s]][[mp]][[p]] , main="",
           col=MP_col[which(names(tEMres[[1]])==mp)],
           border=scen_col[which(names(mse)==s)])
      mtext(mp, 3, line=-2.5, cex=0.8)
      mtext(s, 3, line=-1.2, cex=0.8)
  } # end mp loop
  } # end s loop
  mtext(paste(sp, p, sep=" "), 3, line=-0.2, cex=1, outer=T)

}

## Get Results
for(ii in 1:length(species_list)){
  species<-species_list[[ii]][1]
  sp<-species_list[[ii]][2]

  assign(sp, GetResults(species1=species,sp1=sp)) # save GetResults with sp | SP

  # correct results for EpiM SSB nonsensical results
  sptemp<-get(sp)                       # save SP as sptemp
  # summary(sptemp$epiM@SB_SBMSY)         # see nonsensical sptemp results
  assign(sp, EpiMDeal(sptemp))          # deal with nonsensical results -- resave as SP
  # summary(get(sp)$epiM@SB_SBMSY)        # see updated results


  mse<-get(sp)
  # names(mse)
  # mse[[1]]@MPs

  tEMres<-list() # define list structure
  for(s in names(mse)){
    mtemp<-mse[[s]] # get MSE for OM scenario s
    for(mp in c(2,14,15,5,6)){ # select 2, 14, 15, 5:6 c(2,12,13,3,4)
      tEMres[[s]][[paste0(mtemp@MPs[mp])]]<-FinalEMResults(mtemp, mp=mp) # get results for each mp
    }# end m loop
  }# end s loop
  assign(paste0(sp,"_tEM"), tEMres) # resave w species name

}# end species ii loop

# ## plot
# defsp<-"VS"
# tEMres<- get(paste0(defsp,"_tEM"))
#
# # run plotting loop for every em perf metric (p)
# for(p in names(tEMres[[s]][[mp]]) ){
#   Plot_tEMres(tEMres, p)
# } # end p loop

