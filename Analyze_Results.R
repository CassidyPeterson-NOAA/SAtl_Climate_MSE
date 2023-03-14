## CLEANED MSE PLOTTING AND ANALYSIS CODE ###

source("Analyze_Results_Setup.R")

#### -------------------------------------------------- GET DATA ------------------------------------------------------ #####


##### INPUTS ######
# BSB<-loadRDS("MSE_obj/MSE_BlackSeaBass_base.rds")
set.file<-"MSE_obj/"
# species<-"RedPorgy"; sp<-"RP"
species<-"VermilionSnapper"; sp<-"VS"
# species<-"BlackSeaBass"; sp<-"BSB"
# species<-"RedPorgy_Over"; sp<-"RP_O"
# species<-"VermilionSnapper_Over"; sp<-"VS_O"
# species<-"BlackSeaBass_Over"; sp<-"BSB_O"
## DATA AND PLOTTING INPUTS

# MPs_user_BSB <- c("SCA_1", "pMP_5","pMP_10" ,
#                   "GB_target_BSB", "GB_target_BSB2",
#                   "myICI_BSB", "myIratio_BSB",
#                   "myIT10_BSB", "myItarget_BSB" ,
#                   "GB_slope_BSB","GB_slope_BSB1","GB_slope_BSB2",
#                   "myIslope_BSB","myIslope_BSB2"
# )
# MPs_user_RP <- c("SCA_1", "pMP_5", "pMP_10",
#                  "GB_target_RP", "GB_target_RP2",
#                  "myICI_RP", "myICI_RP2", "myIratio_RP",
#                  "myIT10_RP", "myItarget_RP",
#                  "GB_slope_RP","GB_slope_RP2",
#                  "myIslope_RP", "myIslope_RP2"
# )
# MPs_user_VS <- c("SCA_1", "pMP_5", "pMP_10",
#                  "GB_target_VS", "GB_target_VS2",
#                  "myICI_VS", "myIratio_VS",
#                  "myIT10_VS", "myItarget_VS", "myItarget_VS2",
#                  "GB_slope_VS", "GB_slope_VS2",
#                  "myIslope_VS", "myIslope_VS2"
# )

abbrev=TRUE # true to select best performing MP configurations for each species. false to show all MP results.

orderedMPs<- c(1, 15:16, 2:14) #c(1, 10:11, 2:9, 12:13) #c(1, 9:10, 2:8)
orderedScenarios<-c(3, 5,6,4,1,2,7,10,11,8,9) #c(3,5:6,4,1:2,7:11)
scenarios<-c("base","recdev_hi", "recdev_lo", "epiM", "age0M_hi", "age0M_lo",
             "recns", "uobs_hi", "uobs_lo", "refbias_hi",  "refbias_lo")
# get(sp)$base@MPs
if(sp=="VS" | sp=="VS_O") {
  if(abbrev==TRUE){
    # VS: GB_targ_VS2 | myItarget_VS2? | GB_slope_VS | GB_Islope2
    orderedMPs<-c(1,15:16,2:3,5:8,10:11,14)
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","ICI","Irat","IT10","Itarg",
                     "GBslope","Islope")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt",
                        "ICI","Ira","I10","Ita","GBs","Isl")
  }#end if abbrev==TRUE

  if(abbrev==FALSE){
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","GBtarg2","ICI","Irat","IT10","Itarg","Itarg2",
                     "GBslope","GBslope2","Islope","Islope2")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
                        "ICI","Ira","I10","Ita","It2","GBs","Gs2","Isl","Is2")
  }#end if abbrev==False
} # end if sp==VS

if(sp=="RP" | sp=="RP_O") {
  if(abbrev==FALSE){
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","GBtarg2","ICI","ICI2","Irat","IT10","Itarg",
                     "GBslope","GBslope2","Islope","Islope2")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
                        "ICI","IC2","Ira","I10","Ita","GBs","Gs2","Isl","Is2")
  }#end abbrev=F
  if(abbrev==TRUE){
    # RP: GB_targ_RP2 | myICI_RP2 | GB_slope_RP2 | myIslope_RP2
    orderedMPs<- c(1, 15:16, 2:3, 5, 7:10, 12, 14)
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","ICI","Irat","IT10","Itarg",
                     "GBslope","Islope")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt",
                        "ICI","Ira","I10","Ita","GBs","Isl")
  }#end abbrev=T
}# end RP

if(sp=="BSB" | sp=="BSB_O") {
  if(abbrev==FALSE){
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","GBtarg2","ICI","Irat","IT10","Itarg",
                     "GBslope","GBslope1","GBslope2","Islope","Islope2")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
                        "ICI","Ira","I10","Ita","GBs","Gs1","Gs2","Isl","Is2")
  }# end abbrev=F
  if(abbrev==TRUE){
    orderedMPs<- c(1, 15:16, 2:4, 6:10, 13)
    # BSB: GB_targ_BSB | GB_slope_BSB | myIslope_BSB
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","ICI","Irat","IT10","Itarg",
                     "GBslope","Islope")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt",
                        "ICI","Ira","I10","Ita","GBs","Isl")
  }# end abbrev=T

  } #end BSB


if(abbrev==FALSE){
  MP_R_col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue','cadetblue','cadetblue1','cadetblue3', 'lightseagreen','mediumseagreen', 'aquamarine','aquamarine3')
}
if(abbrev==TRUE){
  MP_R_col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue', 'lightseagreen','cadetblue','cadetblue1')
}

par.args<-list(mar=c(2.4, 2.4, 0.2, 0.2), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
par(par.args)




### Run FUNCTIONS for input species #####
assign(sp, GetResults(species1=species,sp1=sp)) # save GetResults with sp | SP

# correct results for EpiM SSB nonsensical results
sptemp<-get(sp)                       # save SP as sptemp
summary(sptemp$epiM@SB_SBMSY)         # see nonsensical sptemp results
assign(sp, EpiMDeal(sptemp))          # deal with nonsensical results -- resave as SP
summary(get(sp)$epiM@SB_SBMSY)        # see updated results

# Get performance metrics and reorder
CollatePMs_temp<-CollatePMs(sp)
assign(paste(sp,"PMs",sep='_'), CollatePMs_temp$returnlist) #SP_PMs -- UNORDERED
assign(paste(sp, "PM", sep="_"), Reorder_MPs(get(paste0(sp,"_PMs"))) ) #SP_PM ## REORDERED PERF METRICS
assign(paste(sp, "PMs_nest", sep="_"), CollatePMs_temp$returnlist_nest ) #SP_PMs_nested -- UNORDERED ##
assign(paste(sp, "PM_nest", sep="_"), Reorder_MPs(get(paste0(sp,"_PMs_nest")), nested=TRUE) ) #SP_PM ## REORDERED PERF METRICS REORDERED PERF METRICS

# Get performance metrics relative to base OM case
# assign(paste(sp,"RelPM_s", sep='_'), Relative_MP_Perf_s(sp)) #SP_RelPM_s
# assign(paste(sp,"RelPM_d", sep='_'), Relative_MP_Perf_d(sp)) #SP_RelPM_d
assign(paste(sp,"RelPM_rd", sep='_'), Relative_MP_Perf_rd(sp)) #SP_RelPM_rd







# --------------------------------------------------- PLOTTING -------------------------------------------------

### Plot median Trajectories
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_MSY(fsh=sp)
par(mfrow=c(4,3), par.args)
reflinetemp<-get(sp)[[1]]@OM$SSBMSY_SSB0[1]
Plot_SSBtraj_dSSB0(fsh=sp, refline=reflinetemp)
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_rawSSB(fsh=sp)

par(mfrow=c(4,3), par.args)
Plot_Catchtraj(fsh=sp)

### Plot subset median Trajectories
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_dSSB0(fsh=sp, subset=c(1,6:12),#subset=c(1,8:13),
             colsR=c('black','deepskyblue','deeppink','darkolivegreen3','darkorchid','darkorange','steelblue', 'orchid'))
                     # ,'blue','pink','green','orchid','cyan'))
get(sp)$base@MPs[orderedMPs]
Plot_SSBtraj_dSSB0(fsh=sp, subset=c(1,6:7),#subset=c(1,8:13),
             colsR=c('black','deepskyblue','deeppink'))
# BSB: GB_targ_BSB | GB_slope_BSB | myIslope_BSB
# RP: GB_targ_RP2 | myICI_RP2 | GB_slope_RP2 | myIslope_RP2
# RP_O: GB_targ_RP | myICI_RP (doesn't really matter) | myIslope_RP (doesn't really matter)
# VS: GB_targ_VS2 | myItarget_VS2? | GB_slope_VS | GB_Islope2
par(mfrow=c(4,3), par.args)
Plot_SSBtraj(fsh=sp, subset=c(1:5),
             colsR=c('black','deepskyblue','steelblue','deeppink','darkorange'))

### plot violin plots
stat=c("AAVY","trelSSB","treldSSB0", "trelF","t10relSSB",
       "t10reldSSB0","t10relF", "tyield", "cyield", "PNOF", "P100" )
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='AAVY', ylims=c(0,1.5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='treldSSB0', ylims=c(0,1.1), refline2=0.5)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10reldSSB0', ylims=c(0,1.1), refline2=0.5)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='trelSSB', ylimsEpiM=c(0,5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10relSSB', ylimsEpiM=c(0,5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='trelF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10relF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='tyield')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='PNOF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='P100')







### cumulative violins / boxplots
par(mfrow=c(1,1))
PlotCumPM(sp, Pstat="AAVY", refline=c(0.3), title=species, ylims=c(0,1)) # for BSB_O ylims=c(0,5) |for VS ylims=c(0,5)
PlotCumPM(sp, Pstat="PNOF", refline=c(0.5), title=species)
PlotCumPM(sp, Pstat="P100", refline=c(0.5), title=species)
PlotCumPM(sp, Pstat="trelSSB", ylim=c(0,10), refline=c(1), title=species)
PlotCumPM(sp, Pstat="treldSSB0", ylim=c(0,1.1), refline=c(1), title=species)
PlotCumPM(sp, Pstat="t10reldSSB0", ylim=c(0,1.1), refline=c(1), title=species)
PlotCumPM(sp, Pstat="cyield",  refline=c(50), title=species)



### Relative violins
# | Pstat=c('AAVY','trelSSB','trelF','t10relSSB','t10relF','tyield','cyield'),


par(mfrow=c(4,3), mar=c(2.2, 2.2, 0.2, 0.2), mgp=c(1.2, 0.25, 0), tck=-0.01)

# treldSSB0
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 300, -0.5, 1, -0.0, 30, 10, 3, 35, 4, 5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, -0.5, 1, -0.0, 2.5, 4, 3.5, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 8, 0.75, 1, 2.5, 3, 2, 1, 1.5, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(3.5, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}



#t10reldSSB0
if(sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 100, -0.5, 1, -0.0, 15, 3, 2, 35, 4, 4), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 2000, -0.7, 5, -0.0, 30, 6, 10, 50, 8, 1000), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 8, -0.3, 2, 0.15, 2, 4, 3.5, 4, 4, 5), #VS
            minylim=c(0, rep(-1,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB' | sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 8, 0.25, 2, 2, 3, 2.0, 1, 2, 1, 3), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}

# trelSSB
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 600, -0.5, 1, -0.0, 30, 10, 3, 35, 4, 5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 10, -0.5, 1, -0.0, 2.5, 4, 3.5, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='trelSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 8, 0.75, 1, 2.5, 3, 2, 1, 1.5, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}



#t10relSSB
if(sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 2000, -0.5, 2, -0.0, 30, 6, 3, 35, 8, 10), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 2000, -0.7, 5, -0.0, 30, 6, 10, 50, 8, 1000), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 8, -0.3, 2, 0.15, 2, 4, 3.5, 4, 4, 5), #VS
            minylim=c(0, rep(-1,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB' | sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 8, 0.25, 2, 2, 3, 2.0, 1, 2, 1, 3), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}



#trelF
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelF',
            baserefline=1, refline=0,
            maxylim=c(10, 5, 50, 50, 150, 2.5, 3, 50, 1.5, 10, 5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelF',
            baserefline=1, refline=0,
            maxylim=c(3.5, 1.5, 15, 2, 15, 1.5, 4, 20, 10, 20, 15), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='trelF',
            baserefline=1, refline=0,
            maxylim=c(2.25, 10, 6, 10, 20, 5, 3, 5, 2, 3, 5), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelF',
            baserefline=1, refline=0,
            maxylim=c(2.25, 10, 4, 4, 15, 5, 2, 5, 2, 2, 5), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}



#t10relF
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',         ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            baserefline=1, refline=0,
            maxylim=c(10, 3, 50, 150, 150, 1.5, 3, 50, 5, 10, 5), # RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}

if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',           ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            maxylim=c(3.5, 1.5, 15, 2, 15, 1, 4, 20, 10, 20, 15), #VS
            baserefline=1, refline=0,
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',           ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            baserefline=1, refline=0,
            maxylim=c(3, 1.5, 7.5, 2, 7.5, 0.5, 2, 10, 12, 3, 10), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}

if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',           ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            baserefline=1, refline=0,
            maxylim=c(2.25, 7, 4, 50, 13.5, 3, 2, 4, 1, 3, 3), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',           ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            baserefline=1, refline=0,
            maxylim=c(2.25, 7, 2, 50, 13.5, 3, 2, 4, 1, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}




# cyield
if(sp=='RP' | sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(80, 0.5, 10, 1.5,6, 1, 4, 3, 2, 4, 2), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(100, 0, 3.5,    1, 1.5, 0.1, 3,   1,  1, 2, 1.5), #VS
            minylim=c(0, -0.75, 0, -0.5,  0, -0.4, -1, -1, -1, -1, -1) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(80, 0, 2.5, 1, .75, 0.1, 1.25,   1,  1, 1, 1.5), #VS
            minylim=c(0, -0.75, 0, -0.5,  0, -0.4, -1, -1, -1, -1, -1) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(100, 0.2, 2.5, 1.5, 5, 0.1, 1.5, 0.5, 0.5, 0.3, 0.3), #BSB
            minylim=c(0, -1, -0.75, -1,  -0.25, -1, -1, -.75, -0.5, -0.5, -1) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(100, 0.2, 2.5, 1.5, 5, 0.1, 1.5, 0.5, 0.5, 0.3, 0.3), #BSB
            minylim=c(0, -0.9, -0.5, -1,  -0.25, -0.75, -0.75, -.4, -0.75, -0.5, -1) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}



# P100 (last 10 years)
if(sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.1, 0.1, 0.1, 30, 2, 3, 35, 2.5, 2.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 100, 0.1, 0.1, 0.1, 25, 1, 1, 35, 2, 0.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 7.5, -0.75, 1, -0.0, 2, 1, 1, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 20, -0, 5, -0.0, 10, 2, 5, 10, 5, 10), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            # maxylim=c(1.1, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
            maxylim=c(1.1, 100, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}


# P90 (last 10 years)
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='P90',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.1, 0.1, 0.1, 30, 0.5, 3, 35, 2.5, 2.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='P90',
            baserefline=1, refline=0,
            maxylim=c(1.1, 7.5, -0.75, 1, -0.0, 2, 1, 1, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='P90',
            baserefline=1, refline=0,
            maxylim=c(1.1, 7.5, -0, 2, -0.0, 10, 1, 5, 10, 5, 10), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='P90',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='P90', ################################################## ERROR ####
            baserefline=1, refline=0,
            maxylim=c(1.1, 100, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}


# PNOF (last 10 years)
if(sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.1, 0.1, 0.1, 30, 2, 3, 35, 2.5, 2.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 100, 0.1, 0.1, 0.1, 25, 1, 1, 35, 2, 0.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 7.5, -0.75, 1, -0.0, 2, 1, 1, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.2, 5, 0.2, 7.5, 2, 5, 7.5, 4, 10), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            # maxylim=c(1.1, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
            maxylim=c(1.1, 100, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}




### Tradeoff plot
spp_pm<-get(paste(sp, "PM", sep="_"))
par(mfrow=c(1,2))
# TOff_Plot(SPP_PM=spp_pm, PMy='trelSSB', PMx='cyield')
# TOff_Plot(SPP_PM=spp_pm, PMy='trelSSB', PMx='cyield', errbars=F); abline(h=1)
# TOff_Plot(SPP_PM=spp_pm, PMy='trelSSB', PMx='AAVY', errbars=F); abline(h=1)
# TOff_Plot(SPP_PM=spp_pm, PMy='P100', PMx='cyield', errbars=F); abline(h=1)
TOff_Plot(SPP_PM=spp_pm, PMy='PNOF', PMx='cyield', errbars=F); abline(h=1)
TOff_Plot(SPP_PM=spp_pm, PMy='P100', PMx='AAVY', errbars=F); abline(h=1)



### TRADEOFF PLOT FOR DIFFERENCE RESULTS
par(mfrow=c(1,1))
Toff_Difference_Plot(sp, calc='rd',Pstatx='cyield',Pstaty='trelSSB', stat=median)


### wormplot
nms<-get(sp)$base@MPs; nms
if(sp=='') mpnms<-nms[c(1,6,7)]
if(sp=='') mpnms<-nms[c(1,6,7)]
if(sp=='') mpnms<-nms[c(1,6,7)]
names(get(sp))

# par(mfrow=c(1,4))
# myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY")
par(mfrow=c(3,3), oma=c(0,0,1.2,0))
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", nworms=9)
mtext(species, 3, outer=TRUE, line=-0.3)
par(mfrow=c(3,3), oma=c(0,0,1.2,0))
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="recdev_lo", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="epiM", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
mtext(species, 3, outer=TRUE, line=-0.3)



par(mfrow=c(3,3), oma=c(0,0,1.2,0), mar=c(2.4, 3.4, 0.2, 0.2), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="recdev_lo", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="epiM", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
mtext(species, 3, outer=TRUE, line=0)
