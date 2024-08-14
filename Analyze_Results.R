## CLEANED MSE PLOTTING AND ANALYSIS CODE ###

source("Analyze_Results_Setup.R")

#### -------------------------------------------------- GET DATA ------------------------------------------------------ #####


##### INPUTS ######
# BSB<-loadRDS("MSE_obj/MSE_BlackSeaBass_base.rds")
set.file<-"MSE_obj/"
# species<-"RedPorgy"; sp<-"RP"
# species<-"VermilionSnapper"; sp<-"VS"
species<-"BlackSeaBass"; sp<-"BSB"
# species<-"RedPorgy_Over"; sp<-"RP_O"
# species<-"VermilionSnapper_Over"; sp<-"VS_O"
# species<-"BlackSeaBass_Over"; sp<-"BSB_O"
## DATA AND PLOTTING INPUTS

# MPs_user_BSB <- c("ZeroC",
#                   "SCA_1", "SCA_5", "SCA_10",
#                   "pMP_5","pMP_10" ,
#                   "GB_target_BSB", # "GB_target_BSB2",
#                   "myICI_BSB", "myIratio_BSB",
#                   "myIT10_BSB", "myItarget_BSB" ,
#                   "GB_slope_BSB",#"GB_slope_BSB1","GB_slope_BSB2",
#                   "myIslope_BSB2" # "myIslope_BSB",
# )
# MPs_user_RP <- c("ZeroC",
#                  "SCA_1", "SCA_5", "SCA_10",
#                  "pMP_5","pMP_10" ,
#                  "GB_target_RP2", #"GB_target_RP",
#                  "myICI2_RP", #"myICI_RP",
#                  "myIratio_RP",
#                  "myIT10_RP", "myItarget_RP",
#                  "GB_slope_RP2",#"GB_slope_RP",
#                  "myIslope_RP2" #"myIslope_RP",
# )
# MPs_user_VS <- c("ZeroC",
#                  "SCA_1", "SCA_5", "SCA_10",
#                  "pMP_5","pMP_10" ,
#                  "GB_target_VS2", #"GB_target_VS",
#                  "myICI_VS", "myIratio_VS",
#                  "myIT10_VS", "myItarget_VS",
#                  "GB_slope_VS", # "GB_slope_VS2",
#                  "myIslope_VS2" #"myIslope_VS",
# )

# abbrev=TRUE # true to select best performing MP configurations for each species. false to show all MP results.

orderedMPs<-c(1:13)  #c(11,1, 12:13, 2:10) #c(1, 10:11, 2:9, 12:13) #c(1, 9:10, 2:8)
orderedScenarios<-c(3, 5,6,4,1,2,7,10,11) #c(3,5:6,4,1:2,7:11)
scenarios<-c("base","recdev_hi", "recdev_lo", "epiM", "age0M_hi", "age0M_lo",
             "recns", "uobs_hi", "uobs_lo") #, "refbias_hi",  "refbias_lo")
# get(sp)$base@MPs
# if(sp=="VS" | sp=="VS_O") {
  # if(abbrev==TRUE){
  #   # VS: GB_targ_VS2 | myItarget_VS2? | GB_slope_VS | GB_Islope2
  #   orderedMPs<-c(1,15:16,2:3,5:8,10:11,14)
    MP_namesR_leg<-c("ZeroC","SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","ICI","Irat","IT10","Itarg",
                     "GBslope","Islope")
    MP_namesR_abbrev<-c("ZC","S1","5c","10c","5p","10p", "Gt",
                        "ICI","Ir","I10","It","Gs","Is")
#   }#end if abbrev==TRUE
#
#   if(abbrev==FALSE){
#     MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
#                      "GBtarg","GBtarg2","ICI","Irat","IT10","Itarg","Itarg2",
#                      "GBslope","GBslope2","Islope","Islope2")
#     MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
#                         "ICI","Ira","I10","Ita","It2","GBs","Gs2","Isl","Is2")
#   }#end if abbrev==False
# } # end if sp==VS
#
# if(sp=="RP" | sp=="RP_O") {
#   # if(abbrev==FALSE){
#   #   MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
#   #                    "GBtarg","GBtarg2","ICI","ICI2","Irat","IT10","Itarg",
#   #                    "GBslope","GBslope2","Islope","Islope2")
#   #   MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
#   #                       "ICI","IC2","Ira","I10","Ita","GBs","Gs2","Isl","Is2")
#   # }#end abbrev=F
#   # if(abbrev==TRUE){
#     # RP: GB_targ_RP2 | myICI_RP2 | GB_slope_RP2 | myIslope_RP2
#     # orderedMPs<- c(1, 15:16, 2:3, 5, 7:10, 12, 14)
#     MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
#                      "GBtarg","ICI","Irat","IT10","Itarg",
#                      "GBslope","Islope")
#     MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt",
#                         "ICI","Ir","I10","It","GBs","Is")
#   # }#end abbrev=T
# }# end RP
#
# if(sp=="BSB" | sp=="BSB_O") {
#   # if(abbrev==FALSE){
#   #   MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
#   #                    "GBtarg","GBtarg2","ICI","Irat","IT10","Itarg",
#   #                    "GBslope","GBslope1","GBslope2","Islope","Islope2")
#   #   MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
#   #                       "ICI","Ira","I10","Ita","GBs","Gs1","Gs2","Isl","Is2")
#   # }# end abbrev=F
#   # if(abbrev==TRUE){
#     # orderedMPs<- c(1, 15:16, 2:4, 6:10, 13)
#     # BSB: GB_targ_BSB | GB_slope_BSB | myIslope_BSB
#     MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
#                      "GBtarg","ICI","Irat","IT10","Itarg",
#                      "GBslope","Islope")
#     MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt",
#                         "ICI","Ir","I10","It","GBs","Is")
#   # }# end abbrev=T
#
#   } #end BSB


# if(abbrev==FALSE){
#   MP_R_col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue','cadetblue','cadetblue1','cadetblue3', 'lightseagreen','mediumseagreen', 'aquamarine','aquamarine3')
# }
# if(abbrev==TRUE){
  MP_R_col=c('grey15','grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','skyblue3','cadetblue1','cadetblue', 'lightseagreen') #'lightskyblue1',,'lightskyblue','skyblue3''darkturquoise'
# }

par.args<-list(mar=c(2.4, 2.4, 0.2, 0.2), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
par(par.args)




### Run FUNCTIONS for input species #####
assign(sp, GetResults(species1=species,sp1=sp)[-c(10,11)]) # save GetResults with sp | SP


# correct results for EpiM SSB nonsensical results
sptemp<-get(sp)                       # save SP as sptemp
summary(sptemp$epiM@SB_SBMSY)         # see nonsensical sptemp results
assign(sp, EpiMDeal(sptemp))          # deal with nonsensical results -- resave as SP
summary(get(sp)$epiM@SB_SBMSY)        # see updated results
remove(sptemp)

# Get performance metrics and reorder
CollatePMs_temp<-CollatePMs(sp)
assign('SP_PMs', CollatePMs_temp$returnlist) #SP_PMs -- UNORDERED
assign(paste(sp, "PM", sep="_"), Reorder_MPs(SP_PMs) ) #SP_PM ## REORDERED PERF METRICS
assign('SP_PMs_nest', CollatePMs_temp$returnlist_nest ) #SP_PMs_nested -- UNORDERED ##
assign(paste(sp, "PM_nest", sep="_"), Reorder_MPs(SP_PMs_nest, nested=TRUE) ) #SP_PM ## REORDERED PERF METRICS REORDERED PERF METRICS
remove(CollatePMs_temp);remove(SP_PMs); remove(SP_PMs_nest) # remove temp files


# Get performance metrics relative to base OM case
# assign(paste(sp,"RelPM_s", sep='_'), Relative_MP_Perf_s(sp)) #SP_RelPM_s
# assign(paste(sp,"RelPM_d", sep='_'), Relative_MP_Perf_d(sp)) #SP_RelPM_d
assign(paste(sp,"RelPM_rd", sep='_'), Relative_MP_Perf_rd(sp)) #SP_RelPM_rd







# ------------------------------------------------ PLOTTING -------------------------------------------------

### Plot median Trajectories
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_MSY(fsh=sp)
par(mfrow=c(4,3), par.args)
reflinetemp<-get(sp)[[1]]@OM$SSBMSY_SSB0[1]
Plot_SSBtraj_dSSB0(fsh=sp, refline=reflinetemp, subset=c(2:13))
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_rawSSB(fsh=sp)

par(mfrow=c(4,3), par.args)
Plot_Catchtraj(fsh=sp)

### Plot subset median Trajectories
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_dSSB0(fsh=sp, subset=c(2,7:13),#subset=c(1,8:13),
             colsR=c('black','deepskyblue','deeppink','darkolivegreen3','darkorchid','darkorange','steelblue', 'orchid'))
                     # ,'blue','pink','green','orchid','cyan'))

par(mfrow=c(4,3), par.args)
get(sp)$base@MPs[orderedMPs]
Plot_SSBtraj_dSSB0(fsh=sp, subset=c(1,7:8),#subset=c(1,8:13),
             colsR=c('black','deepskyblue','deeppink'))
# BSB: GB_targ_BSB | GB_slope_BSB | myIslope_BSB
# RP: GB_targ_RP2 | myICI_RP2 | GB_slope_RP2 | myIslope_RP2
# RP_O: GB_targ_RP | myICI_RP (doesn't really matter) | myIslope_RP (doesn't really matter)
# VS: GB_targ_VS2 | myItarget_VS2? | GB_slope_VS | GB_Islope2
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_dSSB0(fsh=sp, subset=c(1:6),
             colsR=c('black','deepskyblue','steelblue','deeppink','darkorange','steelblue'))


par(mfrow=c(4,3), par.args); par(oma=c(0,0,1.2,0))
Plot_SSBtraj_dSSB0_CI(fsh=sp, subset=c(2:13),refline=reflinetemp, legend=F)

par(mfrow=c(4,3), par.args)
Plot_SSBtraj_dSSB0_CI(fsh=sp, subset=c(2,8),refline=reflinetemp, legend=F, SeparatePlots=FALSE)
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_dSSB0_CI(fsh=sp, subset=c(2,9),refline=reflinetemp, legend=F, SeparatePlots=FALSE)
get(sp)$base@MPs

## compare indices for base, uobsHi and uobsLo
# higher CV allows for higher observed index values (logn distn; that no upper bound but lower bound of 0) -- higher index values translates to higher observed abundance -- higher TACs, which reduce the pop size relative to Base and uobsLo scenarios.
base<-get(sp)[["base"]]
uobsHi<-get(sp)[["uobs_hi"]]
uobsLo<-get(sp)[["uobs_lo"]]
par(mfrow=c(4,3), oma=c(0,1.2,1.2,0), mar=c(2.4, 2.4, 0.2, 0.2), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
yrs<-uobsHi@PPD$ZeroC@Year
ey<-length(yrs)
obj<-uobsHi@PPD$ZeroC@AddInd[1,1,]
by<-tail(which(is.na(obj)),1)+1
par(mfrow=c(4,3))
for(i in 100:111){
  plot(uobsHi@PPD$ZeroC@AddInd[i,1,], type='l', col='red', ylab="Index",xlab="Year")
  lines(uobsLo@PPD$ZeroC@AddInd[i,1,], type='l', col='deepskyblue')
  lines(base@PPD$ZeroC@AddInd[i,1,], type='l')
}


boxplot(c(uobsHi@PPD$ZeroC@AddInd[,1,59:88]), c(base@PPD$ZeroC@AddInd[,1,59:88]), c(uobsLo@PPD$ZeroC@AddInd[,1,59:88]),
        col=c('red','grey','deepskyblue'),ylab="Index (last 30 years)")
boxplot(c(uobsHi@TAC[,2,21:50]), c(base@TAC[,2,21:50]), c(uobsLo@TAC[,2,21:50]),
        col=c('red','grey','deepskyblue'),ylab="TAC (last 30 years)", ylim=c(0,7500))
# vioplot(c(uobsHi@TAC[,2,1:9]), c(base@TAC[,2,1:9]), c(uobsLo@TAC[,2,1:9]),
#         col=c('red','grey','deepskyblue'),ylab="TAC", ylim=c(0,7500))
# uobsHi@PPD[[2]]@CV_AddInd[,1,]
# uobsHi@Hist@Data@CV_AddInd
remove(uobsHi)
remove(uobsLo)
  # summary(uobsHi)

# example index.
par(mfrow=c(2,1))
tmp<-apply(get(sp)$base@PPD$ZeroC@AddInd[,1,], 2, median, na.rm=T)
tmpH<-apply(get(sp)$uobs_hi@PPD$ZeroC@AddInd[,1,], 2, median, na.rm=T)
tmpL<-apply(get(sp)$uobs_lo@PPD$ZeroC@AddInd[,1,], 2, median, na.rm=T)
yrs<-which(!is.na(tmp))
if(sp=="VS"){const_t<-1.25}
if(sp=="BSB"){const_t<-1.75}
if(sp=="RP"){const_t<-2}
plot(tmp[yrs], x=yrs, type='l', ylim=c(0, max(tmp, na.rm=T)*const_t), ylab='Median index', lwd=2, xlab="Years")
lines(tmpH[yrs], x=yrs, type='l', col='blue', lwd=2)
lines(tmpL[yrs], x=yrs, type='l', col='red', lwd=2)
lines(tmp[yrs], x=yrs, type='l', lwd=2)
mtext("ZeroC Median Index", side=3, line=-1.1, cex=1)

# example rec devs.
# par(mfrow=c(1,1))
plot(apply(get(sp)$base@PPD$ZeroC@Rec, 2, median), type='l', ylim=c(0, max(apply(get(sp)$base@PPD$ZeroC@Rec, 2, median))*1.5), ylab='Median Recruitment', lwd=2, xlab="Years")
lines(apply(get(sp)$recdev_hi@PPD$ZeroC@Rec, 2, median), type='l', col='blue', lwd=2)
lines(apply(get(sp)$recdev_lo@PPD$ZeroC@Rec, 2, median), type='l', col='red', lwd=2)
lines(apply(get(sp)$base@PPD$ZeroC@Rec, 2, median), type='l', lwd=2)
mtext("ZeroC Median Recruitment", side=3, line=-1.1, cex=1)
par(mfrow=c(4,3))


par(mar=c(1,1,0,0), mfrow=c(3,3))
### plot violin plots
stat=c('AAVY','trelSSB','treldSSB0','trelF','t10relSSB','t10reldSSB0',
       't10relF','tyield','cyield', 'PNOF', 'P100','relSSB30',
       'reldSSB030','relF30', 'yield30','cyield30', 'P10030')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='AAVY', ylims=c(0,1.5), legend=F)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='treldSSB0', ylims=c(0,1.1), refline2=0.5)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10reldSSB0', ylims=c(0,1.1), refline2=0.5)

myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relSSB30', ylimsEpiM=c(0,5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='reldSSB030', refline2=reflinetemp, ylims=c(0,1.15))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relF30', ylimsEpiM=c(0,25))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='yield30')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='cyield30', refline1=30)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='P10030', ylims=c(0,1.1))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='P5030', ylims=c(0,1.1))

myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='trelSSB', ylimsEpiM=c(0,5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10relSSB', ylimsEpiM=c(0,5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='trelF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10relF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='tyield')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='PNOF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='P100')


names(get(sp))
## ONLY REGIME SHIFTS
par(mfrow=c(3,2), oma=c(0,0,1.5,0))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relSSB30', ylimsEpiM=c(0,5), scenarios=c("base"), mf=NULL)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relSSB30', ylimsEpiM=c(0,5), scenarios=c("recdev_hi", "recdev_lo"), mf=NULL, legend=F)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relSSB30', ylimsEpiM=c(0,5), scenarios=c("age0M_lo", "age0M_hi"), mf=NULL, legend=F)
mtext(paste0(sp," regime shift" ), outer=T, line=0.2)

par(mfrow=c(1,3), oma=c(0,0,1.5,0))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relSSB30', ylimsEpiM=c(0,5), scenarios=c("base","recns","epiM"), mf=NULL, legend=F)
mtext(paste0(sp," nonstationarity" ), outer=T, line=0.2)

par(mfrow=c(1,3), oma=c(0,0,1.5,0))
# myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relSSB30', ylimsEpiM=c(0,5), scenarios=c("base"), mf=NULL)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relSSB30', ylims=c(0,3.5), scenarios=c("base","uobs_lo", "uobs_hi"), mf=NULL, legend=F)
mtext(paste0(sp," observation regime shift" ), outer=T, line=0.2)

myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relSSB30', ylimsEpiM=c(0,5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='reldSSB030', refline2=reflinetemp, ylims=c(0,1.15))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='relF30')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='yield30')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='cyield30')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='P10030')




### cumulative violins / boxplots
par(mfrow=c(1,1))
PlotCumPM(sp, Pstat="AAVY", refline=c(0.3), title=species, ylims=c(0,1)) # for BSB_O ylims=c(0,5) |for VS ylims=c(0,5)
PlotCumPM(sp, Pstat="PNOF", refline=c(0.5), title=species)
PlotCumPM(sp, Pstat="P100", refline=c(0.5), title=species)
PlotCumPM(sp, Pstat="relSSB30", refline=c(1), title=species, ylims=c(0,10))
PlotCumPM(sp, Pstat="relF30", refline=c(1), title=species, ylims=c(0,7))

PlotCumPM(sp, Pstat="cyield",  refline=c(50), title=species, ylims=c(0,300))


PlotCumPM(sp, Pstat="trelSSB", ylim=c(0,10), refline=c(1), title=species)
PlotCumPM(sp, Pstat="treldSSB0", ylim=c(0,1.1), refline=c(1), title=species)
PlotCumPM(sp, Pstat="t10reldSSB0", ylim=c(0,1.1), refline=c(1), title=species)

### Relative violins
# | Pstat=c('AAVY','trelSSB','trelF','t10relSSB','t10relF','tyield','cyield'),

par(mfrow=c(4,3), mar=c(2.2, 2.2, 0.2, 0.2), mgp=c(1.2, 0.25, 0), tck=-0.01)

# # relSSB30
# if(sp=='RP' | sp=='RP_O'){
#   PlotRelPM(sp, calc='rd', Pstat='relSSB30',
#             baserefline=1, refline=0,
#             maxylim=c(1.1, 300, -0.5, 1, -0.0, 30, 10, 3, 35, 4, 5), #RP
#             minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
# }
# if(sp=='VS' | sp=='VS_O'){
#   PlotRelPM(sp, calc='rd', Pstat='relSSB30',
#             baserefline=1, refline=c(-0.5, 0, 0.5),
#             maxylim=c(1.1, 3, 1.5, 1, 1.5, 1.5, 1, 1.5, 3, 2, 4), #VS
#             minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
# }
# if(sp=='BSB'){
#   PlotRelPM(sp, calc='rd', Pstat='relSSB30',
#             baserefline=1, refline=0,
#             maxylim=c(1.1, 4, 3, 1.5, 5, 3, 2, 1, 2, 1.5, 3), #BSB
#             minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
# }
# if(sp=='BSB_O'){
#   PlotRelPM(sp, calc='rd', Pstat='relSSB30',
#             baserefline=1, refline=0,
#             maxylim=c(3.5, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
#             minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
# }




# treldSSB0
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 300, -0.5, 1, -0.0, 30, 10, 3, 35, 4, 5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=c(-0.5, 0, 0.5),
            maxylim=c(1.1, 3, 1.5, 1, 1.5, 1.5, 1, 1.5, 3, 2, 4), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 4, 3, 1.5, 5, 3, 2, 1, 2, 1.5, 3), #BSB
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
            maxylim=c(1.1, 3, 1.5, 1, 1.5, 1.5, 1, 1.5, 3, 2, 4), #VS
            minylim=c(0, rep(-1,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB' | sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 4, 3, 2, 3, 3, 2.0, 1, 2, 1, 3), #BSB
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
            maxylim=c(3.5, 8, 0.1, 2, 1, 1.5, 2, 2, 3, 2, 3), #VS
            minylim=c(0, rep(-1,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB' | sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3, 7, 0.5, 2, 3, 3, 2.0, 1, 2, 1, 3), #BSB
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
if(sp=='BSB') mpnms<-nms[c(1,5,6,8)]
if(sp=='VS') mpnms<-nms[c(1,6,7)]
if(sp=='RP') mpnms<-nms[c(1,6,7)]
names(get(sp))

# par(mfrow=c(1,4))
# myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY")
par(mfrow=c(3,3), oma=c(0,0,1.2,0))
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", nworms=9)
mtext(species, 3, outer=TRUE, line=-0.3)
par(mfrow=c(3,length(mpnms)), oma=c(0,0,1.2,0))
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="recdev_lo", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="epiM", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
mtext(species, 3, outer=TRUE, line=-0.3)

nms<-get(sp)$base@MPs
par(mfrow=c(3,3), oma=c(0,1.2,1.2,0), mar=c(2.4, 2.4, 0.2, 0.2), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", nworms=9)
mtext(species, 3, outer=TRUE, line=-0.3)
par(mfrow=c(3,3), oma=c(0,0,1.2,0), mar=c(2.4, 3.4, 0.2, 0.2), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="recdev_lo", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="epiM", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
mtext(species, 3, outer=TRUE, line=0)




par(mfrow=c(3,3), oma=c(0,0,1.2,0), mar=c(2.4, 3.4, 0.2, 0.2), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="recdev_lo", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="epiM", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
mtext(species, 3, outer=TRUE, line=0)
