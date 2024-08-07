# install.packages('TMB', type = 'source')
# devtools::install_github("Blue-Matter/openMSE")
# devtools::install_github("Blue-Matter/SAMtool")
# devtools::install_github("nikolaifish/bamExtras", build_vignettes = TRUE)
# devtools::install_github("nikolaifish/bamMSE", build_vignettes = TRUE)
library(abind)
library(magrittr)
library(openMSE)
library(bamExtras)
library(bamMSE)

# install.packages("SAMtool")

rm(list=ls())
t_list <- Sys.time()
myseed <- 8675309

# filepath<-"C:/Users/cassidy.peterson/Documents/Github/"
filepath<-"C:/Users/cassidy.peterson/Documents/git"
# setwd("C:\\Users\\cassidy.peterson\\Documents\\Github\\SEFSCInterimAnalysis\\RunMSE\\SEFSC\\")
setwd(file.path(filepath, "SAtl_Climate_MSE/"))
# setwd("D:/SAtl_MSE")



source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/Assess_diagnostic_NK.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_MP_NK.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_interim_MP_NK.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_projection_MP_NK.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/SCA_NK.R"))
source(file.path(filepath,"SAtl_Climate_MSE/GB_slope1.R"))
source(file.path(filepath,"SAtl_Climate_MSE/GB_target1.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myICI2.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myIratio.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myIslope.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myIT10.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myItarget.R"))


#####

ncores <- 5

nsim <- 250
runScenarios <- TRUE # Run scenarios to do MSE or just generate historical data?
runMSE_args <- list("parallel"=TRUE,"extended"=TRUE,"silent"=FALSE)
lag_Assess_Init <- 2 # Number of years between terminal year of assessment and first year of management. May be modified in scenarios
vulnerability_Assess_Init <- "logistic" # Functional form of vulnerability of catch used in assessments
AddInd_val_Init <- 1  # Index used in interim procedures and possibly assessments. May be modified in scenarios
AddInd_all_assess_Init <- TRUE # Should all available indices be used in assessments? Only works with SCA_NK() May be modified in scenarios
MP_diagnostic <- "min"
maxF <- 3 # set maximum F allowed in simulations

OMName_scen_complete <- NA
#   local({
#   a <- gsub(".rds","",list.files("MSE_obj"))
#   b <- gsub("MSE","OM",a)
#   b
# })

# validcpars()

MPs_user<-c(
  # "SCA_1"
  #"SCA_5"
  # "SCA_10"      # assessment only MPs
  # "pMP_5"
  # "pMP_10"
  # "GB_slope_BSB"
  # "GB_target_BSB"
  # "myICI_BSB"
  # "myIratio_BSB"
  # "myIslope_BSB"
  # "myIT10_BSB"
  # "myItarget_BSB"
  # "GB_slope_RP"
  # "GB_target_RP"
  # "myICI2_RP"
  # "myIratio_RP"
  # "myIT10_RP"
  # "myItarget_RP"
  # "myIslope_RP"
  "GB_slope_VS"
  # "GB_target_VS"
  # "myICI_VS2"
  # "myIratio_VS"
  # "myIT10_VS"
  # "myItarget_VS"
  # "myIslope_VS"
)


MPs_user_interval<- 1 #c(1, 5, 10, 1, 1, 1, 1, 1, 1, 1)
# MPs_user_interval2<-c(5,10)

OMName <- c(
  "OM_BlackSeaBass" #, # Runs
  # "OM_RedPorgy" #, # Runs,
   # "OM_VermilionSnapper" # Runs
)


# Setup loops
scenario <- c("base")
# FUNCTION to generate index observation errors
# gen_AddIerr()
gen_AddIerr <- function(OM,
                        scale_cv=FALSE,
                        bias_cv=FALSE,
                        fix_cv=FALSE,
                        AddIndToMod = 1,
                        args,
                        cv_constant
){

  # args <- get(paste0(scenario_i,"_args"))
  proyears <- OM@proyears
  nyears <- OM@nyears
  years <- OM@nyears+proyears

  # Setup empty array
  AddInd <- OM@cpars$Data@AddInd
  CV_AddInd <- OM@cpars$Data@CV_AddInd
  AddIerr_hist <- AddInd*NA
  AddIerr_proj <- array(NA,
                        dim=c(dim(AddIerr_hist)[1:2],proyears),
                        dimnames = list(dimnames(AddIerr_hist)[[1]],
                                        dimnames(AddIerr_hist)[[2]],
                                        rev(as.numeric(dimnames(AddIerr_hist)[[3]]))[1]+1:proyears
                        )
  )

  # Generate bootstrap residuals for AddInd indices of abundance
  for(i in  1:dim(CV_AddInd)[2]){
    CV_AddInd_i <- CV_AddInd[,i,]
    # Possibly scale cvs
    if(scale_cv & i %in% AddIndToMod){
      CV_AddInd_i <- CV_AddInd_i*args$scale
    }
    # Possibly fix cvs
    if(fix_cv & i %in% AddIndToMod){
      CV_AddInd_i[!is.na(CV_AddInd_i)] <- cv_constant
    }

    if(length(args)==1){
      args$projscale = args$scale
      args$yr1diff = 0
      args$transdur = 0
    }

    AddIerr_hist[,i,] <- t(apply(CV_AddInd_i,1,function(x){
      lnorm_vector_boot(x=x/x,cv=x)
    }))
    AddIerr_proj[,i,] <- t(apply(CV_AddInd_i,1,function(x){
      # x=CV_AddInd_i[1,]
      x_proj <- sample(as.numeric(x[!is.na(x)]),size=proyears,replace=TRUE)
      y<-rep(1,proyears)
      slope <- (args$projscale-args$scale)/args$transdur
      yrdiff <- 0:args$transdur
      yrtrans <- args$yr1diff+yrdiff
      y[yrtrans] <- 1+(slope*yrdiff)
      # Fill in multipliers for years of regime 2
      yrr2 <- (args$yr1diff+args$transdur+1):proyears
      y[yrr2] <- args$projscale
      y=y/min(y)
      # lnorm_vector_boot(x=x_proj/x_proj,cv=x_proj)
      lnorm_vector_boot(x=y,cv=x_proj, standardize=F)
    }))
  }
  AddIerr <- abind::abind(AddIerr_hist,AddIerr_proj,along=3)




  # Add bias to AddInd
  for(i in  1:dim(CV_AddInd)[2]){
    if(bias_cv & i %in% AddIndToMod){
      yr1 <- OM@nyears+args$yr1diff+1
      yrsmod <- yr1:max(years) # Years to modify
      yrdiff <- yrsmod-yr1
      AddIerr[,i,yrsmod] <- t(t(AddIerr[,i,yrsmod])+(args$int+args$slope*yrdiff))
    }
  }

  return(AddIerr)
}

MSEtool::setup(ncores,logical=TRUE) # Run in parallel over ncores
sfLibrary("magrittr", character.only = TRUE, verbose = FALSE)



source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/Assess_diagnostic_NK.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_MP_NK.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_interim_MP_NK.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_projection_MP_NK.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/SCA_NK.R"))
source(file.path(filepath,"SAtl_Climate_MSE/GB_slope1.R"))
source(file.path(filepath,"SAtl_Climate_MSE/GB_target1.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myICI2.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myIratio.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myIslope.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myIT10.R"))
source(file.path(filepath,"SAtl_Climate_MSE/myItarget.R"))
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/merge_MSE.R")) # Define MPs


#
OMName_k<-OMName[1] #OMName_O[3]
scenario_i<-scenario[1] #scenario[1]




MSEName_k <- gsub("OM","MSE",OMName_k)
DataName_k <- gsub("OM","Data",OMName_k)

OMInit_k <- readRDS(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/OM/", paste0(OMName_k, ".rds")))
DataInit_k <- readRDS(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/Data/", paste0(DataName_k, ".rds")))
Data_k <- DataInit_k
MPs_user_k <- MPs_user


# for(scenario_i in scenario) { ######### Loop over scenario
set.seed(myseed)
# All scenarios
OMName_scen <- paste0(OMName_k, "_", scenario_i)
OM_k <- OMInit_k
OM_k@maxF <- maxF
# OM_k@nsim <- nsim
# Set wider range for recruitment autocorrelation
OM_k@AC <- sort(pmin(pmax(OM_k@AC*(1+c(-1,1)*.5),0.05),0.95))
# Remove qs so that runMSE will Optimize for user-specified depletion in last historical year
# May not even be necessary if fixq1 = FALSE in Assess2OM.
OM_k@cpars <- OM_k@cpars[names(OM_k@cpars)[names(OM_k@cpars)!="qs"]]
M_at_age <- TRUE
vulnerability_Assess <- vulnerability_Assess_Init
AddInd_val <- AddInd_val_Init
AddInd_all_assess <- AddInd_all_assess_Init
lag_Assess <- lag_Assess_Init
MSY_frac <- MSY_frac_Init

MSY_frac_Init <- 1.25 # Fraction of MSY for setting TAC. May be modified in scenarios
# BSB
# MSY_frac | med(SSBratio30)
# 1 | 1.15
# 1.1 | 1.1
# 1.2 | 1.06
# 1.25 | 1.046

# VS
# MSY_frac | med(SSBRatio30)
# 1 | 0.97

# RP
# MSY_frac | med(SSBratio30)
#  1 | 0.779
#  0.8 | 1.00

MSY_frac <- MSY_frac_Init
#### Define custom stock assessment function #####
MP_args <- list(.Assess = "SCA_NK", .HCR = "HCR_MSY", MSY_frac = MSY_frac,
                AddInd = AddInd_val, AddInd_all=AddInd_all_assess,diagnostic=MP_diagnostic)
BAM_SCA_args <- list(SR="BH",
                     vulnerability = vulnerability_Assess,
                     early_dev = "comp_onegen",
                     late_dev = "comp50",
                     CAA_dist = "multinomial",
                     lag=lag_Assess,
                     M_at_age=M_at_age)

source(file.path(filepath,'SEFSCInterimAnalysis/RunMSE/SEFSC/fn/iMP.R')) # Define MPs


OM_k@nsim <- nsim
OM_k <- SubCpars(OM_k, sims = 1:nsim) # Limit number of simulations

# Save OM to object
OMName_ki <- paste0(OMName_k, "_", scenario_i)
assign(OMName_ki,OM_k)

######## All MPs ##########
# All intervals are 1 except for SCA MPs which might be longer
# OM_k@interval <- local({
#   a <- rep(1,length(MPs_user))
#   SCA_MPs_ix <- which(grepl("^SCA_",MPs_user))
#   a[SCA_MPs_ix] <- as.numeric(gsub("^SCA_","",MPs_user[SCA_MPs_ix]))
#   a
# })

OM_k@interval <- MPs_user_interval
#set.seed(myseed)
# myHist_Init <- Simulate(OMInit_k)
set.seed(myseed)
# myHist <- Simulate(OM_k)



###### RUN ###########
message(paste("Run all MPs for:", OMName_ki))
t_list <- c(t_list,Sys.time())
message(paste0("at: ",tail(t_list,1),".(",round(diff(tail(t_list,2)),2)," since start)"))
# Run all MPs together so that the Hist objects are always identical
set.seed(myseed)
sfExport(list = c("Assess_diagnostic_NK","SCA_NK","MSY_frac",MPs_user_k))


# source(file.path(filepath,"SAtl_Climate_MSE/myIslope.R"))

# ##### TESTING CMPS ##############
# OM_k<-OM_VS_recdevlo  #recdevhi #Base #recdevlo
# OM_k<-OM_VS_Base #recdevhi #Base #recdevlo
OM_k@interval <- 1
set.seed(myseed)
# Hist <- Simulate(OM_k, parallel = FALSE, silent=runMSE_args$silent)
# Hist_VS <- Hist ; Hist_BSB<- Hist ; Hist_RP<-Hist
MSE <- Project(Hist,MPs = c("SCA_1"),
                parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)
# MSE <- runMSE(OM_k,MPs = c("GB_slope_VS"),
#               parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)

median( MSE@SB_SBMSY[,1,30] )
# median( MSE@SB_SBMSY[,1,31] )

# dim(MSE@SB_SBMSY)
# dim(MSE1@SB_SBMSY)
plot(apply(MSE@SB_SBMSY[,1,], 2, median), type='l', lwd=2, ylab="SSB/SSB_MSY", xlab="Proj years", ylim=c(0, 2))
abline(h=1)
lines(apply(MSE@SB_SBMSY[,1,], 2, median), col='orchid', lwd=2, lty=2)
lines(apply(MSE@SB_SBMSY[,1,], 2, median), col='skyblue2', lwd=2, lty=2)
lines(apply(MSE@SB_SBMSY[,1,], 2, median), col='gray', lwd=2)

#SCA_1 = black, myIT10_VS=lightgreen, myIratio_VS=lightblue, GB_target_VS=orchid

#MSE_refbias_hi<- MSE
# MSE_refbias_lo<-MSE
MSE_refbias_hi@SB_SBMSY[1,,] == MSE_refbias_lo@SB_SBMSY[1,,]

# ## Example for figure.
# OM_k@interval <- 1
# set.seed(myseed)
# MSE_VSdefault <- runMSE(OM_k,MPs = "Itarget1",
#                       parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)
# OM_k@interval <- 1
# set.seed(myseed)
# MSE_VStuned <- runMSE(OM_k,MPs = "myItarget_VS",
#                       parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)
#
# plot(apply(MSE_VSdefault@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, 2), lwd=2, ylab="SSB/SSB_MSY", xlab="Projection years")
# abline(h=1)
# lines(apply(MSE_VStuned@SB_SBMSY[,1,], 2, median), col='deepskyblue', lwd=2)
# legend("bottomright", c("Default", "Tuned"), lwd=2, col=c('black','deepskyblue'), bty='n')
# mtext("Vermilion Snapper Itarget Tuning", 3, line=1.2)


# plot(apply(MSE1@Catch[,1,], 2, median), type='b', ylim=c(0, 2500))
# lines(apply(MSE@Catch[,2,], 2, median), col='blue')
# lines(MSE@Catch[1,1,], col='blue')
# lines(MSE@Catch[1,2,], col='blue')
# lines(MSE@Catch[1,3,], col='red')
# # MSE1@PPD[[1]]
# # > MSE1@PPD[[1]]@AddInd[,1,]
#
# MSE@SSB_hist / MSE@OM$SSBMSY[1:46]
# # set.seed(myseed)
# OM_k@interval<-1
# MSE1<-runMSE(OM_k,MPs="myIslope",
#              parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)
# set.seed(myseed)
# OM_k@interval<-5
# MSE5<-runMSE(OM_k,MPs="SCA_5",
#              parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)
# set.seed(myseed)
# OM_k@interval<-10
# MSE10<-runMSE(OM_k,MPs="SCA_10",
#              parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)
#
# MSE<-merge_MSE(MSE1, MSE5, MSE10)
#



t_list <- c(t_list,Sys.time())
message(paste0("batch 1 finished at ",tail(t_list,1),".(",round(diff(tail(t_list,2)),2)," duration"))


res <- MSE_batch
names(res@PPD) <- res@MPs

saveRDS(res,file = paste0("MSE_obj/", MSEName_k, "_", scenario_i, ".rds"))
# } #end if OMName_scen is not complete
# }
# }

# save.image("run_script.RData")

sfStop()

#
save.image(file="OMs_loaded.RData")


saveRDS(OM_VS_Base, file="OM_VS_Base.RDS")
saveRDS(OM_VS_recdevlo, file="OM_VS_recdevlo.RDS")
saveRDS(OM_VS_recdevhi, file="OM_VS_recdevhi.RDS")
saveRDS(OM_BSB_Base, file="OM_BSB_Base.RDS")
saveRDS(OM_BSB_recdevlo, file="OM_BSB_recdevlo.RDS")
saveRDS(OM_BSB_recdevhi, file="OM_BSB_recdevhi.RDS")
saveRDS(OM_RP_Base, file="OM_RP_Base.RDS")
saveRDS(OM_RP_recdevlo, file="OM_RP_recdevlo.RDS")
saveRDS(OM_RP_recdevhi, file="OM_RP_recdevhi.RDS")

OM_VS_Base<-readRDS("OM_VS_Base.RDS")
OM_VS_recdevlo<-readRDS("OM_VS_recdevlo.RDS")
OM_VS_recdevhi<-readRDS("OM_VS_recdevhi.RDS")
OM_BSB_Base<-readRDS("OM_BSB_Base.RDS")
OM_BSB_recdevlo<-readRDS("OM_BSB_recdevlo.RDS")
OM_BSB_recdevhi<-readRDS("OM_BSB_recdevhi.RDS")
OM_RP_Base<-readRDS("OM_RP_Base.RDS")
OM_RP_recdevlo<-readRDS("OM_RP_recdevlo.RDS")
OM_RP_recdevhi<-readRDS("OM_RP_recdevhi.RDS")
