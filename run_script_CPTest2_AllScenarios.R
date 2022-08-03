# install.packages('TMB', type = 'source')
# devtools::install_github("Blue-Matter/openMSE")
# devtools::install_github("nikolaifish/bamExtras", build_vignettes = TRUE)
# devtools::install_github("nikolaifish/bamMSE", build_vignettes = TRUE)
library(abind)
library(magrittr)
library(openMSE)
library(bamExtras)
library(bamMSE)


# setwd("C:\\Users\\cassidy.peterson\\Documents\\Github\\SEFSCInterimAnalysis\\RunMSE\\SEFSC\\")
setwd("C:/Users/cassidy.peterson/Documents/Github/SAtl_Climate_MSE/")
# setwd("D:/SAtl_MSE")


rm(list=ls())
t_list <- Sys.time()
myseed <- 8675309


source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/Assess_diagnostic_NK.R")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_MP_NK.R")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_interim_MP_NK.R")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_projection_MP_NK.R")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/SCA_NK.R")
source("GB_target1.R")
source("GB_slope1.R")
source("myIslope.R")
source("myIT10.R")
source("myItarget.R")
source("myIratio.R"   )



######
# BSB_init <- readRDS("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/OM/OM_BlackSeaBass.rds")
# BSBo<-Replace(BSB_init, Overages, Name="BSB_Over")
# saveRDS(BSBo,file = "C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/OM/OM_BlackSeaBass_Over.rds")
#
# VS_init <- readRDS("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/OM/OM_VermilionSnapper.rds")
# VSo<-Replace(VS_init, Overages, Name="VS_Over")
# saveRDS(VSo,file = "C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/OM/OM_VermilionSnapper_Over.rds")
#
# RP_init <- readRDS("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/OM/OM_RedPorgy.rds")
# RPo<-Replace(RP_init, Overages, Name="RP_Over")
# saveRDS(RPo,file = "C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/OM/OM_RedPorgy_Over.rds")
#####

ncores <- 10

nsim <- 48 #250
runScenarios <- TRUE # Run scenarios to do MSE or just generate historical data?
runMSE_args <- list("parallel"=TRUE,"extended"=TRUE,"silent"=FALSE)
lag_Assess_Init <- 2 # Number of years between terminal year of assessment and first year of management. May be modified in scenarios
vulnerability_Assess_Init <- "logistic" # Functional form of vulnerability of catch used in assessments
MSY_frac_Init <- 1 # Fraction of MSY for setting TAC. May be modified in scenarios
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

MPs_user <- c(
  #"CC1","SPMSY", # "AvC", "DCAC", "DBSRA", # simple MPs
  "SCA_1",#"SCA_5","SCA_10",        # assessment only MPs
  # "iMP_avg_5",    "iMP_avg_10",    # interim average MPs
  # "iMP_buffer_5", "iMP_buffer_10", # interim buffered MPs
  "pMP_5","pMP_10" ,                # projection MPs
  "GB_slope1", #Geromont and Butterworth index slope Harvest Control Rule (https://dlmtool.openmse.com/reference/GB_slope.html)
  "GB_target1", # tuning parameter (w); Geromont and Butterworth target CPUE and catch MP (https://dlmtool.openmse.com/reference/GB_target.html)
  "ICI2", # Index Confidence Interval, where ICI is more precautionary and ICI2 is less precautionary (https://dlmtool.openmse.com/reference/ICI.html)
  "myIratio", #mean index ratio (https://dlmtool.openmse.com/reference/Iratio.html)
  #"myIslope", #Index Slope Tracking MP -- maintain constant CPUE (https://dlmtool.openmse.com/reference/Islope1.html); Islope1 is the least precautionary
  "myIT10", #iterative index target MP w/ 10% allowable tac change (https://dlmtool.openmse.com/reference/IT5.html)
  "myItarget", #Imulti, xx tuning parameters; Incremental index target MP (https://dlmtool.openmse.com/reference/Itarget1.html) 1 is least precautionary
  "L95target" #Length target TAC MP (https://dlmtool.openmse.com/reference/Ltarget1.html)
)

MPs_user_BSB <- c("SCA_1", "pMP_5","pMP_10" , "GB_slope_BSB", "GB_target_BSB",
                  "myICI_BSB", "myIratio_BSB", "myIslope_BSB", "myIT10_BSB", "myItarget_BSB")
MPs_user_RP <- c("SCA_1", "pMP_5", "pMP_10", "GB_slope_RP", "GB_target_RP", "myICI2_RP", "myIratio",
                 "myIT10_RP", "myItarget_RP", "myIslope_RP")
MPs_user_VS <- c("SCA_1", "pMP_5", "pMP_10", "GB_slope_VS", "GB_target_VS", "myICI2_VS", "myIratio_VS",
                 "myIT10_VS", "myItarget_VS", "myIslope_VS")

MPs_user2 <- c("SCA_5","SCA_10")
MPs_user_interval<-c(1, 5, 10, 1, 1, 1, 1, 1, 1, 1)
MPs_user_interval2<-c(5,10)

# MPs_user_test <- c(
#  "myIratio",
#   "myIslope",
#   "myItarget" #Imulti, xx tuning parameters; Incremental index target MP (https
# )


# OMName_all <- gsub(".rds","",list.files("OM"))
# OMName <- OMName_all[!OMName_all%in%OMName_complete]
OMName <- c(#"OM_BlackSeaBass" #, # Runs
             "OM_RedPorgy" #, # Runs,
            # "OM_SnowyGrouper",
            # "OM_VermilionSnapper" # Runs
            #"OM_RedGrouper", # 2022-1-19 This took 7 hours just to run the base scenario batch 1 so I interrupted it
            #,"OM_GagGrouper"#, # Runs
            #,"OM_GrayTriggerfish" # Problems with lightly fished scenario where "More than 5 % of simulations can't get to the specified level of depletion with these Operating Model parameters"
            #"OM_RedSnapper" # Seems to run but takes a long time
)




# Setup loops
scenario <- c("base"
              ,"recns" #nonstationarity
              ,"age0M_hi" # decrease recruitment regime shift
              ,"age0M_lo" # increase recruitment regime shift
              ,"uobs_hi" # increase index deviations / obs error
              ,"uobs_lo" # decrease index deviations / obs error
              ,"refbias_hi" # ref poitns biased high
              ,"refbias_lo" # ref points biased low
              ,"epiM"  # Red Porgy sometimes has problems getting down to the specified level of depletion
              ,"recdev"     # Regime change
              ######
              #,"hs"
              #,"hd"
              #,"vdome" # Assume dome-shaped selectivity of catch in assessments
              #,"constM"  # M is constant in the operating model
              #,"catcvlo"
              #,"ucvhi"
              #,"ucvlo"
              #,"ubias"
              ##,"SCAfree" # TFree up some parameters in the SCA model (Very long run time for Vermilion Snapper)
              #,"basealt" # Alternative base ("SCAfree" + "catcvlo"). Didn't take very long in Vermilion Snapper
              #,"dep"
              #,"lf"
               #,"lhset"    # set certain life history parameters
              #,"minerr"   # Minimize error and variation in operating model
              #,"minrecdev"  # Minimize recruitment deviations
              #,"Mset"     # set M
              #,"noempind"
              #,"nolag"
              #,"perfobs" # Perfect observations
              #,"steepset"  # set steepness
              #,"tachi"
              #,"taclo"

              #,"vtiv"    # Vulnerability time-invariant in historic period
              ##,"genfle"   # Generic fleet (kicked out error in RedPorgy "'Len_age' must be array with dimensions: nsim, maxage+1, nyears + proyears.")
              #####
)


# Nonstationary recruitment
recns_args <- list("yr1diff"=  0,   # Number of years between the beginning of the projection period and start of change in rec devs
                   "y0"=1,"sd"=5,"mu"=0) # Arguments passed to bamExtras::random_walk

# Set catch cv in SCA models to be very low (like in BAM)
catcvlo_args <- list("cv"=0.05)

# Set life history parameters
lhset_args <- list("h"=0.84, "M"=0.2)

# Depletion scenario args
dep_args <- list("scale"=0.5, "min"=0.05)

# Lightly fished scenario args
lf_args <- list("scale"=2,"max"=1)

# Hyperstable scenario
hs_args <- list("min"=1/3,"max"=2/3)

# Hyperdeplete scenario
hd_args <- list("min"=1.5,"max"=3)

## Episodic M scenario args
# yrprop:  Proportion of years to apply a multiplier on M (Huynh used 0.1)
# M_mult_max:  Limit on how high the multiplier on M can be (Huynh set it to 4 for age-invariant M
#     but when I used that the TACs were absurdly high like 1e+11 causing other values to be absurd)
#     Values as high as 0.5 resulted in some MSY values equal to zero for RedPorgy though it was fine for
#     BlackSeaBass and VermilionSnapper. I think it has to do with how depleted Red Porgy is. Perhaps it is crashing the population
#     when M is too high.
# M_lognorm_sd: lognormal sd on distribution of M_mult. Huynh used 2 but that results in a lot of values above
#     M_mult_max, so when  you apply pmin to limit the maximum value, then M_mult_max
#     ends up being one of the most common M_mult values
epiM_args_init <- list(
  "OM_RedPorgy"=list("yrprop" = 0.1, "M_mult_max" = 3, "M_lognorm_sd" = 0.2),
  "OM_other"  = list("yrprop" = 0.1, "M_mult_max" = 4, "M_lognorm_sd" = 0.2)
)

# Index cv high scenario args
ucvhi_args <- list("scale"=2)

# Index cv low scenario args
ucvlo_args <- list("scale"=0.5)

# Index bias scenario args
ubias_args <- list("int"=0,
                   "slope"=-0.01, # Slope of change in index error per year
                   "yr1diff"=10  # Number of years between the beginning of the projection period and start of change in errors
)

# Regime change scenario args
# rc_args <-  list("yr1diff"=  10,   # Number of years between the beginning of the projection period and start of change in rec devs
#                  "transdur"= 10,   # Duration (in years) of transition between regime 1 and 2
#                  "r2_mult" =  0.75  # Multiplier on rec devs for regime 2. (a value of 1 would mean recruitment was not changing)
# )
# Regime change scenario args
recdev_args <-  list("yr1diff"=  10,   # Number of years between the beginning of the projection period and start of change in rec devs
                 "transdur"= 10,   # Duration (in years) of transition between regime 1 and 2
                 "r2_mult" =  2  # Multiplier on rec devs for regime 2. (a value of 1 would mean recruitment was not changing)
)

#
tachi_args <- list("MSY_frac"=1.25)

# Minimum error arguments
minerr_args <- list("cv_constant"=0.05,"LenCV"=0.05,"Perr"=c(0,0.05))

# SCAfree
SCAfree_args <- list(fix_h=FALSE,fix_F_equilibrium=FALSE, fix_omega=FALSE, fix_tau=FALSE)

# seeds <- setNames(sample(1:10000,length(OMName),replace = FALSE),OMName)


Irefbias_lo_args<-list("min"=0.2, "max"=0.5)
Irefbias_hi_args<-list("min"=1.2, "max"=1.5)
Brefbias_lo_args<-list("min"=0.2, "max"=0.5)
Brefbias_hi_args<-list("min"=1.2, "max"=1.5)
Crefbias_lo_args<-list("min"=0.2, "max"=0.5)
Crefbias_hi_args<-list("min"=1.2, "max"=1.5)

refbias_hi_args<-list(Irefbias_args = Irefbias_hi_args,
                      Brefbias_args = Brefbias_hi_args,
                      Crefbias_args = Crefbias_hi_args)
refbias_lo_args<-list(Irefbias_args = Irefbias_lo_args,
                      Brefbias_args = Brefbias_lo_args,
                      Crefbias_args = Crefbias_lo_args)


# OM_k@cpars$AddIerr <- gen_AddIerr(OM_k,scale_cv = TRUE,args=ucvhi_args)
# uchi_args = list('scale'=2)
proj_ucv_args = list('scale'=1,        # hist scale param
                 'projscale'=2,    # projection scale param
                 "yr1diff"=  10,   # Number of years between the beginning of the projection period and start of change in rec devs
                 "transdur"= 10    # Duration (in years) of transition between regime 1 and 2
)
uobs_lo_args<-list(scale1 = 0, scale2 = -2,
                   startyr = 10, transdur = 10)
uobs_hi_args<-list(scale1 = 0, scale2 = 2,
                   startyr = 10, transdur = 10)
recns_args <- list("yr1diff"=  0,   # Number of years between the beginning of the projection period and start of change in rec devs
                   "y0"=1,"sd"=5,"mu"=0) # Arguments passed to bamExtras::random_walk

age0M_hi_args<- list("yr1diff"= 10, # Number of years between the beginning of the projection period and start of change
                     "transdur"= 10, # Duration (in years) of transition between regime 1 and 2
                     "a0_mult" = 2 # Multiplier on age0 survival for regime 2. (a value of 1 would mean M0 was not changing)
)
age0M_lo_args<- list("yr1diff"= 10, # Number of years between the beginning of the projection period and start of change
                      "transdur"= 10, # Duration (in years) of transition between regime 1 and 2
                      "a0_mult" = 0.5 # Multiplier on age0 survival for regime 2. (a value of 1 would mean M0 was not changing)
)

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



source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/Assess_diagnostic_NK.R")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_MP_NK.R")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_interim_MP_NK.R")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/make_projection_MP_NK.R")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/SCA_NK.R")
source("GB_slope1.R")
source("GB_target1.R")
source("myIratio.R")
source("myIslope.R")
source("myIT10.R")
source("myItarget.R")
source('C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/merge_MSE.R') # Define MPs


#
OMName_k<-OMName[1]
scenario_i<-scenario[1]






for(OMName_k in OMName)       { ######### Loop over operating model

  MSEName_k <- gsub("OM","MSE",OMName_k)
  DataName_k <- gsub("OM","Data",OMName_k)

  OMInit_k <- readRDS(paste0("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/OM/", OMName_k, ".rds"))
  DataInit_k <- readRDS(paste0("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/Data/", DataName_k, ".rds"))
  Data_k <- DataInit_k

  if(OMName_k=="OM_BlackSeaBass") MPs_user_k <- MPs_user_BSB
  if(OMName_k=="OM_RedPorgy") MPs_user_k <- MPs_user_RP
  if(OMName_k=="OM_VermilionSnapper") MPs_user_k <- MPs_user_VS

  for(scenario_i in scenario) { ######### Loop over scenario
    set.seed(myseed)

    # All scenarios
    OMName_scen <- paste0(OMName_k, "_", scenario_i)
    # if(!OMName_scen%in%OMName_scen_complete){
      OM_k <- OMInit_k

      OM_k@maxF <- maxF
      # OM_k@nsim <- nsim

      # Set wider range for recruitment autocorrelation
      OM_k@AC <- sort(pmin(pmax(OM_k@AC*(1+c(-1,1)*.5),0.05),0.95))

      # Remove qs so that runMSE will Optimize for user-specified depletion in last historical year
      # May not even be necessary if fixq1 = FALSE in Assess2OM.
      OM_k@cpars <- OM_k@cpars[names(OM_k@cpars)[names(OM_k@cpars)!="qs"]]

      ## Generate observation error for AddInd
      ## ERROR ##### # OM_k@cpars$AddIerr <- gen_AddIerr(OM_k)




      #### SCENARIOS #####

      # M varies with age in the operating model
      # By default the OMs contain age-varying M but it's usually set to constant since the SCA uses constant M.
      if(scenario_i=="constM"){
        OM_k@cpars$M_ageArray[] <- Data_k@Mort
        M_at_age <-  FALSE
      }else{
        M_at_age <- TRUE
      }

      # Set multiple life history parameters
      if(scenario_i=="lhset"){
        OM_k@cpars$M_ageArray[] <- lhset_args$M
        OM_k@cpars$hs[] <- lhset_args$h
      }

      # Set M
      if(scenario_i=="Mset"){
        OM_k@cpars$M_ageArray[] <- lhset_args$M
      }

      # Set steepness
      if(scenario_i=="steepset"){
        OM_k@cpars$hs[] <- lhset_args$h
      }

      # Hyperstable
      if(scenario_i == "hs") {#OM_k@beta <- c(1/3, 2/3) # beta values below 1 lead to hyperstability
        AddInd_dim <- dim(OM_k@cpars$Data@AddInd)
        AddIbeta <- matrix(1,nrow=AddInd_dim[1],ncol=AddInd_dim[2])
        AddIbeta[,1] <- runif(n=AddInd_dim[1],min=hs_args$min, max=hs_args$max)
        OM_k@cpars$AddIbeta <- AddIbeta
      }
      # Hyperdeplete
      if(scenario_i == "hd") {
        #OM_k@beta <- c(1.5, 3)
        AddInd_dim <- dim(OM_k@cpars$Data@AddInd)
        AddIbeta <- matrix(1,nrow=AddInd_dim[1],ncol=AddInd_dim[2])
        AddIbeta[,1] <- runif(n=AddInd_dim[1],min=hd_args$min, max=hd_args$max)
        OM_k@cpars$AddIbeta <- AddIbeta
      }

      # Depleted
      if(scenario_i == "dep") {
        OM_k@cpars$D <- pmax(dep_args$scale * OM_k@cpars$D, dep_args$min)
      }

      # Lightly fished
      if(scenario_i == "lf")  {
        OM_k@cpars$D <- pmin(lf_args$scale * OM_k@cpars$D, lf_args$max)
      }

      # Episodic M
      if(scenario_i == "epiM") {
        # This is the original code for this scenario written by Quang Huynh
        # (slightly modified to account for OM_k@maxage+1 age classes in the current version of openMSE)
        set.seed(myseed)
        if(OMName_k=="OM_RedPorgy"){
          epiM_args <- epiM_args_init$OM_RedPorgy
        }else{
          epiM_args <- epiM_args_init$OM_other
        }

        # OM_k@cpars$M_ageArray [ nsim , ages 0-maxage , nyears+proyears ]
        OM_k@cpars$M_ageArray <- with(epiM_args,{
          M_mult <- rbinom(OM_k@proyears * OM_k@nsim, 1, yrprop) * pmin(exp(rnorm(n=OM_k@proyears * OM_k@nsim, mean=0, sd=M_lognorm_sd)),
                                                                        M_mult_max)
          M_mult_age <- rep(M_mult,each=OM_k@maxage+1) # Vector of multipliers repeating for each age
          M_array_hist <- OM_k@cpars$M_ageArray[,,1:OM_k@nyears]
          M_array_proj1 <- OM_k@cpars$M_ageArray[,,-(1:OM_k@nyears)]
          a1 <- as.numeric(aperm(M_array_proj1, perm = c(2, 1, 3))) # vectorize array and rearrange dimensions
          M_y <- a1 * (1 + M_mult_age) # Note that one is added to the multiplier so that the observed M is actually the minimum
          M_array_proj <- aperm(array(M_y, dim = c(OM_k@maxage+1,OM_k@nsim, OM_k@proyears)), perm = c(2, 1, 3))
          return(abind::abind(M_array_hist, M_array_proj, along = 3))
        })
        # hist(OM_k@cpars$M_ageArray)
        # NK modified this section to apply to M-at-age
        # OM_k@cpars$M_ageArray <- with(epiM_args,{
        #   M_mult <- rbinom(OM_k@proyears * OM_k@nsim, 1, yrprop) * pmin(exp(rnorm(OM_k@proyears * OM_k@nsim, 0, 2)), M_mult_max)
        #   M_mult_age <- rep(M_mult,each=OM_k@maxage+1) # Vector of multipliers repeating for each age
        #   M_array_hist <- OM_k@cpars$M_ageArray[,,1:OM_k@nyears]
        #   M_array_proj1 <- OM_k@cpars$M_ageArray[,,-(1:OM_k@nyears)]
        #   a1 <- as.numeric(aperm(M_array_proj1, perm = c(2, 1, 3))) # vectorize array and rearrange dimensions
        #   M_y <- a1 * (1 + M_mult_age) # Note that one is added to the multiplier so that the observed M is actually the minimum
        #   M_array_proj <- aperm(array(M_y, dim = c(OM_k@maxage+1,OM_k@nsim, OM_k@proyears)), perm = c(2, 1, 3))
        #   return(abind::abind(M_array_hist, M_array_proj, along = 3))
        # })
      }

      # Index CV high
      if(scenario_i=="ucvhi"){
        ## Generate observation error for AddInd
        OM_k@cpars$AddIerr <- gen_AddIerr(OM_k,scale_cv = TRUE,args=ucvhi_args)

        #### This didn't have the desired effect. It didn't actually seem to change the AddInd in the projection period
        # CV_AddInd1 <- OM_k@cpars$Data@CV_AddInd[,1,]
        # CV_AddInd1[!is.na(CV_AddInd1)] <- ucvhi_args$cv
        # OM_k@cpars$Data@CV_AddInd[,1,] <- CV_AddInd1

      } else if(scenario_i=="ucvlo"){ # Index CV low
        ## Generate observation error for AddInd
        OM_k@cpars$AddIerr <- gen_AddIerr(OM_k,scale_cv = TRUE, args=ucvlo_args)

      }
      if(scenario_i=="proj_ucv"){
        OM_k@cpars$AddIerr <- gen_AddIerr(OM_k,scale_cv = TRUE, args=proj_ucv_args)
      } #else {
      #   ucvreg_args<- list("scale"=1)
      #   OM_k@cpars$AddIerr <- gen_AddIerr(OM_k,scale_cv = TRUE, args=ucvreg_args)
      # } # end if - else if - else statement



      # Regime change (change in average recruitment deviations)
      if(scenario_i=="recdev"){
        args <- get(paste0(scenario_i,"_args"))
        Perr_y <- OM_k@cpars$Perr_y
        years <- dim(Perr_y)[2]
        y_mult <- local({
          x <- rep(1,years)
          yr1 <- (years-OM_k@proyears)+args$yr1diff+1
          # Compute multiplier for transitional period
          slope <- (args$r2_mult-x[yr1])/args$transdur
          yrdiff <- 0:args$transdur
          yrtrans <- yr1+yrdiff
          x[yrtrans] <- 1+(slope*yrdiff)
          # Fill in multipliers for years of regime 2
          yrr2 <- (yr1+args$transdur+1):years
          x[yrr2] <- args$r2_mult
          x
        })
        val <- t(t(Perr_y)*y_mult)
        OM_k@cpars$Perr_y <- val
      }



      # Non-stationary recruitment (random walk in average recruitment deviations)
      # recns_args <- list("yr1diff"=  0,   # Number of years between the beginning of the projection period and start of change in rec devs
      #                    "y0"=1,"sd"=5,"mu"=0) # Arguments passed to bamExtras::random_walk

      if(scenario_i=="recns"){
        args <- get(paste0(scenario_i,"_args"))
        Perr_y <- OM_k@cpars$Perr_y

        years <- dim(Perr_y)[2]
        x <- rep(1,years)
        yr1 <- (years-OM_k@proyears)+args$yr1diff+1
        yrs2 <- yr1:years
        y_adj2 <- sapply(1:dim(Perr_y)[1],FUN=function(x){
          #y0 <- Perr_y[x,yr1]
          0+random_walk(x=yrs2,y0=args$y0,sd=args$sd,mu=args$mu)/100}
        )
        Perr_y[,yrs2] <-  Perr_y[,yrs2]+t(y_adj2)
        Perr_y[Perr_y<0] <- 0.01 # Minimum recruitment multiplier low but not zero

        val <- Perr_y
        OM_k@cpars$Perr_y <- val

        # Plot what these values look like
        #matplot(t(Perr_y),type="l",ylim=c(0,4))
      }


      # Index bias trend
      if(scenario_i=="ubias"){
        ## Generate observation error for AddInd
        OM_k@cpars$AddIerr <- gen_AddIerr(OM_k,bias_cv = TRUE, args=ubias_args)

      }

      ########### induce shift in index observation error over time
      if(scenario_i == "uobs_hi" | scenario_i == "uobs_lo"){
        args=get(paste0(scenario_i,"_args"))
        n = OM_k@proyears + OM_k@nyears
        pron = OM_k@proyears
        AIerr<-array(data=NA, dim=c(dim(OM_k@cpars$Data@AddInd)[1], dim(OM_k@cpars$Data@AddInd)[2], n))

        for(i in 1:dim(OM_k@cpars$Data@AddInd)[2]){
          err <- matrix(rnorm(n = n*OMInit_k@nsim, mean = 0, sd = runif(1, OM_k@Iobs[1], OM_k@Iobs[2])),
                        nrow = dim(OM_k@cpars$Data@AddInd)[1])
          y_mult <- local({
            slope<-(args$scale2-args$scale1)/args$transdur
            x<-rep(args$scale1, length=n)
            yrdiff<-1:args$transdur
            yrtrans<-(n-pron) + args$startyr + yrdiff
            x[yrtrans]<- args$scale1+(slope*yrdiff)
            x[-c(1:(max(yrtrans)))]<-args$scale2
            x_mat <- matrix(rep(x, times=dim(OM_k@cpars$Data@AddInd)[1]), ncol=n,  byrow=T)
            x_mat
          })
          y <- y_mult + err
          AIerr[,i, ] <- y
        } # end for loop


        OM_k@cpars$AddIerr <- AIerr
      }



      # Mult year 0 survival ########################################################
      if(scenario_i == "age0M_hi" | scenario_i=="age0M_lo") {
        # This is the original code for this scenario written by Quang Huynh
        # (slightly modified to account for OM_k@maxage+1 age classes in the current version of openMSE)
        set.seed(myseed)
        args<-get(paste0(scenario_i,"_args"))
        M_ageArray <- OM_k@cpars$M_ageArray #[nsims, ages+1, histyears+projyears]

        M0_mult_y<-local({

          proyears <- OM_k@proyears
          nyears <- OM_k@nyears
          years <- OM_k@nyears+proyears

          x<-rep(1,years)
          yr1 <- nyears + args$yr1diff

          # Compute multiplier for transitional period
          slope <- (args$a0_mult-x[yr1])/args$transdur
          yrdiff <- 0:args$transdur
          yrtrans <- yr1+yrdiff
          x[yrtrans] <- 1+(slope*yrdiff)
          # Fill in multipliers for years of regime 2
          yrr2 <- (yr1+args$transdur+1):years
          x[yrr2] <- args$a0_mult
          x


        })


        M_ageArray[,1,] <- t(t(M_ageArray[,1,])*M0_mult_y)

        # to modify survival instead of F
        # M_ageArray[1,1,] <-  -log(exp(-M_ageArray[1,1,])*x)

        OM_k@cpars$M_ageArray <- M_ageArray

      } # end age0M scenario



      # Bias ####################
      # AddIbeta -- Beta for each sim and index (matrix[nrow=nsim, ncol=n.ind])
      # Cbias - numeric vector length nsim, catch bias by simulation
      # Cobservation error
      if(scenario_i=="refbias_hi" | scenario_i=="refbias_lo"){
        args<-get(paste0(scenario_i,"_args"))

        #Iref
        Iargs<-args$Irefbias_args
        OM_k@cpars$Irefbias <- runif(n=OMInit_k@nsim, min=Iargs$min, max=Iargs$max)

        # Bref
        Bargs<-args$Brefbias_args
        OM_k@cpars$Brefbias <- runif(n=OMInit_k@nsim, min=Bargs$min, max=Bargs$max)

        #Cref
        Cargs<-args$Crefbias_args
        OM_k@cpars$Crefbias <- runif(n=OMInit_k@nsim, min=Cargs$min, max=Cargs$max)

      }



      # Vulnerability time-invariant during historic years. Default is constant across historic years
      if(scenario_i=="vtiv"){
        OM_k@cpars$V <- local({
          V <- OM_k@cpars$V
          V2 <- aperm(V, perm = c(2, 1, 3))
          Vc <- V[1,,OM_k@nyears+1]
          V3 <- array(Vc,dim=dim(V2))
          aperm(V3, perm = c(2, 1, 3))
        })
      }

      # Vulnerability dome shaped in stock assessments
      if(scenario_i=="vdome"){
        vulnerability_Assess <- "dome"
      }else{
        vulnerability_Assess <- vulnerability_Assess_Init
      }

      # Perfect observation
      if(scenario_i=="perfobs"){
        OM_k <- Replace(OM_k, Perfect_Info)
      }

      # Generic fleet
      if(scenario_i=="genfle"){
        OM_k <- Replace(OM_k, Generic_Fleet)
      }

      if(scenario_i=="minerr"){
        # Minimize observation error
        OM_k <- Replace(OM_k, Perfect_Info)
        ## Minimize other sources of error (but don't reduce to zero)
        OM_k@cpars <- OM_k@cpars[names(OM_k@cpars)!="Perr_y"]
        OM_k@Perr <- minerr_args$Perr
        # OM_k@cpars$Data@CV_AddInd[!is.na(OM_k@cpars$Data@CV_AddInd)] <- 0.05
        OM_k@cpars$AddIerr <- gen_AddIerr(OM_k, fix_cv = TRUE, cv_constant = minerr_args$cv_constant)
        OM_k@cpars$LenCV[!is.na(OM_k@cpars$LenCV)] <- minerr_args$LenCV

        # Make vulnerability constant (during historic and projection years)
        OM_k@cpars$V <- local({
          V <- OM_k@cpars$V
          V2 <- aperm(V, perm = c(2, 1, 3))
          Vc <- V[1,,OM_k@nyears+1]
          V3 <- array(Vc,dim=dim(V2))
          aperm(V3, perm = c(2, 1, 3))
        })
      }

      if(scenario_i=="minrecdev"){
        ## Minimize recruitment deviation
        OM_k@cpars <- OM_k@cpars[names(OM_k@cpars)!="Perr_y"]
        OM_k@Perr <- c(0,0.05)
      }

      if(scenario_i=="minrecac"){
        # Minimize recruitment autocorrelation
        OM_k@AC <- c(0,0.05)
      }

      if(scenario_i=="tachi"){
        # Set TAC to high value
        MSY_frac <- tachi_args$MSY_frac
      }else{
        MSY_frac <- MSY_frac_Init
      }


      if(scenario_i=="noempind"){
        # Clear AddInd and related slots from OM_k$cpars$Data
        # Data_e <- Data_empty()
        # # OM_k@cpars$Data
        # for(slotName_m in c("AddInd","CV_AddInd","AddIndV","AddIndType","AddIunits")){
        # slot(OM_k@cpars$Data,slotName_m) <- slot(Data_e,slotName_m)
        # }
        # Just remove the Data which contains only
        #OM_k@cpars <- OM_k@cpars[names(OM_k@cpars)[names(OM_k@cpars)!="Data"]]

        # Actually, AddInd needs to exist because of the way SCA_NK is coded, but in
        # case it will be generated by the operating model but not used by SCA or the interim approaches
        AddInd_val <- "VB"
        AddInd_all_assess <- FALSE
      }else{
        AddInd_val <- AddInd_val_Init
        AddInd_all_assess <- AddInd_all_assess_Init
      }

      # No lag between stock assessment and management
      if(scenario_i=="nolag"){
        lag_Assess <- 0
      }else{
        lag_Assess <- lag_Assess_Init
      }


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
      if(scenario_i %in% c("SCAfree","basealt")){
        BAM_SCA_args <- c(BAM_SCA_args,SCAfree_args)
      }

      if(scenario_i %in% c("catcvlo","basealt")) {
        BAM_SCA_args$control <- list("omega"=catcvlo_args$cv)
      }

      source('C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/iMP.R') # Define MPs


      OM_k@nsim <- nsim
      OM_k <- SubCpars(OM_k, sims = 1:nsim) # Limit number of simulations

      # Save OM to object
      OMName_ki <- paste0(OMName_k, "_", scenario_i)
      assign(OMName_ki,OM_k)
      # Save OM to file
      # saveRDS(get(OMName_ki),
      #         file=paste0("OM_modified/",paste0(OMName_ki, ".rds")))



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
      sfExport(list = c("Assess_diagnostic_NK","SCA_NK","MSY_frac","myIslope"))


      MSE_batch_1 <- runMSE(OM_k,
                            MPs = MPs_user_k,
                            parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)


      OM_k@interval <- MPs_user_interval2
      set.seed(myseed)
      MSE_batch_2 <- runMSE(OM_k,
                            MPs = MPs_user2,
                            parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)


      MSE_batch<-merge_MSE(MSE_batch_1, MSE_batch_2)

      # ##### TESTING CMPS ##############
      # OM_k@interval <- 1
      # set.seed(myseed)
      # MSE <- runMSE(OM_k,MPs = "GB_slope_RP",
      #               parallel = runMSE_args$parallel, extended=runMSE_args$extended, silent=runMSE_args$silent)
      #
      #
      #
      # # dim(MSE1@SB_SBMSY)
      # plot(apply(MSE@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, 2), lwd=2, ylab="SSB/SSB_MSY", xlab="Proj years")
      # abline(h=1)
      # lines(apply(MSE@SB_SBMSY[,1,], 2, median), col='orchid', lwd=2)




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
  }
}

# save.image("run_script.RData")

sfStop()


