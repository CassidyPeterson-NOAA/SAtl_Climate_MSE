## Setup

library(abind)
library(magrittr)
library(openMSE)
library(bamExtras)
library(bamMSE)

setwd("D:/SAtl_MSE/MSE_obj")

filepath <- "C:/Users/cassidy.peterson/Documents/git"
source(file.path(filepath,"SEFSCInterimAnalysis/RunMSE/SEFSC/fn/merge_MSE.R"))


## test case
# resu1<-readRDS("MSE_BlackSeaBass_base.rds")
# resu2<-readRDS("MSE_BlackSeaBass_baseSLOPE3.rds")
# MSE_batch<-merge_MSE(resu1, resu2)
# saveRDS(MSE_batch,file = paste0("MSE_obj/", MSEName_k, "_", scenario_i, ".rds"))



## Fun loop
OMName <- c(#"BlackSeaBass" , # Runs
            #"RedPorgy" , # Runs,
            #"VermilionSnapper",
            "BlackSeaBass_Over" , # Runs
            "RedPorgy_Over" , # Runs,
             "VermilionSnapper_Over" # Runs
)
scenario <- c("base"
              ,"recns" #nonstationarity
              ,"age0M_hi" # decrease recruitment regime shift
              ,"age0M_lo" # increase recruitment regime shift
              ,"uobs_hi" # increase index deviations / obs error
              ,"uobs_lo" # decrease index deviations / obs error
              ,"refbias_hi" # ref poitns biased high
              ,"refbias_lo" # ref points biased low
              ,"epiM"  # Red Porgy sometimes has problems getting down to the specified level of depletion
              ,"recdev_hi"     # Regime change
              ,"recdev_lo")


for(oms in OMName){
  for(s in scenario){
    resu1<-readRDS(file.path("raw",paste0("MSE_",oms,"_", s, ".rds") ))
    resu2<-readRDS(file.path("raw",paste0("MSE_",oms,"_", s, "SLOPE3.rds")) )

    MSE_batch<-merge_MSE(resu1, resu2)


    saveRDS(MSE_batch,file = paste0("MSE_",oms,"_", s, ".rds"))

  } # end s loop
} # end oms loop


