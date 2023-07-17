library(openMSE)
par.args<-list(mar=c(2.4, 2.4, 2.2, 0.2), oma=c(0.1,0.1,0.1,0.1), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
par(par.args)

testEM_BlackSeaBass <- readRDS("~/Github/SAtl_Climate_MSE/SCAtest/testEM_BlackSeaBass_base.rds")
testOM_BlackSeaBass <- readRDS("~/Github/SAtl_Climate_MSE/SCAtest/testOM_BlackSeaBass_base.rds")
testEM_VermilionSnapper <- readRDS("~/Github/SAtl_Climate_MSE/SCAtest/testEM_VermilionSnapper_base.rds")
testOM_VermilionSnapper <- readRDS("~/Github/SAtl_Climate_MSE/SCAtest/testOM_VermilionSnapper_base.rds")
testEM_RedPorgy <- readRDS("~/Github/SAtl_Climate_MSE/SCAtest/testEM_RedPorgy_base.rds")
testOM_RedPorgy <- readRDS("~/Github/SAtl_Climate_MSE/SCAtest/testOM_RedPorgy_base.rds")

evalEM<-function(testOM, testEM, param=c("R0","B0","SSBMSY","tSSB","tSSB_SSBMSY","FMSY","F_FMSY","hbias","NLL")){
  Results<-list()

  #R0
  if("R0" %in% param){
    Results[["R0"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@R0
      pOM<-testOM@SampPars$Stock$R0[i] #testOM@OMParms#R0 ...
      Results[["R0"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==R0

  #B0
  if("B0" %in% param){
    Results[["B0"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@B0
      pOM<-testOM@OMPars$B0[i]
      Results[["B0"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==B0


  #SSBMSY
  if("SSBMSY" %in% param){
    Results[["SSBMSY"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@SSBMSY
      pOM<-testOM@OMPars$SSBMSY[i]
      Results[["SSBMSY"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==SSBMSY


  #tSSB terminal SSB
  if("tSSB" %in% param){
    Results[["tSSB"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@SSB[length(testEM[[1]]@SSB)-1]
      pOM<-(testOM@SampPars$Stock$D * testOM@OMPars$SSB0 )[i]
      Results[["tSSB"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==SSBMSY

 #tSSB_SSBMSY
  if("tSSB_SSBMSY" %in% param){
    Results[["tSSB_SSBMSY"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@SSB_SSBMSY[length(testEM[[1]]@SSB_SSBMSY)-1]
      OMSSBr<-apply(testOM@TSdata$SBiomass[1,,], 1, sum)
      pOM<-(OMSSBr/ testOM@OMPars$SSBMSY[1:length(OMSSBr)])[length(OMSSBr)]
      Results[["tSSB_SSBMSY"]][i]<-(pEM - pOM)/pOM
    }
  }# end tSSB_SSBMSY

  #FMSY
  if("FMSY" %in% param){
    Results[["FMSY"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@FMSY
      pOM<-testOM@OMPars$FMSY[i]
      Results[["FMSY"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==FMSY


  #F_FMSY
  if("F_FMSY" %in% param){
    Results[["F_FMSY"]]<-vector()
    for(i in 1:length(testEM)){
      pOM<-(testOM@TSdata$Find[,ncol(testOM@TSdata$Find)]  /
              testOM@Ref$ByYear$FMSY[,ncol(testOM@Ref$ByYear$FMSY)] )[i] # ANNUAL FMSY
      pEM<-testEM[[i]]@F_FMSY[length(testEM[[1]]@F_FMSY)]
      Results[["F_FMSY"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==FMSY



  # hbias
  if("hbias" %in% param){
    Results[["hbias"]]<-vector()
    for(i in 1:length(testEM)){
      Results[["hbias"]][i]<-testOM@OMPars$hbias[i]
    }
  }# end hbias

  # NLL
  if("NLL" %in% param){
    Results[["NLL"]]<-data.frame(matrix(NA, nrow=length(testEM), ncol=length(testEM[[1]]@NLL)))
    names(Results[["NLL"]])<-names(testEM[[1]]@NLL)
    for(i in 1:length(testEM)){
      Results[["NLL"]][i,]<-testEM[[i]]@NLL
    } # end i loop
  }# end NLL


  return(Results)
}# end function



OM_Perry_hist <- function(testOM, title="OM dynamics"){

  par(mfrow=c(2,1))
  Perry<-testOM@SampPars$Stock$procsd # = testOM@Data@sigmaR
  hist(Perry, breaks=10, xlab="Recruitment Process Error", probability=T, main=title)
  # Perry<-testOM@SampPars$Stock$Perr_y
  # summary(apply(log(Perry),1,sd))
  # hist(apply(log(Perry),1,sd), breaks=10, xlab="Recruitment Process Error", probability=T, main=title)

  AC<-testOM@OMPars$AC
  hist(AC, breaks=10, xlab="Recruitment Autocorrelation", probability=T, main="")


  h<-testOM@OMPars$hs
  # hist(h, breaks=10, xlab="Steepness", probability=T, main="")

  return(list(Perry=Perry, AC=AC, h=h))
}



# testOM<-testOM_BlackSeaBass
# testEM<-testEM_BlackSeaBass
# sp<-"BSB"
# testOM<-testOM_VermilionSnapper
# testEM<-testEM_VermilionSnapper
# sp<-"VS"
# testOM<-testOM_RedPorgy
# testEM<-testEM_RedPorgy
# sp<-"RP"
#
# evalEM_<-evalEM(testOM, testEM)
# assign(paste0("evalEM_",sp),evalEM_)
#
# OM_Perry_hist(testOM, title=paste0(sp," OM recruitment dynamics"))
#
#
# # length(evalEM_)
# par(mfrow=c(3,3))
# for(i in 1:length(evalEM_)){
#   if(names(evalEM_)[i]!="NLL") {
#       hist(evalEM_[[i]], main=paste0(sp," ", names(evalEM_)[i]), xlab="")
#   }
#   if(names(evalEM_)[i]=="NLL") {
#     hist(evalEM_[[i]]$Total, main=paste0(sp," ", names(evalEM_)[i]), xlab="")
#   }
# }
#
#
#
#
# # NOTES
#
# sd(testEM[[1]]@Dev)
# testOM@SampPars$Stock$AC
# testOM@SampPars$Stock$procsd
# apply(testOM@SampPars$Stock$Perr_y,1, sd)
# testOM@Data@sigmaR
# testOM@SampPars$Obs$BMSY_B0bias
# testOM@SampPars$Stock$D # estimated current level of depletion defined as the current SSB divided by the unfished spawnign stock biomass
# testOM@SampPars$Stock$D * testOM@OMPars$SSB0 # END YEAR SSB
# testEM[[1]]@SSB[length(testEM[[1]]@SSB)]
#
#
# testEM[[1]]@SSB0
# testOM@OMPars$SSB0
# testEM[[1]]@SE_FMSY
#
#
#
# testEM[[1]]@SSBMSY
#
#
# testEM[[1]]@FMSY
# testOM@OMPars$FMSY
# testOM@SampPars$Stock$FMSY
#
# testOM@OM@Name
# testOM@SampPars$Fleet$Find
# testOM@TSdata$Find # Find: Historical fishing mortality (scale-free); matrix
#
# (testOM@TSdata$Find[,ncol(testOM@TSdata$Find)] / testOM@OMPars$FMSY) # STATIC FMSY
# (testOM@TSdata$Find[,ncol(testOM@TSdata$Find)]  / testOM@Ref$ByYear$FMSY[,ncol(testOM@Ref$ByYear$FMSY)] ) # ANNUAL FMSY
# testEM[[1]]@F_FMSY[length(testEM[[1]]@F_FMSY)]
#
#
#
# testOM@TSdata$RecDev
# # Rec: Unfished recruitment
