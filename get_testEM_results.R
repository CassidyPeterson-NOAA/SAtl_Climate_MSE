evalEM<-function(testOM, testEM, param){
  Results<-list()
  #R0
  if(param=="R0"){
    Results[["R0"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@R0
      pOM<-testOM@SampPars$Stock$R0[i] #testOM@OMParms#R0 ...
      Results[["R0"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==R0

  #B0
  if(param=="B0"){
    Results[["B0"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@B0
      pOM<-testOM@OMPars$B0[i]
      Results[["B0"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==B0


  #SSBMSY
  if(param=="SSBMSY"){
    Results[["SSBMSY"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@SSBMSY
      pOM<-testOM@OMPars$SSBMSY[i]
      Results[["SSBMSY"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==SSBMSY


  #tSSB
  if(param=="SSB"){
    Results[["SSB"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@SSB[length(testEM[[1]]@SSB)]
      pOM<-(testOM@SampPars$Stock$D * testOM@OMPars$SSB0 )[i]
      Results[["tSSB"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==SSBMSY



  #FMSY
  if(param=="FMSY"){
    Results[["FMSY"]]<-vector()
    for(i in 1:length(testEM)){
      pEM<-testEM[[i]]@FMSY
      pOM<-testOM@OMPars$FMSY[i]
      Results[["FMSY"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==FMSY


  #F_FMSY
  if(param=="F_FMSY"){
    Results[["F_FMSY"]]<-vector()
    for(i in 1:length(testEM)){
      pOM<-(testOM@TSdata$Find[,ncol(testOM@TSdata$Find)]  /
              testOM@Ref$ByYear$FMSY[,ncol(testOM@Ref$ByYear$FMSY)] )[i] # ANNUAL FMSY
      pEM<-testEM[[i]]@F_FMSY[length(testEM[[1]]@F_FMSY)]
      Results[["F_FMSY"]][i]<-(pEM - pOM)/pOM
    }
  } # end if param==FMSY




if(param=="hbias"){
  Results[["hbias"]]<-vector()
  for(i in 1:length(testEM)){
    Results[["hbias"]][i]<-testOM@OMPars$hbias[i]
  } # end hbias


  if(param=="NLL"){
    Results[["NLL"]]<-data.frame(matrix(NA, nrow=length(testEM), ncol=length(testEM[[1]]@NLL)))
    names(Results[["NLL"]])<-names(testEM[[1]]@NLL)
    for(i in 1:length(testEM)){
      Results[["NLL"]][i,]<-testEM[[i]]@NLL
    } # end i loop

  }# end NLL

} # end


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



OM_Perry_hist(testOM)

sd(testEM[[1]]@Dev)
testOM@SampPars$Stock$AC
testOM@SampPars$Stock$procsd
apply(testOM@SampPars$Stock$Perr_y,1, sd)
testOM@Data@sigmaR
testOM@SampPars$Obs$BMSY_B0bias
testOM@SampPars$Stock$D # estimated current level of depletion defined as the current SSB divided by the unfished spawnign stock biomass
testOM@SampPars$Stock$D * testOM@OMPars$SSB0 # END YEAR SSB
testEM[[1]]@SSB[length(testEM[[1]]@SSB)]


testEM[[1]]@SSB0
testOM@OMPars$SSB0
testEM[[1]]@SE_FMSY



testEM[[1]]@SSBMSY


testEM[[1]]@FMSY
testOM@OMPars$FMSY
testOM@SampPars$Stock$FMSY

testOM@OM@Name
testOM@SampPars$Fleet$Find
testOM@TSdata$Find # Find: Historical fishing mortality (scale-free); matrix

(testOM@TSdata$Find[,ncol(testOM@TSdata$Find)] / testOM@OMPars$FMSY) # STATIC FMSY
(testOM@TSdata$Find[,ncol(testOM@TSdata$Find)]  / testOM@Ref$ByYear$FMSY[,ncol(testOM@Ref$ByYear$FMSY)] ) # ANNUAL FMSY
testEM[[1]]@F_FMSY[length(testEM[[1]]@F_FMSY)]



testOM@TSdata$RecDev
# Rec: Unfished recruitment
