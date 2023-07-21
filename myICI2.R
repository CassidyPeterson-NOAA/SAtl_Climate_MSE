myICI<-function (x, Data, reps = 100, plot = FALSE, index="AddInd", ind=1, cmu=1, csd=1)
{
  dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd, Data@CV_AddInd"

  if(index=="AddInd"){
    Index<-Data@AddInd[x,ind,]
  }
  if(index=="Ind"){
    Index <- Data@Ind[x, ]
  }

  Years <- Data@Year[!is.na(Index)]
  Index <- Index[!is.na(Index)]
  nI <- length(Index)
  if(index=="Ind"){
    Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_Ind[x,1])
  }
  if(index=="AddInd"){
    Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_AddInd[1,1,dim(Data@CV_AddInd)[3]]) #Data@CV_AddInd[x,ind,1])
  }
  Ind.samp <- matrix(Ind.samp, ncol = reps)
  muI <- apply(Ind.samp, 2, mean, na.rm = TRUE) * cmu ### added
  sigmaI <- apply(Ind.samp, 2, sd, na.rm = TRUE) * csd ### added
  Ind <- Ind.samp[nI, ]
  z.low <- -1.96
  z.upp <- 1.96
  ci.low <- muI + z.low * sigmaI/sqrt(nI)
  ci.high <- muI + z.upp * sigmaI/sqrt(nI)
  alpha <- rep(NA, reps)
  alpha[Ind < ci.low] <- 0.75
  alpha[Ind > ci.high] <- 1.25
  alpha[Ind >= ci.low & Ind <= ci.high] <- 1
  Cat <- Data@Cat[x, length(Data@Cat[x, ])]
  Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
  TAC <- alpha * Cc
  TAC <- MSEtool::TACfilter(alpha * Cc)
  if (plot)
    ICI_plot(Years, Index, ci.low, ci.high, TAC, Cat, Data)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}
class(myICI)<- "MP"

# BSB
myICI_BSB<-myICI
formals(myICI_BSB)$cmu<-0.45 # 0.5
formals(myICI_BSB)$csd<- 1 #
class(myICI_BSB)<-"MP"

# RP
myICI_RP<-myICI            # TESTED BUT NOT USED
formals(myICI_RP)$cmu<-0.9125 # 0.9175
formals(myICI_RP)$csd<- 0.1 # 1
class(myICI_RP)<-"MP"
#RP
myICI_RP2<-myICI            # THIS ONE USED FOR RESULTS
formals(myICI_RP2)$cmu<- 1.1 # 0.9175
formals(myICI_RP2)$csd<- 1 # 1
class(myICI_RP2)<-"MP"


# VS
myICI_VS<-myICI
formals(myICI_VS)$cmu<- 0.55 # 0.625 #
formals(myICI_VS)$csd<- 1 #
class(myICI_VS)<-"MP"

# myICI_VS<-myICI
# formals(myICI_VS)$cmu<-0.575 #
# formals(myICI_VS)$csd<- 0.1 #
# class(myICI_VS)<-"MP"




# myICI2_VS<-function (x, Data, reps = 100, plot = FALSE, index="AddInd", ind=1, cmu=0.625, csd=1.0) #0.625
# {
#   dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd, Data@CV_AddInd"
#
#    if(index=="AddInd"){
#     Index<-Data@AddInd[x,ind,]
#    }
#    if(index=="Ind"){
#      Index <- Data@Ind[x, ]
#    }
#
#   Years <- Data@Year[!is.na(Index)]
#   Index <- Index[!is.na(Index)]
#   nI <- length(Index)
#    if(index=="Ind"){
#      Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_Ind[x,1])
#    }
#    if(index=="AddInd"){
#     Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_AddInd[1,1,dim(Data@CV_AddInd)[3]]) #Data@CV_AddInd[x,ind,1])
#    }
#   Ind.samp <- matrix(Ind.samp, ncol = reps)
#   muI <- apply(Ind.samp, 2, mean, na.rm = TRUE) * cmu ### added
#   sigmaI <- apply(Ind.samp, 2, sd, na.rm = TRUE) * csd ### added
#   Ind <- Ind.samp[nI, ]
#   z.low <- -1.96
#   z.upp <- 1.96
#   ci.low <- muI + z.low * sigmaI/sqrt(nI)
#   ci.high <- muI + z.upp * sigmaI/sqrt(nI)
#   alpha <- rep(NA, reps)
#   alpha[Ind < ci.low] <- 0.75
#   alpha[Ind > ci.high] <- 1.25
#   alpha[Ind >= ci.low & Ind <= ci.high] <- 1
#   Cat <- Data@Cat[x, length(Data@Cat[x, ])]
#   Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
#   TAC <- alpha * Cc
#   TAC <- MSEtool::TACfilter(alpha * Cc)
#   if (plot)
#     ICI_plot(Years, Index, ci.low, ci.high, TAC, Cat, Data)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
#
# class(myICI2_VS)<- "MP"
#
#
#
# myICI_BSB<-function (x, Data, reps = 100, plot = FALSE, index="AddInd", ind=1, cmu=0.5, csd=1)
# {
#   dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd, Data@CV_AddInd"
#
#   if(index=="AddInd"){
#     Index<-Data@AddInd[x,ind,]
#   }
#   if(index=="Ind"){
#     Index <- Data@Ind[x, ]
#   }
#
#   Years <- Data@Year[!is.na(Index)]
#   Index <- Index[!is.na(Index)]
#   nI <- length(Index)
#   if(index=="Ind"){
#     Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_Ind[x,1])
#   }
#   if(index=="AddInd"){
#     Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_AddInd[1,1,dim(Data@CV_AddInd)[3]]) #Data@CV_AddInd[x,ind,1])
#   }
#   Ind.samp <- matrix(Ind.samp, ncol = reps)
#   muI <- apply(Ind.samp, 2, mean, na.rm = TRUE) * cmu ### added
#   sigmaI <- apply(Ind.samp, 2, sd, na.rm = TRUE) * csd ### added
#   Ind <- Ind.samp[nI, ]
#   z.low <- -1.96
#   z.upp <- 1.96
#   ci.low <- muI + z.low * sigmaI/sqrt(nI)
#   ci.high <- muI + z.upp * sigmaI/sqrt(nI)
#   alpha <- rep(NA, reps)
#   alpha[Ind < ci.low] <- 0.75
#   alpha[Ind > ci.high] <- 1.25
#   alpha[Ind >= ci.low & Ind <= ci.high] <- 1
#   Cat <- Data@Cat[x, length(Data@Cat[x, ])]
#   Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
#   TAC <- alpha * Cc
#   TAC <- MSEtool::TACfilter(alpha * Cc)
#   if (plot)
#     ICI_plot(Years, Index, ci.low, ci.high, TAC, Cat, Data)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
#
# class(myICI_BSB)<- "MP"
#
#
#
#
# myICI2_RP<-function (x, Data, reps = 100, plot = FALSE, index="AddInd", ind=1, cmu=0.93, csd=1)
# {
#   dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd, Data@CV_AddInd"
#
#   if(index=="AddInd"){
#     Index<-Data@AddInd[x,ind,]
#   }
#   if(index=="Ind"){
#     Index <- Data@Ind[x, ]
#   }
#
#   Years <- Data@Year[!is.na(Index)]
#   Index <- Index[!is.na(Index)]
#   nI <- length(Index)
#   if(index=="Ind"){
#     Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_Ind[x,1])
#   }
#   if(index=="AddInd"){
#     Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_AddInd[1,1,dim(Data@CV_AddInd)[3]]) #Data@CV_AddInd[x,ind,1])
#
#   }
#   Ind.samp <- matrix(Ind.samp, ncol = reps)
#   muI <- apply(Ind.samp, 2, mean, na.rm = TRUE) * cmu ### added
#   sigmaI <- apply(Ind.samp, 2, sd, na.rm = TRUE) * csd ### added
#   Ind <- Ind.samp[nI, ]
#   z.low <- -1.96
#   z.upp <- 1.96
#   ci.low <- muI + z.low * sigmaI/sqrt(nI)
#   ci.high <- muI + z.upp * sigmaI/sqrt(nI)
#   alpha <- rep(NA, reps)
#   alpha[Ind < ci.low] <- 0.75
#   alpha[Ind > ci.high] <- 1.25
#   alpha[Ind >= ci.low & Ind <= ci.high] <- 1
#   Cat <- Data@Cat[x, length(Data@Cat[x, ])]
#   Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
#   TAC <- alpha * Cc
#   TAC <- MSEtool::TACfilter(alpha * Cc)
#   if (plot)
#     ICI_plot(Years, Index, ci.low, ci.high, TAC, Cat, Data)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
#
# class(myICI2_RP)<- "MP"


