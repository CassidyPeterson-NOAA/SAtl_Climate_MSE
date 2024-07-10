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
formals(myICI_BSB)$cmu<-0.4 # 0.5
formals(myICI_BSB)$csd<- 0.6 #
class(myICI_BSB)<-"MP"
# cmu | csd | med(SSBratio30)
# 0.45 |1 | 1.189
# 0.5 |1 | 1.27
# 0.4 |1 | 1.05 *
# 0.4 | 0.9 | 1.13
# 0.4 | 0.8 | 1.1
# 0.4 | 0.7 | 1.06
# 0.4 | 0.6 | 1.046

# RP
# myICI_RP<-myICI            # TESTED BUT NOT USED
# formals(myICI_RP)$cmu<-0.9125 # 0.9175
# formals(myICI_RP)$csd<- 0.1 # 1
# class(myICI_RP)<-"MP"
#RP
myICI_RP<-myICI            # THIS ONE USED FOR RESULTS
formals(myICI_RP)$cmu<- 1 # 0.9175
formals(myICI_RP)$csd<- 1 # 1
class(myICI_RP)<-"MP"
# cmu | csd | med(SSBratio30)
# 1.1 |1 | 1.36
# 0.9 |1 | 0.58
# 0.9 |1.2 | 0.54
# 0.9 |0.8 | 0.58
# 0.9 |0.4 | 0.59
# 1 |0.4 | 1.01
# 1 |1 | 1.01


# VS
myICI_VS<-myICI
formals(myICI_VS)$cmu<- 0.5 #0.55 # 0.625 #
formals(myICI_VS)$csd<- 1 #
class(myICI_VS)<-"MP"
# cmu | csd | med(SSBratio30)
# 0.55 | 1 | 1.24
# 1 | 1 | 2.65
# 1 | 0.5 | 2.7
# 0.75 | 0.75 | 2.01
# 0.5 | 0.75 | 1.1
# 0.5 | 0.5 | 1.2
# 0.5 | 0.1 | 1.2
# 0.4 | 1 | 0.66
# 0.45 | 1 | 0.83
# 0.5 | 1 | 1.02



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


