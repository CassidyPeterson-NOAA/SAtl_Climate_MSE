myICI_BSB<-function (x, Data, reps = 100, plot = FALSE, index="AddInd", ind=1, const=0.5)
{
  dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd"

  if(index=="AddInd"){
    Index<-Data@AddInd[x,ind,]
  }
  if(index=="Ind"){
    Index <- Data@Ind[x, ]
  }

  Years <- Data@Year[!is.na(Index)]
  Index <- Index[!is.na(Index)]
  nI <- length(Index)
  Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_Ind[x,
                                                             1])
  Ind.samp <- matrix(Ind.samp, ncol = reps)
  muI <- apply(Ind.samp, 2, mean, na.rm = TRUE) * const ### added
  sigmaI <- apply(Ind.samp, 2, sd, na.rm = TRUE)
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

class(myICI_BSB)<- "MP"



myICI2_RP<-function (x, Data, reps = 100, plot = FALSE, index="AddInd", ind=1, const=0.93)
{
  dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd"

  if(index=="AddInd"){
    Index<-Data@AddInd[x,ind,]
  }
  if(index=="Ind"){
    Index <- Data@Ind[x, ]
  }

  Years <- Data@Year[!is.na(Index)]
  Index <- Index[!is.na(Index)]
  nI <- length(Index)
  Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_Ind[x,
                                                             1])
  Ind.samp <- matrix(Ind.samp, ncol = reps)
  muI <- apply(Ind.samp, 2, mean, na.rm = TRUE) * const ### added
  sigmaI <- apply(Ind.samp, 2, sd, na.rm = TRUE)
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

class(myICI2_RP)<- "MP"




myICI2_VS<-function (x, Data, reps = 100, plot = FALSE, index="AddInd", ind=1, const=0.625)
{
  dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd"

  if(index=="AddInd"){
    Index<-Data@AddInd[x,ind,]
  }
  if(index=="Ind"){
    Index <- Data@Ind[x, ]
  }

  Years <- Data@Year[!is.na(Index)]
  Index <- Index[!is.na(Index)]
  nI <- length(Index)
  Ind.samp <- MSEtool::trlnorm(reps * nI, Index, Data@CV_Ind[x,
                                                             1])
  Ind.samp <- matrix(Ind.samp, ncol = reps)
  muI <- apply(Ind.samp, 2, mean, na.rm = TRUE) * const ### added
  sigmaI <- apply(Ind.samp, 2, sd, na.rm = TRUE)
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

class(myICI2_VS)<- "MP"
