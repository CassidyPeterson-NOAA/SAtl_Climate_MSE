GB_target1<- function (x, Data, reps = 100, plot = FALSE, w = 0.1, index="AddInd", ind=1)
{
  dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd"

  if(index=="AddInd"){
    INDEX<-Data@AddInd[,ind,]
  }
  if(index=="Ind"){
    INDEX<-Data@Ind
  }

  Catrec <- Data@Cat[x, length(Data@Cat[x, ])]
  TACtarg <- MSEtool::trlnorm(reps, Data@Cref[x], Data@CV_Cref)
  Itarg <- MSEtool::trlnorm(reps, Data@Iref[x], Data@CV_Iref)
  Iav <- mean(INDEX[x, (length(INDEX[x, ]) - 4):length(INDEX[x,  ])], na.rm = T)
  Irec <- mean(INDEX[x, (length(INDEX[x, ]) - 3):length(INDEX[x,  ])], na.rm = T)
  I0 <- 0.2 * Iav
  TAC <- rep(NA, reps)
  if (Irec > I0)
    TAC <- TACtarg * (w + (1 - w) * ((Irec - I0)/(Itarg - I0)))
  if (Irec < I0)
    TAC <- TACtarg * w * (Irec/I0)^2
  TAC[TAC > (1.2 * Catrec)] <- 1.2 * Catrec
  TAC[TAC < (0.8 * Catrec)] <- 0.8 * Catrec
  TAC <- MSEtool::TACfilter(TAC)
  if (plot)
    GB_target_plot(Itarg, Irec, I0, Data, Catrec, TAC)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(GB_target1)<-"MP"


# avail(OM)
