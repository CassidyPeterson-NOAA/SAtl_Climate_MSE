# > GB_target
# function (x, Data, reps = 100, plot = FALSE, w = 0.5)
# {
#   dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind"
#   Catrec <- Data@Cat[x, length(Data@Cat[x, ])]
#   TACtarg <- MSEtool::trlnorm(reps, Data@Cref[x], Data@CV_Cref)
#   Itarg <- MSEtool::trlnorm(reps, Data@Iref[x], Data@CV_Iref)
#   Iav <- mean(Data@Ind[x, (length(Data@Ind[x, ]) - 4):length(Data@Ind[x,
#   ])], na.rm = T)
#   Irec <- mean(Data@Ind[x, (length(Data@Ind[x, ]) - 3):length(Data@Ind[x,
#   ])], na.rm = T)
#   I0 <- 0.2 * Iav
#   TAC <- rep(NA, reps)
#   if (Irec > I0)
#     TAC <- TACtarg * (w + (1 - w) * ((Irec - I0)/(Itarg -
#                                                     I0)))
#   if (Irec < I0)
#     TAC <- TACtarg * w * (Irec/I0)^2
#   TAC[TAC > (1.2 * Catrec)] <- 1.2 * Catrec
#   TAC[TAC < (0.8 * Catrec)] <- 0.8 * Catrec
#   TAC <- MSEtool::TACfilter(TAC)
#   if (plot)
#     GB_target_plot(Itarg, Irec, I0, Data, Catrec, TAC)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
# <bytecode: 0x00000215f10e4310>
#   <environment: namespace:DLMtool>
#   attr(,"class")
# [1] "MP"

GB_target_BSB<- function (x, Data, reps = 100, plot = FALSE, w = 0.1, index="AddInd", ind=1)
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

class(GB_target_BSB)<-"MP"


# avail(OM)


GB_target_RP<- function (x, Data, reps = 100, plot = FALSE, w = 0.5, c0=1, index="AddInd", ind=1) # w default = 0.5 | c0 default = 0.2
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
  I0 <- c0 * Iav ############ CHANGE THIS ###############
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

class(GB_target_RP)<-"MP"



GB_target_VS<- function (x, Data, reps = 100, plot = FALSE, w = 0.5, c0=0.5, index="AddInd", ind=1) # w default = 0.5 | c0 default = 0.2
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
  I0 <- c0 * Iav ############ CHANGE THIS ###############
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

class(GB_target_VS)<-"MP"


# avail(OM)

