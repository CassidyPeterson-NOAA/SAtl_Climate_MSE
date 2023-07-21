
myGB_target<- function (x, Data, reps = 100, plot = FALSE,
                          index="AddInd", ind=1,
                          w = 0.5, c0=1, delta=0.2)
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
  I0 <- c0 * Iav
  TAC <- rep(NA, reps)
  if (Irec > I0)
    TAC <- TACtarg * (w + (1 - w) * ((Irec - I0)/(Itarg - I0)))
  if (Irec < I0)
    TAC <- TACtarg * w * (Irec/I0)^2
  TAC[TAC > ((1+delta) * Catrec)] <- (1+delta) * Catrec
  TAC[TAC < ((1-delta) * Catrec)] <- (1-delta) * Catrec
  TAC <- MSEtool::TACfilter(TAC)
  if (plot)
    GB_target_plot(Itarg, Irec, I0, Data, Catrec, TAC)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(myGB_target)<-"MP"



# BSB

GB_target_BSB<-myGB_target # THIS ONE SELECTED FOR RESULTS
formals(GB_target_BSB)$w<-0.1      # 0.5
formals(GB_target_BSB)$c0<-0.25 #0.2    # 0.35
formals(GB_target_BSB)$delta<-0.3  # 0.2
class(GB_target_BSB)<-"MP"

GB_target_BSB2<-myGB_target # TESTED BUT NOT PRESENTED
formals(GB_target_BSB2)$w<-0.5      # 0.5
formals(GB_target_BSB2)$c0<-0.35    # 0.35
formals(GB_target_BSB2)$delta<-0.2  # 0.2
class(GB_target_BSB2)<-"MP"


# RP

GB_target_RP<-myGB_target # TESTED BUT NOT PRESENTED
formals(GB_target_RP)$w<-0.1     #0.25
formals(GB_target_RP)$c0<-0.941   #0.955
formals(GB_target_RP)$delta<-0.3  #0.2
class(GB_target_RP)<-"MP"

GB_target_RP2<-myGB_target # THIS ONE SELECTED FOR RESULTS July 2023
formals(GB_target_RP2)$w<-0.5      # 0.5
formals(GB_target_RP2)$c0<-1.05    # 1
formals(GB_target_RP2)$delta<-0.2  # 0.2
class(GB_target_RP2)<-"MP"

GB_target_RP3<-myGB_target # old results
formals(GB_target_RP3)$w<-0.5      # 0.5
formals(GB_target_RP3)$c0<-1    # 1
formals(GB_target_RP3)$delta<-0.2  # 0.2
class(GB_target_RP3)<-"MP"





# VS
GB_target_VS<-myGB_target # TESTED BUT NOT PRESENTED
formals(GB_target_VS)$w<-0.1      # 0.1
formals(GB_target_VS)$c0<-0.49    # 0.49
formals(GB_target_VS)$delta<-0.3  # 0.3
class(GB_target_VS)<-"MP"


# less responsive GBtarget_VS
GB_target_VS2<-myGB_target # THIS ONE SELECTED IN RESULTS
formals(GB_target_VS2)$w<-0.2 # 0.25
formals(GB_target_VS2)$c0<-0.56    # 0.49
formals(GB_target_VS2)$delta<-0.2
class(GB_target_VS2)<-"MP"



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
