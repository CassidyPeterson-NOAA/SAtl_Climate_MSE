myIslope<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5,
                        lambda = 0.4, # default lambda=0.4 | default xx=0.2
                        xx = 0.2, index="AddInd", ii=1, c1=1) # default c1=1
{
  myIslope_<-function (x, Data, reps , yrsmth, lambda , xx , index, ii, c1)
  {
    ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
    Years <- Data@Year[ind]
    ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
    C_dat <- Data@Cat[x, ind]
    if (is.na(Data@MPrec[x]) || length(Data@Year) == ylast + 1) {
      TACstar <- (1 - xx) *
        MSEtool::trlnorm(reps, mean(C_dat, na.rm = TRUE), Data@CV_Cat/(yrsmth^0.5))
    }
    else {
      TACstar <- rep(Data@MPrec[x], reps)
    }

    if(index=="AddInd"){
      I_hist <- Data@AddInd[x,ii,ind]
    } else {
      I_hist <- Data@Ind[x, ind]
    }
    yind <- 1:yrsmth
    slppar <- summary(lm(log(I_hist) ~ yind))$coefficients[2, 1:2]
    if (reps > 1) {
      Islp <- rnorm(reps, slppar[1], slppar[2])
    }
    else {
      Islp <- slppar[1]
    }
    TAC <- TACstar * (1 + lambda * Islp) * c1
    list(TAC = TAC, TACstar = TACstar, I_hist = I_hist, Islp = Islp,
         C_dat = C_dat, Data = Data, Years = Years)
  } # end myIslope_

  runIslope <- myIslope_(x, Data, reps, yrsmth, lambda, xx, index, ii, c1)
  TAC <- MSEtool::TACfilter(runIslope$TAC)
  runIslope$TAC <- TAC
  if (plot)
    Islope_plot(runIslope, Data)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(myIslope)<-"MP"




# VS

myIslope_VS<-myIslope            # TESTED BUT NOT USED
formals(myIslope_VS)$lambda<-0.45 #0.9 | 0.4
formals(myIslope_VS)$xx<-0.05    # 0.05
formals(myIslope_VS)$c1<-1       # 1

class(myIslope_VS)<-"MP"


myIslope_VS2<-myIslope          # THIS ONE USED IN RESULTS
formals(myIslope_VS2)$lambda<-1 #0.45
formals(myIslope_VS2)$xx<- 0.33    # 0.05
formals(myIslope_VS2)$c1<-1.02       # 1

class(myIslope_VS2)<-"MP"


# BSB
myIslope_BSB2<-myIslope          # THIS ONE USED IN RESULTS
formals(myIslope_BSB2)$lambda<-0.4   #0.4
formals(myIslope_BSB2)$xx<- -0.55     # -0.55 | 0.05
formals(myIslope_BSB2)$c1<-1.01         # 1.01 | 1.02

class(myIslope_BSB2)<-"MP"



myIslope_BSB<-myIslope            # TESTED BUT NOT USED
formals(myIslope_BSB)$lambda<-1   #0.4
formals(myIslope_BSB)$xx<- -1     # -0.55 | 0.05
formals(myIslope_BSB)$c1<-1.06         # 1.01 | 1.02

class(myIslope_BSB)<-"MP"




# RP
myIslope_RP2<-myIslope          # THIS ONE USED IN RESULTS
formals(myIslope_RP2)$lambda<-0.4
formals(myIslope_RP2)$xx<-0.45
formals(myIslope_RP2)$c1<-1

class(myIslope_RP2)<-"MP"

myIslope_RP<-myIslope            # TESTED BUT NOT USED
formals(myIslope_RP)$lambda<-1
formals(myIslope_RP)$xx<-0.5 #0.425 when c1=1
formals(myIslope_RP)$c1<-1.01

class(myIslope_RP)<-"MP"






# > Islope_
# function (x, Data, reps = 100, yrsmth = 5, lambda = 0.4, xx = 0.2)
# {
#   ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#   Years <- Data@Year[ind]
#   ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#   C_dat <- Data@Cat[x, ind]
#   if (is.na(Data@MPrec[x]) || length(Data@Year) == ylast +
#       1) {
#     TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat,
#                                                       na.rm = TRUE), Data@CV_Cat/(yrsmth^0.5))
#   }
#   else {
#     TACstar <- rep(Data@MPrec[x], reps)
#   }
#   I_hist <- Data@Ind[x, ind]
#   yind <- 1:yrsmth
#   slppar <- summary(lm(log(I_hist) ~ yind))$coefficients[2,
#                                                          1:2]
#   if (reps > 1) {
#     Islp <- rnorm(reps, slppar[1], slppar[2])
#   }
#   else {
#     Islp <- slppar[1]
#   }
#   TAC <- TACstar * (1 + lambda * Islp)
#   list(TAC = TAC, TACstar = TACstar, I_hist = I_hist, Islp = Islp,
#        C_dat = C_dat, Data = Data, Years = Years)
# }
# <bytecode: 0x0000017a33ebd190>
#   <environment: namespace:DLMtool>




# myIslope_BSB<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5,
#                         lambda = 0.4, # default lambda=0.4 | default xx=0.2
#                         xx = 0.05, index="AddInd", ii=1, c1=1.02) # default c1=1
# {
#   myIslope_<-function (x, Data, reps , yrsmth, lambda , xx , index, ii, c1)
#   {
#     ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#     Years <- Data@Year[ind]
#     ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#     C_dat <- Data@Cat[x, ind]
#     if (is.na(Data@MPrec[x]) || length(Data@Year) == ylast + 1) {
#       TACstar <- (1 - xx) *
#         MSEtool::trlnorm(reps, mean(C_dat, na.rm = TRUE), Data@CV_Cat/(yrsmth^0.5))
#     }
#     else {
#       TACstar <- rep(Data@MPrec[x], reps)
#     }
#
#     if(index=="AddInd"){
#       I_hist <- Data@AddInd[x,ii,ind]
#     } else {
#       I_hist <- Data@Ind[x, ind]
#     }
#     yind <- 1:yrsmth
#     slppar <- summary(lm(log(I_hist) ~ yind))$coefficients[2, 1:2]
#     if (reps > 1) {
#       Islp <- rnorm(reps, slppar[1], slppar[2])
#     }
#     else {
#       Islp <- slppar[1]
#     }
#     TAC <- TACstar * (1 + lambda * Islp) * c1
#     list(TAC = TAC, TACstar = TACstar, I_hist = I_hist, Islp = Islp,
#          C_dat = C_dat, Data = Data, Years = Years)
#   } # end myIslope_
#
#   runIslope <- myIslope_(x, Data, reps, yrsmth, lambda, xx, index, ii, c1)
#   TAC <- MSEtool::TACfilter(runIslope$TAC)
#   runIslope$TAC <- TAC
#   if (plot)
#     Islope_plot(runIslope, Data)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
#
# class(myIslope_BSB)<-"MP"

# myIslope_VS<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5,
#                        lambda = 0.9, # default lambda=0.4 |0.5
#                        xx = 0.05, index="AddInd", ii=1, c1=1)
# {
#   myIslope_<-function (x, Data, reps , yrsmth, lambda , xx , index, ii, c1)
#   {
#     ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#     Years <- Data@Year[ind]
#     ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#     C_dat <- Data@Cat[x, ind]
#     if (is.na(Data@MPrec[x]) || length(Data@Year) == ylast + 1) {
#       TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat, na.rm = TRUE), Data@CV_Cat/(yrsmth^0.5))
#     }
#     else {
#       TACstar <- rep(Data@MPrec[x], reps)
#     }
#
#     if(index=="AddInd"){
#       I_hist <- Data@AddInd[x,ii,ind]
#     } else {
#       I_hist <- Data@Ind[x, ind]
#     }
#     yind <- 1:yrsmth
#     slppar <- summary(lm(log(I_hist) ~ yind))$coefficients[2, 1:2]
#     if (reps > 1) {
#       Islp <- rnorm(reps, slppar[1], slppar[2])
#     }
#     else {
#       Islp <- slppar[1]
#     }
#     TAC <- TACstar * (1 + lambda * Islp) * c1
#     list(TAC = TAC, TACstar = TACstar, I_hist = I_hist, Islp = Islp,
#          C_dat = C_dat, Data = Data, Years = Years)
#   } # end myIslope_
#
#   runIslope <- myIslope_(x, Data, reps, yrsmth, lambda, xx, index, ii, c1)
#   TAC <- MSEtool::TACfilter(runIslope$TAC)
#   runIslope$TAC <- TAC
#   if (plot)
#     Islope_plot(runIslope, Data)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
#
# class(myIslope_VS)<-"MP"


# myIslope_RP<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5,
#                     lambda = 0.4, # default lambda=0.4
#                     xx = 0.45, index="AddInd", ii=1, c1=1)
# {
#   myIslope_<-function (x, Data, reps , yrsmth, lambda , xx , index, ii, c1)
#   {
#     ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#     Years <- Data@Year[ind]
#     ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#     C_dat <- Data@Cat[x, ind]
#     if (is.na(Data@MPrec[x]) || length(Data@Year) == ylast + 1) {
#       TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat, na.rm = TRUE), Data@CV_Cat/(yrsmth^0.5))
#     }
#     else {
#       TACstar <- rep(Data@MPrec[x], reps)
#     }
#
#     if(index=="AddInd"){
#       I_hist <- Data@AddInd[x,ii,ind]
#     } else {
#       I_hist <- Data@Ind[x, ind]
#     }
#     yind <- 1:yrsmth
#     slppar <- summary(lm(log(I_hist) ~ yind))$coefficients[2, 1:2]
#     if (reps > 1) {
#       Islp <- rnorm(reps, slppar[1], slppar[2])
#     }
#     else {
#       Islp <- slppar[1]
#     }
#     TAC <- TACstar * (1 + lambda * Islp) * c1
#     list(TAC = TAC, TACstar = TACstar, I_hist = I_hist, Islp = Islp,
#          C_dat = C_dat, Data = Data, Years = Years)
#   } #end myIslope_
#
#   runIslope <- myIslope_(x, Data, reps, yrsmth, lambda, xx, index, ii, c1)
#   TAC <- MSEtool::TACfilter(runIslope$TAC)
#   runIslope$TAC <- TAC
#   if (plot)
#     Islope_plot(runIslope, Data)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
#
# class(myIslope_RP)<-"MP"
