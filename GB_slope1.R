
myGB_slope<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, delta=0.2,
                        lambda = 1, index="AddInd", indd=1, const=1) # w default lambda=1, c=1
{
  dependencies = "Data@Cat, Data@Cref, Data@Iref, Data@Ind, Data@AddInd"

  if(index=="AddInd"){
    INDEX<-Data@AddInd[,indd,]
  }
  if(index=="Ind"){
    INDEX<-Data@Ind
  }

  Catrec <- Data@Cat[x, length(Data@Cat[x, ])]
  ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
  I_hist <- INDEX[x, ind]
  yind <- 1:yrsmth
  slppar <- summary(lm(log(I_hist) ~ yind))$coefficients[2,1:2]
  if (reps > 1) {
    Islp <- rnorm(reps, slppar[1], slppar[2])
  }
  else {
    Islp <- slppar[1]
  }
  MuC <- Data@Cat[x, length(Data@Cat[x, ])]
  Cc <- MSEtool::trlnorm(reps, MuC, Data@CV_Cat[x, 1])
  TAC <- Cc * (1 + lambda * Islp) * const
  TAC[TAC > ((1+delta) * Catrec)] <- (1+delta) * Catrec
  TAC[TAC < ((1-delta) * Catrec)] <- (1-delta) * Catrec
  TAC <- MSEtool::TACfilter(TAC)
  if (plot)
    GB_slope_plot(Data, ind, I_hist, MuC, TAC, Islp)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(myGB_slope)<-"MP"


#BSB
GB_slope_BSB<-myGB_slope         # SELECTED FOR RESULTS ANALYSIS  #| OLD: lambda=0.7, const=1.05 | NEW: lambda=0.9, const=1.0525
formals(GB_slope_BSB)$lambda<-1   # 0.9
formals(GB_slope_BSB)$const<-1.075 #1.0525
formals(GB_slope_BSB)$delta<-0.3    #0.2
class(GB_slope_BSB)<-"MP"
# Lambda | const | delta | med(SSB_SSBMSY30)
# 1.5 | 1.1 | 0.3 | 1.2
# 1.2 | 1.1 | 0.3 |0.99
# 1 | 1.1 | 0.3 |0.816
# 1 | 1.3 | 0.3 | 0.488
# 1 | 1.2 | 0.3 | 0.56
# 1 | 1 | 0.3 | 2.2
# 1 | 1.05 | 0.3 | 1.4
# 1 | 1.05 | 0.2 | 1.49
# 1 | 1.075 | 0.3 | 1.01



# #BSB
# GB_slope_BSB1<-myGB_slope         # TESTED BUT NOT USED #| OLD: lambda=0.7, const=1.05 | NEW: lambda=0.9, const=1.0525
# formals(GB_slope_BSB1)$lambda<-0.9   # 0.9
# formals(GB_slope_BSB1)$const<-1.0525 #1.0525
# formals(GB_slope_BSB1)$delta<-0.2    #0.2
# class(GB_slope_BSB1)<-"MP"
#
#
# GB_slope_BSB2<-myGB_slope      # TESTED BUT NOT USED
# formals(GB_slope_BSB2)$lambda<-0.5
# formals(GB_slope_BSB2)$const<-1.03
# formals(GB_slope_BSB2)$delta<-0.2    #0.2
# class(GB_slope_BSB2)<-"MP"



#RP
GB_slope_RP<-myGB_slope         # SELECTED FOR RESULTS ANALYSIS
formals(GB_slope_RP)$lambda<-3.5   # 1.8
formals(GB_slope_RP)$const<-1      #1
formals(GB_slope_RP)$delta<-0.3    #0.2
class(GB_slope_RP)<-"MP"
# Lambda | const | delta | med(SSB_SSBMSY30)
# 1.8 | 1 | 0.2 | 0.75
# 2 | 1 | 0.2 | 0.798
# 2 | 1 | 0.3 | 0.797
# 2 | 0.5 | 0.3 | 1.8
# 2 | 0.95 | 0.3 | 1.33
# 2.5 | 1 | 0.3 | 0.858
# 3 | 1 | 0.3 | 0.91
# 3.5 | 1 | 0.3 | 0.95

# GB_slope_RP<-myGB_slope      # TESTED BUT NOT USED
# formals(GB_slope_RP)$lambda<-1.65   # 1.8 | 1.65 when delta=0.3 & const=1
# formals(GB_slope_RP)$const<-1      #1
# formals(GB_slope_RP)$delta<-0.3    #0.2
# class(GB_slope_RP)<-"MP"
#



#VS
GB_slope_VS<-myGB_slope         # SELECTED FOR RESULTS ANALYSIS
formals(GB_slope_VS)$lambda<-1.1   # 1
formals(GB_slope_VS)$const<-1.055      #1.055
formals(GB_slope_VS)$delta<-0.3    #0.3
class(GB_slope_VS)<-"MP"
# Lambda | const | delta | med(SSB_SSBMSY30)
# 1 | 1.055 | 0.2 | 0.9148
# 0.9 | 1.055 | 0.2 | 0.828
# 1.1 | 1.055 | 0.2 | 0.9955

# GB_slope_VS2<-myGB_slope       # TESTED BUT NOT USED
#  formals(GB_slope_VS2)$lambda<-0.5
#  formals(GB_slope_VS2)$const<-1.02
#  class(GB_slope_VS2)<-"MP"



# > GB_slope
# function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, lambda = 1)
# {
#   dependencies = "Data@Year, Data@Cat, Data@CV_Cat, Data@Ind"
#   Catrec <- Data@Cat[x, length(Data@Cat[x, ])]
#   ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
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
#   MuC <- Data@Cat[x, length(Data@Cat[x, ])]
#   Cc <- MSEtool::trlnorm(reps, MuC, Data@CV_Cat[x, 1])
#   TAC <- Cc * (1 + lambda * Islp)
#   TAC[TAC > (1.2 * Catrec)] <- 1.2 * Catrec
#   TAC[TAC < (0.8 * Catrec)] <- 0.8 * Catrec
#   TAC <- MSEtool::TACfilter(TAC)
#   if (plot)
#     GB_slope_plot(Data, ind, I_hist, MuC, TAC, Islp)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
# <bytecode: 0x00000215f1424c90>
#   <environment: namespace:DLMtool>
#   attr(,"class")
# [1] "MP"
