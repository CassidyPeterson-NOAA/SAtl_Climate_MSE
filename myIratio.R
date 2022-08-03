
# myIratio_RP <- function (x, Data, reps = 100, plot = FALSE, yrs = c(3, 6), index="AddInd", ii=1)
# {
#   dependencies = "Data@Ind, Data@CV_Ind, Data@Cat, Data@CV_Cat, Data@AddInd"
#   ind.num <- (length(Data@Year) - yrs[1] + 1):length(Data@Year)
#   ind.den <- (length(Data@Year) - yrs[2] + 1):(length(Data@Year) - yrs[1])
#   if(index=="AddIndex"){
#     if (reps == 1) {
#       I.num <- Data@AddInd[x, ii, ind.num]
#       I.den <- Data@AddInd[x, ii, ind.den]
#     }  else {
#       I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@AddInd[x, ii,ind.num], Data@CV_Ind[x, ind.num])
#       I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@AddInd[x, ii, ind.den], Data@CV_Ind[x, ind.den])
#     }# end else reps==1
#   } else{
#     if (reps == 1) {
#       I.num <- Data@Ind[x, ind.num]
#       I.den <- Data@Ind[x, ind.den]
#     }  else {
#       I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@Ind[x,ind.num], Data@CV_Ind[x, ind.num])
#       I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@Ind[x, ind.den], Data@CV_Ind[x, ind.den])
#     }# end else reps==1
#   }
#   # end else AddIndex
#
#   I.num <- matrix(I.num, ncol = reps)
#   I.den <- matrix(I.den, ncol = reps)
#   alpha <- apply(I.num, 2, mean, na.rm = TRUE)/apply(I.den, 2, mean, na.rm = TRUE)
#   Cat <- Data@Cat[x, length(Data@Cat[x, ])]
#   Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
#   TAC <- alpha * Cc
#   TAC <- MSEtool::TACfilter(TAC)
#   if (plot)
#     Iratio_plot(Data, I.num, ind.num, I.den, ind.den, alpha,
#                 TAC, Cat)
#   Rec <- new("Rec")
#   Rec@TAC <- TAC
#   Rec
# }
#
# class(myIratio_RP)<-"MP"


myIratio_VS <- function (x, Data, reps = 100, plot = FALSE, yrs = c(2, 5), index="AddInd", ii=1, const=1.08)
{
  dependencies = "Data@Ind, Data@CV_Ind, Data@Cat, Data@CV_Cat, Data@AddInd"
  ind.num <- (length(Data@Year) - yrs[1] + 1):length(Data@Year)
  ind.den <- (length(Data@Year) - yrs[2] + 1):(length(Data@Year) - yrs[1])
  if(index=="AddIndex"){
    if (reps == 1) {
      I.num <- Data@AddInd[x, ii, ind.num]
      I.den <- Data@AddInd[x, ii, ind.den]
    }  else {
      I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@AddInd[x, ii,ind.num], Data@CV_Ind[x, ind.num])
      I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@AddInd[x, ii, ind.den], Data@CV_Ind[x, ind.den])
    }# end else reps==1
  } else{
    if (reps == 1) {
      I.num <- Data@Ind[x, ind.num]
      I.den <- Data@Ind[x, ind.den]
    }  else {
      I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@Ind[x,ind.num], Data@CV_Ind[x, ind.num])
      I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@Ind[x, ind.den], Data@CV_Ind[x, ind.den])
    }# end else reps==1
  }
  # end else AddIndex

  I.num <- matrix(I.num, ncol = reps)
  I.den <- matrix(I.den, ncol = reps)
  alpha <- apply(I.num, 2, mean, na.rm = TRUE)/apply(I.den, 2, mean, na.rm = TRUE) * const #########
  Cat <- Data@Cat[x, length(Data@Cat[x, ])]
  Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
  TAC <- alpha * Cc
  TAC <- MSEtool::TACfilter(TAC)
  if (plot)
    Iratio_plot(Data, I.num, ind.num, I.den, ind.den, alpha,
                TAC, Cat)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(myIratio_VS)<-"MP"



myIratio_BSB <- function (x, Data, reps = 100, plot = FALSE, yrs = c(2, 5), index="AddInd", ii=1, const=1.04)
{
  dependencies = "Data@Ind, Data@CV_Ind, Data@Cat, Data@CV_Cat, Data@AddInd"
  ind.num <- (length(Data@Year) - yrs[1] + 1):length(Data@Year)
  ind.den <- (length(Data@Year) - yrs[2] + 1):(length(Data@Year) - yrs[1])
  if(index=="AddIndex"){
    if (reps == 1) {
      I.num <- Data@AddInd[x, ii, ind.num]
      I.den <- Data@AddInd[x, ii, ind.den]
    }  else {
      I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@AddInd[x, ii,ind.num], Data@CV_Ind[x, ind.num])
      I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@AddInd[x, ii, ind.den], Data@CV_Ind[x, ind.den])
    }# end else reps==1
  } else{
    if (reps == 1) {
      I.num <- Data@Ind[x, ind.num]
      I.den <- Data@Ind[x, ind.den]
    }  else {
      I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@Ind[x,ind.num], Data@CV_Ind[x, ind.num])
      I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@Ind[x, ind.den], Data@CV_Ind[x, ind.den])
    }# end else reps==1
  }
  # end else AddIndex

  I.num <- matrix(I.num, ncol = reps)
  I.den <- matrix(I.den, ncol = reps)
  alpha <- apply(I.num, 2, mean, na.rm = TRUE)/apply(I.den, 2, mean, na.rm = TRUE) * const #########
  Cat <- Data@Cat[x, length(Data@Cat[x, ])]
  Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
  TAC <- alpha * Cc
  TAC <- MSEtool::TACfilter(TAC)
  if (plot)
    Iratio_plot(Data, I.num, ind.num, I.den, ind.den, alpha,
                TAC, Cat)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(myIratio_BSB)<-"MP"

myIratio_RP <- function (x, Data, reps = 100, plot = FALSE, yrs = c(2, 5), index="AddInd", ii=1)
{
  dependencies = "Data@Ind, Data@CV_Ind, Data@Cat, Data@CV_Cat, Data@AddInd"
  ind.num <- (length(Data@Year) - yrs[1] + 1):length(Data@Year)
  ind.den <- (length(Data@Year) - yrs[2] + 1):(length(Data@Year) - yrs[1])
  if(index=="AddIndex"){
    if (reps == 1) {
      I.num <- Data@AddInd[x, ii, ind.num]
      I.den <- Data@AddInd[x, ii, ind.den]
    }  else {
      I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@AddInd[x, ii,ind.num], Data@CV_Ind[x, ind.num])
      I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@AddInd[x, ii, ind.den], Data@CV_Ind[x, ind.den])
    }# end else reps==1
  } else{
    if (reps == 1) {
      I.num <- Data@Ind[x, ind.num]
      I.den <- Data@Ind[x, ind.den]
    }  else {
      I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@Ind[x,ind.num], Data@CV_Ind[x, ind.num])
      I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@Ind[x, ind.den], Data@CV_Ind[x, ind.den])
    }# end else reps==1
  }
  # end else AddIndex

  I.num <- matrix(I.num, ncol = reps)
  I.den <- matrix(I.den, ncol = reps)
  alpha <- apply(I.num, 2, mean, na.rm = TRUE)/apply(I.den,
                                                     2, mean, na.rm = TRUE)
  Cat <- Data@Cat[x, length(Data@Cat[x, ])]
  Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
  TAC <- alpha * Cc
  TAC <- MSEtool::TACfilter(TAC)
  if (plot)
    Iratio_plot(Data, I.num, ind.num, I.den, ind.den, alpha,
                TAC, Cat)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(myIratio_RP)<-"MP"
