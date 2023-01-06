

myIratio <- function (x, Data, reps = 100, plot = FALSE, yrs = c(2, 5), index="AddInd", ii=1, const=1, c1=1)
{
  dependencies = "Data@Ind, Data@CV_Ind, Data@Cat, Data@CV_Cat, Data@AddInd, Data@CV_AddInd" # Data - MSE@Hist@Data
  ind.num <- (length(Data@Year) - yrs[1] + 1):length(Data@Year)
  ind.den <- (length(Data@Year) - yrs[2] + 1):(length(Data@Year) - yrs[1])
  if(index=="AddInd"){
    if (reps == 1) {
      I.num <- Data@AddInd[x, ii, ind.num]
      I.den <- Data@AddInd[x, ii, ind.den]
    }  else {
      #Data@CV_AddInd[1,1,dim(Data@CV_AddInd)[3]] -- [iters, index , yrs ]
      I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@AddInd[x, ii,ind.num], (c1*Data@CV_AddInd[x, ii, ind.num]) )
      I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@AddInd[x, ii, ind.den], (c1*Data@CV_AddInd[x, ii, ind.den]) )
    }# end else reps==1
  } else{
    if (reps == 1) {
      I.num <- Data@Ind[x, ind.num]
      I.den <- Data@Ind[x, ind.den]
    }  else {
      I.num <- MSEtool::trlnorm(reps * length(ind.num), Data@Ind[x,ind.num], Data@CV_Ind[x, ind.num])
      I.den <- MSEtool::trlnorm(reps * length(ind.den), Data@Ind[x, ind.den], Data@CV_Ind[x, ind.den])
    }# end else reps==1
  } # end if-else AddIndex

  I.num <- matrix(I.num, ncol = reps)
  I.den <- matrix(I.den, ncol = reps)
  alpha <- apply(I.num, 2, mean, na.rm = TRUE)/apply(I.den, 2, mean, na.rm = TRUE) * const #########
  Cat <- Data@Cat[x, length(Data@Cat[x, ])]
  Cc <- MSEtool::trlnorm(reps, Cat, Data@CV_Cat[x, 1])
  TAC <- alpha * Cc
  TAC <- MSEtool::TACfilter(TAC)
  if (plot)
    Iratio_plot(Data, I.num, ind.num, I.den, ind.den, alpha, TAC, Cat)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(myIratio)<-"MP"


# VS
myIratio_VS<-myIratio
formals(myIratio_VS)$const<-1.125 #1.125
formals(myIratio_VS)$c1<-1
formals(myIratio_VS)$yrs<-c(2,5) # c(2,5) -> c(2, 6)

class(myIratio_VS)<-"MP"

# BSB
myIratio_BSB<-myIratio
formals(myIratio_BSB)$const<-1.3 # 1.2 or 1.32
formals(myIratio_BSB)$yrs<-c(2,5) # c(2,5) or c(2,6)
class(myIratio_BSB)<-"MP"


# RP
myIratio_RP<-myIratio
formals(myIratio_RP)$const<-0.995 #0.99
class(myIratio_RP)<-"MP"





