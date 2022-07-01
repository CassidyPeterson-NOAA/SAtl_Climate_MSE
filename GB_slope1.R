GB_slope1<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, lambda = 1)
{
  dependencies = "Data@Year, Data@Cat, Data@CV_Cat, Data@Ind"
  Catrec <- Data@Cat[x, length(Data@Cat[x, ])]
  ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
  I_hist <- Data@Ind[x, ind]
  yind <- 1:yrsmth
  slppar <- summary(lm(log(I_hist) ~ yind))$coefficients[2,
                                                         1:2]
  if (reps > 1) {
    Islp <- rnorm(reps, slppar[1], slppar[2])
  }
  else {
    Islp <- slppar[1]
  }
  MuC <- Data@Cat[x, length(Data@Cat[x, ])]
  Cc <- MSEtool::trlnorm(reps, MuC, Data@CV_Cat[x, 1])
  TAC <- Cc * (1 + lambda * Islp)
  TAC[TAC > (1.2 * Catrec)] <- 1.2 * Catrec
  TAC[TAC < (0.8 * Catrec)] <- 0.8 * Catrec
  TAC <- MSEtool::TACfilter(TAC)
  if (plot)
    GB_slope_plot(Data, ind, I_hist, MuC, TAC, Islp)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(GB_slope1)<-"MP"
