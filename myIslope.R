


myIslope<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5,
                    # lambda = 0.4,
                    lambda = 1,
                    xx = 0.2, index="AddInd", ii=1)
{
  myIslope_<-function (x, Data, reps , yrsmth, lambda , xx , index, ii)
  {
    ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
    Years <- Data@Year[ind]
    ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
    C_dat <- Data@Cat[x, ind]
    if (is.na(Data@MPrec[x]) || length(Data@Year) == ylast +
        1) {
      TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat,
                                                        na.rm = TRUE), Data@CV_Cat/(yrsmth^0.5))
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
    slppar <- summary(lm(log(I_hist) ~ yind))$coefficients[2,
                                                           1:2]
    if (reps > 1) {
      Islp <- rnorm(reps, slppar[1], slppar[2])
    }
    else {
      Islp <- slppar[1]
    }
    TAC <- TACstar * (1 + lambda * Islp)
    list(TAC = TAC, TACstar = TACstar, I_hist = I_hist, Islp = Islp,
         C_dat = C_dat, Data = Data, Years = Years)
  }

  runIslope <- myIslope_(x, Data, reps, yrsmth, lambda, xx, index, ii)
  TAC <- MSEtool::TACfilter(runIslope$TAC)
  runIslope$TAC <- TAC
  if (plot)
    Islope_plot(runIslope, Data)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

class(myIslope)<-"MP"


