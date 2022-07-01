
myIT10<- function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, mc = 0.1, index="AddInd", ii=1, c=1.5)
{
  myIT_<-function (x, Data, reps, plot , yrsmth , mc, index, ii, c)
  {
    dependencies = "Data@Ind, Data@MPrec, Data@CV_Ind, Data@Iref, Data@AddInd "
    ind <- max(1, (length(Data@Year) - yrsmth + 1)):length(Data@Year)
    if (is.na(Data@Iref[x]))
      return(list(TAC = rep(as.numeric(NA), reps)))
    if(index=="AddIndex"){
      deltaI <- mean(Data@AddInd[x,ii, ind], na.rm = TRUE)/(Data@Iref[x]*c)
    }else{
      deltaI <- mean(Data@Ind[x, ind], na.rm = TRUE)/(Data@Iref[x]*c)
    }

    if (deltaI < (1 - mc))
      deltaI <- 1 - mc
    if (deltaI > (1 + mc))
      deltaI <- 1 + mc
    TAC <- Data@MPrec[x] * deltaI * MSEtool::trlnorm(reps, 1,
                                                     Data@CV_Ind[x, 1])
    TAC <- MSEtool::TACfilter(TAC)
    if (plot) {
      if(index=="AddInd"){
        op <- par(no.readonly = TRUE)
        on.exit(op)
        par(mfrow = c(1, 2), oma = c(1, 1, 1, 1), mar = c(5,
                                                          4, 1, 4))
        ylim <- range(c(Data@AddInd[x,ii, ind], Data@Iref[x]))
        plot(Data@Year[ind], Data@AddInd[x,ii, ind], xlab = "Year",
             ylab = paste0("Index (previous ", yrsmth, "years)"),
             bty = "l", type = "l", lwd = 2, ylim = ylim)
        lines(Data@Year[ind], rep(mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
                                  length(ind)), lty = 2)
        text(quantile(Data@Year[ind], 0.15), mean(Data@AddInd[x,ii, ind], na.rm = TRUE), "Mean Index", pos = 3)
        lines(Data@Year[ind], rep(mean(Data@Iref[x], na.rm = TRUE),
                                  length(ind)), lty = 3)
        text(quantile(Data@Year[ind], 0.15), Data@Iref[x], "Reference Index",
             pos = 3)
        boxplot(TAC, ylab = paste0("TAC (", Data@Units,
                                   ")"))
        points(1, Data@MPrec[x], cex = 2, col = "blue",
               pch = 16)
        text(1, Data@MPrec[x], cex = 1, col = "blue", "Last Catch",
             pos = 1)
      }else{
        op <- par(no.readonly = TRUE)
        on.exit(op)
        par(mfrow = c(1, 2), oma = c(1, 1, 1, 1), mar = c(5,
                                                          4, 1, 4))
        ylim <- range(c(Data@Ind[x, ind], Data@Iref[x]))
        plot(Data@Year[ind], Data@Ind[x, ind], xlab = "Year",
             ylab = paste0("Index (previous ", yrsmth, "years)"),
             bty = "l", type = "l", lwd = 2, ylim = ylim)
        lines(Data@Year[ind], rep(mean(Data@Ind[x, ind], na.rm = TRUE),
                                  length(ind)), lty = 2)
        text(quantile(Data@Year[ind], 0.15), mean(Data@Ind[x,
                                                           ind], na.rm = TRUE), "Mean Index", pos = 3)
        lines(Data@Year[ind], rep(mean(Data@Iref[x], na.rm = TRUE),
                                  length(ind)), lty = 3)
        text(quantile(Data@Year[ind], 0.15), Data@Iref[x], "Reference Index",
             pos = 3)
        boxplot(TAC, ylab = paste0("TAC (", Data@Units,
                                   ")"))
        points(1, Data@MPrec[x], cex = 2, col = "blue",
               pch = 16)
        text(1, Data@MPrec[x], cex = 1, col = "blue", "Last Catch",
             pos = 1)
      } #end index=NULL

    } #end plot

    list(TAC = TAC)
  }

  runIT <- myIT_(x, Data, reps, plot, yrsmth, mc, index, ii, c)
  Rec <- new("Rec")
  Rec@TAC <- runIT$TAC
  Rec
}


class(myIT10)<-"MP"




