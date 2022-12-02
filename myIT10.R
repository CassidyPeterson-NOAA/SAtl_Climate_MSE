
myIT_<-function (x, Data, reps, plot , yrsmth , mc, index, ii, c1, c2, damp)
{
  dependencies = "Data@Ind, Data@MPrec, Data@CV_Ind, Data@Iref, Data@AddInd "
  ind <- max(1, (length(Data@Year) - yrsmth + 1)):length(Data@Year)
  if (is.na(Data@Iref[x]))
    return(list(TAC = rep(as.numeric(NA), reps)))
  if(index=="AddInd"){
    deltaI <- mean(Data@AddInd[x,ii, ind], na.rm = TRUE)/(Data@Iref[x]*c1)
  }else{
    deltaI <- mean(Data@Ind[x, ind], na.rm = TRUE)/(Data@Iref[x]*c1)
  }

  deltaI<-exp(log(deltaI*damp)) ############ ADD DAMPING PARAM

  if (deltaI < (1 - mc))
    deltaI <- 1 - mc
  if (deltaI > (1 + mc))
    deltaI <- 1 + mc

  if(index=="AddInd"){
    TAC <- Data@MPrec[x] * c2 * deltaI * MSEtool::trlnorm(reps, 1, mean(Data@CV_AddInd[x,ii,], na.rm=T))
  }else{
    TAC <- Data@MPrec[x] * c2 * deltaI * MSEtool::trlnorm(reps, 1, Data@CV_Ind[x, 1])
  }

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

myIT10<- function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, mc = 0.1, index="AddInd", ii=1,
                   c1=2,  # mult target constant
                   c2=1.0,  # mult ref catch constant
                   damp=1) # damping param to reduce annual change-- 1 is no damping, 0 is total damping
{
  runIT <- myIT_(x, Data, reps, plot, yrsmth, mc, index, ii, c1, c2, damp)
  Rec <- new("Rec")
  Rec@TAC <- runIT$TAC
  Rec
}
class(myIT10)<-"MP"



# BSB
myIT10_BSB<-myIT10
formals(myIT10_BSB)$mc<-0.1
formals(myIT10_BSB)$c1<-1
formals(myIT10_BSB)$c2<-1 #0.9975
class(myIT10_BSB)<-"MP"



# RP
# yrsmth=5, mc=0.1, ii=1, c=0.95
myIT10_RP<-myIT10
formals(myIT10_RP)$mc<-0.1
formals(myIT10_RP)$c1<-1
formals(myIT10_RP)$c2<-0.945

class(myIT10_RP)<-"MP"




# VS
# yrsmth=5, mc=0.1, ii=1, c=0.8
myIT10_VS<-myIT10
formals(myIT10_VS)$mc<-0.1
formals(myIT10_VS)$c1<-1
formals(myIT10_VS)$c2<-0.9225
formals(myIT10_VS)$damp<-1

class(myIT10_VS)<-"MP"



