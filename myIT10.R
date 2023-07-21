
## NOTE: myIT_ must be imbedded within the myIT10 function for it to run.

myIT10<- function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, mc = 0.1, index="AddInd", ii=1,
                   c1=1,  # mult target constant
                   c2=1.0,  # mult ref catch constant
                   c3=1.0,  # mult index cv
                   damp=1) # damping param to reduce annual change-- 1 is no damping, 0 is total damping
{

  myIT_<-function (x, Data, reps, plot , yrsmth , mc, index, ii, c1, c2, c3, damp)
  {
    dependencies = "Data@Ind, Data@MPrec, Data@CV_Ind, Data@Iref, Data@AddInd, Data@CV_AddInd "
    ind <- max(1, (length(Data@Year) - yrsmth + 1)):length(Data@Year)
    if (is.na(Data@Iref[x]))
      return(list(TAC = rep(as.numeric(NA), reps)))
    if(index=="AddInd"){
      deltaI1 <- mean(Data@AddInd[x,ii, ind], na.rm = TRUE)/(Data@Iref[x]*c1)
    }else{
      deltaI1 <- mean(Data@Ind[x, ind], na.rm = TRUE)/(Data@Iref[x]*c1)
    }

    deltaI<-exp(log(deltaI1)*damp) ############ ADD DAMPING PARAM

    if (deltaI < (1 - mc))
      deltaI <- 1 - mc
    if (deltaI > (1 + mc))
      deltaI <- 1 + mc

    if(index=="AddInd"){
      TAC <- Data@MPrec[x] * c2 * deltaI * MSEtool::trlnorm(reps, 1, (c3 * mean(Data@CV_AddInd[x,ii,], na.rm=T)) )
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

  runIT <- myIT_(x, Data, reps, plot, yrsmth, mc, index, ii, c1, c2, c3, damp)
  Rec <- new("Rec")
  Rec@TAC <- runIT$TAC
  Rec
}
class(myIT10)<-"MP"

# environment(test_make_interim_MP) <- asNamespace("SAMtool")


# BSB
myIT10_BSB<-myIT10
formals(myIT10_BSB)$mc<-0.2 # 0.1
formals(myIT10_BSB)$c1<-1
formals(myIT10_BSB)$c2<-1.0 #0.9975
formals(myIT10_BSB)$damp<-0.5 #1
class(myIT10_BSB)<-"MP"



# RP
# yrsmth=5, mc=0.1, ii=1, c=0.95
myIT10_RP<-myIT10
formals(myIT10_RP)$mc<-0.3   #0.3 # alternate tuning mc=0.3, c1=1, c2=0.905, damp=1
formals(myIT10_RP)$c1<-1.26     #1
formals(myIT10_RP)$c2<-0.9225 #0.9225
formals(myIT10_RP)$damp<-0.5 #0.75

class(myIT10_RP)<-"MP"

# myIT10_RP2<-myIT10
# formals(myIT10_RP2)$mc<-0.3   #0.1 # alternate tuning mc=0.3, c1=1, c2=0.905, damp=1
# formals(myIT10_RP2)$c1<-1     #1
# formals(myIT10_RP2)$c2<-0.905 #0.945
# formals(myIT10_RP2)$damp<-1 #1
# class(myIT10_RP2)<-"MP"


# VS
# yrsmth=5, mc=0.1, ii=1, c=0.8
myIT10_VS<-myIT10          # SELECTED FOR RESULTS ANALYSIS
formals(myIT10_VS)$mc<-0.3 # 0.3 - ALT tuning: mc=0.9, c1=1.6
formals(myIT10_VS)$c1<- 1.575 # 1.575; TESTING: 1.6 when mc>0.5
formals(myIT10_VS)$c2<-1 # 1; OLD=0.9225
formals(myIT10_VS)$c3<-1
formals(myIT10_VS)$damp<-1 #1

class(myIT10_VS)<-"MP"


myIT10_VS2<-myIT10           # TESTED BUT NOT USED
formals(myIT10_VS2)$mc<-0.4 # 0.3 - ALT tuning: mc=0.9, c1=1.6
formals(myIT10_VS2)$c1<-1.6 # 1.575; TESTING: 1.6 when mc>0.5
formals(myIT10_VS2)$c2<-1 # 1; OLD=0.9225
formals(myIT10_VS2)$c3<-1
formals(myIT10_VS2)$damp<-0.5

class(myIT10_VS2)<-"MP"
