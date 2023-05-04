myItarget<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, xx = 0,
                         Imulti = 1.5,  index="AddIndex", ii=1, c1=0.5, c2=0.8) ### DEFAULT Imulti=1.5 | c1 = 0.5 | c2=0.8
{

  myItarget_<- function (x, Data, reps = 100, plot , yrsmth , xx,
                         Imulti, index, ii, c1, c2)
  {
    ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
    ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
    ind2 <- ((ylast - (yrsmth - 1)):ylast)
    ind3 <- ((ylast - (yrsmth * 2 - 1)):ylast)
    C_dat <- Data@Cat[x, ind2]
    TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat,
                                                      na.rm = TRUE), Data@CV_Cat[x, 1]/(yrsmth^0.5))
    if(index=="AddIndex"){
      Irecent <- mean(Data@AddInd[x,ii, ind], na.rm = TRUE)
      Iave <- mean(Data@AddInd[x,ii, ind3], na.rm = TRUE)
    }else{
      Irecent <- mean(Data@Ind[x, ind], na.rm = TRUE)
      Iave <- mean(Data@Ind[x, ind3], na.rm = TRUE)
    }

    Itarget <- Iave * Imulti
    I0 <- c2 * Iave
    if (Irecent > I0) {
      TAC <- c1 * TACstar * (1 + ((Irecent - I0)/(Itarget - I0)))
    }
    else {
      TAC <- c1 * TACstar * (Irecent/I0)^2
    }
    TAC <- MSEtool::TACfilter(TAC)
    if (plot) {
      if(index=="AddIndex"){
        op <- par(no.readonly = TRUE)
        on.exit(op)
        par(mfrow = c(1, 2))
        ylim <- range(c(Data@AddInd[x,ii, ], Itarget, I0))
        plot(Data@Year, Data@AddInd[x,ii, ], type = "l", lwd = 2,
             bty = "l", xlab = "Year", ylab = "Index",
             ylim = ylim)
        points(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
               cex = 2, pch = 16, col = "blue")
        text(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
             cex = 1, "Irecent", pos = 3, col = "blue",
             xpd = NA)
        lines(Data@Year[ind3], rep(mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
                                   length(ind3)), lty = 2, col = "orange")
        text(mean(Data@Year[ind3]), mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
             "Iave", col = "orange", pos = 1)
        points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
        text(max(Data@Year), Itarget, cex = 1, "Itarget",
             pos = 3, col = "green", xpd = NA)
        points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
        text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
             col = "red", xpd = NA)
        ylim <- range(c(C_dat, TACstar, TAC))
        Years <- Data@Year[ind2]
        if (max(Years) != max(Data@Year)) {
          Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
        }
        yrs <- length(Years) - length(C_dat)
        Cdat <- c(C_dat, rep(NA, yrs))
        plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
             lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
                                                              Data@Units, ")"), ylim = ylim)
        abline(v = max(Data@Year[ind2]), col = "gray",
               lty = 3)
        points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
               cex = 2, col = "orange", pch = 16)
        text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
             "mean TAC*", pos = 2, xpd = NA, col = "orange")
        boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
                axes = FALSE)
      }else{
        op <- par(no.readonly = TRUE)
        on.exit(op)
        par(mfrow = c(1, 2))
        ylim <- range(c(Data@Ind[x, ], Itarget, I0))
        plot(Data@Year, Data@Ind[x, ], type = "l", lwd = 2,
             bty = "l", xlab = "Year", ylab = "Index",
             ylim = ylim)
        points(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
               cex = 2, pch = 16, col = "blue")
        text(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
             cex = 1, "Irecent", pos = 3, col = "blue",
             xpd = NA)
        lines(Data@Year[ind3], rep(mean(Data@Ind[x, ind3], na.rm = TRUE),
                                   length(ind3)), lty = 2, col = "orange")
        text(mean(Data@Year[ind3]), mean(Data@Ind[x, ind3], na.rm = TRUE),
             "Iave", col = "orange", pos = 1)
        points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
        text(max(Data@Year), Itarget, cex = 1, "Itarget",
             pos = 3, col = "green", xpd = NA)
        points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
        text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
             col = "red", xpd = NA)
        ylim <- range(c(C_dat, TACstar, TAC))
        Years <- Data@Year[ind2]
        if (max(Years) != max(Data@Year)) {
          Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
        }
        yrs <- length(Years) - length(C_dat)
        Cdat <- c(C_dat, rep(NA, yrs))
        plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
             lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
                                                              Data@Units, ")"), ylim = ylim)
        abline(v = max(Data@Year[ind2]), col = "gray",
               lty = 3)
        points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
               cex = 2, col = "orange", pch = 16)
        text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
             "mean TAC*", pos = 2, xpd = NA, col = "orange")
        boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
                axes = FALSE)
      }

    }# end if plot

    list(TAC = TAC)
  }


  runItarget <- myItarget_(x, Data, reps, plot, yrsmth, xx, Imulti, index,ii, c1, c2)
  Rec <- new("Rec")
  Rec@TAC <- runItarget$TAC
  Rec
}
class(myItarget)<-"MP"


# BSB
# myItarget_BSB<-myItarget
# formals(myItarget_BSB)$c1<-0.75     #1
# formals(myItarget_BSB)$c2<-0.3   #0.3
# formals(myItarget_BSB)$xx<-0     #0
# formals(myItarget_BSB)$Imulti<-0.5 #1
# class(myItarget_BSB)<-"MP"


# BSB
myItarget_BSB<-myItarget
formals(myItarget_BSB)$c1<-1     #1
formals(myItarget_BSB)$c2<-0.3   #0.3
formals(myItarget_BSB)$xx<-0     #0
formals(myItarget_BSB)$Imulti<-1 #1
class(myItarget_BSB)<-"MP"





# RP
myItarget_RP<-myItarget
formals(myItarget_RP)$c1<-0.5     #0.5
formals(myItarget_RP)$c2<-0.8   #0.8
formals(myItarget_RP)$xx<-0     #0
formals(myItarget_RP)$Imulti<-1.75 #1.75
class(myItarget_RP)<-"MP"







# VS

myItarget_VS<-myItarget        # TESTED BUT NOT USED
formals(myItarget_VS)$c1<-1
formals(myItarget_VS)$c2<-1
formals(myItarget_VS)$xx<-0
formals(myItarget_VS)$Imulti<-2.5
class(myItarget_VS)<-"MP"

myItarget_VS1<-myItarget        # TESTED BUT NOT USED
formals(myItarget_VS1)$c1<-0.65     #1
formals(myItarget_VS1)$c2<-0.6   #0.3
formals(myItarget_VS1)$xx<-0     #0
formals(myItarget_VS1)$Imulti<-1.5 #1
class(myItarget_VS1)<-"MP"


myItarget_VS2<-myItarget         # THIS ONE USED IN RESULTS
formals(myItarget_VS2)$c1<-0.5     #1
formals(myItarget_VS2)$c2<-0.8   #0.3
formals(myItarget_VS2)$xx<-0     #0
formals(myItarget_VS2)$Imulti<-1.1 #1
class(myItarget_VS2)<-"MP"











# > Itarget_
# function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, xx = 0,
#           Imulti = 1.5)
# {
#   ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#   ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#   ind2 <- ((ylast - (yrsmth - 1)):ylast)
#   ind3 <- ((ylast - (yrsmth * 2 - 1)):ylast)
#   C_dat <- Data@Cat[x, ind2]
#   TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat,
#                                                     na.rm = TRUE), Data@CV_Cat[x, 1]/(yrsmth^0.5))
#   Irecent <- mean(Data@Ind[x, ind], na.rm = TRUE)
#   Iave <- mean(Data@Ind[x, ind3], na.rm = TRUE)
#   Itarget <- Iave * Imulti
#   I0 <- 0.8 * Iave
#   if (Irecent > I0) {
#     TAC <- 0.5 * TACstar * (1 + ((Irecent - I0)/(Itarget -
#                                                    I0)))
#   }
#   else {
#     TAC <- 0.5 * TACstar * (Irecent/I0)^2
#   }
#   TAC <- MSEtool::TACfilter(TAC)
#   if (plot) {
#     op <- par(no.readonly = TRUE)
#     on.exit(op)
#     par(mfrow = c(1, 2))
#     ylim <- range(c(Data@Ind[x, ], Itarget, I0))
#     plot(Data@Year, Data@Ind[x, ], type = "l", lwd = 2,
#          bty = "l", xlab = "Year", ylab = "Index",
#          ylim = ylim)
#     points(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#            cex = 2, pch = 16, col = "blue")
#     text(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#          cex = 1, "Irecent", pos = 3, col = "blue",
#          xpd = NA)
#     lines(Data@Year[ind3], rep(mean(Data@Ind[x, ind3], na.rm = TRUE),
#                                length(ind3)), lty = 2, col = "orange")
#     text(mean(Data@Year[ind3]), mean(Data@Ind[x, ind3], na.rm = TRUE),
#          "Iave", col = "orange", pos = 1)
#     points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#     text(max(Data@Year), Itarget, cex = 1, "Itarget",
#          pos = 3, col = "green", xpd = NA)
#     points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#     text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#          col = "red", xpd = NA)
#     ylim <- range(c(C_dat, TACstar, TAC))
#     Years <- Data@Year[ind2]
#     if (max(Years) != max(Data@Year)) {
#       Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#     }
#     yrs <- length(Years) - length(C_dat)
#     Cdat <- c(C_dat, rep(NA, yrs))
#     plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#          lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                           Data@Units, ")"), ylim = ylim)
#     abline(v = max(Data@Year[ind2]), col = "gray",
#            lty = 3)
#     points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#            cex = 2, col = "orange", pch = 16)
#     text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#          "mean TAC*", pos = 2, xpd = NA, col = "orange")
#     boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#             axes = FALSE)
#   }
#   list(TAC = TAC)
# }



# myItarget_BSB<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, xx = 0,
#                          Imulti = 1,  index="AddIndex", ii=1, c1=1, c2=0.3) ### DEFAULT Imulti=1.5 | c1 = 0.5 | c2=0.8
# {
#
#   myItarget_<- function (x, Data, reps = 100, plot , yrsmth , xx,
#                          Imulti, index, ii, c1, c2)
#   {
#     ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#     ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#     ind2 <- ((ylast - (yrsmth - 1)):ylast)
#     ind3 <- ((ylast - (yrsmth * 2 - 1)):ylast)
#     C_dat <- Data@Cat[x, ind2]
#     TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat,
#                                                       na.rm = TRUE), Data@CV_Cat[x, 1]/(yrsmth^0.5))
#     if(index=="AddIndex"){
#       Irecent <- mean(Data@AddInd[x,ii, ind], na.rm = TRUE)
#       Iave <- mean(Data@AddInd[x,ii, ind3], na.rm = TRUE)
#     }else{
#       Irecent <- mean(Data@Ind[x, ind], na.rm = TRUE)
#       Iave <- mean(Data@Ind[x, ind3], na.rm = TRUE)
#     }
#
#     Itarget <- Iave * Imulti
#     I0 <- c2 * Iave
#     if (Irecent > I0) {
#       TAC <- c1 * TACstar * (1 + ((Irecent - I0)/(Itarget - I0)))
#     }
#     else {
#       TAC <- c1 * TACstar * (Irecent/I0)^2
#     }
#     TAC <- MSEtool::TACfilter(TAC)
#     if (plot) {
#       if(index=="AddIndex"){
#         op <- par(no.readonly = TRUE)
#         on.exit(op)
#         par(mfrow = c(1, 2))
#         ylim <- range(c(Data@AddInd[x,ii, ], Itarget, I0))
#         plot(Data@Year, Data@AddInd[x,ii, ], type = "l", lwd = 2,
#              bty = "l", xlab = "Year", ylab = "Index",
#              ylim = ylim)
#         points(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
#                cex = 2, pch = 16, col = "blue")
#         text(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
#              cex = 1, "Irecent", pos = 3, col = "blue",
#              xpd = NA)
#         lines(Data@Year[ind3], rep(mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
#                                    length(ind3)), lty = 2, col = "orange")
#         text(mean(Data@Year[ind3]), mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
#              "Iave", col = "orange", pos = 1)
#         points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#         text(max(Data@Year), Itarget, cex = 1, "Itarget",
#              pos = 3, col = "green", xpd = NA)
#         points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#         text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#              col = "red", xpd = NA)
#         ylim <- range(c(C_dat, TACstar, TAC))
#         Years <- Data@Year[ind2]
#         if (max(Years) != max(Data@Year)) {
#           Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#         }
#         yrs <- length(Years) - length(C_dat)
#         Cdat <- c(C_dat, rep(NA, yrs))
#         plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#              lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                               Data@Units, ")"), ylim = ylim)
#         abline(v = max(Data@Year[ind2]), col = "gray",
#                lty = 3)
#         points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#                cex = 2, col = "orange", pch = 16)
#         text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#              "mean TAC*", pos = 2, xpd = NA, col = "orange")
#         boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#                 axes = FALSE)
#       }else{
#         op <- par(no.readonly = TRUE)
#         on.exit(op)
#         par(mfrow = c(1, 2))
#         ylim <- range(c(Data@Ind[x, ], Itarget, I0))
#         plot(Data@Year, Data@Ind[x, ], type = "l", lwd = 2,
#              bty = "l", xlab = "Year", ylab = "Index",
#              ylim = ylim)
#         points(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#                cex = 2, pch = 16, col = "blue")
#         text(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#              cex = 1, "Irecent", pos = 3, col = "blue",
#              xpd = NA)
#         lines(Data@Year[ind3], rep(mean(Data@Ind[x, ind3], na.rm = TRUE),
#                                    length(ind3)), lty = 2, col = "orange")
#         text(mean(Data@Year[ind3]), mean(Data@Ind[x, ind3], na.rm = TRUE),
#              "Iave", col = "orange", pos = 1)
#         points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#         text(max(Data@Year), Itarget, cex = 1, "Itarget",
#              pos = 3, col = "green", xpd = NA)
#         points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#         text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#              col = "red", xpd = NA)
#         ylim <- range(c(C_dat, TACstar, TAC))
#         Years <- Data@Year[ind2]
#         if (max(Years) != max(Data@Year)) {
#           Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#         }
#         yrs <- length(Years) - length(C_dat)
#         Cdat <- c(C_dat, rep(NA, yrs))
#         plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#              lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                               Data@Units, ")"), ylim = ylim)
#         abline(v = max(Data@Year[ind2]), col = "gray",
#                lty = 3)
#         points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#                cex = 2, col = "orange", pch = 16)
#         text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#              "mean TAC*", pos = 2, xpd = NA, col = "orange")
#         boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#                 axes = FALSE)
#       }
#
#     }# end if plot
#
#     list(TAC = TAC)
#   }
#
#
#   runItarget <- myItarget_(x, Data, reps, plot, yrsmth, xx, Imulti, index,ii, c1, c2)
#   Rec <- new("Rec")
#   Rec@TAC <- runItarget$TAC
#   Rec
# }
#
#
# class(myItarget_BSB)<-"MP"



# myItarget_RP<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, xx = 0,
#                         Imulti = 1.75,  index="AddIndex", ii=1, c1=0.5, c2=0.8) ### DEFAULT Imulti=1.5 | c1 = 0.5 | c2=0.8
# {
#
#   myItarget_<- function (x, Data, reps = 100, plot , yrsmth , xx,
#                          Imulti, index, ii, c1, c2)
#   {
#     ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#     ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#     ind2 <- ((ylast - (yrsmth - 1)):ylast)
#     ind3 <- ((ylast - (yrsmth * 2 - 1)):ylast)
#     C_dat <- Data@Cat[x, ind2]
#     TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat,
#                                                       na.rm = TRUE), Data@CV_Cat[x, 1]/(yrsmth^0.5))
#     if(index=="AddIndex"){
#       Irecent <- mean(Data@AddInd[x,ii, ind], na.rm = TRUE)
#       Iave <- mean(Data@AddInd[x,ii, ind3], na.rm = TRUE)
#     }else{
#       Irecent <- mean(Data@Ind[x, ind], na.rm = TRUE)
#       Iave <- mean(Data@Ind[x, ind3], na.rm = TRUE)
#     }
#
#     Itarget <- Iave * Imulti
#     I0 <- c2 * Iave
#     if (Irecent > I0) {
#       TAC <- c1 * TACstar * (1 + ((Irecent - I0)/(Itarget - I0)))
#     }
#     else {
#       TAC <- c1 * TACstar * (Irecent/I0)^2
#     }
#     TAC <- MSEtool::TACfilter(TAC)
#     if (plot) {
#       if(index=="AddIndex"){
#         op <- par(no.readonly = TRUE)
#         on.exit(op)
#         par(mfrow = c(1, 2))
#         ylim <- range(c(Data@AddInd[x,ii, ], Itarget, I0))
#         plot(Data@Year, Data@AddInd[x,ii, ], type = "l", lwd = 2,
#              bty = "l", xlab = "Year", ylab = "Index",
#              ylim = ylim)
#         points(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
#                cex = 2, pch = 16, col = "blue")
#         text(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
#              cex = 1, "Irecent", pos = 3, col = "blue",
#              xpd = NA)
#         lines(Data@Year[ind3], rep(mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
#                                    length(ind3)), lty = 2, col = "orange")
#         text(mean(Data@Year[ind3]), mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
#              "Iave", col = "orange", pos = 1)
#         points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#         text(max(Data@Year), Itarget, cex = 1, "Itarget",
#              pos = 3, col = "green", xpd = NA)
#         points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#         text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#              col = "red", xpd = NA)
#         ylim <- range(c(C_dat, TACstar, TAC))
#         Years <- Data@Year[ind2]
#         if (max(Years) != max(Data@Year)) {
#           Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#         }
#         yrs <- length(Years) - length(C_dat)
#         Cdat <- c(C_dat, rep(NA, yrs))
#         plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#              lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                               Data@Units, ")"), ylim = ylim)
#         abline(v = max(Data@Year[ind2]), col = "gray",
#                lty = 3)
#         points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#                cex = 2, col = "orange", pch = 16)
#         text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#              "mean TAC*", pos = 2, xpd = NA, col = "orange")
#         boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#                 axes = FALSE)
#       }else{
#         op <- par(no.readonly = TRUE)
#         on.exit(op)
#         par(mfrow = c(1, 2))
#         ylim <- range(c(Data@Ind[x, ], Itarget, I0))
#         plot(Data@Year, Data@Ind[x, ], type = "l", lwd = 2,
#              bty = "l", xlab = "Year", ylab = "Index",
#              ylim = ylim)
#         points(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#                cex = 2, pch = 16, col = "blue")
#         text(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#              cex = 1, "Irecent", pos = 3, col = "blue",
#              xpd = NA)
#         lines(Data@Year[ind3], rep(mean(Data@Ind[x, ind3], na.rm = TRUE),
#                                    length(ind3)), lty = 2, col = "orange")
#         text(mean(Data@Year[ind3]), mean(Data@Ind[x, ind3], na.rm = TRUE),
#              "Iave", col = "orange", pos = 1)
#         points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#         text(max(Data@Year), Itarget, cex = 1, "Itarget",
#              pos = 3, col = "green", xpd = NA)
#         points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#         text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#              col = "red", xpd = NA)
#         ylim <- range(c(C_dat, TACstar, TAC))
#         Years <- Data@Year[ind2]
#         if (max(Years) != max(Data@Year)) {
#           Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#         }
#         yrs <- length(Years) - length(C_dat)
#         Cdat <- c(C_dat, rep(NA, yrs))
#         plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#              lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                               Data@Units, ")"), ylim = ylim)
#         abline(v = max(Data@Year[ind2]), col = "gray",
#                lty = 3)
#         points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#                cex = 2, col = "orange", pch = 16)
#         text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#              "mean TAC*", pos = 2, xpd = NA, col = "orange")
#         boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#                 axes = FALSE)
#       }
#
#     }# end if plot
#
#     list(TAC = TAC)
#   }
#
#
#   runItarget <- myItarget_(x, Data, reps, plot, yrsmth, xx, Imulti, index,ii, c1, c2)
#   Rec <- new("Rec")
#   Rec@TAC <- runItarget$TAC
#   Rec
# }
#
#
# class(myItarget_RP)<-"MP"




#
# myItarget_VS2<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, xx = 0,
#                          Imulti = 1.1,  index="AddIndex", ii=1, c1=0.5, c2=0.8) ### DEFAULT Imulti=1.5 | c1 = 0.5 | c2=0.8
# {
#
#   myItarget_<- function (x, Data, reps = 100, plot , yrsmth , xx,
#                          Imulti, index, ii, c1, c2)
#   {
#     ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#     ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#     ind2 <- ((ylast - (yrsmth - 1)):ylast)
#     ind3 <- ((ylast - (yrsmth * 2 - 1)):ylast)
#     C_dat <- Data@Cat[x, ind2]
#     TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat,
#                                                       na.rm = TRUE), Data@CV_Cat[x, 1]/(yrsmth^0.5))
#     if(index=="AddIndex"){
#       Irecent <- mean(Data@AddInd[x,ii, ind], na.rm = TRUE)
#       Iave <- mean(Data@AddInd[x,ii, ind3], na.rm = TRUE)
#     }else{
#       Irecent <- mean(Data@Ind[x, ind], na.rm = TRUE)
#       Iave <- mean(Data@Ind[x, ind3], na.rm = TRUE)
#     }
#
#     Itarget <- Iave * Imulti
#     I0 <- c2 * Iave
#     if (Irecent > I0) {
#       TAC <- c1 * TACstar * (1 + ((Irecent - I0)/(Itarget - I0)))
#     }
#     else {
#       TAC <- c1 * TACstar * (Irecent/I0)^2
#     }
#     TAC <- MSEtool::TACfilter(TAC)
#     if (plot) {
#       if(index=="AddIndex"){
#         op <- par(no.readonly = TRUE)
#         on.exit(op)
#         par(mfrow = c(1, 2))
#         ylim <- range(c(Data@AddInd[x,ii, ], Itarget, I0))
#         plot(Data@Year, Data@AddInd[x,ii, ], type = "l", lwd = 2,
#              bty = "l", xlab = "Year", ylab = "Index",
#              ylim = ylim)
#         points(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
#                cex = 2, pch = 16, col = "blue")
#         text(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
#              cex = 1, "Irecent", pos = 3, col = "blue",
#              xpd = NA)
#         lines(Data@Year[ind3], rep(mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
#                                    length(ind3)), lty = 2, col = "orange")
#         text(mean(Data@Year[ind3]), mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
#              "Iave", col = "orange", pos = 1)
#         points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#         text(max(Data@Year), Itarget, cex = 1, "Itarget",
#              pos = 3, col = "green", xpd = NA)
#         points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#         text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#              col = "red", xpd = NA)
#         ylim <- range(c(C_dat, TACstar, TAC))
#         Years <- Data@Year[ind2]
#         if (max(Years) != max(Data@Year)) {
#           Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#         }
#         yrs <- length(Years) - length(C_dat)
#         Cdat <- c(C_dat, rep(NA, yrs))
#         plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#              lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                               Data@Units, ")"), ylim = ylim)
#         abline(v = max(Data@Year[ind2]), col = "gray",
#                lty = 3)
#         points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#                cex = 2, col = "orange", pch = 16)
#         text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#              "mean TAC*", pos = 2, xpd = NA, col = "orange")
#         boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#                 axes = FALSE)
#       }else{
#         op <- par(no.readonly = TRUE)
#         on.exit(op)
#         par(mfrow = c(1, 2))
#         ylim <- range(c(Data@Ind[x, ], Itarget, I0))
#         plot(Data@Year, Data@Ind[x, ], type = "l", lwd = 2,
#              bty = "l", xlab = "Year", ylab = "Index",
#              ylim = ylim)
#         points(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#                cex = 2, pch = 16, col = "blue")
#         text(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#              cex = 1, "Irecent", pos = 3, col = "blue",
#              xpd = NA)
#         lines(Data@Year[ind3], rep(mean(Data@Ind[x, ind3], na.rm = TRUE),
#                                    length(ind3)), lty = 2, col = "orange")
#         text(mean(Data@Year[ind3]), mean(Data@Ind[x, ind3], na.rm = TRUE),
#              "Iave", col = "orange", pos = 1)
#         points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#         text(max(Data@Year), Itarget, cex = 1, "Itarget",
#              pos = 3, col = "green", xpd = NA)
#         points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#         text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#              col = "red", xpd = NA)
#         ylim <- range(c(C_dat, TACstar, TAC))
#         Years <- Data@Year[ind2]
#         if (max(Years) != max(Data@Year)) {
#           Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#         }
#         yrs <- length(Years) - length(C_dat)
#         Cdat <- c(C_dat, rep(NA, yrs))
#         plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#              lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                               Data@Units, ")"), ylim = ylim)
#         abline(v = max(Data@Year[ind2]), col = "gray",
#                lty = 3)
#         points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#                cex = 2, col = "orange", pch = 16)
#         text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#              "mean TAC*", pos = 2, xpd = NA, col = "orange")
#         boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#                 axes = FALSE)
#       }
#
#     }# end if plot
#
#     list(TAC = TAC)
#   }
#
#
#   runItarget <- myItarget_(x, Data, reps, plot, yrsmth, xx, Imulti, index,ii, c1, c2)
#   Rec <- new("Rec")
#   Rec@TAC <- runItarget$TAC
#   Rec
# }
#
#
# class(myItarget_VS2)<-"MP"
#
#
#
# myItarget_VS1<-function (x, Data, reps = 100, plot = FALSE, yrsmth = 5, xx = 0,
#                          Imulti = 1.5,  index="AddIndex", ii=1, c1=0.65, c2=0.6) ### DEFAULT Imulti=1.5 | c1 = 0.5 | c2=0.8
# {
#
#   myItarget_<- function (x, Data, reps = 100, plot , yrsmth , xx,
#                          Imulti, index, ii, c1, c2)
#   {
#     ind <- (length(Data@Year) - (yrsmth - 1)):length(Data@Year)
#     ylast <- (Data@LHYear[1] - Data@Year[1]) + 1
#     ind2 <- ((ylast - (yrsmth - 1)):ylast)
#     ind3 <- ((ylast - (yrsmth * 2 - 1)):ylast)
#     C_dat <- Data@Cat[x, ind2]
#     TACstar <- (1 - xx) * MSEtool::trlnorm(reps, mean(C_dat,
#                                                       na.rm = TRUE), Data@CV_Cat[x, 1]/(yrsmth^0.5))
#     if(index=="AddIndex"){
#       Irecent <- mean(Data@AddInd[x,ii, ind], na.rm = TRUE)
#       Iave <- mean(Data@AddInd[x,ii, ind3], na.rm = TRUE)
#     }else{
#       Irecent <- mean(Data@Ind[x, ind], na.rm = TRUE)
#       Iave <- mean(Data@Ind[x, ind3], na.rm = TRUE)
#     }
#
#     Itarget <- Iave * Imulti
#     I0 <- c2 * Iave
#     if (Irecent > I0) {
#       TAC <- c1 * TACstar * (1 + ((Irecent - I0)/(Itarget - I0)))
#     }
#     else {
#       TAC <- c1 * TACstar * (Irecent/I0)^2
#     }
#     TAC <- MSEtool::TACfilter(TAC)
#     if (plot) {
#       if(index=="AddIndex"){
#         op <- par(no.readonly = TRUE)
#         on.exit(op)
#         par(mfrow = c(1, 2))
#         ylim <- range(c(Data@AddInd[x,ii, ], Itarget, I0))
#         plot(Data@Year, Data@AddInd[x,ii, ], type = "l", lwd = 2,
#              bty = "l", xlab = "Year", ylab = "Index",
#              ylim = ylim)
#         points(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
#                cex = 2, pch = 16, col = "blue")
#         text(max(Data@Year), mean(Data@AddInd[x,ii, ind], na.rm = TRUE),
#              cex = 1, "Irecent", pos = 3, col = "blue",
#              xpd = NA)
#         lines(Data@Year[ind3], rep(mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
#                                    length(ind3)), lty = 2, col = "orange")
#         text(mean(Data@Year[ind3]), mean(Data@AddInd[x,ii, ind3], na.rm = TRUE),
#              "Iave", col = "orange", pos = 1)
#         points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#         text(max(Data@Year), Itarget, cex = 1, "Itarget",
#              pos = 3, col = "green", xpd = NA)
#         points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#         text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#              col = "red", xpd = NA)
#         ylim <- range(c(C_dat, TACstar, TAC))
#         Years <- Data@Year[ind2]
#         if (max(Years) != max(Data@Year)) {
#           Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#         }
#         yrs <- length(Years) - length(C_dat)
#         Cdat <- c(C_dat, rep(NA, yrs))
#         plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#              lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                               Data@Units, ")"), ylim = ylim)
#         abline(v = max(Data@Year[ind2]), col = "gray",
#                lty = 3)
#         points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#                cex = 2, col = "orange", pch = 16)
#         text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#              "mean TAC*", pos = 2, xpd = NA, col = "orange")
#         boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#                 axes = FALSE)
#       }else{
#         op <- par(no.readonly = TRUE)
#         on.exit(op)
#         par(mfrow = c(1, 2))
#         ylim <- range(c(Data@Ind[x, ], Itarget, I0))
#         plot(Data@Year, Data@Ind[x, ], type = "l", lwd = 2,
#              bty = "l", xlab = "Year", ylab = "Index",
#              ylim = ylim)
#         points(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#                cex = 2, pch = 16, col = "blue")
#         text(max(Data@Year), mean(Data@Ind[x, ind], na.rm = TRUE),
#              cex = 1, "Irecent", pos = 3, col = "blue",
#              xpd = NA)
#         lines(Data@Year[ind3], rep(mean(Data@Ind[x, ind3], na.rm = TRUE),
#                                    length(ind3)), lty = 2, col = "orange")
#         text(mean(Data@Year[ind3]), mean(Data@Ind[x, ind3], na.rm = TRUE),
#              "Iave", col = "orange", pos = 1)
#         points(max(Data@Year), Itarget, cex = 2, pch = 16, col = "green")
#         text(max(Data@Year), Itarget, cex = 1, "Itarget",
#              pos = 3, col = "green", xpd = NA)
#         points(max(Data@Year), I0, cex = 2, pch = 16, col = "red")
#         text(max(Data@Year), I0, cex = 1, "I0", pos = 3,
#              col = "red", xpd = NA)
#         ylim <- range(c(C_dat, TACstar, TAC))
#         Years <- Data@Year[ind2]
#         if (max(Years) != max(Data@Year)) {
#           Years <- c(Years, (max(Data@Year[ind2]) + 1):max(Data@Year))
#         }
#         yrs <- length(Years) - length(C_dat)
#         Cdat <- c(C_dat, rep(NA, yrs))
#         plot(c(Years, max(Years) + 1), c(Cdat, NA), type = "l",
#              lwd = 2, bty = "l", xlab = "Year", ylab = paste0("Catch (",
#                                                               Data@Units, ")"), ylim = ylim)
#         abline(v = max(Data@Year[ind2]), col = "gray",
#                lty = 3)
#         points(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#                cex = 2, col = "orange", pch = 16)
#         text(max(Data@Year[ind2]), mean(TACstar, na.rm = TRUE),
#              "mean TAC*", pos = 2, xpd = NA, col = "orange")
#         boxplot(TAC, at = max(Years) + 1, add = TRUE, col = "gray",
#                 axes = FALSE)
#       }
#
#     }# end if plot
#
#     list(TAC = TAC)
#   }
#
#
#   runItarget <- myItarget_(x, Data, reps, plot, yrsmth, xx, Imulti, index,ii, c1, c2)
#   Rec <- new("Rec")
#   Rec@TAC <- runItarget$TAC
#   Rec
# }
#
#
# class(myItarget_VS1)<-"MP"

