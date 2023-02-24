## CLEANED MSE PLOTTING AND ANALYSIS CODE ###

#---------------------------------------------------- LOAD PACKAGES -------------------------------------------------

library(openMSE)
library(vioplot)
# library(RColorBrewer)
# display.brewer.all()
# library(paletteer)

avail("PM")
source("C:/Users/cassidy.peterson/Documents/Github/SEFSCInterimAnalysis/RunMSE/SEFSC/fn/merge_MSE.R")



# ---------------------------------------------- Homemade functions ----------------------------------------------------
#make a function transparent
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  ## Save the color
  invisible(t.col)
}


# Deal with nonsensical ref points related to high (episodic) values (where SSB_SSBMSY == Inf, set SSBMSY == dynamic SSB0)
EpiMDeal<-function(data){
  proj_dynSSB0<-data$epiM@RefPoint$Dynamic_Unfished$SSB0[,40:89]
  for(i in 1:dim(data$epiM@SB_SBMSY)[2]){
    # print("before = "); print(summary(c(results@SB_SBMSY[,i,])))
    data$epiM@SB_SBMSY[,i,][which(data$epiM@SB_SBMSY[,i,]=="Inf")] <-
      data$epiM@SSB[which(data$epiM@SB_SBMSY[,i,]=="Inf")] /
      proj_dynSSB0[which(data$epiM@SB_SBMSY[,i,]=="Inf")]
    # print("after = "); print(summary(c(results@SB_SBMSY[,i,])))
  }
  return(data)
}

# Modify built-in Yield PM to calculate raw Yield
myYield<-function (MSEobj = NULL, Ref = 1, Yrs = NULL)
{
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Yield relative to Reference Yield (Years ",
                       Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Mean Relative Yield (Years ", Yrs[1],
                          "-", Yrs[2], ")")
  # RefYd <- array(MSEobj@OM$RefY, dim = dim(MSEobj@Catch[,, Yrs[1]:Yrs[2]]))
  PMobj@Stat <- MSEobj@Catch[, , Yrs[1]:Yrs[2]] #/RefYd
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(myYield)<-"PM"


# calculate cumulative yield by summing yield across each MP
SumYieldMP<-function(dataMSE){
  SumY<-apply(Yield(dataMSE)@Stat[,1,],1,sum)
  for(j in 2:dataMSE@nMPs){
    SumY<-cbind(SumY, apply(Yield(dataMSE)@Stat[,j,],1,sum) )
  } # end j for loop
  colnames(SumY)<-dataMSE@MPs
  return(SumY)
}# end SumYieldMP

# calculate cumulative yield by summing yield across each MP
SumMyYieldMP<-function(dataMSE){
  SumY<-apply(myYield(dataMSE)@Stat[,1,],1,sum)
  for(j in 2:dataMSE@nMPs){
    SumY<-cbind(SumY, apply(myYield(dataMSE)@Stat[,j,],1,sum) )
  } # end j for loop
  colnames(SumY)<-dataMSE@MPs
  return(SumY)
}# end SumYieldMP


## BUILD FUNCTIONS ##
# Get Results collated into one list (SP)
GetResults<-function(species1=species, sp1=sp, oScenarios=orderedScenarios){
  if(grepl(species1, pattern="Over")){ # get files of matching species name (correcting for Over results)
    files1<-list.files(set.file, pattern=species1)
  } else {
    files1<-grep(list.files(set.file, pattern=species1), pattern="Over", invert=TRUE, value=TRUE) #
  }

  temp_res<-list() # create empty list with sp title.
  for(f in 1:length(files1)){ # get results for each OM
    lab1 <- gsub("MSE_|\\..*", "", files1[f]); lab2<- gsub(species1, sp1, lab1); lab3<-gsub(paste0(sp1,"_"),"",lab2) # get rds label
    assign(lab2, readRDS(file.path(set.file,files1[f]))) # get results for each OM
    temp_res[[lab3]]<-get(lab2) # # save OM results in a single object titled sp
  } # end for loop
  temp_res<-temp_res[oScenarios]
  # assign(sp, temp_res)
  return(temp_res)
}


# COLLATE AAVY, terminal relative SSB ratio (last year and mean over last 10 years), terminal relative F ratio (last year and mean over last 10 years), terminal yield, cumulative yield across OMs (get SP_PMs)
# # remove Inf values with very large F/FMSY values ==100
CollatePMs <- function(dataN=sp,
                       stat=c('AAVY','trelSSB','treldSSB0','trelF','t10relSSB','t10reldSSB0',
                              't10relF','tyield','cyield', 'PNOF', 'P100'),
                       nested=FALSE){
  data<-get(dataN)
  MP_names<-data[[1]]@MPs
  returnlist<-list()
  returnlist_nest<-list()
  OM_names<-names(data)

  #AAVY
  if('AAVY' %in% stat){
    AAVYs<-AAVY(data[[1]])@Stat
    returnlist_nest[['AAVY']][[OM_names[1]]]<-AAVYs
    colnames(returnlist_nest[['AAVY']][[OM_names[1]]])<-MP_names

    for(i in 2:length(data)){
      AAVYs<-rbind(AAVYs, AAVY(data[[i]])@Stat)
      returnlist_nest[['AAVY']][[OM_names[i]]]<-AAVY(data[[i]])@Stat
      colnames(returnlist_nest[['AAVY']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(AAVYs)<-MP_names
    returnlist$AAVY<-AAVYs
  } # end AAVY


  #terminal relative SSB ratio
  if('trelSSB' %in% stat){
    trelSSB<-P100(data[[1]], Yrs=-1)@Stat
    returnlist_nest[['trelSSB']][[OM_names[1]]]<-trelSSB
    colnames(returnlist_nest[['trelSSB']][[OM_names[1]]])<-MP_names
    for(i in 2:length(data)){
      trelSSB<-rbind(trelSSB, P100(data[[i]], Yrs=-1)@Stat)
      returnlist_nest[['trelSSB']][[OM_names[i]]]<-P100(data[[i]], Yrs=-1)@Stat
      colnames(returnlist_nest[['trelSSB']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(trelSSB)<-MP_names
    returnlist$trelSSB<-trelSSB
  } # end trelSSB

  #terminal relative dSSB0 ratio
  if('treldSSB0' %in% stat){
    l1<-dim(data[[1]]@SSB[,1,])[2]
    l2<-dim(data[[1]]@RefPoint$Dynamic_Unfished$SSB0)[2]

    treldSSB0<-data[[1]]@SSB[,,l1] / data[[1]]@RefPoint$Dynamic_Unfished$SSB0[,l2]
    returnlist_nest[['treldSSB0']][[OM_names[1]]]<-treldSSB0
    colnames(returnlist_nest[['treldSSB0']][[OM_names[1]]])<-MP_names

    for(i in 2:length(data)){
      treldSSB0<-rbind(treldSSB0, (data[[i]]@SSB[,,l1] / data[[i]]@RefPoint$Dynamic_Unfished$SSB0[,l2]) )
      returnlist_nest[['treldSSB0']][[OM_names[i]]]<- (data[[i]]@SSB[,,l1] / data[[i]]@RefPoint$Dynamic_Unfished$SSB0[,l2])
      colnames(returnlist_nest[['treldSSB0']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(treldSSB0)<-MP_names
    returnlist$treldSSB0<-treldSSB0
  } # end treldSSB0


#terminal relative SSB ratio
  if('t10relSSB' %in% stat){
    t10relSSB<-apply(P100(data[[1]], Yrs=-1)@Stat, c(1,2), mean)
    returnlist_nest[['t10relSSB']][[OM_names[1]]]<-t10relSSB
    colnames(returnlist_nest[['t10relSSB']][[OM_names[1]]])<-MP_names

    for(i in 2:length(data)){
      t10relSSB<-rbind(t10relSSB, apply(P100(data[[i]], Yrs=-1)@Stat, c(1,2), mean))
      returnlist_nest[['t10relSSB']][[OM_names[i]]]<-apply(P100(data[[i]], Yrs=-1)@Stat, c(1,2), mean)
      colnames(returnlist_nest[['t10relSSB']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(t10relSSB)<-MP_names
    returnlist$t10relSSB<-t10relSSB
  } # end t10relSSB


  #terminal relative dSSB0 ratio
  if('t10reldSSB0' %in% stat){
    l1<-(dim(data[[1]]@SSB[,1,])[2]-9):dim(data[[1]]@SSB[,1,])[2]
    l2<-(dim(data[[1]]@RefPoint$Dynamic_Unfished$SSB0)[2]-9):dim(data[[1]]@RefPoint$Dynamic_Unfished$SSB0)[2]
    t10reldSSB0<-c()
    for(s in 1:length(data)){
      temp<-c()
      for(c in 1:dim(data[[1]]@SSB)[2]){
        temp<-cbind( temp, apply((data[[s]]@SSB[,c,l1] / data[[s]]@RefPoint$Dynamic_Unfished$SSB0[,l2]), 1, mean) )
    }# end cloop (where c is CMP)

    t10reldSSB0<-rbind(t10reldSSB0, temp)
    returnlist_nest[['t10reldSSB0']][[OM_names[s]]]<-t10reldSSB0
    colnames(returnlist_nest[['t10reldSSB0']][[OM_names[s]]])<-MP_names

    } # end sloop (where s in OM scenario)

    dim(t10reldSSB0)

    colnames(t10reldSSB0)<-MP_names
    returnlist$t10reldSSB0<-t10reldSSB0
  } # end t10reldSSB0


  #terminal F ratio
  if('trelF' %in% stat){
    trelF<-PNOF(data[[1]], Yrs=-1)@Stat
    trelF[which(trelF==Inf)]<-100 # remove Inf values with very large F/FMSY values ==100
    returnlist_nest[['trelF']][[OM_names[1]]]<-trelF
    colnames(returnlist_nest[['trelF']][[OM_names[1]]])<-MP_names

    for(i in 2:length(data)){
      yyy<-PNOF(data[[i]], Yrs=-1)@Stat
      yyy[which(yyy==Inf)]<-100 # remove Inf values with very large F/FMSY values ==100
      trelF<-rbind(trelF, yyy)
      returnlist_nest[['trelF']][[OM_names[i]]]<-yyy
      colnames(returnlist_nest[['trelF']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(trelF)<-MP_names
    returnlist$trelF<-trelF
  } # end trelF



  #terminal 10-year F ratio
  if('t10relF' %in% stat){
    trelF<-apply(PNOF(data[[1]], Yrs=-1)@Stat, c(1,2), mean)
    trelF[which(trelF==Inf)]<-100 # remove Inf values with very large F/FMSY values ==100
    returnlist_nest[['t10relF']][[OM_names[1]]]<-trelF
    colnames(returnlist_nest[['t10relF']][[OM_names[1]]])<-MP_names
    for(i in 2:length(data)){
      yyy<-apply(PNOF(data[[i]], Yrs=-1)@Stat, c(1,2), mean)
      yyy[which(yyy==Inf)]<-100 # remove Inf values with very large F/FMSY values ==100
      trelF<-rbind(trelF, yyy)
      returnlist_nest[['t10relF']][[OM_names[i]]]<-yyy
      colnames(returnlist_nest[['t10relF']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(trelF)<-MP_names
    returnlist$t10relF<-trelF
  } # end trelF


  # terminal yield
  if('tyield' %in% stat){
    tyield<-Yield(data[[1]], Yrs=-1)@Stat
    returnlist_nest[['tyield']][[OM_names[1]]]<-tyield
    colnames(returnlist_nest[['tyield']][[OM_names[1]]])<-MP_names
    for(i in 2:length(data)){
      tyield<-rbind(tyield, Yield(data[[i]], Yrs=-1)@Stat)
      returnlist_nest[['tyield']][[OM_names[i]]]<-Yield(data[[i]], Yrs=-1)@Stat
      colnames(returnlist_nest[['tyield']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(tyield)<-MP_names
    returnlist$tyield<-tyield
  } # end tyield


  # cumulative yield
  if('cyield' %in% stat){

    # build function to total yield for each iteration
    SumYieldMP<-function(dataMSE){
      SumY<-apply(Yield(dataMSE)@Stat[,1,],1,sum)
      for(j in 2:dataMSE@nMPs){
        SumY<-cbind(SumY, apply(Yield(dataMSE)@Stat[,j,],1,sum) )
      } # end j for loop
      return(SumY)
    }# end SumYieldMP

    cyield<-SumYieldMP(data[[1]])
    returnlist_nest[['cyield']][[OM_names[1]]]<-cyield
    colnames(returnlist_nest[['cyield']][[OM_names[1]]])<-MP_names

    for(i in 2:length(data)){
      cyield<-rbind(cyield, SumYieldMP(data[[i]]))
      returnlist_nest[['cyield']][[OM_names[i]]]<-SumYieldMP(data[[i]])
      colnames(returnlist_nest[['cyield']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(cyield)<-MP_names
    returnlist$cyield<-cyield
  } # end cyield



  #PNOF
  if('PNOF' %in% stat){
    PNOFs<-PNOF(data[[1]])@Prob
    returnlist_nest[['PNOF']][[OM_names[1]]]<-PNOFs
    colnames(returnlist_nest[['PNOF']][[OM_names[1]]])<-MP_names

    for(i in 2:length(data)){
      PNOFs<-rbind(PNOFs, PNOF(data[[i]])@Prob)
      returnlist_nest[['PNOF']][[OM_names[i]]]<-PNOF(data[[i]])@Prob
      colnames(returnlist_nest[['PNOF']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(PNOFs)<-MP_names
    returnlist$PNOF<-PNOFs
  } # end PNOF



  #P100
  if('P100' %in% stat){
    P100s<-P100(data[[1]])@Prob
    returnlist_nest[['P100']][[OM_names[1]]]<-P100s
    colnames(returnlist_nest[['P100']][[OM_names[1]]])<-MP_names

    for(i in 2:length(data)){
      P100s<-rbind(P100s, P100(data[[i]])@Prob)
      returnlist_nest[['P100']][[OM_names[i]]]<- P100(data[[i]])@Prob
      colnames(returnlist_nest[['P100']][[OM_names[i]]])<-MP_names
    } # end for loop

    colnames(P100s)<-MP_names
    returnlist$P100<-P100s
  } # end P100


  if(nested==FALSE){ return(returnlist) }
  if(nested==TRUE){ return(returnlist_nest) }
}

# Build function to reorder list of CMPs -- takes CollatePMs result and orders the MP columns (SP_PMs -> SP_PM)
Reorder_MPs<-function(PMs_obj, oMPs=orderedMPs, nested=FALSE){

  for(l in 1:length(PMs_obj)){

    if(nested==FALSE){ PMs_obj[[l]]<-PMs_obj[[l]][,oMPs] }

    if(nested==TRUE){
      for(oo in 1:length(PMs_obj[[l]])){
        PMs_obj[[l]][[oo]]<-PMs_obj[[l]][[oo]][,oMPs]
      }#end oo loop
    }#end nested==TRUE

  } # end l loop
  return(PMs_obj)
}

# Calculate OM-specific response RELATIVE to BASE (_s for subtraction, _d for division)
Relative_MP_Perf_s<-function(sp1=sp, stat=c('trelSSB','trelF','t10relSSB','t10relF','tyield','cyield', 'PNOF', 'P100'), oMPs=orderedMPs){

  data<-get(sp)           # get data (SP)
  MP_names<-data[[1]]@MPs # get MP_names
  returnlist<-list()      # create results list

  for(ss in stat){        # loop over stats

    returnlist[[ss]]<-list()       # create nested list structure
    b<-which(names(data)=="base")  # define base OM

    for(oms in names(data)[-b]){   # loop over all non-base OMs

      returnlist[[ss]][[oms]]<-data.frame()   # create a dataframe in nested list (PM >> OM)

      if(ss=='AAVY'){
        returnlist[[ss]][[oms]]<- AAVY(data[[oms]])@Stat - AAVY(data[[b]])@Stat
      }#AAVY -- doesn't make as much sense.
      if(ss=='trelSSB'){
        returnlist[[ss]][[oms]]<- P100(data[[oms]], Yrs=-1)@Stat - P100(data[[b]], Yrs=-1)@Stat
      }#trelSSB
      if(ss=='trelF'){
        returnlist[[ss]][[oms]]<- PNOF(data[[oms]], Yrs=-1)@Stat - PNOF(data[[b]], Yrs=-1)@Stat
      }#trelF
      if(ss=='t10relSSB'){
        returnlist[[ss]][[oms]]<- apply(P100(data[[oms]], Yrs=-10)@Stat, c(1,2), mean) - apply(P100(data[[b]], Yrs=-10)@Stat, c(1,2), mean)
      }#t10relSSB
      if(ss=='t10relF'){
        returnlist[[ss]][[oms]]<- apply(PNOF(data[[oms]], Yrs=-10)@Stat, c(1,2), mean) - apply(PNOF(data[[b]], Yrs=-10)@Stat, c(1,2), mean)
      }#t10relF
      if(ss=='tyield'){
        returnlist[[ss]][[oms]]<- Yield(data[[oms]], Yrs=-1)@Stat - Yield(data[[b]], Yrs=-1)@Stat
      }#tyield
      if(ss=='cyield'){
        # build function to total yield for each iteration
        SumYieldMP<-function(dataMSE){
          SumY<-apply(Yield(dataMSE)@Stat[,1,],1,sum)
          for(j in 2:dataMSE@nMPs){
            SumY<-cbind(SumY, apply(Yield(dataMSE)@Stat[,j,],1,sum) )
          } # end j for loop
          return(SumY)
        }# end SumYieldMP
        returnlist[[ss]][[oms]]<- SumYieldMP(data[[oms]]) - SumYieldMP(data[[b]])
      }#cyield
      if(ss=='PNOF'){
        returnlist[[ss]][[oms]]<- PNOF(data[[oms]])@Prob - PNOF(data[[b]])@Prob
      }#PNOF
      if(ss=='P100'){
        returnlist[[ss]][[oms]]<- P100(data[[oms]], Yrs=-10)@Prob - P100(data[[b]], Yrs=-10)@Prob
      }#P100

      colnames(returnlist[[ss]][[oms]])<-MP_names
      returnlist[[ss]][[oms]]<-returnlist[[ss]][[oms]][,oMPs]

    }# end for oms loop
  } # end ss loop

  return(returnlist)
}# end function

Relative_MP_Perf_d<-function(sp1=sp, stat=c('trelSSB','trelF','t10relSSB','t10relF','tyield','cyield', 'PNOF', 'P100'), oMPs=orderedMPs){

  data<-get(sp)           # get data (SP)
  MP_names<-data[[1]]@MPs # get MP_names
  returnlist<-list()      # create results list

  for(ss in stat){        # loop over stats

    returnlist[[ss]]<-list()       # create nested list structure
    b<-which(names(data)=="base")  # define base OM

    for(oms in names(data)[-b]){   # loop over all non-base OMs

      returnlist[[ss]][[oms]]<-data.frame()   # create a dataframe in nested list (PM >> OM)

      if(ss=='AAVY'){
        returnlist[[ss]][[oms]]<- AAVY(data[[oms]])@Stat / AAVY(data[[b]])@Stat
      }#AAVY -- doesn't make as much sense.
      if(ss=='trelSSB'){
        returnlist[[ss]][[oms]]<- P100(data[[oms]], Yrs=-1)@Stat / P100(data[[b]], Yrs=-1)@Stat
      }#trelSSB
      if(ss=='trelF'){
        returnlist[[ss]][[oms]]<- PNOF(data[[oms]], Yrs=-1)@Stat / PNOF(data[[b]], Yrs=-1)@Stat
      }#trelF
      if(ss=='t10relSSB'){
        returnlist[[ss]][[oms]]<- apply(P100(data[[oms]], Yrs=-10)@Stat, c(1,2), mean) / apply(P100(data[[b]], Yrs=-10)@Stat, c(1,2), mean)
      }#t10relSSB
      if(ss=='t10relF'){
        returnlist[[ss]][[oms]]<- apply(PNOF(data[[oms]], Yrs=-10)@Stat, c(1,2), mean) / apply(PNOF(data[[b]], Yrs=-10)@Stat, c(1,2), mean)
      }#t10relF
      if(ss=='tyield'){
        returnlist[[ss]][[oms]]<- Yield(data[[oms]], Yrs=-1)@Stat / Yield(data[[b]], Yrs=-1)@Stat
      }#tyield
      if(ss=='cyield'){
        # build function to total yield for each iteration
        SumYieldMP<-function(dataMSE){
          SumY<-apply(Yield(dataMSE)@Stat[,1,],1,sum)
          for(j in 2:dataMSE@nMPs){
            SumY<-cbind(SumY, apply(Yield(dataMSE)@Stat[,j,],1,sum) )
          } # end j for loop
          return(SumY)
        }# end SumYieldMP
        returnlist[[ss]][[oms]]<- SumYieldMP(data[[oms]]) / SumYieldMP(data[[b]])
      }#cyield
      if(ss=='PNOF'){
        returnlist[[ss]][[oms]]<- PNOF(data[[oms]])@Prob / PNOF(data[[b]])@Prob
      }#PNOF
      if(ss=='P100'){
        returnlist[[ss]][[oms]]<- P100(data[[oms]], Yrs=-10)@Prob / P100(data[[b]], Yrs=-10)@Prob
      }#P100

      colnames(returnlist[[ss]][[oms]])<-MP_names
      returnlist[[ss]][[oms]]<-returnlist[[ss]][[oms]][,oMPs]

    }# end for oms loop
  } # end ss loop

  return(returnlist)
}# end function

Relative_MP_Perf_rd<-function(sp1=sp, stat=c('trelSSB','treldSSB0','trelF','t10relSSB','t10reldSSB0','t10relF','tyield','cyield','PNOF','P100','P90'), oMPs=orderedMPs){

  data<-get(sp)           # get data (SP)
  MP_names<-data[[1]]@MPs # get MP_names
  returnlist<-list()      # create results list

  for(ss in stat){        # loop over stats

    returnlist[[ss]]<-list()       # create nested list structure
    b<-which(names(data)=="base")  # define base OM

    for(oms in names(data)[-b]){   # loop over all non-base OMs

      returnlist[[ss]][[oms]]<-data.frame()   # create a dataframe in nested list (PM >> OM)

      if(ss=='AAVY'){
        returnlist[[ss]][[oms]]<- (AAVY(data[[oms]])@Stat - AAVY(data[[b]])@Stat) / AAVY(data[[b]])@Stat
      }#AAVY -- doesn't make as much sense.
      if(ss=='trelSSB'){
        returnlist[[ss]][[oms]]<- (P100(data[[oms]], Yrs=-1)@Stat - P100(data[[b]], Yrs=-1)@Stat) /P100(data[[b]], Yrs=-1)@Stat
      }#trelSSB

      if(ss=='treldSSB0'){
          l1<-dim(data[[1]]@SSB[,1,])[2]
          l2<-dim(data[[1]]@RefPoint$Dynamic_Unfished$SSB0)[2]
          b1<- (data[[b]]@SSB[,,l1] / data[[b]]@RefPoint$Dynamic_Unfished$SSB0[,l2])
          o1<- (data[[oms]]@SSB[,,l1] / data[[oms]]@RefPoint$Dynamic_Unfished$SSB0[,l2])
          returnlist[[ss]][[oms]]<-(o1 - b1)/(b1)
      }#treldSSB0
      if(ss=='trelF'){
        returnlist[[ss]][[oms]]<- (PNOF(data[[oms]], Yrs=-1)@Stat - PNOF(data[[b]], Yrs=-1)@Stat) /PNOF(data[[b]], Yrs=-1)@Stat
      }#trelF
      if(ss=='t10relSSB'){
        returnlist[[ss]][[oms]]<- (apply(P100(data[[oms]], Yrs=-10)@Stat, c(1,2), mean) - apply(P100(data[[b]], Yrs=-10)@Stat, c(1,2), mean)) / apply(P100(data[[b]], Yrs=-10)@Stat, c(1,2), mean)
      }#t10relSSB

      if(ss=='t10reldSSB0'){
        l1<-(dim(data[[1]]@SSB[,1,])[2]-9):dim(data[[1]]@SSB[,1,])[2]
        l2<-(dim(data[[1]]@RefPoint$Dynamic_Unfished$SSB0)[2]-9):dim(data[[1]]@RefPoint$Dynamic_Unfished$SSB0)[2]
        # Calc t10reldSSB0_b
        t10reldSSB0_b<-c()
        for(c in 1:dim(data[[b]]@SSB)[2]){
          t10reldSSB0_b<-cbind( t10reldSSB0_b,
                                apply((data[[b]]@SSB[,c,l1] /
                                         data[[b]]@RefPoint$Dynamic_Unfished$SSB0[,l2]), 1, mean) )
        }# end cloop (where c is CMP)

        # stats for oms
          temp<-c()
          for(c in 1:dim(data[[oms]]@SSB)[2]){
            temp<-cbind( temp, apply((data[[oms]]@SSB[,c,l1] / data[[oms]]@RefPoint$Dynamic_Unfished$SSB0[,l2]), 1, mean) )
          }# end cloop (where c is CMP)
          returnlist[[ss]][[oms]]<-  (temp - t10reldSSB0_b) / (t10reldSSB0_b)

      }#t10reldSSB0
###################################################### END HERE - 2/22/23 #################################



      if(ss=='t10relF'){
        yyy<-PNOF(data[[oms]], Yrs=-10)@Stat # get observed values for EpiM
        yyy[which(yyy==Inf)]<-100            # remove Inf values with very large F/FMSY values ==100
        xxx<-PNOF(data[[b]], Yrs=-10)@Stat   # get observed values for Base
        xxx[which(xxx==Inf)]<-100            # remove Inf values with very large F/FMSY values ==100
        returnlist[[ss]][[oms]]<- (apply(yyy, c(1,2), mean) - apply(xxx, c(1,2), mean)) / apply(xxx, c(1,2), mean)
      }#t10relF
      if(ss=='tyield'){
        returnlist[[ss]][[oms]]<- (Yield(data[[oms]], Yrs=-1)@Stat - Yield(data[[b]], Yrs=-1)@Stat) / Yield(data[[b]], Yrs=-1)@Stat
      }#tyield
      if(ss=='cyield'){
        # build function to total yield for each iteration
        SumYieldMP<-function(dataMSE){
          SumY<-apply(Yield(dataMSE)@Stat[,1,],1,sum)
          for(j in 2:dataMSE@nMPs){
            SumY<-cbind(SumY, apply(Yield(dataMSE)@Stat[,j,],1,sum) )
          } # end j for loop
          return(SumY)
        }# end SumYieldMP
        returnlist[[ss]][[oms]]<- (SumYieldMP(data[[oms]]) - SumYieldMP(data[[b]])) / SumYieldMP(data[[b]])
      }#cyield
      if(ss=='PNOF'){
        returnlist[[ss]][[oms]]<- ((PNOF(data[[oms]])@Prob+0.001) - (PNOF(data[[b]])@Prob+0.001)) / (PNOF(data[[b]])@Prob+0.001)
      }#PNOF ## WITH CORRECTION SO NO INFs
      if(ss=='P100'){
        returnlist[[ss]][[oms]]<- ((P100(data[[oms]], Yrs=-10)@Prob+0.01) - (P100(data[[b]], Yrs=-10)@Prob+0.01)) / (P100(data[[b]], Yrs=-10)@Prob+0.01)
      }#P100## WITH CORRECTION SO NO INFs
      if(ss=='P90'){
        returnlist[[ss]][[oms]]<- ((P100(data[[oms]], Ref=0.9, Yrs=-10)@Prob+0.01) - (P100(data[[b]], Ref=0.9, Yrs=-10)@Prob+0.01)) / (P100(data[[b]], Ref=0.9, Yrs=-10)@Prob+0.01)
      }#P100## WITH CORRECTION SO NO INFs

      colnames(returnlist[[ss]][[oms]])<-MP_names
      returnlist[[ss]][[oms]]<-returnlist[[ss]][[oms]][,oMPs]

    }# end for oms loop
  } # end ss loop

  return(returnlist)
}# end function


#### PLOTTING FUNCTIONS ####
### trajectories ###
# par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
## one plot each -- new color
Plot_SSBtraj_MSY<-function( fsh, scenarios=NULL, save.png=F,
                        oMPs=orderedMPs, oScenarios=NULL,
                        colsR=MP_R_col, namesR=MP_namesR_leg,
                        subset=NULL){

  data<-get(fsh)
  if(is.null(scenarios)){
    if(!is.null(oScenarios)){
      scenarios<-names(data)[oScenarios]
    } else {
      scenarios<-names(data)
    } # end if-else ; define scenarios
  } # end get scenarios

  for(res in scenarios){
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[oMPs])
    if(!is.null(subset)){
      MP_namesR<-MP_namesR[subset]
      namesR<-MP_namesR_leg[subset]}


    ## SSB/SSBMSY
    if(save.png==T){
      png(
        filename = paste0("Plots/", fsh,"_",res, "_SSBSSBMSY.png"),
        type = "cairo",
        units = "mm",
        width = 300,
        height = 225,
        pointsize = 24,
        res = 300
      )
      par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    } # end save.png

    max_y<-0
    for(ii in 1:dim(results@SB_SBMSY)[2]){
      max_y<-max(max_y, apply(results@SB_SBMSY[,ii,], 2, median) )
    }
    plot(apply(results@SB_SBMSY[,1,], 2, median), type='l', ylim=c(0, max(max_y*1.1, 1.1)), lwd=2,
         ylab=expression("SSB / SSB"['MSY']), xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@SB_SBMSY[,i,], 2, median), type='l', lwd=2, lty=iorder, col=colsR[iorder])
    }
    if(save.png==T) legend("bottom", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=5, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1.2)

    if(save.png==T){
      dev.off()
    } # end save.png

  } # end for res

  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  legend("center", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=2, cex=0.75)

} # end function



Plot_SSBtraj_dSSB0<-function( fsh, scenarios=NULL, save.png=F,
                             oMPs=orderedMPs, oScenarios=NULL,
                             colsR=MP_R_col, namesR=MP_namesR_leg,
                             subset=NULL, ylims=c(0,1.1)){

  data<-get(fsh)
  if(is.null(scenarios)){
    if(!is.null(oScenarios)){
      scenarios<-names(data)[oScenarios]
    } else {
      scenarios<-names(data)
    } # end if-else ; define scenarios
  } # end get scenarios

  for(res in scenarios){
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[oMPs])
    if(!is.null(subset)){
      MP_namesR<-MP_namesR[subset]
      namesR<-MP_namesR_leg[subset]}


    ## SSB/SSBMSY
    if(save.png==T){
      png(
        filename = paste0("Plots/", fsh,"_",res, "_SSBSSBMSY.png"),
        type = "cairo",
        units = "mm",
        width = 300,
        height = 225,
        pointsize = 24,
        res = 300
      )
      par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    } # end save.png


    # SSBtmp<-cbind(results@SSB_hist, results@SSB[,1,]) / results@RefPoint$Dynamic_Unfished$SSB0
    lngth<-(dim(results@SSB_hist)[2]+1):dim(results@RefPoint$Dynamic_Unfished$SSB0)[2]
    SSBtmp<-results@SSB[,1,] / results@RefPoint$Dynamic_Unfished$SSB0[,lngth]
    SSBSSB0<-apply(SSBtmp, 2, median)

    plot(SSBSSB0, type='l', ylim=ylims, lwd=2,
         ylab=expression("SSB / dSSB"['0']), xlab="Projected Years")

    abline(h=1)
    abline(h=0.5, lty=2)

    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      # SSBSSB0<-apply(cbind(results@SSB_hist, results@SSB[,i,]) /
      #                  results@RefPoint$Dynamic_Unfished$SSB0, 2, median)
      SSBSSB0<-apply(results@SSB[,i,] /
                       results@RefPoint$Dynamic_Unfished$SSB0[,lngth], 2, median)
      lines(SSBSSB0, type='l', lwd=2, lty=iorder, col=colsR[iorder])
    }
    if(save.png==T) legend("bottom", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=5, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1.2)

    if(save.png==T){
      dev.off()
    } # end save.png

  } # end for res

  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  legend("center", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=2, cex=0.75)

} # end function

Plot_SSBtraj_rawSSB<-function( fsh, scenarios=NULL, save.png=F,
                             oMPs=orderedMPs, oScenarios=NULL,
                             colsR=MP_R_col, namesR=MP_namesR_leg,
                             subset=NULL){

  data<-get(fsh)
  if(is.null(scenarios)){
    if(!is.null(oScenarios)){
      scenarios<-names(data)[oScenarios]
    } else {
      scenarios<-names(data)
    } # end if-else ; define scenarios
  } # end get scenarios

  for(res in scenarios){
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[oMPs])
    if(!is.null(subset)){
      MP_namesR<-MP_namesR[subset]
      namesR<-MP_namesR_leg[subset]}


    ## SSB/SSBMSY
    if(save.png==T){
      png(
        filename = paste0("Plots/", fsh,"_",res, "_SSBSSBMSY.png"),
        type = "cairo",
        units = "mm",
        width = 300,
        height = 225,
        pointsize = 24,
        res = 300
      )
      par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    } # end save.png

    max_y<-0
    for(ii in 1:dim(results@SSB)[2]){
      max_y<-max(max_y, apply(results@SSB[,ii,], 2, median) )
    }

    plot(apply(results@SSB[,1,], 2, median), type='l', ylim=c(0, max_y*0.95), lwd=2,
         ylab="SSB" , xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@SSB[,i,], 2, median), type='l', lwd=2, lty=iorder, col=colsR[iorder])
    }
    if(save.png==T) legend("bottom", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=5, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1.2)

    if(save.png==T){
      dev.off()
    } # end save.png

  } # end for res

  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  legend("center", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=2, cex=0.75)

} # end function


Plot_Catchtraj<-function( fsh, scenarios=NULL, save.png=F,
                          oMPs=orderedMPs, oScenarios=NULL,
                          colsR=MP_R_col, namesR=MP_namesR_leg){

  data<-get(fsh)
  if(is.null(scenarios)){
    if(!is.null(oScenarios)){
      scenarios<-names(data)[oScenarios]
    } else {
      scenarios<-names(data)
    } # end if-else ; define scenarios
  } # end get scenarios

  for(res in scenarios){
    results<-get(res,data)

    MP_names<-data[[1]]@MPs
    MP_namesR<-c(MP_names[oMPs])


    ## SSB/SSBMSY
    if(save.png==T){
      png(
        filename = paste0("Plots/", fsh,"_",res, "_Catch.png"),
        type = "cairo",
        units = "mm",
        width = 300,
        height = 225,
        pointsize = 24,
        res = 300
      )
      par(mfrow=c(1,1), mar=c(2.6, 2.6, 0.6, 0.6), mgp=c(1.3, 0.25, 0), tck=-0.01)
    } # end save.png

    max_y<-0
    for(ii in 1:dim(results@Catch)[2]){
      max_y<-max(max_y, apply(results@Catch[,ii,], 2, median) )
    }
    plot(apply(results@Catch[,1,], 2, median), type='l', ylim=c(0, max(max_y*1.1, 1.1)), lwd=2, ylab="Catch", xlab="Projected Years")
    abline(h=1)
    for(iname in MP_namesR[2:length(MP_namesR)]){
      i = which(results@MPs==iname)
      iorder<-which(MP_namesR==iname)
      lines(apply(results@Catch[,i,], 2, median), type='l', lwd=2, lty=iorder, col=colsR[iorder])
    }
    if(save.png==T) legend("bottom", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=5, cex=0.75)
    mtext( paste0(fsh, " ", res), side=3, line=-1.2)

    if(save.png==T){
      dev.off()
    } # end save.png

  } # end for res

  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  legend("center", namesR, lwd=2, lty=1:length(namesR), col=colsR, bty='n', ncol=2, cex=0.75)

} # end function


## VIOLIN PLOTS (by OM scenario) ##
myPlot_Violin<-function(SPP_nest, stat=c("AAVY","trelSSB","treldSSB0", "trelF","t10relSSB",
                                         "t10reldSSB0","t10relF", "tyield", "cyield", "PNOF", "P100" ),
                        ylims=c(NULL), ylimsEpiM=c(NULL), MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                        mf=c(4,3), xlab.cex=0.79, refline1=1, refline2=NULL){
  par(mfrow=mf)
  data<-SPP_nest[[stat]]
  for(ii in names(data)){
    if(ii=='epiM'){
      ylims1=ylimsEpiM
      if(is.null(ylimsEpiM) & !is.null(ylims)){ylims1=ylims}
    }#end if epiM
    if(ii!='epiM'){ylims1=ylims}
    vioplot(data[[ii]], col=MPcol, names=NA, ylim=ylims1, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=refline1); if(!is.null(refline2)){abline(h=refline2, lty=2)}

    mtext(ii, 3, line=-1.2)
    mtext(stat, 2, line=1.1)
  }# end for loop
}

## NOTE YIELD IS CALCULATED RELATIVE TO REFERENCE YIELD IF FISHERY WERE BEING EXPLOITED AT FMSY
Plot_my_tyield<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                         mf=c(4,3), xlab.cex=0.79, oMPs=orderedMPs){
  par(mfrow=mf)
  for(ii in 1:length(names(SPP))){
    vioplot(myYield(SPP[[ii]], Yrs=-1)@Stat[,oMPs], col=MPcol, names=NA, ylim=ylims, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("tYield", 2, line=1.1)
  }# end for loop
}
Plot_my_cyield<-function(SPP, ylims=c(NULL), refline=NULL, MPnam=MP_namesR_abbrev, MPcol=MP_R_col,
                         oMPs=orderedMPs, mf=c(4,3), xlab.cex=0.79){
  par(mfrow=mf)
  dat<-lapply(SPP, SumMyYieldMP)
  for(ii in 1:length(names(SPP))){
    vioplot(dat[[ii]][,oMPs], col=MPcol, names=NA, ylim=ylims, axes=F)
    axis(2)
    axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
    box(); abline(h=refline, lty=2)
    mtext(names(SPP)[ii], 3, line=-1.2)
    mtext("tYield", 2, line=1.1)
  }# end for loop
}




# cumulative violin plots (across all OMs)
PlotCumPM<-function(sp, title=NULL,
                    Pstat=c('AAVY','trelSSB','treldSSB0','trelF','t10relSSB','t10reldSSB0',
                            't10relF','tyield','cyield','PNOF','P100'),
                    ylims=c(NULL), refline=NULL, MPnam=MP_namesR_abbrev, MPcol=MP_R_col, xlab.cex=0.9){
  dat<-get(paste0(sp,"_PM"))
  datStat<-get(Pstat, dat)

  vioplot(datStat, col=MPcol, names=NA, ylim=ylims, axes=F)
  axis(2)
  axis(1, at=1:length(MPnam), labels=MPnam, cex.axis=xlab.cex)
  box()
  abline(h=refline, lty=2)
  if(is.null(title)) title<-sp
  mtext(title, 3, line=-1.2)
  mtext(Pstat, 2, line=1.1)
}


## violin plots (for PMs relative to base) "trelSSB"   "trelF"     "t10relSSB" "t10relF"   "tyield"    "cyield"
PlotRelPM<-function(sp, calc='rd', title=NULL,
                    Pstat=c('AAVY','trelSSB','treldSSB0','trelF','t10relSSB','t10reldSSB0',
                            't10relF','tyield','cyield','PNOF','P100','P90'),
                    ylims=c(NULL), baserefline=NULL, refline=NULL, MPnam=MP_namesR_abbrev,
                    MPcol=MP_R_col, mf=c(4,3), oMPs=orderedMPs, minylim=NULL, maxylim=NULL,
                    xlab.cex=0.79){
  # par(cex.axis=1)
  dat<-get(paste(sp,'RelPM',calc, sep="_"))
  datStat<-get(Pstat, dat)
  par(mfrow=mf, oma=c(0,0,2.2,0))

  ## get Base Results
  BaseRes<-get(paste0(sp,"_PM"))[[Pstat]]

  # Plot BaseRes
  ylims1=c(minylim[1],maxylim[1])    # get ylims
  # boxplot(BaseRes, col=MPcol, names=MPnam, ylim=ylims1)
  boxplot(BaseRes, col=MPcol, names=MPnam, ylim=ylims1, axes=F)
  axis(2)
  axis(1, at=1:dim(BaseRes)[2], labels=MPnam, cex.axis=xlab.cex)
  # axis(1, at=1:dim(BaseRes)[2], labels=F)
  # text(x=1:dim(BaseRes)[2]-0.1, y=par("usr")[3]-0.25, xpd=NA, labels=MPnam, srt=35, cex=xlab.cex)
  box()
  abline(h=baserefline)
  mtext("Base Results", 3, line=-1.1)
  mtext(Pstat, 2, line=1.1)

  # plot differences for each
  for(ss in names(datStat)){

    if(is.null(ylims)){                              # get ylims
      if(!is.null(minylim) && !is.null(maxylim)){
        ylims1<-c(minylim[which(names(datStat)==ss)+1], maxylim[which(names(datStat)==ss)+1])
      } else{ylims<-NULL} # end nested if-else
    }else{ ylims1<-ylims} # end outer if-else

    # boxplot(get(ss,datStat), col=MPcol, names=MPnam, ylim=ylims1, na.rm=T)
    boxplot(get(ss,datStat), col=MPcol, names=MPnam, ylim=ylims1, na.rm=T, axes=F)
    axis(2)
    axis(1, at=1:dim(BaseRes)[2], labels=MPnam, cex.axis=xlab.cex)
    # axis(1, at=1:dim(BaseRes)[2], labels=F)
    # text(x=1:dim(BaseRes)[2]-0.1, y=par("usr")[3]-0.25, xpd=NA, labels=MPnam, srt=35, cex=xlab.cex)
    box()
    mtext("Difference", 2, line=1)
    mtext(ss, 3, line=-1.1)
    abline(h=refline)
  }# end scenario / OM loop
  if(is.null(title)) title<-sp
  mtext(title, 3, line=1, outer=T)
  mtext(Pstat, 3, line=0, outer=T)
}


### TradeOff Plot ###
TOff_Plot<- function(SPP_PM, PMx, PMy, lbound=0.1, ubound=0.9, errbars=TRUE, mpnames=MP_namesR_leg , mpcols=MP_R_col){

  loquant<-function(x, lbound=0.1){
    return( quantile(x,probs=lbound) ) ## NOTE default 10 & 90th quantiles on trade-off plots
  }
  upquant<-function(x, ubound=0.9){
    return( quantile(x,probs=ubound) )
  }
  xx<-get(PMx, SPP_PM)
  yy<-get(PMy, SPP_PM)
  xx1<-apply(xx,2,median)
  yy1<-apply(yy,2,median)

  if(errbars==T){
    xx_lo<-apply(xx, 2, loquant)
    xx_hi<-apply(xx, 2, upquant)
    yy_lo<-apply(yy, 2, loquant)
    yy_hi<-apply(yy, 2, upquant)

    ylims<-c( min(yy_lo)-(0.05*min(yy_lo)), max(yy_hi)+(0.025*max(yy_hi)) )
    xlims<-c( min(xx_lo)-(0.05*min(xx_lo)), max(xx_hi)+(0.025*max(xx_hi)) )
  }
  if(errbars==F){
    ylims<-c( (min(yy1)-0.025*min(yy1)),
              (max(yy1)+0.02*max(yy1)) )
    xlims<-c( (min(xx1)-0.02*min(xx1)),
              (max(xx1)+0.025*max(xx1)) )
  }

  # xx_lower<-apply(xx,1,quantile(probs=0.1))
  plot(xx1, yy1, pch=16, col=mpcols,
       ylim=ylims,
       xlim=xlims,
       ylab=PMy, xlab=PMx)
  if(errbars==T){
    for(ii in 1:dim(xx)[2]){
      arrows(x0=xx_lo[ii], y0=yy1[ii], x1=xx_hi[ii], y1=yy1[ii], code=3, angle=90, length=0, col=mpcols[ii], lwd=2)
      arrows(x0=xx1[ii], y0=yy_lo[ii], x1=xx1[ii], y1=yy_hi[ii], code=3, angle=90, length=0, col=mpcols[ii], lwd=2)
    }# end for ii loop
    text( (xx1 + 0.1*xx1),
         (yy1 - 0.1*yy1), labels=mpnames, col=mpcols)
  } #end if errbars==T

  if(errbars==F){
    text((xx1 + 0.02*xx1),
       (yy1 - 0.02*yy1), labels=mpnames, col=mpcols)
  } # end if errbars==F

}

## WORMPLOT ##
myWorm<-function(sp, OM="base", MPs=NULL,                 ### NOTE: MPs names should match sp$OM@MPs names.
                 metric=c("SB_SBMSY","F_FMSY","AddInd", "Catch"),
                 nworms=4, iters=NULL, seed=NULL , byMP=FALSE, ... ){
  dat<-get(OM, get(sp))
  if(is.null(seed)) seed<-8675309
  set.seed(seed)
  if(is.null(iters)) iters<-sample(1:dat@nsim, nworms, replace=FALSE)


  if(metric=="SB_SBMSY" ){ datmet<-dat@SB_SBMSY}
  if(metric=="F_FMSY"){ datmet<-dat@F_FMSY}
  if(metric=="Catch"){ datmet<-dat@Catch}

  if(metric=="SB_SBMSY" | metric=="F_FMSY" | metric=="Catch"){

    if(byMP==F){
      for(i in 1:nworms){
        datplot<-datmet[iters[i], , ]

        plot(datplot[which(dat@MPs==MPs[1]),], type='l', lty=1, col=1, lwd=2, #col=which(MPs==m)
             ylim=c(min(datplot)-0.1*(min(datplot)), max(datplot)+0.1*(max(datplot))),
             ylab=metric, xlab="Projected Year" ); abline(h=1)
        for(m in 2:length(MPs) ){
          lines(datplot[which(dat@MPs==MPs[m]),], lty=m,col=m ,lwd=2)
        } # end m loop

      } # end i for loop
      legend("topright",MPs,lty=1:length(MPs), col=1:length(MPs), bty='n')
    } # end if byMP==F



    if(byMP==T){
      # separate by MP
      for(m in MPs){
        datplot<-datmet[iters, which(dat@MPs==m), ]
        plot(datplot[1,], type='l', lty=1, col=1, lwd=2, #col=which(MPs==m)
             ylim=c(min(datplot)-0.1*(min(datplot)), max(datplot)+0.1*(max(datplot))),
             ylab=metric, xlab="Projected Year" ); abline(h=1); mtext(m, 3, line=-1.2)
        for(i in 2:nworms){
          lines(datplot[i,], lty=i,col=i ,lwd=2)
        } # end i loop

      } # end m for loop
    } # if byMP==T



  } # if metric == SBSBMSY, FFMSY, Catch



  ### IF metric=="AddInd" UNDER CONSTRUCTION
  # if(metric=="AddInd"){
  #   datmet<-c()
  #   for(m in MPs){
  #     datmet<-rbind(datmet, get(m, dat@PPD)@AddInd[iters,1,])
  #   }# end for m loop
  # }# end if metric=="AddInd"

  return(list(seed=seed, iters=iters))
} # end function


## TRADEOFF PLOT FOR DIFFERENCE RESULTS
CollateDifferenceResults<-function(sp, calc, Pstat, stat){
  ddat<-get(paste0(sp,'_RelPM_', calc))
  Pddat<-ddat[[Pstat]]

  RP_rel<-array(unlist(Pddat), dim=c(dim(Pddat[[1]]), length(Pddat)))
  res<-apply(RP_rel, 2, stat)
  res1<-matrix(res, nrow=1)
  colnames(res1) <- colnames(Pddat[[1]])
  return(res1)
} # potential Pstat == [1] "trelSSB"   "trelF"     "t10relSSB" "t10relF"   "tyield"    "cyield"

Toff_Difference_Plot<-function(sp, calc='rd', Pstaty='trelSSB',Pstatx='cyield', stat=median, mpcols=MP_R_col[-1]){
  yy1<-CollateDifferenceResults(sp, calc, Pstat=Pstaty, stat)
  xx1<-CollateDifferenceResults(sp, calc, Pstat=Pstatx, stat)
  ylims<-c( (min(yy1)-0.025*min(yy1)),
            (max(yy1)+0.02*max(yy1)) )
  xlims<-c( (min(xx1)-0.02*min(xx1)),
            (max(xx1)+0.025*max(xx1)) )
  plot( xx1,yy1,
        pch=16, col=mpcols,
        xlab=Pstatx, ylab=Pstaty, ylim=ylims, xlim=xlims)
  text( (xx1 + 0.1*xx1),
        (yy1 - 0.1*yy1), labels=MP_namesR_leg, col=mpcols)
  abline(h=0); abline(v=0)

}






#### -------------------------------------------------- GET DATA ------------------------------------------------------ #####


##### INPUTS ######
# BSB<-loadRDS("MSE_obj/MSE_BlackSeaBass_base.rds")
set.file<-"MSE_obj/"
# species<-"RedPorgy"; sp<-"RP"
# species<-"VermilionSnapper"; sp<-"VS"
species<-"BlackSeaBass"; sp<-"BSB"
# species<-"RedPorgy_Over"; sp<-"RP_O"
# species<-"VermilionSnapper_Over"; sp<-"VS_O"
# species<-"BlackSeaBass_Over"; sp<-"BSB_O"
## DATA AND PLOTTING INPUTS

# MPs_user_BSB <- c("SCA_1", "pMP_5","pMP_10" ,
#                   "GB_target_BSB", "GB_target_BSB2",
#                   "myICI_BSB", "myIratio_BSB",
#                   "myIT10_BSB", "myItarget_BSB" ,
#                   "GB_slope_BSB","GB_slope_BSB1","GB_slope_BSB2",
#                   "myIslope_BSB","myIslope_BSB2"
# )
# MPs_user_RP <- c("SCA_1", "pMP_5", "pMP_10",
#                  "GB_target_RP", "GB_target_RP2",
#                  "myICI_RP", "myICI_RP2", "myIratio_RP",
#                  "myIT10_RP", "myItarget_RP",
#                  "GB_slope_RP","GB_slope_RP2",
#                  "myIslope_RP", "myIslope_RP2"
# )
# MPs_user_VS <- c("SCA_1", "pMP_5", "pMP_10",
#                  "GB_target_VS", "GB_target_VS2",
#                  "myICI_VS", "myIratio_VS",
#                  "myIT10_VS", "myItarget_VS", "myItarget_VS2",
#                  "GB_slope_VS", "GB_slope_VS2",
#                  "myIslope_VS", "myIslope_VS2"
# )

abbrev=TRUE # true to select best performing MP configurations for each species. false to show all MP results.

orderedMPs<- c(1, 15:16, 2:14) #c(1, 10:11, 2:9, 12:13) #c(1, 9:10, 2:8)
orderedScenarios<-c(3, 5,6,4,1,2,7,10,11,8,9) #c(3,5:6,4,1:2,7:11)
scenarios<-c("base","recdev_hi", "recdev_lo", "epiM", "age0M_hi", "age0M_lo",
             "recns", "uobs_hi", "uobs_lo", "refbias_hi",  "refbias_lo")
# get(sp)$base@MPs
if(sp=="VS" | sp=="VS_O") {
  if(abbrev==TRUE){
    # VS: GB_targ_VS2 | myItarget_VS2? | GB_slope_VS | GB_Islope2
    orderedMPs<-c(1,15:16,2:3,5:8,10:11,14)
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","ICI","Irat","IT10","Itarg",
                     "GBslope","Islope")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt",
                        "ICI","Ira","I10","Ita","GBs","Isl")
  }#end if abbrev==TRUE

  if(abbrev==FALSE){
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","GBtarg2","ICI","Irat","IT10","Itarg","Itarg2",
                     "GBslope","GBslope2","Islope","Islope2")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
                        "ICI","Ira","I10","Ita","It2","GBs","Gs2","Isl","Is2")
  }#end if abbrev==False
} # end if sp==VS

if(sp=="RP" | sp=="RP_O") {
  if(abbrev==FALSE){
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","GBtarg2","ICI","ICI2","Irat","IT10","Itarg",
                     "GBslope","GBslope2","Islope","Islope2")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
                        "ICI","IC2","Ira","I10","Ita","GBs","Gs2","Isl","Is2")
  }#end abbrev=F
  if(abbrev==TRUE){
    # RP: GB_targ_RP2 | myICI_RP2 | GB_slope_RP2 | myIslope_RP2
    orderedMPs<- c(1, 15:16, 2:3, 5, 7:10, 12, 14)
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","ICI","Irat","IT10","Itarg",
                     "GBslope","Islope")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt",
                        "ICI","Ira","I10","Ita","GBs","Isl")
  }#end abbrev=T
}# end RP

if(sp=="BSB" | sp=="BSB_O") {
  if(abbrev==FALSE){
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","GBtarg2","ICI","Irat","IT10","Itarg",
                     "GBslope","GBslope1","GBslope2","Islope","Islope2")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt","Gt2",
                        "ICI","Ira","I10","Ita","GBs","Gs1","Gs2","Isl","Is2")
  }# end abbrev=F
  if(abbrev==TRUE){
    orderedMPs<- c(1, 15:16, 2:4, 6:10, 13)
    # BSB: GB_targ_BSB | GB_slope_BSB | myIslope_BSB
    MP_namesR_leg<-c("SCA1","SCA5_c","SCA10_c","SCA5_p","SCA10_p",
                     "GBtarg","ICI","Irat","IT10","Itarg",
                     "GBslope","Islope")
    MP_namesR_abbrev<-c("S1","5c","10c","5p","10p", "GBt",
                        "ICI","Ira","I10","Ita","GBs","Isl")
  }# end abbrev=T

  } #end BSB


if(abbrev==FALSE){
  MP_R_col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue','cadetblue','cadetblue1','cadetblue3', 'lightseagreen','mediumseagreen', 'aquamarine','aquamarine3')
}
if(abbrev==TRUE){
  MP_R_col=c('grey30','gray40','gray50','gray60','gray70','deepskyblue4','deepskyblue3','deepskyblue','lightskyblue1','lightskyblue', 'lightseagreen','cadetblue','cadetblue1')
}

par.args<-list(mar=c(2.4, 2.4, 0.2, 0.2), mgp=c(1.1, 0.25, 0), tck=-0.01, cex.axis=1)
par(par.args)




### Run FUNCTIONS for input species #####
assign(sp, GetResults(species1=species,sp1=sp)) # save GetResults with sp | SP

# correct results for EpiM SSB nonsensical results
sptemp<-get(sp)                       # save SP as sptemp
summary(sptemp$epiM@SB_SBMSY)         # see nonsensical sptemp results
assign(sp, EpiMDeal(sptemp))          # deal with nonsensical results -- resave as SP
summary(get(sp)$epiM@SB_SBMSY)        # see updated results

# Get performance metrics and reorder
assign(paste(sp,"PMs",sep='_'), CollatePMs(sp)) #SP_PMs -- UNORDERED
assign(paste(sp, "PM", sep="_"), Reorder_MPs(get(paste0(sp,"_PMs"))) ) #SP_PM ## REORDERED PERF METRICS
assign(paste(sp, "PMs_nest", sep="_"), CollatePMs(sp, nested=TRUE) ) #SP_PMs_nested -- UNORDERED ##
assign(paste(sp, "PM_nest", sep="_"), Reorder_MPs(get(paste0(sp,"_PMs_nest")), nested=TRUE) ) #SP_PM ## REORDERED PERF METRICS REORDERED PERF METRICS

# Get performance metrics relative to base OM case
# assign(paste(sp,"RelPM_s", sep='_'), Relative_MP_Perf_s(sp)) #SP_RelPM_s
# assign(paste(sp,"RelPM_d", sep='_'), Relative_MP_Perf_d(sp)) #SP_RelPM_d
assign(paste(sp,"RelPM_rd", sep='_'), Relative_MP_Perf_rd(sp)) #SP_RelPM_rd







# --------------------------------------------------- PLOTTING -------------------------------------------------

### Plot median Trajectories
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_MSY(fsh=sp)
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_dSSB0(fsh=sp)
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_rawSSB(fsh=sp)

par(mfrow=c(4,3), par.args)
Plot_Catchtraj(fsh=sp)

### Plot subset median Trajectories
par(mfrow=c(4,3), par.args)
Plot_SSBtraj_dSSB0(fsh=sp, subset=c(1,6:12),#subset=c(1,8:13),
             colsR=c('black','deepskyblue','deeppink','darkolivegreen3','darkorchid','darkorange','steelblue', 'orchid'))
                     # ,'blue','pink','green','orchid','cyan'))
get(sp)$base@MPs[orderedMPs]
Plot_SSBtraj_dSSB0(fsh=sp, subset=c(1,6:7),#subset=c(1,8:13),
             colsR=c('black','deepskyblue','deeppink'))
# BSB: GB_targ_BSB | GB_slope_BSB | myIslope_BSB
# RP: GB_targ_RP2 | myICI_RP2 | GB_slope_RP2 | myIslope_RP2
# RP_O: GB_targ_RP | myICI_RP (doesn't really matter) | myIslope_RP (doesn't really matter)
# VS: GB_targ_VS2 | myItarget_VS2? | GB_slope_VS | GB_Islope2
par(mfrow=c(4,3), par.args)
Plot_SSBtraj(fsh=sp, subset=c(1:5),
             colsR=c('black','deepskyblue','steelblue','deeppink','darkorange'))

### plot violin plots
stat=c("AAVY","trelSSB","treldSSB0", "trelF","t10relSSB",
       "t10reldSSB0","t10relF", "tyield", "cyield", "PNOF", "P100" )
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='AAVY')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='trelSSB', ylimsEpiM=c(0,5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='treldSSB0', ylims=c(0,1.1), refline2=0.5)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10relSSB', ylimsEpiM=c(0,5))
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10reldSSB0', ylims=c(0,1.1), refline2=0.5)
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='trelF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='t10relF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='tyield')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='PNOF')
myPlot_Violin(get(paste0(sp,"_PM_nest")), stat='P100')



Plot_trelSSB(SPP=spec) # terminal year relSSB
Plot_treldSSB0(SPP=spec, ylims=c(0,1.1))
Plot_trawSSB(SPP=spec)
Plot_trelF(SPP=spec)
Plot_t10relSSB(SPP=spec) # terminal 10 years mean relSSB
# Plot_t10reldSSB10(SPP=spec) # terminal 10 years mean relSSB
Plot_t10relF(SPP=spec)
Plot_PNOF(SPP=spec, refline=0.5, ylims=c(0,1.2))
Plot_P100(SPP=spec, refline=0.5, ylims=c(0,1.2))
Plot_P100(SPP=spec, rref=0.9, refline=0.5, ylims=c(0,1.2))
Plot_AAVY(SPP=spec, refline=0.30, ylims=c(0,1.5))
Plot_AAVY(SPP=spec, refline=0.30)





### cumulative violins / boxplots
par(mfrow=c(1,1))
PlotCumPM(sp, Pstat="AAVY", refline=c(0.3), title=species, ylims=c(0,1)) # for BSB_O ylims=c(0,5) |for VS ylims=c(0,5)
PlotCumPM(sp, Pstat="PNOF", refline=c(0.5), title=species)
PlotCumPM(sp, Pstat="P100", refline=c(0.5), title=species)
PlotCumPM(sp, Pstat="trelSSB", ylim=c(0,10), refline=c(1), title=species)
PlotCumPM(sp, Pstat="treldSSB0", ylim=c(0,1.1), refline=c(1), title=species)
PlotCumPM(sp, Pstat="t10reldSSB0", ylim=c(0,1.1), refline=c(1), title=species)
PlotCumPM(sp, Pstat="cyield",  refline=c(50), title=species)



### Relative violins
# | Pstat=c('AAVY','trelSSB','trelF','t10relSSB','t10relF','tyield','cyield'),


par(mfrow=c(4,3), mar=c(2.2, 2.2, 0.2, 0.2), mgp=c(1.2, 0.25, 0), tck=-0.01)

# treldSSB0
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 300, -0.5, 1, -0.0, 30, 10, 3, 35, 4, 5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, -0.5, 1, -0.0, 2.5, 4, 3.5, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 8, 0.75, 1, 2.5, 3, 2, 1, 1.5, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='treldSSB0',
            baserefline=1, refline=0,
            maxylim=c(3.5, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}



#t10reldSSB0
if(sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 100, -0.5, 1, -0.0, 15, 3, 2, 35, 4, 4), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 2000, -0.7, 5, -0.0, 30, 6, 10, 50, 8, 1000), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 8, -0.3, 2, 0.15, 2, 4, 3.5, 4, 4, 5), #VS
            minylim=c(0, rep(-1,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB' | sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10reldSSB0',
            baserefline=1, refline=0,
            maxylim=c(1.1, 8, 0.25, 2, 2, 3, 2.0, 1, 2, 1, 3), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}

# trelSSB
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 600, -0.5, 1, -0.0, 30, 10, 3, 35, 4, 5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 10, -0.5, 1, -0.0, 2.5, 4, 3.5, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='trelSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 8, 0.75, 1, 2.5, 3, 2, 1, 1.5, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}



#t10relSSB
if(sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 2000, -0.5, 2, -0.0, 30, 6, 3, 35, 8, 10), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 2000, -0.7, 5, -0.0, 30, 6, 10, 50, 8, 1000), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 8, -0.3, 2, 0.15, 2, 4, 3.5, 4, 4, 5), #VS
            minylim=c(0, rep(-1,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB' | sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relSSB',
            baserefline=1, refline=0,
            maxylim=c(3.5, 8, 0.25, 2, 2, 3, 2.0, 1, 2, 1, 3), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}



#trelF
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelF',
            baserefline=1, refline=0,
            maxylim=c(10, 5, 50, 50, 150, 2.5, 3, 50, 1.5, 10, 5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS' | sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelF',
            baserefline=1, refline=0,
            maxylim=c(3.5, 1.5, 15, 2, 15, 1.5, 4, 20, 10, 20, 15), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='trelF',
            baserefline=1, refline=0,
            maxylim=c(2.25, 10, 6, 10, 20, 5, 3, 5, 2, 3, 5), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='trelF',
            baserefline=1, refline=0,
            maxylim=c(2.25, 10, 4, 4, 15, 5, 2, 5, 2, 2, 5), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}



#t10relF
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',         ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            baserefline=1, refline=0,
            maxylim=c(10, 3, 50, 150, 150, 1.5, 3, 50, 5, 10, 5), # RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}

if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',           ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            maxylim=c(3.5, 1.5, 15, 2, 15, 1, 4, 20, 10, 20, 15), #VS
            baserefline=1, refline=0,
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',           ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            baserefline=1, refline=0,
            maxylim=c(3, 1.5, 7.5, 2, 7.5, 0.5, 2, 10, 12, 3, 10), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}

if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',           ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            baserefline=1, refline=0,
            maxylim=c(2.25, 7, 4, 50, 13.5, 3, 2, 4, 1, 3, 3), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='t10relF',           ### NOTE THAT relF is arbitrarily set to 100 where F/FMSY == INF
            baserefline=1, refline=0,
            maxylim=c(2.25, 7, 2, 50, 13.5, 3, 2, 4, 1, 1.5, 3), #BSB
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}




# cyield
if(sp=='RP' | sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(80, 0.5, 10, 1.5,6, 1, 4, 3, 2, 4, 2), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(100, 0, 3.5,    1, 1.5, 0.1, 3,   1,  1, 2, 1.5), #VS
            minylim=c(0, -0.75, 0, -0.5,  0, -0.4, -1, -1, -1, -1, -1) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(80, 0, 2.5, 1, .75, 0.1, 1.25,   1,  1, 1, 1.5), #VS
            minylim=c(0, -0.75, 0, -0.5,  0, -0.4, -1, -1, -1, -1, -1) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(100, 0.2, 2.5, 1.5, 5, 0.1, 1.5, 0.5, 0.5, 0.3, 0.3), #BSB
            minylim=c(0, -1, -0.75, -1,  -0.25, -1, -1, -.75, -0.5, -0.5, -1) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='cyield',
            baserefline=1, refline=0,
            maxylim=c(100, 0.2, 2.5, 1.5, 5, 0.1, 1.5, 0.5, 0.5, 0.3, 0.3), #BSB
            minylim=c(0, -0.9, -0.5, -1,  -0.25, -0.75, -0.75, -.4, -0.75, -0.5, -1) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}



# P100 (last 10 years)
if(sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.1, 0.1, 0.1, 30, 2, 3, 35, 2.5, 2.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 100, 0.1, 0.1, 0.1, 25, 1, 1, 35, 2, 0.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 7.5, -0.75, 1, -0.0, 2, 1, 1, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 20, -0, 5, -0.0, 10, 2, 5, 10, 5, 10), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='P100',
            baserefline=1, refline=0,
            # maxylim=c(1.1, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
            maxylim=c(1.1, 100, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}


# P90 (last 10 years)
if(sp=='RP' | sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='P90',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.1, 0.1, 0.1, 30, 0.5, 3, 35, 2.5, 2.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='P90',
            baserefline=1, refline=0,
            maxylim=c(1.1, 7.5, -0.75, 1, -0.0, 2, 1, 1, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='P90',
            baserefline=1, refline=0,
            maxylim=c(1.1, 7.5, -0, 2, -0.0, 10, 1, 5, 10, 5, 10), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='P90',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 3), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='P90', ################################################## ERROR ####
            baserefline=1, refline=0,
            maxylim=c(1.1, 100, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}


# PNOF (last 10 years)
if(sp=='RP'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.1, 0.1, 0.1, 30, 2, 3, 35, 2.5, 2.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='RP_O'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 100, 0.1, 0.1, 0.1, 25, 1, 1, 35, 2, 0.5), #RP
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 7.5, -0.75, 1, -0.0, 2, 1, 1, 5, 4, 5), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='VS_O'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.2, 5, 0.2, 7.5, 2, 5, 7.5, 4, 10), #VS
            minylim=c(0, rep(-1.05,10)) ) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OFF
}
if(sp=='BSB'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            maxylim=c(1.1, 10, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}
if(sp=='BSB_O'){
  PlotRelPM(sp, calc='rd', Pstat='PNOF',
            baserefline=1, refline=0,
            # maxylim=c(1.1, 10, 0.5, 1, 2, 3, 2, 1, 2, 1.5, 3), #BSB
            maxylim=c(1.1, 100, 0.5, 1.5, 5, 4.5, 1.5, 1, 2, 1, 5), #BSB
            minylim=c(0, rep(-1.05,10))) ## PLEASE NOTE THAT OUTLIERS HAVE BEEN CUT OF
}




### Tradeoff plot
spp_pm<-get(paste(sp, "PM", sep="_"))
par(mfrow=c(1,2))
# TOff_Plot(SPP_PM=spp_pm, PMy='trelSSB', PMx='cyield')
# TOff_Plot(SPP_PM=spp_pm, PMy='trelSSB', PMx='cyield', errbars=F); abline(h=1)
# TOff_Plot(SPP_PM=spp_pm, PMy='trelSSB', PMx='AAVY', errbars=F); abline(h=1)
# TOff_Plot(SPP_PM=spp_pm, PMy='P100', PMx='cyield', errbars=F); abline(h=1)
TOff_Plot(SPP_PM=spp_pm, PMy='PNOF', PMx='cyield', errbars=F); abline(h=1)
TOff_Plot(SPP_PM=spp_pm, PMy='P100', PMx='AAVY', errbars=F); abline(h=1)



### TRADEOFF PLOT FOR DIFFERENCE RESULTS
par(mfrow=c(1,1))
Toff_Difference_Plot(sp, calc='rd',Pstatx='cyield',Pstaty='trelSSB', stat=median)


### wormplot
nms<-get(sp)$base@MPs; nms
if(sp=='') mpnms<-nms[c(1,6,7)]
if(sp=='') mpnms<-nms[c(1,6,7)]
if(sp=='') mpnms<-nms[c(1,6,7)]
names(get(sp))

# par(mfrow=c(1,4))
# myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY")
par(mfrow=c(3,3), oma=c(0,0,1.2,0))
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", nworms=9)
mtext(species, 3, outer=TRUE, line=-0.3)
par(mfrow=c(3,3), oma=c(0,0,1.2,0))
myWorm(sp, OM="base", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="recdev_lo", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
myWorm(sp, OM="epiM", MPs=mpnms, metric="SB_SBMSY", byMP=T, nworms=5)
mtext(species, 3, outer=TRUE, line=-0.3)
