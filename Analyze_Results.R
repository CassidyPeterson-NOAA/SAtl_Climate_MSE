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
quart1<-function(y, type="1st Qu."){
  return(summary(c(y))[type])
}
quart3<-function(y, type="3rd Qu."){
  return(summary(c(y))[type])
}

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




# -------------------------------------------------- GET DATA ------------------------------------------------------
set.file<-"MSE_obj/"
files1<-list.files(set.file, pattern="BlackSeaBass")

list.files(set.file, pattern="Over")

species<-"BlackSeaBass"; sp<-"BSB"
GetResults<-function(species=species, sp=sp){
  if(grepl(species, pattern="Over")){ # get files of matching species name (correcting for Over results)
    files1<-list.files(set.file, pattern=species)
  } else {
    files1<-grep(list.files(set.file, pattern=species), pattern="Over", invert=TRUE, value=TRUE) #
  }

  temp_res<-list() # create empty list with sp title.
  for(f in 1:length(files1)){ # get results for each OM
    lab1 <- gsub("MSE_|\\..*", "", files1[f]); lab2<- gsub(species, sp, lab1); lab3<-gsub(paste0(sp,"_"),"",lab2) # get rds label
    assign(lab2, readRDS(file.path(set.file,files1[f]))) # get results for each OM
    temp_res[lab3]<-get(lab2) # # save OM results in a single object titled sp
  } # end for loop
  # assign(sp, temp_res)
  return(temp_res)
}

assign(GetResults(species=species,sp=sp), sp) # save GetResults with sp

# correct results for EpiM SSB nonsensical results
sptemp<-get(sp)
summary(sptemp$epiM@SB_SBMSY)
assign(sp, EpiMDeal(sptemp))
summary(get(sp)$epiM@SB_SBMSY)



############## ----------------------------------------------------------------- END HERE 11/23
scenarios<-c("base","age0M_hi","age0M_lo","epiM","recdev_hi","recdev_lo",
             "recns","refbias_hi","refbias_lo","uobs_hi","uobs_lo")


