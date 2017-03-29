# test the growth features of the fishmethods package

########################## set up workspace #######################################

library(fishmethods)
source("conleyte.R")
source("datefish.R")
source("sitefish.R")

# get a list of recaptured fish
leyte <- conleyte()
fish <- leyte %>% tbl("clownfish") %>% filter(!is.na(capid) | recap == "Y") %>% select(capid, recap, tagid, size, sample_id, anem_table_id) %>% collect()

# get the first capture of tag recaptured fish
tags <- leyte %>% tbl("clownfish") %>% filter(tagid %in% fish$tagid) %>% select(capid, recap, tagid, size, sample_id, anem_table_id) %>% collect()
tags$tagid <- as.character(tags$tagid)
fish <- rbind(fish, tags)

# attach dates
date <- datefish(fish$anem_table_id)
date$date <- as.Date(date$date)
fish <- left_join(fish, date, by = "anem_table_id")

# attach sites
site <- sitefish(fish$anem_table_id)
site$dive_table_id <- NULL # remove column so it is not duplicated in the join 
fish <- left_join(fish, site, by = "anem_table_id")

rm(date, site, tags, leyte)
fish$anem_table_id <- NULL
fish$dive_table_id <- NULL

# deal with empty value cells
# is.na(fish) <- "0" # this doesn't work, NAs stay NA, also using 0 doesn't work.
# fish$capid[1] <- "0" # that worked
fish$capid[is.na(fish$capid)] <- "0" 
fish$recap[is.na(fish$recap)] <- "0" 
fish$tagid[is.na(fish$tagid)] <- "0" 
fish$size[is.na(fish$size)] <- "0" 
fish$sample_id[is.na(fish$sample_id)] <- "0"
fish$name[is.na(fish$name)] <- "0"
fish$date[is.na(fish$date)] <- "1901-01-01"

# remove samples with missing sizes or dates because we can't calculate growth for those
fish$capid[which(fish$size == 0)] <- NA
fish$capid[which(fish$date == "1901-01-01")] <- NA
fish <- fish[!is.na(fish$capid), ]

# remove fish that were caught on the same date
datedif <- data.frame()
dates <- unique(fish$date) # create a list of all of the dates in the fish table to iterate through
for(i in 1:length(dates)){
    X <- fish[which(fish$date == dates[i]), ]
    Y <- X[X$capid != 0, ]
    Y <- Y %>% distinct(capid) # now we have a table of fish with capids that are not duplicated
    Z <- X[X$tagid != 0 & X$capid == 0, ]
    Z <- Z %>% distinct(tagid) # now we have another table of fish with tagids that are not duplicated
    datedif <- rbind(datedif, Y, Z)
}

rm(X, Y, Z, dates, i)

# populate an L1 and L2, and TAL column for geno recaptured fish
recapture <- data.frame()
X <- datedif[datedif$capid !=0, ] # create a list of all capids that are not 0
caps <- unique(X$capid)
for(i in 1:length(caps)){
  X <- datedif[which(datedif$capid == caps[i]), ]
  if (nrow(X) > 1){
    X$L1 <- min(X$size)
    X$L2 <- max(X$size)
    X$T1 <- min(X$date)
    X$T2 <- max(X$date)
    X$tal <- max(X$date) - min(X$date)
    recapture <- rbind(recapture, X[1,])
  }
}

rm(X, caps, i)

# add in samples that are tag recaptures
X <- datedif[datedif$tagid != 0 & datedif$capid == 0, ] # create a list of tag recaptures that are not in the capid table
tags <- unique(X$tagid)
for(i in 1:length(tags)){
  X <- datedif[which(datedif$tagid == tags[i]), ]
  if (nrow(X) > 1){
    X$L1 <- min(X$size)
    X$L2 <- max(X$size)
    X$T1 <- min(X$date)
    X$T2 <- max(X$date)
    X$tal <- max(X$date) - min(X$date)
    recapture <- rbind(recapture, X[1,])
  }
}

# remove duplicate rows
recapture <- dplyr::distinct(recapture)


# fix table formatting
recapture$L1 <- as.numeric(recapture$L1)
recapture$L2 <- as.numeric(recapture$L2)


# convert tal from days to fraction of year
recapture$tal <- recapture$tal/365

# K originally 0.27 (from fishbase), using admb output of 0.0379 - doesn't help 
grow <- growhamp(L1 = recapture$L1, L2 = recapture$L2, TAL = recapture$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))

write.csv(grow$results, file = paste(Sys.Date(), "grow_results.csv", sep = ""))

# now plot it 

# rename columns for growthTraject function
# names(recapture) <- c("capid", "recap", "tagid", "size", "sample_id", "date", "site", "lentag", "lenrec", "T1", "T2", "timelib")

# # 3/14/2017 I'm getting error messages that Error: is.numeric(lentag) && all(lentag > 0) is not TRUE even though both of those statements are TRUE if I run them, going to simplify the table to see if that helps
# recapture$capid <- NULL
# recapture$recap <- NULL
# recapture$tagid <- NULL
# recapture$size <- NULL
# recapture$sample_id <- NULL
# recapture$date <- NULL


growthTraject(0.19,97.5,lentag=recapture$L1, lenrec=recapture$L2,timelib=recapture$tal) # had to remove the elementary school fish in order to get this to work without the timelib is numeric and not >0

# growthTraject(grow$results[1,3], grow$results[1,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, , main = "Growth Trajectories and Fitted Curve", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm",xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)

# Error in seq.int(from, to, length.out = n) : 'to' must be finite
# In addition: Warning message:
#   In log(1 - lentag/Linf) : NaNs produced


growthTraject(grow$results[1,3], grow$results[1,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Faber Growth trajectories and fitted curve, K=1.13, Linf = 10.48")
# Error in plot.window(...) : need finite 'xlim' values
# In addition: Warning message:
#   In log(1 - lentag/Linf) : NaNs produced

# ### Because Faber has the smallest AIC, change Linf to 11.1 and it will plot...
# growthTraject(grow$results[1,3], 11.1, lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Faber Growth trajectories and fitted curve, K=1.13, Linf = 11.1")


growthTraject(grow$results[2,3], grow$results[2,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$timelib, main = "Kirkwood & Somers Growth trajectories and fitted curve, K=46.74, Linf=9.2")
# Error in plot.window(...) : need finite 'xlim' values
# In addition: Warning message:
#   In log(1 - lentag/Linf) : NaNs produced

growthTraject(grow$results[3,3], grow$results[3,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Kirkwood & Somers Growth with ME trajectories and fitted curve, K=0.759, Linf=11.097")

growthTraject(grow$results[4,3], grow$results[4,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Kirkwood & Somers Growth with ME & RLE trajectories and fitted curve, K=0.759, Linf=11.097")

growthTraject(grow$results[5,3], grow$results[5,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Sainsbury trajectories and fitted curve, K=24.95, Linf=9.81")
# Error in plot.window(...) : need finite 'xlim' values
# In addition: Warning message:
#   In log(1 - lentag/Linf) : NaNs produced

growthTraject(grow$results[6,3], grow$results[6,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Sainsbury with ME trajectories and fitted curve, K=1.38, Linf=18.21")

growthTraject(grow$results[7,3], grow$results[7,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Sainsbury with ME & RLE trajectories and fitted curve, K=1.38, Linf=18.21")


# By site -----------------------------------------------------------------


# examine growth rate by site
cab <- subset(recapture, name == "Cabatuan")
elem <- subset(recapture, name == "Elementary School")
haina <- subset(recapture, name == "Haina")
mag <- subset(recapture, name == "Magbangon")
pal <- subset(recapture, name == "Palanas")
rose <- subset(recapture, name == "Poroc Rose")
flower <- subset(recapture, name == "Poroc San Flower")
sitio <- subset(recapture, name == "Sitio Baybayon")
tamakin <- subset(recapture, name == "Tamakin Dacot")
visca <- subset(recapture, name == "Visca")
wang <- subset(recapture, name == "Wangag")


################################# Sitio Baybayon n= 43 #################################

sitiogrow <- growhamp(L1 = sitio$L1, L2 = sitio$L2, TAL = sitio$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))

write.csv(sitiogrow$results, file = paste(Sys.Date(), "sitiogrow_results.csv", sep = ""))

######## Sitio results - Faber and plain Kirkwood and somers have unrealistic Linf and a K that is much higher than the K reported on fishbase.  Adding ME and RLE help make numbers more realistic #################

############################# plots ########################
growthTraject(sitiogrow$results[3,3], sitiogrow$results[3,2], lentag = sitio$L1, lenrec = sitio$L2, timelib = sitio$tal, , main = "Sitio Kirkwood & Somers w/ ME", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# ,xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)

growthTraject(sitiogrow$results[4,3], sitiogrow$results[4,2], lentag = sitio$L1, lenrec = sitio$L2, timelib = sitio$tal, , main = "Sitio Kirkwood & Somers w/ ME & RLE", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
#, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)

growthTraject(sitiogrow$results[5,3], sitiogrow$results[5,2], lentag = sitio$L1, lenrec = sitio$L2, timelib = sitio$tal, , main = "Sitio Sainsbury", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
#, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)

growthTraject(sitiogrow$results[6,3], sitiogrow$results[6,2], lentag = sitio$L1, lenrec = sitio$L2, timelib = sitio$tal, , main = "Sitio Sainsbury w/ ME", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
#, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)

growthTraject(sitiogrow$results[7,3], sitiogrow$results[7,2], lentag = sitio$L1, lenrec = sitio$L2, timelib = sitio$tal, , main = "Sitio Sainsbury w/ ME & RLE", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
#, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)


######################### Wangag n = 38 #################################
wanggrow <- growhamp(L1 = wang$L1, L2 = wang$L2, TAL = wang$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))

#Error in optim(parms, sain, hessian = varcov[cnt], method = mm$method[index],  : non-finite finite-difference value [2]

# write.csv(wanggrow$results, file = paste(Sys.Date(), "wanggrow_results.csv", sep = ""))
# 
# ######## Wangag results - Error in optim(parms, sain, hessian = varcov[cnt], method = mm$method[index],  : non-finite finite-difference value [2]#################
# 
# ############################# plots ########################
# growthTraject(wanggrow$results[3,3], wanggrow$results[3,2], lentag = wang$L1, lenrec = wang$L2, timelib = wang$tal, , main = "Wangag Kirkwood & Somers w/ ME", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# # ,xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(wanggrow$results[4,3], wanggrow$results[4,2], lentag = wang$L1, lenrec = wang$L2, timelib = wang$tal, , main = "Wangag Kirkwood & Somers w/ ME & RLE", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(wanggrow$results[5,3], wanggrow$results[5,2], lentag = wang$L1, lenrec = wang$L2, timelib = wang$tal, , main = "Wangag Sainsbury", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(wanggrow$results[6,3], wanggrow$results[6,2], lentag = wang$L1, lenrec = wang$L2, timelib = wang$tal, , main = "Wangag Sainsbury w/ ME", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(wanggrow$results[7,3], wanggrow$results[7,2], lentag = wang$L1, lenrec = wang$L2, timelib = wang$tal, , main = "Wangag Sainsbury w/ ME & RLE", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)

#################### Palanas n = 36 ###############################
palgrow <- growhamp(L1 = pal$L1, L2 = pal$L2, TAL = pal$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))

#Error in optim(parms, sain, hessian = varcov[cnt], method = mm$method[index],  : non-finite finite-difference value [2]
# 
# write.csv(palgrow$results, file = paste(Sys.Date(), "palgrow_results.csv", sep = ""))
# 
# ######## Palanas results - Error in optim(parms, sain, hessian = varcov[cnt], method = mm$method[index],  : non-finite finite-difference value [2]#################
# 
# ############################# plots ########################
# growthTraject(palgrow$results[3,3], palgrow$results[3,2], lentag = pal$L1, lenrec = pal$L2, timelib = pal$tal, , main = "Palanas Kirkwood & Somers w/ ME", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# # ,xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# 
# growthTraject(palgrow$results[4,3], palgrow$results[4,2], lentag = pal$L1, lenrec = pal$L2, timelib = pal$tal, , main = "Palanas Kirkwood & Somers w/ ME & RLE", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(palgrow$results[5,3], palgrow$results[5,2], lentag = pal$L1, lenrec = pal$L2, timelib = pal$tal, , main = "Palanas Sainsbury", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(palgrow$results[6,3], palgrow$results[6,2], lentag = pal$L1, lenrec = pal$L2, timelib = pal$tal, , main = "Palanas Sainsbury w/ ME", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(palgrow$results[7,3], palgrow$results[7,2], lentag = pal$L1, lenrec = pal$L2, timelib = pal$tal, , main = "Palanas Sainsbury w/ ME & RLE", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)

################### Magbangon n = 16 ###########################

maggrow <- growhamp(L1 = mag$L1, L2 = mag$L2, TAL = mag$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))

#Error in optim(parms, sain, hessian = varcov[cnt], method = mm$method[index],  : non-finite finite-difference value [2]
# 
# write.csv(maggrow$results, file = paste(Sys.Date(), "maggrow_results.csv", sep = ""))
# 
# ######## Magbangon results - Error in optim(parms, sain, hessian = varcov[cnt], method = mm$method[index],  : non-finite finite-difference value [2]#################
# 
# ############################# plots ########################
# growthTraject(maggrow$results[3,3], maggrow$results[3,2], lentag = mag$L1, lenrec = mag$L2, timelib = mag$tal, , main = "Magbangon Kirkwood & Somers w/ ME", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# # ,xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# 
# growthTraject(maggrow$results[4,3], maggrow$results[4,2], lentag = mag$L1, lenrec = mag$L2, timelib = mag$tal, , main = "Magbangon Kirkwood & Somers w/ ME & RLE", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(maggrow$results[5,3], maggrow$results[5,2], lentag = mag$L1, lenrec = mag$L2, timelib = mag$tal, , main = "Magbangon Sainsbury", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(maggrow$results[6,3], maggrow$results[6,2], lentag = mag$L1, lenrec = mag$L2, timelib = mag$tal, , main = "Magbangon Sainsbury w/ ME", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)
# 
# growthTraject(maggrow$results[7,3], maggrow$results[7,2], lentag = mag$L1, lenrec = mag$L2, timelib = mag$tal, , main = "Magbangon Sainsbury w/ ME & RLE", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm")
# #, xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)




#######################Junk code###################################
# # make a list of the site subsets just made
# sites <- list(cab, flower, haina, mag, wang, rose, sitio, tamakin, visca, pal)
# for (j in 1:length(sites)){
#   recapture <- data.frame()
#   Y <- as.data.frame(sites[j])
#   for(i in 1:nrow(Y)){
#     X <- fish[which(fish$capid == Y$capid[i]), ]
#     X$L1 <- min(X$size)
#     X$L2 <- max(X$size)
#     X$tal <- max(X$date) - min(X$date)
#     recapture <- rbind(recapture, X[1,])
#   }
#   # remove duplicate rows
#   recapture <- dplyr::distinct(recapture)
#   
#   # convert tal from days to portions of year
#   recapture$tal <- recapture$tal/365
#   
#   growhamp(L1 = recapture$L1, L2 = recapture$L2, TAL = recapture$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
#     sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
#     sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10)) 
# }


#### CAN'T use the growth because don't have ages and can't use year caught as a proxy for age ###
# # try the regular growth with TAL as age...
# growth(size=fish$size, age = fish$age, Sinf=15.9,K=0.26,t0=-1)
# 
# # create a smaller number for age than year (12=1, 13=2, 14=3, 15=4, 16=5)
# fish$tal <- NA
# fish$tal[fish$age == 12] <- 1
# fish$tal[fish$age == 13] <- 2
# fish$tal[fish$age == 14] <- 3
# fish$tal[fish$age == 15] <- 4
# fish$tal[fish$age == 16] <- 5
# 
# # try growth again
# growth(size=fish$size, age = fish$tal, Sinf=15.9,K=0.26,t0=-0.73)





# # # try it out with their data
# ## Not run: 
# ## Models 1,2 and 3 below are models 1,2, and 4 in Table 4.17 of ##Quinn and Deriso 
# data(trout)
# growhamp(L1=trout$L1,L2=trout$L2,TAL=trout$dt,models=c(1,2,3),
#   method=c("Nelder-Mead","Nelder-Mead","L-BFGS-B"),
#   varcov=c(TRUE,TRUE,TRUE),
#   Linf=list(startLinf=650,lowerLinf=400,upperLinf=800),       
#   K=list(startK=0.30,lowerK=0.01,upperK=1),
#   sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
#   sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
#   sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))
# 
# ## End(Not run)
# 
# data(pinfish)
# growth(intype=1,unit=1,size=pinfish$sl,age=pinfish$age,
#   calctype=1,wgtby=1,error=1,Sinf=200,K=0.3,t0=-1)
# 
# # create an L1L2 file for BET-grow in admb
# lens <- recapture[ , c("L1", "L2")]
# 
# msg1 <- c("#init_int numLenShObs")
# msg2 <- nrow(lens)
# msg3 <- c("# init_matrix LenShDat(1,numLenShObs,1,2)")
# 
# # have to convert the data to a tab separated vector or else every line will have msg1, msg2, etc.
# L1 <- vector()
# for (i in 1:nrow(lens)){
#   L1[i] <- paste(lens$L1[i], lens$L2[i], sep = "  ")
# }
# 
# out <- c(msg1, msg2, msg3, L1)
# 
# write.table(out, file = paste("admb/",Sys.Date(), "L1L2.tsv", sep = ""), row.names=FALSE, quote=FALSE, col.names=FALSE) # won't let me use header=FALSE - use col.names instead of header
