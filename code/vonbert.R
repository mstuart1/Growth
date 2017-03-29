# test the growth features of the fishmethods package

########################## set up workspace #######################################

library(fishmethods)
library(readr)
library(stringr)
library(tidyverse)
source("../Phil_code/conleyte.R")
source("../Phil_code/dateanemobs.R")
source("../Phil_code/siteanem.R")

# get a list of recaptured fish ####
leyte <- conleyte()
fish <- leyte %>% tbl("clownfish") %>% 
  filter(!is.na(capid) | recap == "Y") %>% 
  select(capid, recap, tagid, size, sample_id, anem_table_id) %>% 
  collect()

# get the first capture of tag recaptured fish ####
tags <- leyte %>% tbl("clownfish") %>% 
  filter(tagid %in% fish$tagid) %>% 
  select(capid, recap, tagid, size, sample_id, anem_table_id) %>% 
  collect()

tags$tagid <- as.character(tags$tagid)
fish <- rbind(fish, tags)

# attach dates based on anem not fish because all fish don't have sample_ids ####
date <- dateanem(fish$anem_table_id)
date$date <- as.Date(date$date)
fish <- left_join(fish, date, by = "anem_table_id")

# attach sites ####
site <- siteanem(fish$anem_table_id)
site$dive_table_id <- NULL # remove column so it is not duplicated in the join 
fish <- left_join(fish, site, by = "anem_table_id")

rm(date, site, tags, leyte)

# deal with empty values - these can't be NA for the code below to work
fish$capid[is.na(fish$capid)] <- "0" 
fish$recap[is.na(fish$recap)] <- "0" 
fish$tagid[is.na(fish$tagid)] <- "0" 
fish$size[is.na(fish$size)] <- "0" 
fish$sample_id[is.na(fish$sample_id)] <- "0"
fish$date[is.na(fish$date)] <- "1901-01-01"

# remove samples with missing sizes or dates ####
# look at which fish do not have a size
fish[fish$size == "0",]

# need to remove the complete pair of the recapture event, even if one of the fish has a measurement
# currently only fish with capids have missing values, if there were some with tagids, those would also be changed to X and removed.

# make a list of capids to remove
X <- fish$capid[fish$size == "0"] 

fish$capid[fish$capid %in% X] <- "X"

# repeat for missing dates
fish[fish$date == "1901-01-01", ] # in this case has already been taken care of

# remove the samples with X values
fish <- fish[fish$capid != "X", ]

# remove fish that were caught on the same date and have capid ####
datedif <- data.frame() # empty data frame
dates <- unique(fish$date) # create a list of all of the dates in the fish table to iterate through
for(i in 1:length(dates)){
  # all of the fish that were caught on the same date
  X <- fish[which(fish$date == dates[i]), ] # which fish were captured on date i
  Y <- X %>% filter(capid != 0) # which of those fish have a capid
  Y <- Y %>% distinct(capid) # now we have a table of fish with capids that are not duplicated
  Z <- X %>% filter(tagid!=0 & capid == 0) # which of those fish don't have a capid but do have a tagid
  Z <- Z %>% distinct(tagid) # now we have another table of fish with tagids that are not duplicated
  datedif <- rbind(datedif, Y, Z)
}

fish <- datedif  
# clean up 
rm(X, Y, Z, dates, i, datedif)


rm(X, Y, Z, dates, i, datedif)

# create a table of recapture events ####

# start with geno recaptures
recap <- data.frame()
X <- fish[fish$capid !=0, ] # create a list of all capids that are not 0
caps <- unique(X$capid) # create a list of unique capids
for(i in 1:length(caps)){
  X <- fish[which(fish$capid == caps[i]), ] # get all of the fish for one capid
  X <- arrange(X, date) # make sure they are arranged in date order
  if(nrow(X) > 1){
    X$L1 <- X$size[1] # take the size in the first capture
    X$L2 <- X$size[nrow(X)] # the size in the last capture
    X$T1 <- X$date[1] # the first date of capture
    X$T2 <- X$date[nrow(X)] # the last date of capture
    X$growth <- as.numeric(X$size[nrow(X)]) - as.numeric(X$size[1]) # calculate the difference in size
    X$tal <- as.numeric(X$date[nrow(X)]) - as.numeric(X$date[1]) # calculate the time at large in days
    recap <- rbind(recap, X[1,]) # join to the recapture table
  }
}

rm(X, caps, i)

# add in samples that are tag recaptures
X <- fish[fish$tagid != 0 & fish$capid == 0, ] # create a list of tag recaptures that are not in the capid table
tags <- unique(X$tagid) # create a list of unique tagids
for(i in 1:length(tags)){
  X <- fish[which(fish$tagid == tags[i]), ] # for all of the fish with one tagid
  X <- arrange(X, date) # make sure they are in date order
  if (nrow(X) > 1){
    X$L1 <- X$size[1] # take the size in the first capture
    X$L2 <- X$size[nrow(X)] # the size in the last capture
    X$T1 <- X$date[1] # the first date of capture
    X$T2 <- X$date[nrow(X)] # the last date of capture
    X$growth <- as.numeric(X$size[nrow(X)]) - as.numeric(X$size[1]) # calculate the difference in size
    X$tal <- as.numeric(X$date[nrow(X)]) - as.numeric(X$date[1]) # calculate the time at large in days
    recap <- rbind(recap, X[1,])
  }
}

# remove duplicate rows
recap <- dplyr::distinct(recap)

# fix table formatting
recap$L1 <- as.numeric(recap$L1)
recap$L2 <- as.numeric(recap$L2)

# convert tal from days to fraction of year ####
recap$tal <- recap$tal/365

# remove fish that shrank (recap was smaller than first cap due to measurement error)
recap <- recap %>% 
  filter(growth > 0)


# use fish methods

# first run all of the models and see which is most realistic
# K originally 0.27 (from fishbase), using admb output of 0.0379 - doesn't help 
grow <- growhamp(L1 = recap$L1, L2 = recap$L2, TAL = recap$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))

write_csv(grow$results, path = str_c(Sys.Date(), "grow_results.csv", sep = ""))

# based on the above results, Faber and Kirkwood and Somers with ME were the most realistic, Sainsbury ight be the only one that will make a curve.
  # Nelder-Mead was the default method, tried all of the other ones, L-BFGS-B and SANN were the only two to produce different results but they also said (lengths of method and models do not match)
grow <- growhamp(L1 = recap$L1, L2 = recap$L2, TAL = recap$tal, method = "SANN", Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))


grow$results
# now plot it 
# Faber
growthTraject(grow$results[1,3], grow$results[1,2], lentag = recap$L1, lenrec = recap$L2, timelib = recap$tal, main = "Faber Growth trajectories", xlim = NULL)

# plot doesn't have a curve, doesn't look good.

# Kirkwood and Somers
growthTraject(grow$results[3,3], grow$results[3,2], lentag = recap$L1, lenrec = recap$L2, timelib = recap$tal, main = "Kirkwood & Somers Growth trajectories with ME and fitted curve")

# plot doesn't have a curve, doesn't look good.

# Sainsbury
growthTraject(grow$results[5,3], grow$results[5,2], lentag = recap$L1, lenrec = recap$L2, timelib = recap$tal, main = "Sainsbury Growth trajectories and fitted curve")

# only plot that is reasonable
