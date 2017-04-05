# create a data file to feed into admb
library(readr)
library(stringr)
library(tidyverse)
source("../Phil_code/conleyte.R")
source("../Phil_code/dateanemobs.R") # where x is table$anem_table_id

# get a list of recaptured fish ####
leyte <- conleyte()

fish <- leyte %>% tbl("clownfish") %>% 
  # find fish that either have a capid or were scanned and already had a tag
  filter(!is.na(capid) | recap == "Y") %>% 
  select(capid, recap, tagid, size, sample_id, anem_table_id) %>% 
  collect()

# for fish that were tagged get the first capture of tag recaptured fish (was not a recapture event) ####
tags <- leyte %>% tbl("clownfish") %>% 
  # find fish that match the tagids listed in the fish table
  filter(tagid %in% fish$tagid) %>% 
  select(capid, recap, tagid, size, sample_id, anem_table_id) %>% 
  collect()

# change the format of the tagid column from num to character
tags$tagid <- as.character(tags$tagid)

# paste the tags rows under the fish rows
fish <- rbind(fish, tags)

# remove dupliate rows
fish <- distinct(fish)

# attach dates of anem obs (not all have sample_id so can't use datefishcap) ####
date <- dateanem(fish$anem_table_id)
date$date <- as.Date(date$date) # change dates from characters to date format
fish <- left_join(fish, date, by = "anem_table_id")

# clean up
rm(date, tags)

# deal with empty values - these can't be NA for the code below to work
fish$capid[is.na(fish$capid)] <- "0" 
fish$recap[is.na(fish$recap)] <- "0" 
fish$tagid[is.na(fish$tagid)] <- "0" 
fish$size[is.na(fish$size)] <- "0" 
fish$sample_id[is.na(fish$sample_id)] <- "0"
fish$date[is.na(fish$date)] <- "1901-01-01"

# remove samples with missing sizes or dates ####
  # look at which fish do not have a size
fish[(fish$size == 0),]

# need to remove the complete pair of the recapture event, even if one of the fish has a measurement
# currently only fish with capids have missing values, if there were some with tagids, those would also be changed to X and removed.

# make a list of capids to remove
X <- fish$capid[fish$size == 0] 

fish$capid[fish$capid %in% X] <- "X"

# repeat for missing dates
fish[fish$date == "1901-01-01", ] # in this case has already been taken care of by size above

# remove the samples with X values
fish <- fish[fish$capid != "X", ]

# clean up 
rm(X)

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

# create the table for admb ####

# start with geno recaptures
recapture <- data.frame()
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
    recapture <- rbind(recapture, X[1,]) # join to the recapture table
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
    recapture <- rbind(recapture, X[1,])
  }
}

# remove duplicate rows
recapture <- distinct(recapture)

# reduce table to only necessary columns
recapture <- recapture %>% 
  select(L1, L2, growth, tal)

# convert format of columns from chr to num
recapture$L1 <- as.numeric(recapture$L1)
recapture$L2 <- as.numeric(recapture$L2)

# convert tal to proportion of year
recapture <- mutate(recapture, tal = tal/365)

# remove fish with negative growth
recapture <- recapture %>% 
  filter(growth >= 0)

# Build the dat components for the outfile
msg1 <- c("# init_int numGrowIncObs")
write_lines(msg1, path = "admb/BET-grow.dat", append = F)

msg2 <- nrow(recapture)
write_lines(msg2, path = "admb/BET-grow.dat", append = T)

msg3 <- c("# init_matrix GrowIncDat(1,numGrowIncObs,1,4)")
write_lines(msg3, path = "admb/BET-grow.dat", append = T)

msg4 <- c("# L1	L2	GrowInc	Time")
write_lines(msg4, path = "admb/BET-grow.dat", append = T)

write_tsv(recapture, "admb/BET-grow.dat", append = T)
