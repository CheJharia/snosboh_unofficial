
# *------------------------------------------------------------------
# | PROGRAM NAME: Produce csv files for each client from large extraction file
# | DATE: 20151214
# | CREATED BY: chejoharia
# | PROJECT FILE: Extract - Transform              
# *----------------------------------------------------------------
# | PURPOSE: Produce csv files for each client, given a large txt file as input               
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  
# |  2: 
# |  3: 
# |*------------------------------------------------------------------
# | DATA USED:               
# |
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  
# |  PART 2: 
# |  PART 3: 
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# *------------------------------------------------------------------


#######################################################
# The purpose of this scripting file 
# is to automate the exportation in the form of CSV files
# from a given extraction.
# Each exportation, unless stated otherwise, will be based on ClientID
# An example, an exportation from Enquiry extraction refers to 
# the data for a client. 
#######################################################

library(sqldf)
library(foreach)
library(tcltk)
library(stringi)

rm(list=ls()) # get rid ofany existing data 
ls() # view open data sets


# show current wd
getwd()

# set to Enquiry folder
setwd("C:/Users/afiq.johari/Documents/Data Scientist/Incoming_Data/!Enrolment/New")


# get all Enquire txt files
(Enq_files <- list.files(pattern = '*.txt'))
length(Enq_files)
# Preliminary verification (Client_BK is used for filtering)
for (fe in Enq_files) {
  dd <- read.delim(file = fe, sep = '\t', nrows = 5)
  print(colnames(dd)[24])
}


x <- 1
(fname <- strsplit(Enq_files[x], '\\.')[[1]][1])
f <- read.delim(file = Enq_files[4], sep = '\t', header = F)
colnames(f) <- cc
Enq_files[1]
write.table(f, file=Enq_files[4], sep = "\t", row.names = F)

####################################################
# For each large files, we perform the following tasks
# - create a directory
# - read the file
# - create a directory for each client of the file
# - write/output a csv file for each client and save in the created directory
####################################################

for (gfile in Enq_files) {
  
  ##################################################
  # Create folder for file
  ##################################################
  fname <- ""
  dirpath <- ""
  (fname <- strsplit(gfile, '\\.')[[1]][1])
  (dirpath <- paste("C:/Users/afiq.johari/Documents/Data Scientist/Incoming_Data/!Enrolment",'/!Files/',fname, sep = ""))
  dir.create(dirpath)
  
  ##################################################
  # Read file
  ##################################################
  
  f <- read.delim(file = gfile, sep = '\t')
  #write.csv(unique(f) ,file = 'test.csv', row.names = F)
  #write.csv(unique(fn$sqldf("select * from f where client_id = '$client'")) ,file = 'test.csv', row.names = F)
  ##################################################
  # Get colnames and rename the i..client_id column to client_id
  # - assuming i..client_id is the first column
  ##################################################
  colnames(f)
  colnames(f)
  
  ##################################################
  # Get all distinct clients in the file
  ##################################################
  clients <- as.data.frame(fn$sqldf("select distinct Client_BK from f"))
  # get rid of the annoying factor levels
  clients <- levels(factor(clients$Client_BK))
  
  ##################################################
  # For each client we create a csv file
  ##################################################
  for (client in clients) {
    # create subfolder for client
    subdirpath <- paste(dirpath,'/',client, sep = "")
    dir.create(subdirpath)
    # create filename
    fdate <- stri_sub(fname,-8)
    csvfilename <- paste(subdirpath,'/','ENR_',client,'_',fdate,'.csv', sep = "")
    # output unique records to csv file
    write.csv(unique(fn$sqldf("select * from f where Client_BK = '$client'")) ,file = csvfilename, row.names = F)
  }
  
}


#################################################
# VERIF
dim(f)
summary(f)
################################################
# PARAMETERS
LIMIT <- 10
################################################


unique(df)

