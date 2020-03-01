# Title     : TODO
# Objective : TODO
# Created by: stillsen
# Created on: 01.03.20

library('plotrix')
library('ggplot2')
library('iRF')

path <- '/home/stillsen/Documents/Uni/HiWi/Source/Drug_Analysis'
filename <- 'reordered.csv'
abs_filename <- paste(path, '/', filename, sep='')

# read csv, which is actually tsv ^^
df <- read.table(abs_filename, sep='\t', header=TRUE, stringsAsFactors = FALSE)
# forget about zero filled first column
df <- df[,-1]
# attach to search path for easier variable access
attach(df)
# convert false char to numeric
df <- transform(df, trial_3=as.numeric(trial_3))

# Sub_Dataframe for singular drugs AMP CPR DOX ERY FOX FUS STR TMP in concentration 1
# comnpute their mean and standard error
# attach to respective list
# and convert all lists to one dataframe
drugs <- c('AMP', 'CPR', 'DOX', 'ERY', 'FOX', 'FUS', 'STR', 'TMP')
mean_vec <- c()
stde_vec <- c()
drug_vec <- c()
lvls = c(1,2,3)

# replace all NAs with 0 from the interaction part (col 1-8)
df_int <- df[, 1:8]
df_int[is.na(df_int)] <- 0
df <- cbind(df_int,df[,9:12])

# extract features
x <- df[1:4218,1:8]

# and response vector
y <- df[1:4218,9]
iRF(x = data.matrix(x) ,y = y)