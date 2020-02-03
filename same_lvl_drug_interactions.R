# Title     : TODO
# Objective : TODO
# Created by: stillsen
# Created on: 01.02.20

library('plotrix')
library('ggplot2')

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

df[is.na(df)] <- 0
# all lvl =1  drug interactions
decimals = seq(1,255)
m <- t(sapply(decimals,function(x){ as.integer(intToBits(x)[1:8])}))

select <- append(m[1,],c(1,1,1,1))
sub_df <- df[which(AMP==select[1] & CPR==select[2] & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]

# mean_lvl <- list()
# stde_lvl <- list()
# mean_lvl <- c()
# stde_lvl <- c()
# lvl_vec <- c()



for (i in lvls){
  for (drug in drugs){
    print(i)
    print(drug)
    attach(df)
    # cerate sub dataframe respectively
    switch(drug,
           'AMP'={sub_df <- df[which(AMP==i & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
           'CPR'={sub_df <- df[which(is.na(AMP) & CPR==i & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
           'DOX'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & DOX==i & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
           'ERY'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & ERY==i & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
           'FOX'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & FOX==i & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
           'FUS'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & FUS==i & is.na(STR) & is.na(TMP)),]},
           'STR'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & STR==i & is.na(TMP)),]},
           'TMP'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & TMP==i),]},
            {print('default')}
    )
    # convert it's fake char column to numeric
    detach(df)
    sub_df <- transform(sub_df, trial_3=as.numeric(sub_df$trial_3))
    # compute mean and std error over all trials for a certain druf and add it to a vector
    mean_vec <- append(mean_vec, mean( append(append(sub_df$trial_1, sub_df$trial_2), (sub_df$trial_3)) ))
    stde_vec <- append(stde_vec, std.error( append(append(sub_df$trial_1, sub_df$trial_2), sub_df$trial_3) ))
  }
  mean_lvl <- append(mean_lvl,mean_vec)
  stde_lvl <- append(stde_lvl, stde_vec)
  lvl_vec <- append(lvl_vec, rep(i, length(drugs)))
  mean_vec <- c()
  stde_vec <- c()
}

# # wrapping all up
# result_df <- data.frame('mean' = mean_lvl,
#                         'std_error' = stde_lvl,
#                         'lvl'=lvl_vec,
#                         'drug' = rep(drugs,3),
#                         stringsAsFactors = FALSE)