# Title     : TODO
# Objective : TODO
# Created by: stillsen
# Created on: 1/22/20


# package for std error implementation
# install.packages('plotrix')
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

for (drug in drugs){
  print(drug)
  attach(df)
  # cerate sub dataframe respectively
  switch(drug,
         'AMP'={sub_df <- df[which(AMP==1 & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
         'CPR'={sub_df <- df[which(is.na(AMP) & CPR==1 & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
         'DOX'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & DOX==1 & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
         'ERY'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & ERY==1 & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
         'FOX'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & FOX==1 & is.na(FUS) & is.na(STR) & is.na(TMP)),]},
         'FUS'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & FUS==1 & is.na(STR) & is.na(TMP)),]},
         'STR'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & STR==1 & is.na(TMP)),]},
         'TMP'={sub_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & TMP==1),]},
          {print('default')}
  )
  # convert it's fake char column to numeric
  detach(df)
  sub_df <- transform(sub_df, trial_3=as.numeric(sub_df$trial_3))
  # compute mean and std error
  mean_t1 <- mean(sub_df$trial_1)
  mean_t2 <- mean(sub_df$trial_2)
  mean_t3 <- mean(sub_df$trial_3)
  stde_t1 <- std.error(sub_df$trial_1)
  stde_t2 <- std.error(sub_df$trial_2)
  stde_t3 <- std.error(sub_df$trial_3)
  # add to pooling vector
  mean_vec <- c(mean_vec, mean_t1, mean_t2, mean_t3)
  stde_vec <- c(stde_vec, stde_t1, stde_t2, stde_t3)
  drug_vec <- c(drug_vec, drug, drug, drug)
}

# wrapping all up
result_df <- data.frame('mean' = mean_vec, 'std_error' = stde_vec, 'drug' = drug_vec, stringsAsFactors = FALSE)
detach(df)
attach(result_df)

ggplot(result_df, aes(x=mean, y=drug, colour=drug)) +
    geom_errorbar(aes(ymin=mean-std_error, ymax=mean+std_error), width=.1) +
    geom_line() +
    geom_point()