# Title     : TODO
# Objective : TODO
# Created by: stillsen
# Created on: 04.02.20



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

i <- 1
sub_df <- df[which(AMP==i & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]

sub_df <- transform(sub_df, trial_3=as.numeric(sub_df$trial_3))
amp_entries <- nrow(sub_df)
amp_solo_mean <- mean( append(append(sub_df$trial_1, sub_df$trial_2), (sub_df$trial_3)))
print(c("solo AMP records:",amp_entries))
print(c("mean across those is: ", amp_solo_mean))


attach(sub_df)

write.csv(sub_df,'/home/stillsen/Documents/Uni/HiWi/Source/Drug_Analysis/solo_AMP_subdf.csv', row.names = FALSE)
