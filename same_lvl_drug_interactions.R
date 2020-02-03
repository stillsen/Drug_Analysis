# Title     : TODO
# Objective : TODO
# Created by: stillsen
# Created on: 01.02.20

library('plotrix')
# library('ggplot2')

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

# all lvl =1  drug interactions
decimals = seq(1,255)
m <- t(sapply(decimals,function(x){ as.integer(intToBits(x)[1:8])}))

attach(df)
# mean_per_lvl <- list()
# stde_per_lvl <- list()
mean_per_lvl <- c()
stde_per_lvl <- c()
lvl_vec <- c()
interaction_vec <- c()
for (lvl in lvls){
  print(lvl)
  values_per_lvl <- list()
  # loop over interaction matrix
  for (n in 1:nrow(m)){
    # select interaction and multiply with lvl
    # and subset df for those interactions
    select <- m[n,]
    no_interactions <- sum(select)
    select <- select*lvl
    sub_df <- df[which(AMP==select[1] & CPR==select[2] & DOX==select[3] & ERY==select[4] & FOX==select[5] & FUS==select[6] & STR==select[7] & TMP==select[8]),]

    values_per_lvl[[no_interactions]] <- unlist(append(values_per_lvl[no_interactions], sub_df$trial_1))
    values_per_lvl[[no_interactions]] <- unlist(append(values_per_lvl[no_interactions], sub_df$trial_2))
    values_per_lvl[[no_interactions]] <- unlist(append(values_per_lvl[no_interactions], sub_df$trial_3))
  }
  # mean_per_lvl[[lvl]] <- c( mean(unlist(values_per_lvl[1])),mean(unlist(values_per_lvl[2])),mean(unlist(values_per_lvl[3])),mean(unlist(values_per_lvl[4])),mean(unlist(values_per_lvl[5])))
  # stde_per_lvl[[lvl]] <- c( std.error(unlist(values_per_lvl[1])),std.error(unlist(values_per_lvl[2])),std.error(unlist(values_per_lvl[3])),std.error(unlist(values_per_lvl[4])),std.error(unlist(values_per_lvl[5])))
  mean_per_lvl <- append(mean_per_lvl, c( mean(unlist(values_per_lvl[1])),mean(unlist(values_per_lvl[2])),mean(unlist(values_per_lvl[3])),mean(unlist(values_per_lvl[4])),mean(unlist(values_per_lvl[5]))))
  stde_per_lvl <- append(stde_per_lvl, c( std.error(unlist(values_per_lvl[1])),std.error(unlist(values_per_lvl[2])),std.error(unlist(values_per_lvl[3])),std.error(unlist(values_per_lvl[4])),std.error(unlist(values_per_lvl[5]))))
  lvl_vec <- append(lvl_vec, c(lvl, lvl, lvl, lvl, lvl))
  interaction_vec <- append(interaction_vec, c(1,2,3,4,5))
}
result_df <- data.frame('mean' = mean_per_lvl,
                        'std_error' = stde_per_lvl,
                        'interactions' = interaction_vec,
                        'lvl'=lvl_vec,
                        stringsAsFactors = FALSE)

print(result_df)
detach(df)
attach(result_df)

ggplot(data = result_df) +
  geom_errorbar(mapping = aes(ymin=mean-std_error, ymax=mean+std_error, x=interactions, group=lvl), position =position_dodge(width = .5),width=.1) +
  geom_point(mapping = aes(y=mean, x=interactions, group=lvl), position =position_dodge(width = .5))+
  geom_line(mapping = aes(y=mean, x=interactions, group=lvl), position =position_dodge(width = .5))