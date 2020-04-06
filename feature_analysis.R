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
x <- df[1:5000,1:8]
# x <- df[,1:8]

# and response vector
df_resp <- df[1:5000,9:11]
# df_resp <- df[,9:11]
df_resp[is.na(df_resp)] <- 0
y <- rowMeans(df_resp)

# y <- unlist(lapply(y,trunc))
#####3
# y <- unlist(lapply(y, function(x) {
#   res <- 2
#   if (x<95)
#     {res <- 1}
#   if (x<85)
#     {res <- 0}
#   # else {res <- 1}
#   res
# }))

n.cores <- 15
# rit.params <- list(depth=5, nchild=2, ntree=500, class.id=1, class.cut=NULL)
rit.params <- list(depth=5, nchild=2, ntree=500, class.id=1, class.cut=NULL)

fit <-  iRF(#x=X[train.id,],
          x = data.matrix(x),
           #y=as.factor(Y[train.id]),
          # y = as.factor(y),
          y = y,
           # xtest=X[test.id,],
           # ytest=as.factor(Y[test.id]),
           n.iter=5,
           interactions.return=5,
           rit.params=rit.params,
           # varnames.grp=varnames.all$Predictor_collapsed,
           n.core=n.cores,
           n.bootstrap=30,
           rit.param=rit.params
          )

interactions <- fit$interaction[5]
idf <- data.frame(interactions)
colnames(idf) <- c("interaction", "stability")

## computing order
# interaction col to string
str_int <- lapply(idf[,1], toString)
# splitting strings
split_str <- lapply(str_int, strsplit, "_")
# counting split elements
order <- lapply(split_str, lengths)
# attach to df
idf$order <- unlist(order)

ggplot(data = idf) +geom_point(mapping = aes(y=interaction, x=stability), colour=order, group=order)
# ggplot(data = idf,aes(y=interaction, x=stability)) +geom_point( colour=order)
  # geom_errorbar(mapping = aes(ymin=mean-std_error,  colour=lvl, ymax=mean+std_error, x=interactions, group=lvl), position =position_dodge(width = .25),width=.1) +
  # geom_point(mapping = aes(y=interaction, x=stability) , colour=lvl, group=lvl), position =position_dodge(width = .25))+
  # geom_line(mapping = aes(y=mean, x=interactions, colour=lvl, group=lvl), position =position_dodge(width = .25))
ggsave('interaction_df_1_to_5000.pdf')