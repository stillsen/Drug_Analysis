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

# draw n random samples
n <- 5000
sample_row_idx <- sample(nrow(df),n)
sub_df <- df[sample_row_idx,]

# extract n random features
x <- sub_df[,1:8]

# and response mean vector and attach it to sub_df
df_resp <- sub_df[,9:11]
df_resp[is.na(df_resp)] <- 0
y <- rowMeans(df_resp)
sub_df$response <- y

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
int_order <- lapply(split_str, lengths)
# attach to df
idf$int_order <- unlist(int_order)

ggplot(data = idf) +geom_point(mapping = aes(y=interaction, x=stability), colour=int_order, group=int_order)
# ggsave('interaction_df_1_to_5000.pdf')
ggsave('interaction_5000_rdm_samples.pdf')

sub_idf <- idf[which(idf$stability==1 & idf$int_order==2),]

# for each order 2 sample with stability score 1, plot a figure
for (i in seq(1,nrow(sub_idf))){
  # create a sub dataframe from sub_df (with interaction != 0) and all not interacting drugs are 0
  # get interactions as strings
  interaction <- unlist(strsplit(toString(sub_idf[i,1]),"_"))
  # column names as selector
  select <- append(interaction, "response")
  int_resp_df <- subset(sub_df, select=select)
  # drop 0 interactions
  int_resp_df <- int_resp_df[which(int_resp_df[1]!=0 & int_resp_df[2]!=0),]
  write.csv(int_resp_df,'stability1_interaction_response.csv')
  # ggplot(data = int_resp_df)+geom_point(mapping = aes(y=response, x=AMP) , colour=FUS, group=FUS, position =position_dodge(width = .25))
    # geom_errorbar(mapping = aes(ymin=mean-std_error,  colour=lvl, ymax=mean+std_error, x=interactions, group=lvl), position =position_dodge(width = .25),width=.1) +

    # geom_line(mapping = aes(y=mean, x=interactions, colour=lvl, group=lvl), position =position_dodge(width = .25))
}

i <- 1
interaction <- unlist(strsplit(toString(sub_idf[i,1]),"_"))
select <- append(interaction, "response")
int_resp_df <- subset(sub_df, select=select)
int_resp_df <- int_resp_df[which(int_resp_df[1]!=0 & int_resp_df[2]!=0),]
int_resp_df$int <- paste(int_resp_df[,1],int_resp_df[,2])

library(dplyr)
group_by(int_resp_df, int) %>% summarise(ResponseMean=mean(response), ResponseSD=sd(response))
# ggplot(data = idf,aes(y=interaction, x=stability)) +geom_point( colour=order)
  # geom_errorbar(mapping = aes(ymin=mean-std_error,  colour=lvl, ymax=mean+std_error, x=interactions, group=lvl), position =position_dodge(width = .25),width=.1) +
  # geom_point(mapping = aes(y=interaction, x=stability) , colour=lvl, group=lvl), position =position_dodge(width = .25))+
  # geom_line(mapping = aes(y=mean, x=interactions, colour=lvl, group=lvl), position =position_dodge(width = .25))