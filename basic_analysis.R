# Title     : TODO
# Objective : TODO
# Created by: stillsen
# Created on: 1/22/20


# package for std error implementation
# install.packages('plotrix')
library('plotrix')

path <- '/home/stillsen/Documents/Uni/HiWi/Source/Drug_Analysis'
filename <- 'reordered.csv'
abs_filename <- paste(path, '/', filename, sep='')

# read csv, which is actually tsv ^^
df <- read.table(abs_filename, sep='\t', header=TRUE)
# forget about zero filled first column
df <- df[,-1]
# attach to search path for easier variable access
attach(df)


# Sub DF for drugs AMP CPR DOX ERY FOX FUS STR TMP
# taking their mean and standard error
# AMP
amp_df <- df[which(AMP==1 & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]
mean_amp_t1 <- mean(as.numeric(amp_df$trial_1))
mean_amp_t2 <- mean(as.numeric(amp_df$trial_2))
mean_amp_t3 <- mean(as.numeric(amp_df$trial_3))
stde_amp_t1 <- std.error(as.numeric(amp_df$trial_1))
stde_amp_t2 <- std.error(as.numeric(amp_df$trial_2))
stde_amp_t3 <- std.error(as.numeric(amp_df$trial_3))

# CPR
cpr_df <- df[which(is.na(AMP) & CPR==1 & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]
mean_cpr_t1 <- mean(as.numeric(cpr_df$trial_1))
mean_cpr_t2 <- mean(as.numeric(cpr_df$trial_2))
mean_cpr_t3 <- mean(as.numeric(cpr_df$trial_3))
stde_cpr_t1 <- std.error(as.numeric(cpr_df$trial_1))
stde_cpr_t2 <- std.error(as.numeric(cpr_df$trial_2))
stde_cpr_t3 <- std.error(as.numeric(cpr_df$trial_3))

# DOX
dox_df <- df[which(is.na(AMP) & is.na(CPR) & DOX==1 & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]
mean_dox_t1 <- mean(as.numeric(dox_df$trial_1))
mean_dox_t2 <- mean(as.numeric(dox_df$trial_2))
mean_dox_t3 <- mean(as.numeric(dox_df$trial_3))
stde_dox_t1 <- std.error(as.numeric(dox_df$trial_1))
stde_dox_t2 <- std.error(as.numeric(dox_df$trial_2))
stde_dox_t3 <- std.error(as.numeric(dox_df$trial_3))

# ERY
ery_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & ERY==1 & is.na(FOX) & is.na(FUS) & is.na(STR) & is.na(TMP)),]
mean_ery_t1 <- mean(as.numeric(ery_df$trial_1))
mean_ery_t2 <- mean(as.numeric(ery_df$trial_2))
mean_ery_t3 <- mean(as.numeric(ery_df$trial_3))
stde_ery_t1 <- std.error(as.numeric(ery_df$trial_1))
stde_ery_t2 <- std.error(as.numeric(ery_df$trial_2))
stde_ery_t3 <- std.error(as.numeric(ery_df$trial_3))

# FOX
fox_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & FOX==1 & is.na(FUS) & is.na(STR) & is.na(TMP)),]
mean_fox_t1 <- mean(as.numeric(fox_df$trial_1))
mean_fox_t2 <- mean(as.numeric(fox_df$trial_2))
mean_fox_t3 <- mean(as.numeric(fox_df$trial_3))
stde_fox_t1 <- std.error(as.numeric(fox_df$trial_1))
stde_fox_t2 <- std.error(as.numeric(fox_df$trial_2))
stde_fox_t3 <- std.error(as.numeric(fox_df$trial_3))

# FUS
fus_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & FUS==1 & is.na(STR) & is.na(TMP)),]
mean_fus_t1 <- mean(as.numeric(fus_df$trial_1))
mean_fus_t2 <- mean(as.numeric(fus_df$trial_2))
mean_fus_t3 <- mean(as.numeric(fus_df$trial_3))
stde_fus_t1 <- std.error(as.numeric(fus_df$trial_1))
stde_fus_t2 <- std.error(as.numeric(fus_df$trial_2))
stde_fus_t3 <- std.error(as.numeric(fus_df$trial_3))

# STR
str_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & STR==1 & is.na(TMP)),]
mean_str_t1 <- mean(as.numeric(str_df$trial_1))
mean_str_t2 <- mean(as.numeric(str_df$trial_2))
mean_str_t3 <- mean(as.numeric(str_df$trial_3))
stde_str_t1 <- std.error(as.numeric(str_df$trial_1))
stde_str_t2 <- std.error(as.numeric(str_df$trial_2))
stde_str_t3 <- std.error(as.numeric(str_df$trial_3))

# TMP
tmp_df <- df[which(is.na(AMP) & is.na(CPR) & is.na(DOX) & is.na(ERY) & is.na(FOX) & is.na(FUS) & is.na(STR) & TMP==1),]
mean_tmp_t1 <- mean(as.numeric(tmp_df$trial_1))
mean_tmp_t2 <- mean(as.numeric(tmp_df$trial_2))
mean_tmp_t3 <- mean(as.numeric(tmp_df$trial_3))
stde_tmp_t1 <- std.error(as.numeric(tmp_df$trial_1))
stde_tmp_t2 <- std.error(as.numeric(tmp_df$trial_2))
stde_tmp_t3 <- std.error(as.numeric(tmp_df$trial_3))
