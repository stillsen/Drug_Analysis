# Title     : TODO
# Objective : TODO
# Created by: stillsen
# Created on: 1/22/20

path <- '/home/stillsen/Documents/Uni/HiWi/Source/Drug_Analysis'
filename <- 'reordered.csv'
abs_filename <- paste(path, '/', filename, sep='')

# read csv, which is actually tsv ^^
df <- read.table(abs_filename, sep='\t', header=TRUE)
# forget about zero filled first column
df <- df[,-1]

