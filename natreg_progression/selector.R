# Specify the input/output file path
args<-commandArgs(trailingOnly = TRUE)      #to pass the selector.sh arguments to R
input_file_path<-args[1]
output_file_path<-args[2]

library(dplyr)
#library("dplyr",lib.loc="/home/chk/R/s390x-ibm-linux-gnu-library/3.2")

# Read the input file
dat<-read.delim(input_file_path,sep="|",header=TRUE)
colnames(dat) <- c("SubjectID", "form_name", "feature_name", "feature_value", "feature_unit", "feature_delta")

#assign the subject to a clusters and get the selected features
ind_cl<-1       #just an example
#ind<-(1:6)      #just an example
#outdat<-dat[ind,]

# select features hands, leg, mouth, respiratory, onset_delta, ALSFRS_R_Total

vars <- dat %>% filter(feature_name=="hands" | feature_name=="leg"
                      | feature_name=="mouth" | feature_name=="respiratory"
                      | feature_name=="onset_delta" | feature_name=="ALSFRS_R_Total")

outdat <- vars %>% arrange(SubjectID, feature_name)   # sort output by SubjectID and feature_name

#write the output file
outdat<-rbind(c(paste("cluster: ",ind_cl,sep=""),rep("",dim(outdat)[2]-1)),as.matrix(outdat))
write.table(outdat,output_file_path,sep="|",row.names=F,col.names=F,quote=F)

