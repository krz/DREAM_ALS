# Specify the input/output file path
args<-commandArgs(trailingOnly = TRUE)      #to pass the selector.sh arguments to R
input_file_path<-args[1]
output_file_path<-args[2]

library(dplyr)

# Read the input file
dat<-read.delim(input_file_path,sep="|",header=TRUE)

#assign the subject to a clusters and get the selected features
ind_cl<-1       #just an example
#ind<-(1:6)      #just an example
#outdat<-dat[ind,]

# select features alsfrs, onset_delta, onset_site, mouth, Age, respiratory

vars <- dat %>% filter(feature_name=="onset_site" | feature_name=="onset_delta"
                      | feature_name=="fvc_percent" | feature_name=="fvc"
                      | feature_name=="Age" | feature_name=="ALSFRS_Total")

outdat <- vars %>% arrange(SubjectID, feature_name)   # sort output by SubjectID and feature_name

#write the output file
outdat<-rbind(c(paste("cluster: ",ind_cl,sep=""),rep("",dim(outdat)[2]-1)),as.matrix(outdat))
write.table(outdat,output_file_path,sep="|",row.names=F,col.names=F,quote=F)

