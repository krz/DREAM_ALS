# Specify the input/output file path
args<-commandArgs(trailingOnly = TRUE)      #to pass the selector.sh arguments to R
input_file_path<-args[1]
output_file_path<-args[2]

library(dplyr)

# Read the input file
dat<-read.delim(input_file_path,sep="|",header=TRUE)
colnames(dat) <- c("SubjectID", "form_name", "feature_name", "feature_value", "feature_unit", "feature_delta")
#dat$SubjectID <- gsub("I", "999", as.character(dat$SubjectID))
#dat$SubjectID <- as.numeric(as.character(dat$SubjectID))


#assign the subject to a clusters and get the selected features
ind_cl<-1       #just an example
#ind<-(1:6)      #just an example
#outdat<-dat[ind,]

# select features alsfrs, onset_delta, onset_site, mouth, Age, respiratory

vars <- dat %>% filter(feature_name=="onset_site" | feature_name=="onset_delta"
                      | feature_name=="mouth" | feature_name=="respiratory"
                      | feature_name=="Age" | feature_name=="ALSFRS_R_Total")


outdat <- vars %>% arrange(SubjectID, feature_name)   # sort output by SubjectID and feature_name

#write the output file
outdat<-rbind(c(paste("cluster: ",ind_cl,sep=""),rep("",dim(outdat)[2]-1)),as.matrix(outdat))
write.table(outdat,output_file_path,sep="|",row.names=F,col.names=F,quote=F)

