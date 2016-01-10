set.seed(56)
library(plyr)
library(dplyr)
library(reshape2)

dat <- read.csv("/alsdream/training_data/registries/all_forms_registry_training.txt", header=T, sep="|")
colnames(dat) <- c("SubjectID", "form_name", "feature_name", "feature_value", "feature_unit", "feature_delta")
dat$SubjectID <- gsub("I", "999", as.character(dat$SubjectID))
dat$SubjectID <- as.numeric(as.character(dat$SubjectID))


dat <- dat %>% filter(feature_name=="onset_site" | feature_name=="onset_delta"
                      | feature_name=="mouth" | feature_name=="respiratory"
                      | feature_name=="Age" | feature_name=="ALSFRS_R_Total")


####### datacleaning and filtering

#demographic
demographic <- filter(dat, form_name=="Demographic")
demographic$feature_unit <- NULL
demographic$feature_delta <- NULL
demographic$feature_value <- factor(demographic$feature_value)   # delete unused factor levels
demographic$feature_name <- factor(demographic$feature_name)   # delete unused factor levels
demographic$feature_value <- as.numeric(as.character(demographic$feature_value))   # to numeric

#alsfrs
alsfrs <- filter(dat, form_name=="ALSFRS")
alsfrs$feature_unit <- NULL
alsfrs$form_name <- factor(alsfrs$form_name)  # delete unused factor levels
alsfrs$feature_name <- factor(alsfrs$feature_name)  # delete unused factor levels
alsfrs$feature_value <- factor(alsfrs$feature_value)  # delete unused factor levels
alsfrs$feature_delta <- factor(alsfrs$feature_delta)  # delete unused factor levels
alsfrs$feature_value <- as.numeric(as.character(alsfrs$feature_value))   # to numeric
alsfrs$feature_delta <- as.numeric(as.character(alsfrs$feature_delta))   # to numeric

# ALSHX
alshx <- filter(dat, form_name=="ALSHx")
alshx$feature_unit <- NULL
alshx$feature_delta <- NULL
alshx$feature_name <- factor(alshx$feature_name)  # delete unused factor levels
alshx$feature_value <- factor(alshx$feature_value)  # delete unused factor levels
#alshx$feature_value <- as.numeric(as.character(alshx$feature_value))   # to numeric

#################################################
###### generate variables
#################################################

age.score <- demographic %>% filter(feature_name=="Age") %>%
  dcast(SubjectID~feature_name, value.var='feature_value')
age.score$Age <- as.numeric(age.score$Age)

##################
### alsfrs
##################

alsfrs_bySubject <- alsfrs %>%
  filter(feature_name=="ALSFRS_R_Total" & feature_delta <= 91) %>%
  group_by(SubjectID)


alsfrs.vars <- summarise(alsfrs_bySubject,
                         alsfrs_mean=mean(feature_value, na.rm = TRUE),
                         #alsfrs_sd=sd(feature_value, na.rm = TRUE),
                         alsfrs_max=max(feature_value, na.rm = TRUE),
                         alsfrs_min=min(feature_value, na.rm = TRUE),
                         alsfrs_diff=alsfrs_max-alsfrs_min,
                         alsfrs_first=feature_value[1],
                         alsfrs_last=last(feature_value),
                         alsfrs_len=length(feature_value))
                         #alsfrs_minmax=(max(feature_value) - min(feature_value)) / 
                         #   (first(feature_delta[feature_value == max(feature_value)]) - 
                         #     last(feature_delta[feature_value == min(feature_value)])))

alsfrs.fit <- alsfrs_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(alsfrs_slope = coef(fit)[2])


alsfrs.score <- cbind(alsfrs.vars, alsfrs.fit)

####

mouth_bySubject <- alsfrs %>%
  filter(feature_name=="mouth" & feature_delta <= 91) %>%
  group_by(SubjectID)


mouth.vars <- summarise(mouth_bySubject,
                         mouth_mean=mean(feature_value, na.rm = TRUE),
                         #mouth_sd=sd(feature_value, na.rm = TRUE),
                         mouth_max=max(feature_value, na.rm = TRUE),
                         mouth_min=min(feature_value, na.rm = TRUE),
                         mouth_diff=mouth_max-mouth_min,
                         mouth_first=feature_value[1],
                         mouth_last=last(feature_value),
                         mouth_len=length(feature_value))
                         #mouth_minmax=(max(feature_value) - min(feature_value)) / 
                         #(first(feature_delta[feature_value == max(feature_value)]) - 
                         #  last(feature_delta[feature_value == min(feature_value)])))

mouth.fit <- mouth_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(mouth_slope = coef(fit)[2])


mouth.score <- cbind(mouth.vars, mouth.fit)

#####


respiratory_bySubject <- alsfrs %>%
  filter(feature_name=="respiratory" & feature_delta <= 91) %>%
  group_by(SubjectID)


respiratory.vars <- summarise(respiratory_bySubject,
                               respiratory_mean=mean(feature_value, na.rm = TRUE),
                               #respiratory_sd=sd(feature_value, na.rm = TRUE),
                               respiratory_max=max(feature_value, na.rm = TRUE),
                               respiratory_min=min(feature_value, na.rm = TRUE),
                               respiratory_diff=respiratory_max-respiratory_min,
                               respiratory_first=feature_value[1],
                               respiratory_last=last(feature_value),
                               respiratory_len=length(feature_value))
                               #respiratory_minmax=(max(feature_value) - min(feature_value)) / 
                               #   (first(feature_delta[feature_value == max(feature_value)]) - 
                               #      last(feature_delta[feature_value == min(feature_value)])))

respiratory.fit <- respiratory_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(respiratory_slope = coef(fit)[2])


respiratory.score <- cbind(respiratory.vars, respiratory.fit)

############
### alshx
############

alshx.score <- alshx %>% dcast(SubjectID ~ feature_name, value.var='feature_value', na.rm=TRUE)
alshx.score$onset_delta <- as.numeric(alshx.score$onset_delta)
alshx.score$onset_site <- ifelse(alshx.score$onset_site=="Limb",0,1) #to binary variable

###############
###############

############ merge data

data <- Reduce(function(x, y) merge(x, y, all=TRUE, by="SubjectID"),
               list(age.score, alsfrs.score, alshx.score, mouth.score, respiratory.score))

#print(head(data))
#print(colnames(data))

# load survival outcome

# load survival outcome

slope <- read.csv("/alsdream/training_data/registries/surv_response_registry_training.txt", header=T, sep="|")
slope$SubjectID <- gsub("I", "999", as.character(slope$SubjectID))
slope$SubjectID <- as.numeric(as.character(slope$SubjectID))

# merge with slope by SubjectID
gold <- merge(data, slope, by="SubjectID", all.y=TRUE)

# delete rows with more than 50% missing
gold <- gold[-which(rowMeans(is.na(gold)) > 0.5), ]

# build model
library(randomForestSRC)
mod <- rfsrc(Surv(time_event,status)~., data=gold, na.action = "na.impute", do.trace=T)


rm(list=setdiff(ls(), c("mod","gold")))
save.image("~/als_submission/NatRegSurvival/model_final_surv_reg.Rdata")

chk@89a740a477f3:~/als_submission/NatRegSurvival$ ^C
chk@89a740a477f3:~/als_submission/NatRegSurvival$ 
chk@89a740a477f3:~/als_submission/NatRegSurvival$ 
chk@89a740a477f3:~/als_submission/NatRegSurvival$ 
chk@89a740a477f3:~/als_submission/NatRegSurvival$ 
chk@89a740a477f3:~/als_submission/NatRegSurvival$ 
chk@89a740a477f3:~/als_submission/NatRegSurvival$ 
chk@89a740a477f3:~/als_submission/NatRegSurvival$ cat exampleSELECTORscript.r 
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

library(plyr)
library(dplyr)
library(reshape2)

# Specify the input/output file path
args<-commandArgs(trailingOnly = TRUE)      #to pass the selector.sh arguments to R
input_file_path<-args[1]
output_file_path<-args[2]

# Read the input file

dat <- read.csv(input_file_path, header=T, sep="|")
colnames(dat) <- c("SubjectID", "form_name", "feature_name", "feature_value", "feature_unit", "feature_delta")
dat$SubjectID <- gsub("I", "999", as.character(dat$SubjectID))
dat$SubjectID <- as.numeric(as.character(dat$SubjectID))

load("~/als_submission/NatRegSurvival/model_final_surv_reg.Rdata")

print(dat)

####### datacleaning and filtering

#demographic
demographic <- filter(dat, form_name=="Demographic")
demographic$feature_unit <- NULL
demographic$feature_delta <- NULL
demographic$feature_value <- factor(demographic$feature_value)   # delete unused factor levels
demographic$feature_name <- factor(demographic$feature_name)   # delete unused factor levels
demographic$feature_value <- as.numeric(as.character(demographic$feature_value))   # to numeric

#alsfrs
alsfrs <- filter(dat, form_name=="ALSFRS")
alsfrs$feature_unit <- NULL
alsfrs$form_name <- factor(alsfrs$form_name)  # delete unused factor levels
alsfrs$feature_name <- factor(alsfrs$feature_name)  # delete unused factor levels
alsfrs$feature_value <- factor(alsfrs$feature_value)  # delete unused factor levels
alsfrs$feature_delta <- factor(alsfrs$feature_delta)  # delete unused factor levels
alsfrs$feature_value <- as.numeric(as.character(alsfrs$feature_value))   # to numeric
alsfrs$feature_delta <- as.numeric(as.character(alsfrs$feature_delta))   # to numeric

# ALSHX
alshx <- filter(dat, form_name=="ALSHx")
alshx$feature_unit <- NULL
alshx$feature_delta <- NULL
alshx$feature_name <- factor(alshx$feature_name)  # delete unused factor levels
alshx$feature_value <- factor(alshx$feature_value)  # delete unused factor levels
#alshx$feature_value <- as.numeric(as.character(alshx$feature_value))   # to numeric

#################################################
###### generate variables

age <- demographic %>% filter(feature_name=="Age") %>% select(Age=feature_value)
if(nrow(age) == 0) age <- NA#65.6 

################

als_data <- alsfrs %>% filter(feature_name=='ALSFRS_R_Total' & feature_delta <= 91) %>% arrange(feature_name, feature_delta)


alsfrs_mean <- als_data %>% select(alsfrs_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(alsfrs_mean == "NaN" | is.na(alsfrs_mean)) alsfrs_mean <- NA#32.5

# max
alsfrs_max <- als_data %>% select(alsfrs_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(alsfrs_max) | alsfrs_max == "NaN" | alsfrs_max == "-Inf") alsfrs_max <- NA#34.0
#min
alsfrs_min <- als_data %>% select(alsfrs_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(alsfrs_min) | alsfrs_min == "NaN" | alsfrs_min == "Inf") alsfrs_min <- NA#31.5
#diff
alsfrs_diff <- alsfrs_max-alsfrs_min
#first
alsfrs_first <- als_data %>% select(alsfrs_first=feature_value) %>% head(1)
if(nrow(alsfrs_first) == 0 ) alsfrs_first <- NA#34.0
#last
alsfrs_last <- als_data %>% select(alsfrs_last=feature_value) %>% as.matrix %>% last(default=NA)
#length
alsfrs_len <- als_data %>% select(alsfrs_len=feature_value) %>% as.matrix %>% length()
if(alsfrs_len == 0 ) alsfrs_len <- NA#2.0
#slope
alsfrs_slope <- als_data %>% group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(alsfrs_slope = coef(fit)[2])                
if(is.na(as.numeric(unlist(alsfrs_slope))[1])) alsfrs_slope <- NA

###############

mouth_data <- alsfrs %>% filter(feature_name=='mouth' & feature_delta <= 91) %>% arrange(feature_name, feature_delta)

mouth_mean <- mouth_data %>% select(mouth_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(mouth_mean == "NaN" | is.na(mouth_mean)) mouth_mean <- NA#11.0

# max
mouth_max <- mouth_data %>% select(mouth_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(mouth_max) | mouth_max == "NaN" | mouth_max == "-Inf") mouth_max <- NA#11.0
#min
mouth_min <- mouth_data %>% select(mouth_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(mouth_min) | mouth_min == "NaN" | mouth_min == "Inf") mouth_min <- NA#11.0
#diff
mouth_diff <- mouth_max-mouth_min
#first
mouth_first <- mouth_data %>% select(mouth_first=feature_value) %>% head(1)
if(nrow(mouth_first) == 0 ) mouth_first <- NA#11.0
#last
mouth_last <- mouth_data %>% select(mouth_last=feature_value) %>% as.matrix %>% last(default=NA) 
#length
mouth_len <- mouth_data %>% select(mouth_len=feature_value) %>% as.matrix %>% length()
if(mouth_len == 0 ) mouth_len <- NA#2.0
#slope
mouth_slope <- mouth_data %>% group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(mouth_slope = coef(fit)[2])
if(is.na(as.numeric(unlist(mouth_slope))[1])) mouth_slope <- NA


###

respiratory_data <- alsfrs %>% filter(feature_name=='respiratory' & feature_delta <= 91) %>% 
                       arrange(feature_name, feature_delta)

respiratory_mean <- respiratory_data %>% select(respiratory_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(respiratory_mean == "NaN" | is.na(respiratory_mean)) respiratory_mean <- NA#4.0
print(respiratory_mean)
# max
respiratory_max <- respiratory_data %>% select(respiratory_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(respiratory_max) | respiratory_max == "NaN" | respiratory_max == "-Inf") respiratory_max <- NA#4.0
print(respiratory_max)
#min
respiratory_min <- respiratory_data %>% select(respiratory_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(respiratory_min) | respiratory_min == "NaN" | respiratory_min == "Inf") respiratory_min <- NA#4.0
print(respiratory_min)
#diff
respiratory_diff <- respiratory_max-respiratory_min
#first
respiratory_first <- respiratory_data %>% select(respiratory_first=feature_value) %>% head(1)
if(nrow(respiratory_first) == 0 ) respiratory_first <- NA#4.0
print(respiratory_first)
#length
respiratory_len <- respiratory_data %>% select(respiratory_len=feature_value) %>% as.matrix %>% length()
if(respiratory_len == 0) respiratory_len <- NA
#last
respiratory_last <- respiratory_data %>% select(respiratory_last=feature_value) %>% as.matrix %>% last(default=NA)
#slope
respiratory_slope <- respiratory_data %>% group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(respiratory_slope = coef(fit)[2])
if(is.na(as.numeric(unlist(respiratory_slope))[1])) respiratory_slope <- NA


###########
### alshx
###########

delta_data <- alshx %>% filter(feature_name=='onset_delta')
delta_data$feature_value <- as.numeric(as.character(delta_data$feature_value))
onset_delta <- delta_data$feature_value[1]

onset_site <- alshx %>% filter(feature_name=="onset_site") %>% select(onset_site=feature_value) %>% head(1)
#if(nrow(onset_delta) == 0 ) onset_delta <- "Limb"
onset_site <- ifelse(onset_site=="Limb",1,0)

###########
### subject id
###########

SubjectID <- dat[1,1]

###############
###############

############ merge data

preddata <- data.frame(Age = as.vector(age),
                   alsfrs_mean = as.vector(alsfrs_mean),
                   alsfrs_max = as.vector(alsfrs_max),
                   alsfrs_min = as.vector(alsfrs_min),
                   alsfrs_diff = as.vector(alsfrs_diff),
                   alsfrs_first = as.vector(alsfrs_first),
                   alsfrs_last = as.vector(alsfrs_last),
                   alsfrs_len = as.vector(alsfrs_len),
                   alsfrs_slope = as.vector(alsfrs_slope),
                   mouth_mean = as.vector(mouth_mean),
                   mouth_max = as.vector(mouth_max),
                   mouth_min = as.vector(mouth_min),
                   mouth_diff = as.vector(mouth_diff),
                   mouth_first = as.vector(mouth_first),
                   mouth_last = as.vector(mouth_last),
                   mouth_len = as.vector(mouth_len),
                   mouth_slope = as.vector(mouth_len),
                   respiratory_mean = as.vector(respiratory_mean),
                   respiratory_max = as.vector(respiratory_max),
                   respiratory_min = as.vector(respiratory_min),
                   respiratory_diff = as.vector(respiratory_diff),
                   respiratory_first = as.vector(respiratory_first),
                   respiratory_last = as.vector(respiratory_last),
                   respiratory_len = as.vector(respiratory_len),
                   respiratory_slope = as.vector(respiratory_slope),
                   onset_delta = as.vector(onset_delta),
                   onset_site = as.vector(onset_site),
                   SubjectID = SubjectID)



print(str(preddata))                


print("preddata:")
print(preddata)



detach("package:dplyr", unload=TRUE)

library(randomForestSRC)
print("ok")

#load("~/als_submission/NatRegSurvival/model_final_surv_reg.Rdata")
pred <- predict(mod, preddata, na.action="na.impute")

# 12,18,24 months
outdat <- cbind(pred$survival[,51], pred$survival[,104], pred$survival[,157])
print(outdat)

write.table(outdat,output_file_path,sep="|",row.names=F,col.names=F,quote=F)

