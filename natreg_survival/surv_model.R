library(plyr)
library(dplyr)
library(reshape2)

# Specify the input/output file path
#args<-commandArgs(trailingOnly = TRUE)      #to pass the selector.sh arguments to R
#input_file_path<-args[1]
#output_file_path<-args[2]

# Read the input file
dat <- read.csv("~/Copy/Scripts/dream_als/registry training final/all_forms_registry_training.txt", header=T, sep="|") %>%
  arrange(SubjectID, feature_name, feature_delta) %>%
  slice(which(feature_value!="-"))


#dat <- read.csv(input_file_path, header=T, sep="|")
colnames(dat) <- c("SubjectID", "form_name", "feature_name", "feature_value", "feature_unit", "feature_delta")
dat$SubjectID <- gsub("I", "999", as.character(dat$SubjectID))
dat$SubjectID <- as.numeric(as.character(dat$SubjectID))

load("~/als_submission/NatRegProgression/model_final_nat_slope.Rdata")

print(dat)

####### datacleaning and filtering

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
alshx$feature_value <- as.numeric(as.character(alshx$feature_value))   # to numeric

#################################################
###### generate variables


als_data <- alsfrs %>% filter(feature_name=='ALSFRS_Total' & feature_delta <= 91) %>% arrange(feature_name, feature_delta)


alsfrs_mean <- als_data %>% select(alsfrs_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(alsfrs_mean == "NaN" | is.na(alsfrs_mean)) alsfrs_mean <- median(g1$alsfrs_mean, na.rm=T)

# max
alsfrs_max <- als_data %>% select(alsfrs_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(alsfrs_max) | alsfrs_max == "NaN" | alsfrs_max == "-Inf") alsfrs_max <- median(g1$alsfrs_max, na.rm=T)
#min
alsfrs_min <- als_data %>% select(alsfrs_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(alsfrs_min) | alsfrs_min == "NaN" | alsfrs_min == "Inf") alsfrs_min <- median(g1$alsfrs_min, na.rm=T)
#diff
alsfrs_diff <- alsfrs_max-alsfrs_min
#first
alsfrs_first <- als_data %>% select(alsfrs_first=feature_value) %>% head(1)
if(nrow(alsfrs_first) == 0 ) alsfrs_first <- median(g1$alsfrs_first, na.rm=T)
#length
alsfrs_len <- als_data %>% select(alsfrs_len=feature_value) %>% as.matrix %>% length()
if(is.na(alsfrs_len)) alsfrs_len <- median(g1$alsfrs_len, na.rm=T)
#slope
alsfrs_slope <- als_data %>% group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(alsfrs_slope = coef(fit)[2])                
if(is.na(as.numeric(unlist(alsfrs_slope))[1])) alsfrs_slope <- median(g1$alsfrs_slope, na.rm=T)

###

hands_data <- alsfrs %>% filter(feature_name=='hands' & feature_delta <= 91) %>% arrange(feature_name, feature_delta)

hands_mean <- hands_data %>% select(hands_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(hands_mean == "NaN" | is.na(hands_mean)) hands_mean <- median(g1$hands_mean, na.rm=T)

# max
hands_max <- hands_data %>% select(hands_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(hands_max) | hands_max == "NaN" | hands_max == "-Inf") hands_max <- median(g1$hands_max, na.rm=T)
#min
hands_min <- hands_data %>% select(hands_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(hands_min) | hands_min == "NaN" | hands_min == "Inf") hands_min <- median(g1$hands_min, na.rm=T)
#diff
hands_diff <- hands_max-hands_min
#first
hands_first <- hands_data %>% select(hands_first=feature_value) %>% head(1)
if(nrow(hands_first) == 0 ) hands_first <- median(g1$hands_first, na.rm=T)
#length
hands_len <- hands_data %>% select(hands_len=feature_value) %>% as.matrix %>% length()
if(is.na(hands_len)) hands_len <- median(g1$hands_len, na.rm=T)

###

leg_data <- alsfrs %>% filter(feature_name=='leg'& feature_delta <= 91) %>% arrange(feature_name, feature_delta)

leg_mean <- leg_data %>% select(leg_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(leg_mean == "NaN" | is.na(leg_mean)) leg_mean <- median(g1$leg_mean, na.rm=T)

# max
leg_max <- leg_data %>% select(leg_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(leg_max) | leg_max == "NaN" | leg_max == "-Inf") leg_max <- median(g1$leg_max, na.rm=T)
#min
leg_min <- leg_data %>% select(leg_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(leg_min) | leg_min == "NaN" | leg_min == "Inf") leg_min <- median(g1$leg_min, na.rm=T)
#diff
leg_diff <- leg_max-leg_min
#first
leg_first <- leg_data %>% select(leg_first=feature_value) %>% head(1)
if(nrow(leg_first) == 0 ) leg_first <- median(g1$leg_first, na.rm=T)
#length
leg_len <- leg_data %>% select(leg_len=feature_value) %>% as.matrix %>% length()
if(is.na(leg_len) ) leg_len <- median(g1$leg_len, na.rm=T)

###

mouth_data <- alsfrs %>% filter(feature_name=='mouth' & feature_delta <= 91) %>% arrange(feature_name, feature_delta)

mouth_mean <- mouth_data %>% select(mouth_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(mouth_mean == "NaN" | is.na(mouth_mean)) mouth_mean <- median(g1$mouth_mean, na.rm=T)

# max
mouth_max <- mouth_data %>% select(mouth_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(mouth_max) | mouth_max == "NaN" | mouth_max == "-Inf") mouth_max <- median(g1$mouth_max, na.rm=T)
#min
mouth_min <- mouth_data %>% select(mouth_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(mouth_min) | mouth_min == "NaN" | mouth_min == "Inf") mouth_min <- median(g1$mouth_min, na.rm=T)
#diff
mouth_diff <- mouth_max-mouth_min
#first
mouth_first <- mouth_data %>% select(mouth_first=feature_value) %>% head(1)
if(nrow(mouth_first) == 0 ) mouth_first <- median(g1$mouth_first, na.rm=T)
#length
mouth_len <- mouth_data %>% select(mouth_len=feature_value) %>% as.matrix %>% length()
if(is.na(mouth_len) ) mouth_len <- median(g1$mouth_len, na.rm=T)

###

respiratory_data <- alsfrs %>% filter(feature_name=='respiratory' & feature_delta <= 91) %>% arrange(feature_name, feature_delta)

respiratory_mean <- als_data %>% select(respiratory_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(respiratory_mean == "NaN" | is.na(respiratory_mean)) respiratory_mean <- median(g1$respiratory_mean, na.rm=T)

# max
respiratory_max <- respiratory_data %>% select(respiratory_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(respiratory_max) | respiratory_max == "NaN" | respiratory_max == "-Inf") respiratory_max <- median(g1$respiratory_max, na.rm=T)
#min
respiratory_min <- respiratory_data %>% select(respiratory_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(respiratory_min) | respiratory_min == "NaN" | respiratory_min == "Inf") respiratory_min <- median(g1$respiratory_min, na.rm=T)
#diff
respiratory_diff <- respiratory_max-respiratory_min
#first
respiratory_first <- respiratory_data %>% select(respiratory_first=feature_value) %>% head(1)
if(nrow(respiratory_first) == 0 ) respiratory_first <- median(g1$respiratory_first, na.rm=T)
#length
respiratory_len <- respiratory_data %>% select(respiratory_len=feature_value) %>% as.matrix %>% length()
if(is.na(respiratory_len) ) respiratory_len <- median(g1$respiratory_len, na.rm=T)

###########
### alshx
###########

alshx_data <- alshx %>% filter(feature_name=='onset_delta')
onset_delta <- alshx_data %>% select(onset_delta=feature_value) %>% head(1)
if(nrow(onset_delta) == 0 ) onset_delta <- median(g1$onset_delta, na.rm=T)

###########
### subject id
###########

SubjectID <- dat[1,1]

###############
###############

############ merge data

preddata <- data.frame(alsfrs_mean = as.vector(alsfrs_mean),
                   alsfrs_max = as.vector(alsfrs_max),
                   alsfrs_min = as.vector(alsfrs_min),
                   alsfrs_diff = as.vector(alsfrs_diff),
                   alsfrs_first = as.vector(alsfrs_first),
                   alsfrs_len = as.vector(alsfrs_len),
                   alsfrs_slope = as.vector(alsfrs_slope),
                   hands_mean = as.vector(hands_mean),
                   hands_max = as.vector(hands_max),
                   hands_min = as.vector(hands_min),
                   hands_diff = as.vector(hands_diff),
                   hands_first = as.vector(hands_first),
                   hands_len = as.vector(hands_len),
                   leg_mean = as.vector(leg_mean),
                   leg_max = as.vector(leg_max),
                   leg_min = as.vector(leg_min),
                   leg_diff = as.vector(leg_diff),
                   leg_first = as.vector(leg_first),
                   leg_len = as.vector(leg_len),
                   mouth_mean = as.vector(mouth_mean),
                   mouth_max = as.vector(mouth_max),
                   mouth_min = as.vector(mouth_min),
                   mouth_diff = as.vector(mouth_diff),
                   mouth_first = as.vector(mouth_first),
                   mouth_len = as.vector(mouth_len),
                   respiratory_mean = as.vector(respiratory_mean),
                   respiratory_max = as.vector(respiratory_max),
                   respiratory_min = as.vector(respiratory_min),
                   respiratory_diff = as.vector(respiratory_diff),
                   respiratory_first = as.vector(respiratory_first),
                   respiratory_len = as.vector(respiratory_len),
                   onset_delta = as.vector(onset_delta),
                   SubjectID = SubjectID)


print(str(preddata))
                  
#print("preddata:")
#print(preddata)

#preddata[is.na(preddata)] <- 0

detach("package:dplyr", unload=TRUE)

library(caretEnsemble)
print("ok")

pred1 <- predict(mod1, newdata=rbind(preddata,preddata))[1]
pred2 <- predict(mod2, newdata=rbind(preddata,preddata))[1]
pred3 <- predict(mod3, newdata=rbind(preddata,preddata))[1]
pred4 <- predict(mod4, newdata=rbind(preddata,preddata))[1]
pred5 <- predict(mod5, newdata=rbind(preddata,preddata))[1]



preds <- c(pred1,pred2,pred3,pred4,pred5)

insec <- sd(preds)

outdat <- cbind(mean(preds),insec)

write.table(outdat,output_file_path,sep="|",row.names=F,col.names=F,quote=F)



