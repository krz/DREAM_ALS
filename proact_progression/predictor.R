library(plyr)
library(dplyr)
library(reshape2)
load("~/als_submission/ProActProgression/model_final_pro_slope.Rdata")

print(str(gold1))

# Specify the input/output file path
args<-commandArgs(trailingOnly = TRUE)      #to pass the selector.sh arguments to R
input_file_path<-args[1]
output_file_path<-args[2]

# Read the input file

dat <- read.csv(input_file_path, header=T, sep="|")

colnames(dat) <- c("SubjectID", "form_name", "feature_name", "feature_value", "feature_unit", "feature_delta")

sID <- dat[1,1]

####### datacleaning and filtering

#alsfrs
alsfrs <- filter(dat, form_name=="ALSFRS")
alsfrs$feature_unit <- NULL
alsfrs$form_name <- factor(alsfrs$form_name)  # delete unused factor levels
alsfrs$feature_name <- factor(alsfrs$feature_name)  # delete unused factor levels
alsfrs$feature_value <- factor(alsfrs$feature_value)  # delete unused factor levels
alsfrs$feature_delta <- factor(alsfrs$feature_delta)  # delete unused factor levels
alsfrs$feature_delta <- as.numeric(as.character(alsfrs$feature_delta))   # to numeric
alsfrs$feature_value <- as.numeric(as.character(alsfrs$feature_value))   # to numeric

# ALSHX
alshx <- filter(dat, form_name=="ALSHX")
alshx$feature_unit <- NULL
alshx$feature_delta <- NULL
alshx$feature_name <- factor(alshx$feature_name)  # delete unused factor levels
alshx$feature_value <- factor(alshx$feature_value)  # delete unused factor levels
alshx$feature_value <- as.numeric(as.character(alshx$feature_value))   # to numeric

# FVC
fvc <- filter(dat, form_name=="FVC")
fvc$form_name <- factor(fvc$form_name)  # delete unused factor levels
fvc$feature_name <- factor(fvc$feature_name)  # delete unused factor levels
fvc$feature_value <- factor(fvc$feature_value)  # delete unused factor levels
fvc$feature_delta <- factor(fvc$feature_delta)  # delete unused factor levels
fvc$feature_unit <- NULL
fvc$feature_delta <- as.numeric(as.character(fvc$feature_delta))  # to numeric
fvc$feature_value <- as.numeric(as.character(fvc$feature_value))  # to numeric

# Lab Test
lab <- filter(dat, form_name=="Lab Test")
lab$feature_name <- factor(lab$feature_name)  # delete unused factor levels
lab$feature_value <- factor(lab$feature_value)  # delete unused factor levels
lab$feature_delta <- factor(lab$feature_delta)  # delete unused factor levels
lab$feature_unit <- NULL
lab$feature_delta <- as.numeric(as.character(lab$feature_delta))  # to numeric
lab$feature_value <- as.numeric(as.character(lab$feature_value))  # to numeric

# Vitals
vit <- filter(dat, form_name=="Vitals")
vit$feature_name <- factor(vit$feature_name)  # delete unused factor levels
vit$feature_value <- factor(vit$feature_value)  # delete unused factor levels
vit$feature_delta <- factor(vit$feature_delta)  # delete unused factor levels
vit$feature_unit <- NULL
vit$feature_delta <- as.numeric(as.character(vit$feature_delta))  # to numeric
vit$feature_value <- as.numeric(as.character(vit$feature_value))  # to numeric

#################################################
###### generate variables


##################
### alsfrs
##################


als_data <- alsfrs %>% filter(feature_name=='ALSFRS_Total' & feature_delta <= 91) %>% 
  arrange(feature_name, feature_delta)

# max
alsfrs_max <- als_data %>% select(alsfrs_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.infinite(alsfrs_max)) alsfrs_max <- 32.0
#min
alsfrs_min <- als_data %>% select(alsfrs_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.infinite(alsfrs_min) | is.null(alsfrs_min)) alsfrs_min <- 30.0
#diff
alsfrs_diff <- try(alsfrs_max - alsfrs_min)
if(class(alsfrs_diff) == "try-error") alsfrs_diff <- median(gold1$alsfrs_diff, na.rm=T)


#first
alsfrs_first <- als_data %>% select(alsfrs_first=feature_value) %>% head(1)
if(nrow(alsfrs_first) == 0 ) alsfrs_first <- median(gold1$alsfrs_first, na.rm=T)
#length
alsfrs_len <- als_data %>% select(alsfrs_len=feature_value) %>% as.matrix %>% length()
if(is.na(alsfrs_len) | alsfrs_len == "NaN") alsfrs_len <- median(gold1$alsfrs_len, na.rm=T)
# minmax
alsfrs_minmax <- als_data %>% 
  summarise(alsfrs_minmax=(max(feature_value) - min(feature_value)) / 
              (first(feature_delta[feature_value == max(feature_value)]) - 
                 last(feature_delta[feature_value == min(feature_value)])))
if(nrow(alsfrs_minmax) == 0 | alsfrs_minmax == "NaN" | is.na(alsfrs_minmax)) alsfrs_minmax <- median(gold1$alsfrs_minmax, na.rm=T)
#slope
alsfrs_slope <- als_data %>% group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(alsfrs_slope = coef(fit)[2])                
alsfrs_slope <- as.numeric(unlist(alsfrs_slope))
if(is.na(alsfrs_slope[1])) alsfrs_slope <- median(gold1$alsfrs_slope, na.rm=T)


############
### fvc
############

fvc$feature_value <- as.numeric(as.character(fvc$feature_value))

fvcp_data <- fvc %>% filter(feature_name=="fvc_percent" & feature_value != "NaN" &
                             feature_delta <= 91) %>% arrange(feature_name, feature_delta)

# sd
fvcp_sd <- fvcp_data %>% select(fvcp_sd=feature_value) %>% as.matrix %>% sd(na.rm = TRUE)
if(is.na(fvcp_sd) | fvcp_sd == "NaN") fvcp_sd <- median(gold1$fvcp_sd, na.rm=T)
# max
fvcp_max <- fvcp_data %>% select(fvcp_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE) 
if(is.infinite(fvcp_max)) fvcp_max <- median(gold1$fvcp_max, na.rm=T)
#min
fvcp_min <- fvcp_data %>% select(fvcp_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE) 
if(is.infinite(fvcp_min) | is.null(fvcp_min)) fvcp_min <- 79.73
#diff
fvcp_diff <- try(fvcp_max - fvcp_min)
if(class(fvcp_diff) == "try-error") fvcp_diff <- median(gold1$fvcp_diff, na.rm=T)
#first
fvcp_first <- fvcp_data %>% select(fvcp_first=feature_value) %>% head(1)
if(nrow(fvcp_first) == 0) fvcp_first <- median(gold1$fvcp_first, na.rm=T)
#last
fvcp_last <- fvcp_data %>% select(fvcp_last=feature_value) %>% as.matrix %>% last(default=median(gold1$fvcp_last, na.rm=T))
#length
fvcp_len <- fvcp_data %>% select(fvcp_len=feature_value) %>% as.matrix %>% length()
if(is.na(fvcp_len) ) fvcp_len <- 0
#slope
fvcp_slope <- fvcp_data %>% group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(fvcp_slope = coef(fit)[2])                
fvcp_slope <- as.numeric(unlist(fvcp_slope))
if(is.na(fvcp_slope[1])) fvcp_slope <- median(gold1$fvcp_slope, na.rm=T)


#####

fvc_data <- fvc %>% filter(feature_name=="fvc" & feature_value != "NaN" &
                           feature_delta <= 91) %>% arrange(feature_name, feature_delta)

# sd
fvc_sd <- fvc_data %>% select(fvc_sd=feature_value) %>% as.matrix %>% sd(na.rm = TRUE)
if(is.na(fvc_sd) | fvc_sd == "NaN") fvc_sd <- median(gold1$fvc_sd, na.rm=T)
# max
fvc_max <- fvc_data %>% select(fvc_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE) 
if(is.infinite(fvc_max)) fvc_max <- 3.733
#min
fvc_min <- fvc_data %>% select(fvc_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE) 
if(is.infinite(fvc_min) | is.null(fvc_min)) fvc_min <- 3.290
#diff
fvc_diff <- try(fvc_max - fvc_min)
if(class(fvc_diff) == "try-error") fvc_diff <- median(gold1$fvc_diff, na.rm=T)
#first
fvc_first <- fvc_data %>% select(fvc_first=feature_value) %>% head(1)
if(nrow(fvc_first) == 0) fvc_first <- median(gold1$fvc_first, na.rm=T)
#length
fvc_len <- fvc_data %>% select(fvc_len=feature_value) %>% as.matrix %>% length()
if(is.na(fvc_len) ) fvc_len <- 0#median(gold1$fvc_len, na.rm=T)
#slope
fvc_slope <- fvc_data %>% group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(fvc_slope = coef(fit)[2])                
fvc_slope <- as.numeric(unlist(fvc_slope))
if(is.na(fvc_slope[1])) fvc_slope <- median(gold1$fvc_slope, na.rm=T)

###########
### vitals
###########

vit$feature_value <- as.numeric(as.character(vit$feature_value))

vit_data <- vit %>% filter(feature_name=="weight" & feature_value != "NaN" &
                            feature_delta <= 91) %>% arrange(feature_name, feature_delta)


# mean
weight_mean <- vit_data %>% select(weight_mean=feature_value) %>% colMeans(na.rm = TRUE)
if(weight_mean == "NaN" | is.na(weight_mean)) weight_mean <- median(gold1$weight_mean, na.rm=T)
# min
vit_min <- vit_data %>% select(vit_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE) 
if(is.infinite(vit_min) | is.null(vit_min)) vit_min <- median(gold1$weight_min, na.rm=T)
# sd
vit_sd <- vit_data %>% select(vit_sd=feature_value) %>% as.matrix %>% sd(na.rm = TRUE)
if(is.na(vit_sd) | vit_sd == "NaN") vit_sd <- median(gold1$weight_sd, na.rm=T)
#slope
weight_slope <- vit_data %>% group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(weight_slope = coef(fit)[2])                
weight_slope <- as.numeric(unlist(weight_slope))
if(is.na(weight_slope[1])) weight_slope <- median(gold1$weight_slope, na.rm=T)


###########
### alshx
###########

alshx_data <- alshx %>% filter(feature_name=='onset_delta')
onset_delta <- alshx_data %>% select(onset_delta=feature_value) %>% head(1)
if(nrow(onset_delta) == 0 ) onset_delta <- median(gold1$onset_delta, na.rm=T)

###########
### lab
###########
lab$feature_value <- as.numeric(as.character(lab$feature_value))
# only Hemoglobin
#intercept
Hemoglobin <- lab %>% filter(feature_name=="Hemoglobin" & feature_delta <= 91) %>%
  group_by(SubjectID) %>%
  do({if(all(is.na(.$feature_value)) | all(is.na(.$feature_delta)))
    data.frame(Hemoglobin = median(gold1$Hemoglobin, na.rm=T))
    else data.frame(Hemoglobin = coef(lm(feature_value ~ feature_delta, .))[1])}) %>%
  slice(1L) %>% ungroup %>% select(-SubjectID)
if(nrow(Hemoglobin) == 0) Hemoglobin <- median(gold1$Hemoglobin, na.rm=T)


###############
###############

############ merge data

preddata <- data.frame(alsfrs_minmax = as.vector(alsfrs_minmax),
                       alsfrs_diff = log(as.vector(alsfrs_diff) + 1),
                       alsfrs_first = as.vector(alsfrs_first),
                       alsfrs_slope = as.vector(alsfrs_slope),
                       alsfrs_len = as.vector(alsfrs_len),
                       fvc_sd = log(as.vector(fvc_sd) + 1),
                       fvc_diff = log(as.vector(fvc_diff) + 1),
                       fvc_first = as.vector(fvc_first),
                       fvc_len = as.vector(fvc_len),
                       fvc_slope = as.vector(fvc_slope),
                       weight_mean = as.vector(weight_mean),
                       weight_min = as.vector(vit_min),
                       weight_sd = log(as.vector(vit_sd) + 1),
                       weight_slope = as.vector(weight_slope),
                       fvcp_sd = log(as.vector(fvcp_sd) + 1),
                       fvcp_max = as.vector(fvcp_max),
                       fvcp_diff = as.vector(fvcp_diff),
                       fvcp_first = as.vector(fvcp_first),
                       fvcp_slope = as.vector(fvcp_slope),
                       fvcp_last = as.vector(fvcp_last),
                       onset_delta = as.vector(onset_delta),
                       Hemoglobin = as.vector(Hemoglobin))



print("preddata:")
print(preddata)
print(str(preddata))

#preddata[is.na(preddata)] <- 0


detach("package:dplyr", unload=TRUE)

library(caretEnsemble)
print("ok")

#load("~/als_submission/ProActProgression/model_final_pro_slope.Rdata")
pred1 <- predict(mod1, newdata=rbind(preddata,preddata))[1]
pred2 <- predict(mod2, newdata=rbind(preddata,preddata))[1]
pred3 <- predict(mod3, newdata=rbind(preddata,preddata))[1]
pred4 <- predict(mod4, newdata=rbind(preddata,preddata))[1]
pred5 <- predict(mod5, newdata=rbind(preddata,preddata))[1]


print(pred1)
print(pred2)
print(pred3)
print(pred4)
print(pred5)

predframe <- cbind(pred1, pred2, pred3, pred4, pred5)
print(predframe)
pred <- mean(predframe)
print(pred)

insec <- max(predframe) - min(predframe)

outdat <- cbind(pred,insec)
#outdat <- pred

#print(outdat)
#write outdat
write.table(outdat,output_file_path,sep="|",row.names=F,col.names=F,quote=F)



