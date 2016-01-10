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

print(dat)

load("~/als_submission/ProActSurvival/model_final_pro_surv.Rdata")

sID <- dat[1,1]

####### datacleaning and filtering

# demographic
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
alsfrs$feature_delta <- as.numeric(as.character(alsfrs$feature_delta))   # to numeric
alsfrs$feature_value <- as.numeric(as.character(alsfrs$feature_value))   # to numeric

# FVC
fvc <- filter(dat, form_name=="FVC")
fvc$form_name <- factor(fvc$form_name)  # delete unused factor levels
fvc$feature_name <- factor(fvc$feature_name)  # delete unused factor levels
fvc$feature_value <- factor(fvc$feature_value)  # delete unused factor levels
fvc$feature_delta <- factor(fvc$feature_delta)  # delete unused factor levels
fvc$feature_unit <- NULL
fvc$feature_delta <- as.numeric(as.character(fvc$feature_delta))  # to numeric
fvc$feature_value <- as.numeric(as.character(fvc$feature_value))  # to numeric

# ALSHX
alshx <- filter(dat, form_name=="ALSHX")
alshx$feature_unit <- NULL
alshx$feature_delta <- NULL
alshx$feature_name <- factor(alshx$feature_name)  # delete unused factor levels
alshx$feature_value <- factor(alshx$feature_value)  # delete unused factor levels
#alshx$feature_value <- as.numeric(as.character(alshx$feature_value))   # to numeric





#################################################
###### generate variables

age <- demographic %>% filter(feature_name=="Age") %>% select(Age=feature_value)
if(nrow(age) == 0) age <- NA#57.23

################

als_data <- alsfrs %>% filter(feature_name=='ALSFRS_Total' & feature_delta <= 91) %>% 
  arrange(feature_name, feature_delta)

# sd
alsfrs_sd <- als_data %>% select(alsfrs_sd=feature_value) %>% as.matrix %>% sd(na.rm = TRUE)
if(is.na(alsfrs_sd) | alsfrs_sd == "NaN") alsfrs_sd <- NA#1.2583
# max
alsfrs_max <- als_data %>% select(alsfrs_max=feature_value) %>% as.matrix %>% max(na.rm = TRUE)
if(is.na(alsfrs_max) | alsfrs_max == "NaN" | alsfrs_max == "-Inf") alsfrs_max <- median(gold1$alsfrs_max, na.rm=T)
#min
alsfrs_min <- als_data %>% select(alsfrs_min=feature_value) %>% as.matrix %>% min(na.rm = TRUE)
if(is.na(alsfrs_min) | alsfrs_min == "NaN" | alsfrs_min == "Inf") alsfrs_min <- 23#median(gold1$alsfrs_min, na.rm=T)
#diff
alsfrs_diff <- alsfrs_max-alsfrs_min
#length
alsfrs_len <- als_data %>% select(alsfrs_len=feature_value) %>% as.matrix %>% length()
if(is.na(alsfrs_len) | alsfrs_len == "NaN") alsfrs_len <- 0
# minmax
alsfrs_minmax <- als_data %>% #group_by(SubjectID) %>%
  summarise(alsfrs_minmax=(max(feature_value) - min(feature_value)) / 
              (first(feature_delta[feature_value == max(feature_value)]) - 
                 last(feature_delta[feature_value == min(feature_value)])))
if(nrow(alsfrs_minmax) == 0 | alsfrs_minmax == "NaN" | is.na(alsfrs_minmax)) alsfrs_minmax <- NA#-0.03509

#slope
alsfrs_s <- als_data %>%
  group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(alsfrs_slope = coef(fit)[2])

alsfrs_slope <- as.numeric(unlist(alsfrs_s$alsfrs_slope))[1]
if(is.na(as.numeric(unlist(alsfrs_s$alsfrs_slope))[1])) alsfrs_slope <- NA#median(gold1$alsfrs_slope, na.rm=T)


###############

fvc$feature_value <- as.numeric(as.character(fvc$feature_value))

fvc_data <- fvc %>% filter(feature_name=='fvc_percent' & feature_delta <= 91 & feature_value != "NaN") %>%
  arrange(feature_name, feature_delta)

# sd
fvc_sd <- fvc_data %>% select(fvc_sd=feature_value) %>% as.matrix %>% sd(na.rm = TRUE)
if(is.na(fvc_sd) | fvc_sd == "NaN") fvc_sd <- NA#4.691
#first
fvc_first <- fvc_data %>% select(fvc_first=feature_value) %>% head(1)
if(nrow(fvc_first) == 0) fvc_first <- NA#median(g1$fvc_first, na.rm=T)
#length
fvc_len <- fvc_data %>% select(fvc_len=feature_value) %>% as.matrix %>% length()
if(fvc_len == 0 ) fvc_len <- 0
#slope
fvc_s <- fvc_data %>%
  group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(fvc_slope = coef(fit)[2])
fvc_slope <- as.numeric(unlist(fvc_s$fvc_slope))[1]

if(is.na(as.numeric(unlist(fvc_s$fvc_slope))[1])) fvc_slope <- NA#median(gold1$fvc_slope, na.rm=T)


####

fvcp_data <- fvc %>% filter(feature_name=='fvc_percent' & feature_delta <= 91 & feature_value != "NaN") %>%
  arrange(feature_name, feature_delta)

# sd
fvcp_sd <- fvcp_data %>% select(fvcp_sd=feature_value) %>% as.matrix %>% sd(na.rm = TRUE)
if(is.na(fvcp_sd) | fvcp_sd == "NaN") fvcp_sd <- NA#4.691
#first
fvcp_first <- fvcp_data %>% select(fvcp_first=feature_value) %>% head(1)
if(nrow(fvcp_first) == 0) fvcp_first <- NA#median(gold1$fvcp_first, na.rm=T)
#last
fvcp_last <- fvcp_data %>% select(fvcp_last=feature_value) %>% as.matrix %>% last(default=NA)
#length
fvcp_len <- fvcp_data %>% select(fvcp_len=feature_value) %>% as.matrix %>% length()
if(fvcp_len == 0 ) fvcp_len <- 0
#slope
fvcp_s <- fvcp_data %>%
  group_by(SubjectID) %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(fvcp_slope = coef(fit)[2])
fvcp_slope <- as.numeric(unlist(fvcp_s$fvcp_slope))[1]

if(is.na(as.numeric(unlist(fvcp_s$fvcp_slope))[1])) fvcp_slope <- NA#median(g1$fvcp_slope, na.rm=T)


###########
### alshx
###########

delta_data <- alshx %>% filter(feature_name=='onset_delta')
delta_data$feature_value <- as.numeric(as.character(delta_data$feature_value))
onset_delta <- delta_data$feature_value[1]

onset_site <- alshx %>% filter(feature_name=="onset_site") %>% select(onset_site=feature_value)# %>% first()
if(nrow(onset_site)==0) onset_site <- "Limb"
#if(onset_site=="Other") onset_site <- "Limb"
if(onset_site=="Limb and Bulbar") onset_site <- "Bulbar"
onset_site <- ifelse(onset_site=="Bulbar",1,0) # to binary


preddata <- data.frame(Age = as.vector(age),
                       alsfrs_sd = as.vector(alsfrs_sd),
                       alsfrs_diff = as.vector(alsfrs_diff),
                       alsfrs_max = as.vector(alsfrs_max),
                       alsfrs_len = as.vector(alsfrs_len),
                       alsfrs_minmax = as.vector(alsfrs_minmax),
                       alsfrs_slope = as.vector(alsfrs_slope),
                       fvc_sd = as.vector(fvc_sd),
                       fvc_first = as.vector(fvc_first),
                       fvc_len = as.vector(fvc_len),
                       fvc_slope = as.vector(fvc_slope),
                       fvcp_sd = as.vector(fvcp_sd),
                       fvcp_first = as.vector(fvcp_first),
                       fvcp_last = as.vector(fvcp_last),
                       fvcp_len = as.vector(fvcp_len),
                       fvcp_slope = as.vector(fvcp_slope),
                       onset_delta = as.vector(onset_delta),
                       onset_site = as.vector(onset_site))


print("preddata:")
print(preddata)
print(str(preddata))

detach("package:dplyr", unload=TRUE)

library(randomForestSRC)
print("ok")

#load("~/als_submission/ProActSurvival/model_final_pro_surv.Rdata")
pred1 <- predict(mod1, newdata=preddata, na.action="na.impute")
pred2 <- predict(mod2, newdata=preddata, na.action="na.impute")
pred3 <- predict(mod3, newdata=preddata, na.action="na.impute")
pred4 <- predict(mod4, newdata=preddata, na.action="na.impute")
pred5 <- predict(mod5, newdata=preddata, na.action="na.impute")


pred12 <- c(pred1$survival[,62], pred2$survival[,62], pred3$survival[,62],
            pred4$survival[,62], pred5$survival[,62]) 
pred18 <- c(pred1$survival[,117], pred2$survival[,117], pred3$survival[,117],   
            pred4$survival[,117], pred5$survival[,117])
pred24 <- c(pred1$survival[,145], pred2$survival[,145], pred3$survival[,145],   
            pred4$survival[,145], pred5$survival[,145])


print(pred12)
print(pred18)
print(pred24)

# 12,18,24 months
outdat <- cbind(mean(pred12), mean(pred18), mean(pred24))
print(outdat)

write.table(outdat,output_file_path,sep="|",row.names=F,col.names=F,quote=F)

















