set.seed(56)
library(plyr)
library(dplyr)
library(reshape2)

dat1 <- read.csv("/alsdream/training_data/proact/all_forms_PROACT.txt", header=T, sep="|") %>%
  arrange(SubjectID, feature_name, feature_delta) %>%
  slice(which(feature_value!="-"))

dat2 <- read.csv("/alsdream/training_data/proact/all_forms_validate_spike.txt", header=T, sep="|") %>%
  arrange(SubjectID, feature_name, feature_delta) %>%
  slice(which(feature_value!="-"))

dat3 <- read.csv("/alsdream/training_data/proact/all_forms_validate_leader.txt", header=T, sep="|") %>%
  arrange(SubjectID, feature_name, feature_delta) %>%
  slice(which(feature_value!="-"))


dat <- rbind(dat1, dat2, dat3)



dat <- dat %>% filter(feature_name=="onset_site" | feature_name=="onset_delta"
                      | feature_name=="fvc_percent" | feature_name=="fvc"
                      | feature_name=="Age" | feature_name=="ALSFRS_Total")

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


age.score <- demographic %>% filter(feature_name=="Age") %>%
  dcast(SubjectID~feature_name, value.var='feature_value')
age.score$Age <- as.numeric(age.score$Age)

##################
### alsfrs
##################

alsfrs_bySubject <- alsfrs %>%
  filter(feature_name=="ALSFRS_Total" & feature_delta <= 91) %>%
  group_by(SubjectID)


alsfrs.vars <- summarise(alsfrs_bySubject,
                         alsfrs_mean=mean(feature_value, na.rm = TRUE),
                         alsfrs_sd=sd(feature_value, na.rm = TRUE),
                         alsfrs_max=max(feature_value, na.rm = TRUE),
                         alsfrs_min=min(feature_value, na.rm = TRUE),
                         alsfrs_diff=alsfrs_max-alsfrs_min,
                         alsfrs_first=feature_value[1],
                         alsfrs_last=last(feature_value),
                         alsfrs_len=length(feature_value),
                         alsfrs_minmax=(max(feature_value) - min(feature_value)) / 
                           (first(feature_delta[feature_value == max(feature_value)]) - 
                              last(feature_delta[feature_value == min(feature_value)])))

alsfrs.fit <- alsfrs_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(alsfrs_slope = coef(fit)[2])


alsfrs.score <- cbind(alsfrs.vars, alsfrs.fit)


############
### alshx
############

alshx.score <- alshx %>% dcast(SubjectID ~ feature_name, value.var='feature_value', na.rm=TRUE)
alshx.score$onset_delta <- as.numeric(alshx.score$onset_delta)

alshx.score$onset_site <- ifelse(alshx.score$onset_site=="Other","Limb",alshx.score$onset_site)
alshx.score$onset_site <- ifelse(alshx.score$onset_site=="Limb and Bulbar","Bulbar",alshx.score$onset_site)
alshx.score$onset_site <- ifelse(alshx.score$onset_site=="Bulbar",1,0) # to binary

#############
### FVC
#############


fvc_bySubject <- fvc %>%
  filter(feature_name=="fvc" & feature_value != "NaN" & feature_delta <= 91) %>%
  group_by(SubjectID) %>% arrange(feature_delta)

fvc_bySubject$feature_value <- as.numeric(as.character(fvc_bySubject$feature_value))

fvc.vars <- summarise(fvc_bySubject,
                      fvc_mean=mean(feature_value, na.rm = TRUE),
                      fvc_sd=sd(feature_value, na.rm = TRUE),
                      fvc_max=max(feature_value, na.rm = TRUE),
                      fvc_min=min(feature_value, na.rm = TRUE),
                      fvc_diff=fvc_max-fvc_min,
                      fvc_first=feature_value[1],
                      fvc_last=last(feature_value),
                      fvc_len=length(feature_value))


fvc.fit <- fvc_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(fvc_slope = coef(fit)[2])

fvc.score <- cbind(fvc.vars, fvc.fit)

###

fvcp_bySubject <- fvc %>%
  filter(feature_name=="fvc_percent" & feature_value != "NaN" & feature_delta <= 91) %>%
  group_by(SubjectID) %>% arrange(feature_delta)

fvcp_bySubject$feature_value <- as.numeric(as.character(fvcp_bySubject$feature_value))

fvcp.vars <- summarise(fvcp_bySubject,
                       fvcp_mean=mean(feature_value, na.rm = TRUE),
                       fvcp_sd=sd(feature_value, na.rm = TRUE),
                       fvcp_max=max(feature_value, na.rm = TRUE),
                       fvcp_min=min(feature_value, na.rm = TRUE),
                       fvcp_diff=fvcp_max-fvcp_min,
                       fvcp_first=feature_value[1],
                       fvcp_last=last(feature_value),
                       fvcp_len=length(feature_value))


fvcp.fit <- fvcp_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(fvcp_slope = coef(fit)[2])

fvcp.score <- cbind(fvcp.vars, fvcp.fit)


###############
###############

############ merge data 

data <- Reduce(function(x, y) merge(x, y, all=TRUE, by="SubjectID"),
               list(age.score, alsfrs.score, fvc.score, alshx.score, fvcp.score))

data$PROACT <- ifelse(data$SubjectID %in% dat1$SubjectID, 0,1)

#print(head(data))
#print(colnames(data))

# load survival outcome

slope1 <- read.csv("/alsdream/training_data/proact/surv_response_PROACT.txt", header=T, sep="|")
slope1 <- slope1[-which(slope1$time_event > 2500),] # delete outlier

slope2 <- read.csv("/alsdream/training_data/proact/surv_response_validate_spike.txt", header=T, sep="|")
slope3 <- read.csv("/alsdream/training_data/proact/surv_response_validate_leader.txt", header=T, sep="|")

slope <- rbind(slope1, slope2, slope3)

gold <- merge(data, slope, by="SubjectID", all.y=TRUE)

# delete SubjectID for learning
gold$SubjectID <- NULL

print(summary(gold))
print(dim(gold))

# delete rows with more than 40% missing
gold <- gold[-which(rowMeans(is.na(gold)) > 0.4), ]

gold$fvc_len[which(is.na(gold$fvc_len))] <- 0
gold$fvcp_len[which(is.na(gold$fvcp_len))] <- 0
gold$alsfrs_len[which(is.na(gold$alsfrs_len))] <- 0

library(mice)
imp <- mice(gold)

print(colnames(gold))

library(MatchIt)


### 1
g1 <- complete(imp, 1)
###
m1 <- matchit(PROACT ~ time_event + status + Age + alsfrs_slope + onset_delta, data=g1, method="genetic", ratio=1, discard="control", pop.size=200)
gold1 <- match.data(m1)
gold1$weights <- NULL
gold1$distance <- NULL
gold1$PROACT <- NULL


### 2
g2 <- complete(imp, 2)
###
m2 <- matchit(PROACT ~ time_event + status + Age + alsfrs_slope + onset_delta, data=g2, method="genetic", ratio=1, discard="control", pop.size=200)
gold2 <- match.data(m2)
gold2$weights <- NULL
gold2$distance <- NULL
gold2$PROACT <- NULL


### 3
g3 <- complete(imp, 3)
###
m3 <- matchit(PROACT ~ time_event + status + Age + alsfrs_slope + onset_delta, data=g3, method="genetic", ratio=1, discard="control", pop.size=200)
gold3 <- match.data(m3)
gold3$weights <- NULL
gold3$distance <- NULL
gold3$PROACT <- NULL


### 4
g4 <- complete(imp, 4)
###
m4 <- matchit(PROACT ~ time_event + status + Age + alsfrs_slope + onset_delta, data=g4, method="genetic", ratio=1, discard="control", pop.size=200)
gold4 <- match.data(m4)
gold4$weights <- NULL
gold4$distance <- NULL
gold4$PROACT <- NULL


### 5
g5 <- complete(imp, 5)
###
m5 <- matchit(PROACT ~ time_event + status + Age + alsfrs_slope + onset_delta, data=g5, method="genetic", ratio=1, discard="control", pop.size=200)
gold5 <- match.data(m5)
gold5$weights <- NULL
gold5$distance <- NULL
gold5$PROACT <- NULL


detach("package:dplyr", unload=TRUE)

library(caret)

# delete highly correlated variables

print(colnames(gold1))

descrCor <-  cor(gold1)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
gold1 <- gold1[,-highlyCorDescr]
gold2 <- gold2[,-highlyCorDescr]
gold3 <- gold3[,-highlyCorDescr]
gold4 <- gold4[,-highlyCorDescr]
gold5 <- gold5[,-highlyCorDescr]

print("dim gold1 + colnames")
print(dim(gold1))
print(colnames(gold1))


library(randomForestSRC)

mod1 <- rfsrc(Surv(time_event,status)~., data=gold1, na.action = "na.impute")

mod2 <- rfsrc(Surv(time_event,status)~., data=gold2, na.action = "na.impute")

mod3 <- rfsrc(Surv(time_event,status)~., data=gold3, na.action = "na.impute")

mod4 <- rfsrc(Surv(time_event,status)~., data=gold4, na.action = "na.impute")

mod5 <- rfsrc(Surv(time_event,status)~., data=gold5, na.action = "na.impute")


rm(list=setdiff(ls(), c("mod1", "mod2", "mod3", "mod4", "mod5","gold1","gold2","gold3","gold4","gold5")))
save.image("~/als_submission/ProActSurvival/model_final_pro_surv.Rdata")

