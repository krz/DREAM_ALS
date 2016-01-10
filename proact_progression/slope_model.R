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






dat <- dat %>% filter(feature_name=="onset_delta" | feature_name=="weight"
                      | feature_name=="Hemoglobin" | feature_name=="fvc"
                      | feature_name=="fvc_percent" | feature_name=="ALSFRS_Total")

####### datacleaning and filtering
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

alsfrs_bySubject <- alsfrs %>%
  filter(feature_name=="ALSFRS_Total" & feature_delta <= 91) %>%
  group_by(SubjectID)

#alsfrs_bySubject$feature_name <- factor(alsfrs_bySubject$feature_name)  # delete unused factor levels

alsfrs.vars <- summarise(alsfrs_bySubject,
                         alsfrs_mean=mean(feature_value, na.rm = TRUE),
                         alsfrs_sd=sd(feature_value, na.rm = TRUE),
                         alsfrs_max=max(feature_value, na.rm = TRUE),
                         alsfrs_min=min(feature_value, na.rm = TRUE),
                         alsfrs_diff=alsfrs_max-alsfrs_min,
                         alsfrs_first=first(feature_value),
                         alsfrs_last=last(feature_value),
                         alsfrs_len=length(feature_value),
                         alsfrs_minmax=(max(feature_value) - min(feature_value)) / 
                           (first(feature_delta[feature_value == max(feature_value)]) - 
                              last(feature_delta[feature_value == min(feature_value)])))

alsfrs.fit <- alsfrs_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(alsfrs_intercept = coef(fit)[1],
            alsfrs_slope = coef(fit)[2])
#  summarise(alsfrs_slope = coef(fit)[2])

#alsfrs.mm <- alsfrs_bySubject %>%
#  summarize(alsfrs_minmax=(max(feature_value) - min(feature_value)) / 
#              (first(feature_delta[feature_value == max(feature_value)]) - 
#                 last(feature_delta[feature_value == min(feature_value)])) )

alsfrs.score <- cbind(alsfrs.vars, alsfrs.fit)


############
### alshx
############

alshx.score <- alshx %>% filter(feature_name=="onset_delta") %>%
  dcast(SubjectID ~ feature_name, value.var='feature_value', na.rm=TRUE)
alshx.score$onset_delta <- as.numeric(alshx.score$onset_delta)

############
### fvc
############

fvcp_bySubject <- fvc %>%
  filter(feature_name=="fvc_percent" & feature_value != "NaN" & feature_delta <= 91) %>%
  group_by(SubjectID)

fvcp_bySubject$feature_value <- as.numeric(as.character(fvcp_bySubject$feature_value))

fvcp.vars <- summarise(fvcp_bySubject,
                      fvcp_mean=mean(feature_value, na.rm = TRUE),
                      fvcp_sd=sd(feature_value, na.rm = TRUE),
                      fvcp_max=max(feature_value, na.rm = TRUE),
                      fvcp_min=min(feature_value, na.rm = TRUE),
                      fvcp_diff=fvcp_max-fvcp_min,
                      fvcp_first=first(feature_value),
                      fvcp_last=last(feature_value))

fvcp.fit <- fvcp_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(fvcp_intercept = coef(fit)[1],
            fvcp_slope = coef(fit)[2])

fvcp.score <- cbind(fvcp.vars, fvcp.fit)

######

fvc_bySubject <- fvc %>%
  filter(feature_name=="fvc" & feature_value != "NaN" & feature_delta <= 91) %>%
  group_by(SubjectID)

fvc_bySubject$feature_value <- as.numeric(as.character(fvc_bySubject$feature_value))

fvc.vars <- summarise(fvc_bySubject,
                      fvc_mean=mean(feature_value, na.rm = TRUE),
                      fvc_sd=sd(feature_value, na.rm = TRUE),
                      fvc_max=max(feature_value, na.rm = TRUE),
                      fvc_min=min(feature_value, na.rm = TRUE),
                      fvc_diff=fvc_max-fvc_min,
                      fvc_first=first(feature_value),
                      fvc_last=last(feature_value),
                      fvc_len=length(feature_value))

fvc.fit <- fvc_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(fvc_intercept = coef(fit)[1],
            fvc_slope = coef(fit)[2])

fvc.score <- cbind(fvc.vars, fvc.fit)


###########
### lab
###########
#lab$feature_value <- as.numeric(as.character(lab$feature_value))

# select
lab.complete <- lab %>%
  filter(feature_delta <= 91) %>%
  dcast(SubjectID ~ feature_name, value.var='feature_value', fun.aggregate = function(x) x[1])

lab.complete$Hemoglobin[lab.complete$Hemoglobin < 30 | lab.complete$Hemoglobin > 300] <- NA

###########
### vitals
###########

weight_bySubject <- vit %>% filter(feature_name == "weight" & feature_delta <= 91) %>%
  group_by(SubjectID)

weight.vars <- summarise(weight_bySubject,
                         weight_mean=mean(feature_value, na.rm = TRUE),
                         weight_max=max(feature_value, na.rm = TRUE),
                         weight_min=min(feature_value, na.rm = TRUE),
                         weight_diff=weight_max-weight_min,
                         weight_first=first(feature_value),
                         weight_last=last(feature_value),
                         weight_sd=sd(feature_value))

weight.fit <- weight_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(weight_slope = coef(fit)[2])


weight.score <- cbind(weight.vars, weight.fit)

###############
###############

############ merge data 

data <- Reduce(function(x, y) merge(x, y, all=TRUE, by="SubjectID"),
               list(alshx.score, fvc.score,  weight.score, fvcp.score, alsfrs.score, lab.complete))


data$PROACT <- ifelse(data$SubjectID %in% dat1$SubjectID, 0,1)

# load slope outcome
slope1 <- read.csv("/alsdream/training_data/proact/ALSFRS_slope_PROACT_filtered.txt", header=T, sep="|")

slope2 <- read.csv("/alsdream/training_data/proact/ALSFRS_slope_validate_spike_filtered.txt", header=T, sep="|")

slope3 <- read.csv("/alsdream/training_data/proact/ALSFRS_slope_validate_leader_filtered.txt", header=T, sep="|")

slope <- rbind(slope1, slope2, slope3)

# merge with slope by SubjectID
gold <- merge(data, slope, by="SubjectID", all.y=TRUE)
# delete SubjectID for learning
gold$SubjectID <- NULL

gold$fvc_len[which(is.na(gold$fvc_len))] <- 0

# build model

# delete rows with more than 45% missing
gold <- gold[-which(rowMeans(is.na(gold)) > 0.45), ]

library(mice)
imp <- mice(gold)

library(MatchIt)


### 1
g1 <- complete(imp, 1)
g1$fvc_diff <- log(g1$fvc_diff+1)
g1$fvc_sd <- log(g1$fvc_sd+1)
g1$weight_diff <- log(g1$weight_diff+1)
g1$weight_sd <- log(g1$weight_sd+1)
g1$fvcp_diff <- log(g1$fvcp_diff+1)
g1$fvcp_sd <- log(g1$fvcp_sd+1)
g1$alsfrs_sd <- log(g1$alsfrs_sd+1)
g1$alsfrs_diff <- log(g1$alsfrs_diff+1)
m1 <- matchit(PROACT ~ ALSFRS_slope + alsfrs_slope + fvc_slope + fvcp_mean + weight_mean +
                onset_delta, data=g1, method="genetic", ratio=2, discard="control", pop.size=200)
gold1 <- match.data(m1)
gold1$weights <- NULL
gold1$distance <- NULL
gold1$PROACT <- NULL

### 2
g2 <- complete(imp, 2)
g2$fvc_diff <- log(g2$fvc_diff+1)
g2$fvc_sd <- log(g2$fvc_sd+1)
g2$weight_diff <- log(g2$weight_diff+1)
g2$weight_sd <- log(g2$weight_sd+1)
g2$fvcp_diff <- log(g2$fvcp_diff+1)
g2$fvcp_sd <- log(g2$fvcp_sd+1)
g2$alsfrs_sd <- log(g2$alsfrs_sd+1)
g2$alsfrs_diff <- log(g2$alsfrs_diff+1)
m2 <- matchit(PROACT ~ ALSFRS_slope + alsfrs_slope + fvc_slope + fvcp_mean + weight_mean +
                onset_delta, data=g2, method="genetic", ratio=2, discard="control", pop.size=200)
gold2 <- match.data(m2)
gold2$weights <- NULL
gold2$distance <- NULL
gold2$PROACT <- NULL

### 3
g3 <- complete(imp, 3)
g3$fvc_diff <- log(g3$fvc_diff+1)
g3$fvc_sd <- log(g3$fvc_sd+1)
g3$weight_diff <- log(g3$weight_diff+1)
g3$weight_sd <- log(g3$weight_sd+1)
g3$fvcp_diff <- log(g3$fvcp_diff+1)
g3$fvcp_sd <- log(g3$fvcp_sd+1)
g3$alsfrs_sd <- log(g3$alsfrs_sd+1)
g3$alsfrs_diff <- log(g3$alsfrs_diff+1)
m3 <- matchit(PROACT ~ ALSFRS_slope + alsfrs_slope + fvc_slope + fvcp_mean + weight_mean +
                onset_delta, data=g3, method="genetic", ratio=2, discard="control", pop.size=200)
gold3 <- match.data(m3)
gold3$weights <- NULL
gold3$distance <- NULL
gold3$PROACT <- NULL

### 4
g4 <- complete(imp, 4)
g4$fvc_diff <- log(g4$fvc_diff+1)
g4$fvc_sd <- log(g4$fvc_sd+1)
g4$weight_diff <- log(g4$weight_diff+1)
g4$weight_sd <- log(g4$weight_sd+1)
g4$fvcp_diff <- log(g4$fvcp_diff+1)
g4$fvcp_sd <- log(g4$fvcp_sd+1)
g4$alsfrs_sd <- log(g4$alsfrs_sd+1)
g4$alsfrs_diff <- log(g4$alsfrs_diff+1)
m4 <- matchit(PROACT ~ ALSFRS_slope + alsfrs_slope + fvc_slope + fvcp_mean + weight_mean +
                onset_delta, data=g4, method="genetic", ratio=2, discard="control", pop.size=200)
gold4 <- match.data(m4)
gold4$weights <- NULL
gold4$distance <- NULL
gold4$PROACT <- NULL

### 5
g5 <- complete(imp, 5)
g5$fvc_diff <- log(g5$fvc_diff+1)
g5$fvc_sd <- log(g5$fvc_sd+1)
g5$weight_diff <- log(g5$weight_diff+1)
g5$weight_sd <- log(g5$weight_sd+1)
g5$fvcp_diff <- log(g5$fvcp_diff+1)
g5$fvcp_sd <- log(g5$fvcp_sd+1)
g5$alsfrs_sd <- log(g5$alsfrs_sd+1)
g5$alsfrs_diff <- log(g5$alsfrs_diff+1)
m5 <- matchit(PROACT ~ ALSFRS_slope + alsfrs_slope + fvc_slope + fvcp_mean + weight_mean +
                onset_delta, data=g5, method="genetic", ratio=2, discard="control", pop.size=200)
gold5 <- match.data(m5)
gold5$weights <- NULL
gold5$distance <- NULL
gold5$PROACT <- NULL


y1 <- gold1$ALSFRS_slope
gold1$ALSFRS_slope <- NULL
y2 <- gold2$ALSFRS_slope
gold2$ALSFRS_slope <- NULL
y3 <- gold3$ALSFRS_slope
gold3$ALSFRS_slope <- NULL
y4 <- gold4$ALSFRS_slope
gold4$ALSFRS_slope <- NULL
y5 <- gold5$ALSFRS_slope
gold5$ALSFRS_slope <- NULL


detach("package:dplyr", unload=TRUE)

library(caret)

# delete highly correlated variables

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

library(caretEnsemble)

fitControl <- trainControl(method = "boot",
                           number = 50,
                           savePredictions=TRUE)


modlist1 <- caretList(
  x=gold1,y=y1,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7)
  )
)

modlist2 <- caretList(
  x=gold2,y=y2,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7)
  )
)

modlist3 <- caretList(
  x=gold3,y=y3,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7)
  )
)

modlist4 <- caretList(
  x=gold4,y=y4,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7)
  )
)

modlist5 <- caretList(
  x=gold5,y=y5,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7)
  )
)


print(modlist1)
print(modlist2)
print(modlist3)
print(modlist4)
print(modlist5)



mod1 <- caretEnsemble(modlist1)
mod2 <- caretEnsemble(modlist2)
mod3 <- caretEnsemble(modlist3)
mod4 <- caretEnsemble(modlist4)
mod5 <- caretEnsemble(modlist5)


rm(list=setdiff(ls(), c("mod1", "mod2", "mod3", "mod4", "mod5", "gold1")))
save.image("~/als_submission/ProActProgression/model_final_pro_slope.Rdata")

