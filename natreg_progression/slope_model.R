set.seed(56)
library(plyr)
library(dplyr)
library(reshape2)

dat <- read.csv("/alsdream/training_data/registries/all_forms_registry_training.txt", header=T, sep="|")
colnames(dat) <- c("SubjectID", "form_name", "feature_name", "feature_value", "feature_unit", "feature_delta")
dat$SubjectID <- gsub("I", "999", as.character(dat$SubjectID))
dat$SubjectID <- as.numeric(as.character(dat$SubjectID))

#dat <- dat %>%
#  arrange(SubjectID, feature_name, feature_delta) %>%
#  slice(which(feature_value!="-"))

dat <- dat %>% filter(feature_name=="hands" | feature_name=="leg"
                      | feature_name=="mouth" | feature_name=="respiratory"
                      | feature_name=="onset_delta" | feature_name=="ALSFRS_R_Total")


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
#alshx$feature_value <- as.numeric(as.character(alshx$feature_value))   # to numeric

#################################################
###### generate variables
#################################################


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
                       #  alsfrs_last=last(feature_value),
                         alsfrs_len=length(feature_value),
                         alsfrs_minmax=(max(feature_value) - min(feature_value)) / 
                           (first(feature_delta[feature_value == max(feature_value)]) - 
                              last(feature_delta[feature_value == min(feature_value)])))

alsfrs.fit <- alsfrs_bySubject %>%
  do(fit = lm(feature_value ~ feature_delta, .)) %>%
  summarise(alsfrs_slope = coef(fit)[2])


alsfrs.score <- cbind(alsfrs.vars, alsfrs.fit)

####

hands_bySubject <- alsfrs %>%
  filter(feature_name=="hands" & feature_delta <= 91) %>%
  group_by(SubjectID)

hands.score <- summarise(hands_bySubject,
                         hands_mean=mean(feature_value, na.rm = TRUE),
                         #alsfrs_sd=sd(feature_value, na.rm = TRUE),
                         hands_max=max(feature_value, na.rm = TRUE),
                         hands_min=min(feature_value, na.rm = TRUE),
                         hands_diff=hands_max-hands_min,
                         hands_first=feature_value[1],
                         hands_last=last(feature_value),
                        # hands_len=length(feature_value),
                         hands_minmax=(max(feature_value) - min(feature_value)) / 
                           (first(feature_delta[feature_value == max(feature_value)]) - 
                              last(feature_delta[feature_value == min(feature_value)])))

####

leg_bySubject <- alsfrs %>%
  filter(feature_name=="leg" & feature_delta <= 91) %>%
  group_by(SubjectID)


leg.score <- summarise(leg_bySubject,
                       leg_mean=mean(feature_value, na.rm = TRUE),
                       #alsfrs_sd=sd(feature_value, na.rm = TRUE),
                       leg_max=max(feature_value, na.rm = TRUE),
                       leg_min=min(feature_value, na.rm = TRUE),
                       leg_diff=leg_max-leg_min,
                       leg_first=feature_value[1],
                       leg_last=last(feature_value),
                     # leg_len=length(feature_value),
                       leg_minmax=(max(feature_value) - min(feature_value)) / 
                           (first(feature_delta[feature_value == max(feature_value)]) - 
                              last(feature_delta[feature_value == min(feature_value)])))

#####


mouth_bySubject <- alsfrs %>%
  filter(feature_name=="mouth" & feature_delta <= 91) %>%
  group_by(SubjectID)


mouth.score <- summarise(mouth_bySubject,
                         mouth_mean=mean(feature_value, na.rm = TRUE),
                         #alsfrs_sd=sd(feature_value, na.rm = TRUE),
                         mouth_max=max(feature_value, na.rm = TRUE),
                         mouth_min=min(feature_value, na.rm = TRUE),
                         mouth_diff=mouth_max-mouth_min,
                         mouth_first=feature_value[1],
                         mouth_last=last(feature_value),
                      #   mouth_len=length(feature_value),
                         mouth_minmax=(max(feature_value) - min(feature_value)) / 
                           (first(feature_delta[feature_value == max(feature_value)]) - 
                              last(feature_delta[feature_value == min(feature_value)])))

####


respiratory_bySubject <- alsfrs %>%
  filter(feature_name=="respiratory" & feature_delta <= 91) %>%
  group_by(SubjectID)


respiratory.score <- summarise(respiratory_bySubject,
                               respiratory_mean=mean(feature_value, na.rm = TRUE),
                               #alsfrs_sd=sd(feature_value, na.rm = TRUE),
                               respiratory_max=max(feature_value, na.rm = TRUE),
                               respiratory_min=min(feature_value, na.rm = TRUE),
                               respiratory_diff=respiratory_max-respiratory_min,
                               respiratory_first=feature_value[1],
                               respiratory_last=last(feature_value),
                         #      respiratory_len=length(feature_value),
                               respiratory_minmax=(max(feature_value) - min(feature_value)) / 
                           (first(feature_delta[feature_value == max(feature_value)]) - 
                              last(feature_delta[feature_value == min(feature_value)])))


############
### alshx
############

alshx.score <- alshx %>% dcast(SubjectID ~ feature_name, value.var='feature_value', na.rm=TRUE)
alshx.score$onset_delta <- as.numeric(alshx.score$onset_delta)


###############
###############


############ merge data


data <- Reduce(function(x, y) merge(x, y, all=TRUE, by="SubjectID"),
               list(alsfrs.score, alshx.score, hands.score, leg.score, mouth.score, respiratory.score))

# load slope outcome

slope <- read.csv("/alsdream/training_data/registries/ALSFRS_slope_registry_train_filtered.txt", header=T, sep="|")
slope$SubjectID <- gsub("I", "999", as.character(slope$SubjectID))
slope$SubjectID <- as.numeric(as.character(slope$SubjectID))

gold <- merge(data, slope, by="SubjectID", all.y=TRUE)

library(mice)
imp <- mice(gold, meth="rf")

y <- gold$ALSFRS_slope


detach("package:dplyr", unload=TRUE)

library(caretEnsemble)

fitControl <- trainControl(method = "boot",
                           number = 50,
                           savePredictions=TRUE)


g1 <- complete(imp, 1)
g1$ALSFRS_slope <- NULL
modlist1 <- caretList(
  x=g1,y=y,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial', 'rf'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7),
    gbm=caretModelSpec(method='rf', 
                             tuneLength = 5)
  )
)

g2 <- complete(imp, 2)
g2$ALSFRS_slope <- NULL
modlist2 <- caretList(
  x=g1,y=y,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial', 'rf'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7),
    rf=caretModelSpec(method='rf', 
                      tuneLength = 5)
  )
)



g3 <- complete(imp, 3)
g3$ALSFRS_slope <- NULL
modlist3 <- caretList(
  x=g3,y=y,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial', 'rf'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7),
    rf=caretModelSpec(method='rf', 
                      tuneLength = 5)
  )
)


g4 <- complete(imp, 4)
g4$ALSFRS_slope <- NULL
modlist4 <- caretList(
  x=g4,y=y,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial', 'rf'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7),
    rf=caretModelSpec(method='rf', 
                      tuneLength = 5)
  )
)


g5 <- complete(imp, 5)
g5$ALSFRS_slope <- NULL
modlist5 <- caretList(
  x=g5,y=y,
  trControl=fitControl,
  metric="RMSE",
  methodList=c('glmboost','svmRadial', 'rf'),
  tuneList=list(
    glmboost=caretModelSpec(method='glmboost', 
                            tuneLength = 7),
    svmRadial=caretModelSpec(method='svmRadial', 
                             tuneLength = 7),
    rf=caretModelSpec(method='rf', 
                      tuneLength = 5)
  )
)

mod1 <- caretEnsemble(modlist1)
mod2 <- caretEnsemble(modlist2)
mod3 <- caretEnsemble(modlist3)
mod4 <- caretEnsemble(modlist4)
mod5 <- caretEnsemble(modlist5)


print(mod1)
print(colnames(g1))

rm(list=setdiff(ls(), c("mod1", "mod2", "mod3", "mod4", "mod5", "g1")))
save.image("~/als_submission/NatRegProgression/model_final_nat_slope.Rdata")

