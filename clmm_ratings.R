### load data ####
ratings <- read.delim(file = file.choose()) # opens window, chose file "data_ratings.csv" which should have been downloaded from the same github repository

#The data contains the following variables and additional information:
#PARTICIPANT = index for the participant (categorical, random effect)
#STIMULUS = index for the stimulus
#RATING_LIT = the obtained ratings for the literalness question (ordinal, dependend variable)
#RATING_MOTION = the obtained ratings for the actual motion question (ordinal, dependend variable)
#SENTENCE = stimulus sentence
#CONDITION = type of motion (categorical variable with four levels: literal, fictive, music, metaphorical; predictor)
#VERB = motion verb in the stimulus sentence (categorical, random effect)
#FREQ.ABS.BNC = absolute frequency of the motion verb in the BNC
#FREQ.ABS.BNC.LOG2 = logged absolute frequency of the verb (numeric variable, predictor)

## Participant metadata:
#QUESTNNR = indicates which questionnaire (the questionnaire was distributed via 1) MTurk - "literalnessAmTurk" -, 2) Linguistlist - "literalnessLingList" - and 3) among graduate students at the Univerity of Birmingham - "literalness")
#SEX = whether participant is male or female (categorical)
#AGE = participant's age (numeric)
#MUSIC.EDU = participants were asked to indicate their level of (classical) music education on a scale from 1 (low) to 5 (high) (numeric variable, predictor)
#EDU = educational level ("What is the highest level of education you have completed?) (categorical variable with 4 levels: school certificate, BA, MA, PhD)
#LGE.EN = indicates whether English is the participant's native language ("yes") or not ("no") (categorical)
#LING.EDU = participants were asked to indicate their level of knowledge of (theoretical) linguistics on a scale from 1 (low) to 5 (high) (numeric)

#Hypotheses:
#1)
#Musical motion is rated as more literal than metaphorical motion.
#Musical motion is rated similar to fictive motion.

#2)
#The degree of musical education of the participant has an effect on how the musical stimuli are rated, i.e. we expect an interaction between motion condition and degree of musical education. "Music experts" rate the musical motion stimuli as more literal.

#### data preparation ####

ratings$PARTICIPANT <- as.factor(ratings$PARTICIPANT)
ratings$CONDITION <- factor(ratings$CONDITION, levels = c("literal", "fictive", "music", "metaphorical"))
ratings$FREQ.ABS.BNC.LOG2 <- log2(ratings$FREQ.ABS.BNC) #log the absolute BNC freqeuncies (log to the base of 2, cf. Gries 2013: 254)
ratings$EDU <- relevel(ratings$EDU, "school certificate")

ratings$RATING_LIT <- factor(ratings$RATING_LIT, ordered = T)
ratings$RATING_MOTION <- factor(ratings$RATING_MOTION, ordered = T)

#inspect data
str(ratings)
summary(ratings)
attach(ratings)

#load packages
library(ordinal)
library(car)
library(ggplot2)
library(effects)
library(rms)
library(rcompanion)
library(RVAideMemoire)

#### exploratory tables & plots ####

#literalness ratings (FIRST BLOCK)
#table: ratings ~condition
table(RATING_LIT, CONDITION)
tapply(RATING_LIT, CONDITION, median)
tapply(as.numeric(RATING_LIT), CONDITION, na.rm = T, FUN = "mean")

#with ggplot:
ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_LIT)) + ggtitle("Literalness")
ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_LIT), position = "dodge") + ggtitle("Literalness")

#actual motion ratings (SECOND BLOCK)
table(RATING_MOTION, CONDITION)
tapply(RATING_MOTION, CONDITION, FUN = "median")
tapply(as.numeric(RATING_MOTION), CONDITION, mean, na.rm = T)

ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_MOTION)) + ggtitle("Actual Motion")
ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_MOTION), position = "dodge") + ggtitle("Actual Motion")

#boxplots both
par(mfrow=c(1,2))
boxplot(RATING_LIT~CONDITION, main="literalness ratings"); text(seq(CONDITION), tapply(as.numeric(as.character(RATING_LIT)), CONDITION, mean, na.rm=T), "x", col = "red")
boxplot(RATING_MOTION~CONDITION, main = "actual motion ratings"); text(seq(CONDITION), tapply(as.numeric(as.character(RATING_MOTION)), CONDITION, mean, na.rm=T), "x", col = "green")
par(mfrow=c(1,1))

#ratings: literalness vs. actual motion
par(mfrow=c(1,2))
barplot(table(RATING_LIT), space = F, ylim = c(0,2000), main = "Literalness Ratings", ylab = "count", xlab = "Rating")
barplot(table(RATING_MOTION), space = F, ylim = c(0,2000), main = "Actual Motion Ratings", ylab = "count", xlab = "Rating")
par(mfrow=c(1,1))
#literalness ratings are more heterogeneous, actual motion rations tend to be more categorical, i.e. either motional (5) or not (1)

#### ANALYSIS ####

#### LITERALNESS RATINGS ####

#9 participants did not provide their sex and age. Both variables will be tested first. If they do not turn out to have a significant effect on the results, we can discard them as variables and we can keep the 9 participants that did not supply information on sex and age in the data set.

#sex
model <- clmm(RATING_LIT ~ SEX + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # p = .681 --> SEX is not significant

#alternative test:
wilcox.test(as.numeric(as.character(RATING_LIT[SEX=="male"])), as.numeric(as.character(RATING_LIT[SEX=="female"])), paired = F, alternative = "two.sided") # not significant: W = 1847500, p-value = 0.1831

#age
model2 <- clmm(RATING_LIT ~ AGE + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model2) # p = .327 --> AGE is not significant

#alternative test:
cor.test(AGE, as.numeric(as.character(RATING_LIT)), method = "kendall") # significant, p<.01 BUT the correlation coefficient is zero which means that there is no correlation

#Result: both variables sex and age can be discarded from the data set

qwe <- which(colnames(ratings)=="SEX"); qwe # identify SEX column
ratings <- ratings[,-qwe] # delete SEX column

asd <- which(colnames(ratings)=="AGE"); asd
ratings <- ratings[,-asd]

# 1 participant did not provide an answer on his / her level of knowledge of theoretical linguistics. We well check whether the variable has a significant influence. If it doesn't, we can discard the variable and retain the data from that participant

#ling.edu
model <- clmm(RATING_LIT ~ LING.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # not significant, p = 0.846

cor.test(LING.EDU, as.numeric(as.character(RATING_LIT)), method = "kendall") # not significant, z = 0.32022, p-value = 0.7488

#Result: ling.edu is not significant so we can delete it
yxc <- which(colnames(ratings)=="LING.EDU"); yxc
ratings <- ratings[,-yxc]

#further individual test for participant metadata variables which are not considered as main predictors
#lge.en
model <- clmm(RATING_LIT ~ LGE.EN + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # not significant, p = 0.937

#alternative test:
wilcox.test(as.numeric(as.character(RATING_LIT[LGE.EN=="yes"])), as.numeric(as.character(RATING_LIT[LGE.EN=="no"])), paired = F, alternative = "two.sided") # not significant, W = 532680, p-value = 0.5551

#edu
model <- clmm(RATING_LIT ~ EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # we get more than on p-value because EDU is categorical with more than 2 levels

#compare model with EDU against a null model
model0 <- clmm(RATING_LIT ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings)
anova(model, model0) # model is not significantly better than the null model, X2(3)=1.1668, p = 0.761, EDU is not significant

#questinnaire
model <- clmm(RATING_LIT ~ QUESTNNR + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # more than one p-value

#comparison with null model
anova(model, model0) # the model with questnnr is not significantly better than a null model, X2(2)=4.459 p = 0.11, thus questnnr is not a significant predictor

#music.edu
model <- clmm(RATING_LIT ~ MUSIC.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # just not significant: 0.0633 but will be kept because interaction with CONDITION is expected (degree of music.edu is expected to influence only the ratings of the musical motion sentences, not all motion stimuli)
#comparison with null model
model0.1 <- clmm(RATING_LIT ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings$MUSIC.EDU),])
anova(model, model0.1) # a model with music.edu as a main predictor does not perform significantly better than a null model (X2(1)=3.3834 p=.07)

#frequency
model <- clmm(RATING_LIT ~ FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # frequency is a significant predictor, Z = 2.721 p < .01 (positive slope: with higher freq, higher literalness ratings are more likely)
#comparison with null model
anova(model, model0) # a model with FREQ as a predictor performs significantly better than the null model (X(1)=5.8657, p <.5)

#condition
model <- clmm(RATING_LIT ~ CONDITION + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # more than one p-value, all are significantly different from the reference level (literal motion condition)
#comparison with null model
anova(model,model0) # including condition makes the model significantly better (X2(3)=2264.1, p<.001)

#Result: frequency and condition have significant main effects on the ratings. Now we test the interaction

# 3-way interaction
model <- clmm(RATING_LIT ~ (CONDITION*MUSIC.EDU*FREQ.ABS.BNC.LOG2)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
summary(model)
drop1(model, test="Chisq") #result: 3-way interaction is not significant (X2(3)=0.84627 p=.84)

#continue with testing two-way interactions
model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + MUSIC.EDU*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
summary(model)
drop1(model, test = "Chisq") #Result: interaction between MUSIC.EDU and FREQ is not significant (X2(1)=1.553 p=.21)

#continue with the remaining interactions
model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
summary(model)
drop1(model, test = "Chisq")
#Result: both two-way interactions are significant: CONDITION*MUSIC.EDU (X2(3)=8.023, p<.05) & CONDITION*FREQ (X2(3)=34.065, p<.001), no further factors can be dropped --> we have arrived at the final model

#check whether the random effects are significant

#compare model with random affect to model without random effect
model1 <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|VERB), data = ratings[complete.cases(ratings),])

anova(model, model1) # model with PARTICIPANT as random effect is significantly better  (X2(1)=672.5 p<.001)

model1 <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT), data = ratings[complete.cases(ratings),])
anova(model, model1) #model with VERB as a random effect is significantly better (X2(1)=467.51 p<.001)

#Result: model with two 2-way interactions and two random effects is the final model for the literalness ratings

# final model literalness ratings ####
#set the musical motion condition as the reference level
ratings$CONDITION <- relevel(ratings$CONDITION, "music")

model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(model) #cf. Table 10.4

ratings$CONDITION <- relevel(ratings$CONDITION, "literal")
model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

#### model predictions ####

# How does the degree of music.edu affect the ratings in the 4 motion conditions?
# predicted probabilities for the ratings for the interactive effect CONDITION:MUSIC.EDU

#set reference level back to literal
ratings$CONDITION <- relevel(ratings$CONDITION, "literal")
model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

#determine predicted probability of each rating outcome for the different values of the variables
pred.intact <- effect("CONDITION:MUSIC.EDU", model)
pred.intact.data <- data.frame(pred.intact$x, pred.intact$prob)
pred.intact.data1 <- data.frame(pred.intact.data, RATING_LIT.pred=sub("^prob.X","",names(pred.intact.data)[-(1:2)][max.col(pred.intact.data[,-(1:2)])]))

#### plot the predictions ##
#re-arrange effect data frame for plotting
aa <- pred.intact.data1$CONDITION
bb <- pred.intact.data1$MUSIC.EDU
cc <- unlist(append(pred.intact.data1[3], pred.intact.data1[c(4:7)]))

data.interim <- data.frame(condition=rep(aa,5), music.edu=rep(bb, 5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=20))

data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (completely literal)","4", "3", "2", "1 (not literal at all)")) #re-organisers factor levels for the plot

colnames(data.interim)[5] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

#area plot
ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=levels(data.interim$RATING)) + ggtitle("CONDITION*MUSICAL BACKGROUND") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

#Result: High level of musical education leads to rating the musical motion sentences as more literal. (And high level of musical education leads to rating the fictive motion sentences as more literal, same for literal); metaphorical condition is not effected by music.edu

###

# How does the frequency of the verb affect the ratings in the 4 motion conditions?
## predicted probabilities for the ratings for the interactive effect CONDITION:FREQ ##

pred.intact2 <- effect("CONDITION:FREQ.ABS.BNC.LOG2", model)
pred.intact.data2 <- data.frame(pred.intact2$x, pred.intact2$prob)
pred.intact.data2 <- data.frame(pred.intact.data2, RATING_LIT.pred=sub("^prob.X","",names(pred.intact.data2)[-(1:2)][max.col(pred.intact.data2[,-(1:2)])]))

#### plot the predictions ##
#re-arrange effect data frame for plotting
aa <- pred.intact.data2$CONDITION
bb <- pred.intact.data2$FREQ.ABS.BNC.LOG2
cc <- unlist(append(pred.intact.data2[3], pred.intact.data2[c(4:7)]))

data.interim <- data.frame(condition=rep(aa,5), freq=rep(bb, 5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=28))

data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (completely literal)","4", "3", "2", "1 (not literal at all)")) #re-organisers factor levels for the plot

colnames(data.interim)[5] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

#area plot
ggplot(data.interim, aes(x=freq, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Reds", breaks=levels(data.interim$RATING)) + ggtitle("CONDITION*FREQUENCY") + xlab("Absolute frequency of motion verb (BNC, logged)") + ylab("predicted probabilities (stacked)")

#Result: The higher the frequency of the verb, the more literal a sentence will be rated, especially in the metaphorical condition.

#re-set reference level of condition to "metaphorical" to show that effect of FREQ in the metaphorical condition is signicantly different from the effect of FREQ in the other motion conditions

ratings$CONDITION <- relevel(ratings$CONDITION, "metaphorical")

model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(model)
#CONDITIONliteral:FREQ.ABS.BNC.LOG2 -0.12545    0.04084  -3.072  0.00213 ** 
#  CONDITIONmusic:FREQ.ABS.BNC.LOG2   -0.14507    0.03184  -4.556 5.21e-06 ***
#  CONDITIONfictive:FREQ.ABS.BNC.LOG2 -0.17364    0.03223  -5.388 7.13e-08 ***

#re-level to "literal" again
ratings$CONDITION <- relevel(ratings$CONDITION, "literal")
model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

#### measures of model quality ####

model0 <- clmm(RATING_LIT ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

anova(model, model0) # final model performs significantly better than the null model (X2(11)=2261 p<.001)

model.base <- clmm(RATING_LIT ~ CONDITION + FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

anova(model, model.base) # final model performs significantly better than the base model (X2(7)=45.538 p<.001)

#AIC
model$info[5] #10326.45
model0$info[5] #12565.49
model.base$info[5] #10357.99

#BIC
BIC(model) # 10434.38
BIC(model0) # 12603.59
BIC(model.base) # 10421.48

#Nagelkerke's pseudo R2
nagelkerke(fit = model, null = model0)[[2]][3] #0.436743
nagelkerke(fit = model, null = model.base)[[2]][3] #0.0117341


#### ACTUAL MOTION RATINGS ####

rm(list=ls(all=TRUE)) # clear memory

### load data ####
ratings <- read.delim(file = file.choose()) # opens window, chose file "data_ratings.csv" which should have been downloaded from the same github repository

#### data preparation ####

ratings$PARTICIPANT <- as.factor(ratings$PARTICIPANT)
ratings$CONDITION <- factor(ratings$CONDITION, levels = c("literal", "fictive", "music", "metaphorical"))
ratings$FREQ.ABS.BNC.LOG2 <- log2(ratings$FREQ.ABS.BNC) #log the absolute BNC freqeuncies (log to the base of 2, cf. Gries 2013: 254)
ratings$EDU <- relevel(ratings$EDU, "school certificate")

ratings$RATING_LIT <- factor(ratings$RATING_LIT, ordered = T)
ratings$RATING_MOTION <- factor(ratings$RATING_MOTION, ordered = T)

#inspect data
str(ratings)
summary(ratings)
attach(ratings)

#load packages
library(ordinal)
library(car)
library(ggplot2)
library(effects)
library(rms)
library(rcompanion)
library(RVAideMemoire)

#### ANALYSIS ####

#test potential predictors individually

#sex
model <- clmm(RATING_MOTION ~ SEX + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # p<.05, significant

#comparison with null model
model0 <- clmm(RATING_MOTION ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings$SEX),])
anova(model, model0) # model with SEX performs significantly better (X2(1)=3.9654, p<.05)

#age
model <- clmm(RATING_MOTION ~ AGE + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # not significant, p =.12

#we can discard AGE from the data set and thus keep the participants who did not provide their age

qwe <- which(colnames(ratings)=="AGE"); qwe # identifies age column
ratings <- ratings[,-qwe]

#ling.edu
model <- clmm(RATING_MOTION ~ LING.EDU +  (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # p<.01, significant

model0 <- clmm(RATING_MOTION ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings$LING.EDU),])
anova(model, model0) # model with LING.EDU performs significantly better (X2(1)=8.857, p<.01)

#lge.en
model <- clmm(RATING_MOTION ~ LGE.EN +  (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # p = .40, not significant

#edu
model <- clmm(RATING_MOTION ~ EDU +  (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # we get more than one p-value

model0 <- clmm(RATING_MOTION ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings)
anova(model, model0) # significant, X2(3)=14.742 p<.01

# QUESTIONNAIRE
model <- clmm(RATING_MOTION ~ QUESTNNR +(1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # we get more than one p-value

#comparison with null model
anova(model, model0) # questionnaire is not significant

#music.edu
model <- clmm(RATING_MOTION ~ MUSIC.EDU +(1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # not significant (as a main effect), p=.27

#comparison with null model
model0 <- clmm(RATING_MOTION ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings$MUSIC.EDU),])
anova(model, model0) # not significantly better, X2(1)=1.2286 p=.27

#verb frequency
model <- clmm(RATING_MOTION ~ FREQ.ABS.BNC.LOG2 +(1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # not significant, p=.52

#comparison with null model
model0 <- clmm(RATING_MOTION ~ 1 +(1|PARTICIPANT) + (1|VERB), data = ratings)
anova(model, model0) # not significantly better, X2(1)=0.4074 p=.52

#condition
model <- clmm(RATING_MOTION ~ CONDITION +(1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # more than on p-value

#comparison with null model
anova(model, model0) # significant as main effect, X2(3)=3802.8 p<.001

## interim result:
#sex, ling.edu, edu, condition have a significant effect on the actual motion ratings when tested individually. We will now test whether there are also interactions which the theoretically motivated variables MUSIC.EDU and FREQUENCY. We will keep sex,ling.edu, edu as main effects. From this maximal model we will subsequently test if and if so which predictor terms ca be deleted.

#maximal model

model <-  clmm(RATING_MOTION ~ (CONDITION*MUSIC.EDU*FREQ.ABS.BNC.LOG2)^3 + SEX + LING.EDU + EDU + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

#backward selection with drop1()

drop1(model, test = "Chisq")
#CONDITION:MUSIC.EDU:FREQ.ABS.BNC.LOG2  3 7074.5 2.7392  0.43360  
#3-way interaction does not make the model significantly better, can be dropped:
model <- update(model, ~. -CONDITION:MUSIC.EDU:FREQ.ABS.BNC.LOG2)

#test if further predictor terms can be dropped
drop1(model, test = "Chisq")
#CONDITION:FREQ.ABS.BNC.LOG2  3 7070.2  1.762  0.62323
#2-way interaction between condition and verb frequency can be dropped (was significant for literalness ratings!):
model <- update(model, ~. -CONDITION:FREQ.ABS.BNC.LOG2)

#test if further predictor terms can be dropped
drop1(model, test = "Chisq")
#MUSIC.EDU:FREQ.ABS.BNC.LOG2  1 7069.6  1.376   0.24074
#2-way interaction between music.edu and verb frequency can be dropped:
model <- update(model, ~. -MUSIC.EDU:FREQ.ABS.BNC.LOG2)

#test if further predictor terms can be dropped
drop1(model, test = "Chisq")
#FREQ.ABS.BNC.LOG2    1 7068.4  0.834   0.36107
#freq is not significant as a main effect or as a predictor terms in an interaction and can be dropped (X2(1)=0.834 p=.36):
model <- update(model,~. -FREQ.ABS.BNC.LOG2)

#test if further predictor terms can be dropped
drop1(model, test = "Chisq")
# SEX                  1 7068.1  1.669   0.19644 
#sex is not significant and can be dropped (X2(1)=1.669 p=.20):
model <- update(model, ~. -SEX)

#test if further predictor terms can be dropped
drop1(model, test = "Chisq")
#LING.EDU             1 7069.4  3.240   0.07187 . 
#ling.edu is not significant and can be dropped (X2(1)=3.240 p=.07):
model <- update(model, ~. -LING.EDU)

#test if further predictor terms can be dropped
drop1(model, test = "Chisq")

#no more predictor terms can be dropped

#test if there is a 3-way interaction between the remaining predictors

model <- clmm(RATING_MOTION ~ (CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

drop1(model, test = "Chisq")
#3 way interaction is significant

#due to few or missing data points in some cells for some constallations between music.edu and edu (cf. PARTICIPANTS METADA below), we will continue with the two-way interaction between music.edu and condition only.
#This model can be fit on a larger data set because SEX and LING.EDU turned out to be not significant

asd <- which(colnames(ratings)=="SEX");asd
ratings <- ratings[,-asd]

yxc <- which(colnames(ratings)=="LING.EDU"); yxc
ratings <- ratings[,-yxc]

## final model actual motion ratings ####
model <- clmm(RATING_MOTION ~ CONDITION*MUSIC.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)

summary(model)

#check whether the random effects significantly improve the model
#compare model with random effect to model without random effect
#participant
model1 <- clmm(RATING_MOTION ~ CONDITION*MUSIC.EDU + (1|VERB), data = ratings)
anova(model, model1) # the model with PARTICIPANT as a random effect performs significantly better (X2(1)=631.12 p<.001)

#verb
model1 <- clmm(RATING_MOTION ~ CONDITION*MUSIC.EDU + (1|PARTICIPANT), data = ratings)
anova(model, model1) # the model with VERB as a random effect does not perform significantly better (X2(1)=0.0862 p=.37), in line with the philosophy of "keeping it maximal" VERB will remain as a random effect term

#set reference level to musical motion condition
ratings$CONDITION <- relevel(ratings$CONDITION, "music")
model <- clmm(RATING_MOTION ~ CONDITION*MUSIC.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)

summary(model)

#### model predictions ####

# How does the degree of music.edu affect the ratings in the 4 motion conditions?
#predicted probabilities for the actual motion ratings for, the interactive effect CONDITION:MUSIC.EDU

#set reference level back to literal
ratings$CONDITION <- relevel(ratings$CONDITION, "literal")
model <- clmm(RATING_MOTION ~ CONDITION*MUSIC.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)

#determine predicted probability of each rating outcome for the different values of the variables
pred.motion.2 <- effect("CONDITION:MUSIC.EDU", model)
pred.motion.2 <- data.frame(pred.motion.2$x, pred.motion.2$prob)
pred.motion.2 <- data.frame(pred.motion.2, RATING_MOT.pred=sub("^prob.X","",names(pred.motion.2)[-(1:2)][max.col(pred.motion.2[,-(1:2)])]))

#### plot the predictions ##
#re-arrange effect data frame for plotting
aa <- pred.motion.2$CONDITION
bb <- pred.motion.2$MUSIC.EDU
cc <- unlist(append(pred.motion.2[3], pred.motion.2[c(4:7)]))

data.interim <- data.frame(condition=rep(aa,5), music.edu=rep(bb, 5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=20))

data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (strongly associated\n   with actual motion)","4", "3", "2", "1 (not associated with\n   actual motion)")) #re-organisers factor levels for the plot

colnames(data.interim)[5] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

#area plot
ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=levels(data.interim$RATING)) + ggtitle("CONDITION*MUSICAL BACKGROUND") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

#result: with increasing knowledge of music, more motional ratings become more likely for the musical motion sentence (and only for these!)

#### measures of model quality ####
#comparison with null model
model0 <- clmm(RATING_MOTION ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings$MUSIC.EDU),])
anova(model0, model) # the final model performs significantly better than the null model (X2(7)=3792.5 p<.001)

#comparison with base model (which includes FREQ)
model.base <- clmm(RATING_MOTION ~ (CONDITION*MUSIC.EDU*FREQ.ABS.BNC.LOG2)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings)
anova(model, model.base) #no significant difference (X2(8)=8.3348, p=.40)

#AIC
AIC(model, model0, model.base) #model: 8000.794, null model: 11779.336, base model: 8008.459

#BIC
BIC(model, model0, model.base) # model: 8083.374, null: 11817.449, base: 8141.858

#Nagelkerke's pseudo R2
nagelkerke(fit = model, null = model0)[[2]][3]
nagelkerke(fit = model, null = model.base)[[2]][3]

#### 3-way interaction with EDU ####

#relevel: music as reference level for CONDITION, and school certificate as reference level for EDU
ratings$CONDITION <- relevel(ratings$CONDITION, "music")
ratings$EDU <- relevel(ratings$EDU, "school certificate")

model3 <- clmm(RATING_MOTION ~ (CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model3)

#re-set reference levels
ratings$CONDITION <- relevel(ratings$CONDITION, "literal")
#ratings$EDU <- relevel(ratings$EDU, "school certificate")

model3 <- clmm(RATING_MOTION ~ (CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings)

#predicted probabilities
pred.intact3 <- effect("CONDITION:MUSIC.EDU:EDU", model3)
pred.intact3.data <- data.frame(pred.intact3$x, pred.intact3$prob) 
pred.intact3.data1 <- data.frame(pred.intact3.data, RATING_MOT.pred=sub("^prob.X","",names(pred.intact3.data)[-(1:3)][max.col(pred.intact3.data[,-(1:3)])]))

#re-arrange effect data frame for plotting
#all in one plot (par(mfrow) does not work with ggplot?)
aa <- pred.intact3.data$CONDITION
bb <- pred.intact3.data$MUSIC.EDU
dd <- pred.intact3.data$EDU
cc <- unlist(append(pred.intact3.data[4], pred.intact3.data[c(5:8)]))

data.interim <- data.frame(condition=rep(aa,5), music.edu=rep(bb, 5), edu=rep(dd,5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=80))

data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (strongly associated\n   with actual motion)","4", "3", "2", "1 (not associated with\n   actual motion)")) #re-organisers factor levels for the plot

colnames(data.interim)[6] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

#area plots
ggplot(data.interim[data.interim$condition=="music",], aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~edu) + scale_fill_brewer(palette = "Greens", breaks=levels(data.interim$RATING)) + ggtitle("MUSICAL BACKGROUND*EDUCATION for MUSICAL MOTION") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

ggplot(data.interim[data.interim$condition=="fictive",], aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~edu) + scale_fill_brewer(palette = "Oranges", breaks=levels(data.interim$RATING)) + ggtitle("MUSICAL BACKGROUND*EDUCATION for FICTIVE MOTION") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

ggplot(data.interim[data.interim$condition=="literal",], aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~edu) + scale_fill_brewer(palette = "Greys", breaks=levels(data.interim$RATING)) + ggtitle("MUSICAL BACKGROUND*EDUCATION for LITERAL MOTION") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

ggplot(data.interim[data.interim$condition=="metaphorical",], aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~edu) + scale_fill_brewer(palette = "Purples", breaks=levels(data.interim$RATING)) + ggtitle("MUSICAL BACKGROUND*EDUCATION for METAPHORICAL MOTION") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")


#### PARTICIPANTS METADATA ####

#load meta data participants
meta.particip <- read.delim(file = file.choose())

#music.edu and edu
table(meta.particip$music.edu, meta.particip$edu)

#is there a correlation between music.edu and edu, i.e. is higher degree of knowledge of music correlated with a higher educational degree?

chisq.test(table(meta.particip$music.edu, meta.particip$edu))
#not significant, X2(12)=11.235 p=.51

fisher.test(table(meta.particip$music.edu, meta.particip$edu))
#not significant, X2(12)=11.235 p=.63

#RESULT: EDU and MUSIC.EDU are not correlated

#correlation between EDU and QUESTNNR
table(meta.particip$edu, meta.particip$QUESTNNR)
chisq.test(table(meta.particip$edu, meta.particip$QUESTNNR))
fisher.test(table(meta.particip$edu, meta.particip$QUESTNNR))
#significant, X2(6)=45.08 p<.001

#Result: Level of education correlates with how participants were recruited (X2(6)=45.08, p<.01): Participants with a PhD mostly were recruited via the Linguist List, while participants with a BA or school certificate are overrepresented among the participants recruited via Amazon Mechanical Turk.
