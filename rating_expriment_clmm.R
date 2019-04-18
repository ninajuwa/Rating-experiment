## Is musical motion less metaphorical than metaphorical motion proper?

#Ordinal mixed-effects regression with clmm for literalness / motion ratings of figurative sentences

#In an online rating experiment 83 participants were presented with 52 stimulus sentences. There were 2 blocks. In the first block, participants had to rate the stimulus sentences according to the question "In your opinion, how literal is the sentence?" on a scale from 1 = "not literal at all" to 5 = "completely literal" (only the two extremes were labeled). In the second block, participants had to rate the same stimulus sentences according to the question "How strongly do you associate the sentence with actual motion?" on a scale from 1 = "not at all associated with acutal motion" to 5 = "very strongly associated with actual motion". The stimulus sentences all expressed motion events. The motion event was either literal ("Carl arrived at the concert hall"), fictive ("The road arrives at an intersection"), related to music ("The music arrives at the final chord") or metaphorical ("The two studies arrive at very different conclusions"). The stimulus sentencs contain 13 different verbs, each verb appears in all 4 conditions (13 verbs * 4 conditions = 52 stimulus sentences). Every participant rated every stimulus for both questions. The stimulus sentences were pseudo-randomised in that two successive stimuli neither contained the same verb nor expressed the same motion condition. The aim of the experiment is to find out whether musical motion is perceived as less metaphorical (as suggested by the literature) and perceived as closer to fictive motion (a previous corpus analysis suggests that musical motion is both structurally and functionally similar to fictive motion). 
#Explanation of the two tasks: Instead of asking participants "How metaphorical is the sentence?", the question "How LITERAL is the sentence?" was chosen because participants may have very different ideas about what counts as a metaphor in contrast to linguists. Moreover, participants may judge a sentence as more literal simply because the sentence is very frequent. For this reason a second question "How stronly do you associate the sentence with acutal motion" was added which explicily spells out the source domain to try to get participants judge how similar source and target of the stimulus sentences are.

#load data ####
ratings <- read.delim(file = file.choose()) # read "data_manipulated" in /Users/ninajulich/Documents/Uni/Forschung:Dissertation/5_Experiment/Rating_Task/data_manipulated.csv
#NEU: FILE: data_ratings.csv

#The data contains the following variables and additional information:

#PARTICIPANT = index for the participant (factor, random effect)
#STIMULUS = index for the stimulus (factor, random effect)
#RATING_LIT = the obtained ratings for the literalness question (ordinal, dependend variable)
#RATING_MOTION = the obtained ratings for the actual motion question (ordinal, dependend variable)
#Sentence = stimulus sentence, not a variable, included for informative reasons
#CONDITION = type of motion (factor with four levels: literal, fictive, music, metaphorical; predictor)
#Verb = verb of the stimulus sentence, not a variable because I thought that random variation due to verb is already captured by the random effect STIMULUS
#FREQ.ABS.BNC = absolute frequency of the verb in the BNC
#FREQ.ABS.BNC.LOG2 = log2(FREQ.ABS.BNC) (numeric, predictor)
## Participant metadata:
#QUESTNNR = indicates which questionnaire (the questionnaire was distributed via 1) MTurk, 2) Linguistlist and 3) among graduate students at the Univerity of Birmingham)
#SEX
#AGE
#MUSIC.EDU = numeric predictor, participants were asked to indicate their level of (classical) music education on a scale from 1 (low) to 5 (high)
#EDU = educational level ("What is the highest level of education you have completed?)
#LGE.EN = indicates whether English is the participant's native language ("yes") or not ("no")
#LING.EDU = participants were asked to indicate their level of knowledge of (theoretical) linguistics on a scale from 1 (low) to 5 (high)

#Hypotheses:
#1)
#Musical motion is rated as more literal than metaphorical motion.
#Musical motion is rated similar to fictive motion.

#2)
#The degree of musical education of the participant has an effect on how the musical stimuli are rated, i.e. we expect an interaction between motion condition and degree of musical education. "Music experts" rate the musical motion stimuli as more literal.

#3)
#Frequency influences the ratings for the literalness question but not the ratings for the actual motion question.

#data preparation

ratings$PARTICIPANT <- as.factor(ratings$PARTICIPANT)
ratings$CONDITION <- factor(ratings$CONDITION, levels = c("literal", "fictive", "music", "metaphorical"))
ratings$FREQ.ABS.BNC.LOG2 <- log2(ratings$FREQ.ABS.BNC) #log the absolute BNC freqeuncies (log to the base of 2, cf. Gries 2013: 254)
ratings$EDU <- relevel(ratings$EDU, "school certificate")

#for ordinal modelling:
ratings$RATING_LIT <- factor(ratings$RATING_LIT, ordered = T)
ratings$RATING_MOTION <- factor(ratings$RATING_MOTION, ordered = T)

#delete cases with missing values?
#ratings <- ratings[complete.cases(ratings),]
#attach(ratings)


str(ratings)
summary(ratings)
attach(ratings)

#load packages
library(ordinal) #
library(car)
library(ggplot2) #
library(effects) #
library(rms)
library(rcompanion) #
library(RVAideMemoire) # cf. http://rcompanion.org/handbook/G_08.html

#functions
ilogit <- function(x) { # inverse logit: maps the range (-???,???) to the range (0,1)
  1/(1+exp(-x)) # Gelman & Hill (2007:80)
}

#### exploratory plots ####
#full data

#data without sex and age and NAs

#first plots and descriptive stats

#condition
#literalness
#table: ratings ~condition
table(RATING_LIT, CONDITION)
tapply(RATING_LIT, CONDITION, median)
tapply(as.numeric(RATING_LIT), CONDITION, na.rm = T, FUN = "mean")
plot(table(CONDITION, RATING_LIT))

#with ggplot:
ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_LIT)) + ggtitle("Literalness")
ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_LIT), position = "dodge") + ggtitle("Literalness")

#literal is clearly literal, looks like metaphorical is rated as more literal than music

#boxplots both
par(mfrow=c(1,2))
boxplot(RATING_LIT~CONDITION, main="literalness ratings"); text(seq(CONDITION), tapply(as.numeric(as.character(RATING_LIT)), CONDITION, mean, na.rm=T), "x", col = "red")
boxplot(RATING_MOTION~CONDITION, main = "actual motion ratings"); text(seq(CONDITION), tapply(as.numeric(as.character(RATING_MOTION)), CONDITION, mean, na.rm=T), "x", col = "green")
par(mfrow=c(1,1))

#actual motion
table(RATING_MOTION, CONDITION)
tapply(RATING_MOTION, CONDITION, FUN = "median")
tapply(as.numeric(RATING_MOTION), CONDITION, mean, na.rm = T)
plot(table(CONDITION, RATING_MOTION))

ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_MOTION)) + ggtitle("Actual Motion")
ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_MOTION), position = "dodge") + ggtitle("Actual Motion")

#indicates that literal stimulus sentences are still clearly rated as most motional, in comparison to literalness ratings, however, the figurative conditions are all rated as more clearly rated as different from literal. AND music is rated as slightly more motional than metaphorical motion (if means are compared)

#music.edu & condition
table(RATING_LIT, CONDITION, MUSIC.EDU)

ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_LIT)) + ggtitle("Literalness") + facet_wrap(~MUSIC.EDU)

table(RATING_MOTION, CONDITION, MUSIC.EDU)

ggplot(ratings[complete.cases(ratings),], aes(x=CONDITION)) + geom_bar(aes(fill=RATING_MOTION)) + ggtitle("Literalness") + facet_wrap(~MUSIC.EDU)

#with more musical edu, musical motion is rated as more motional

#Frequency
plot(RATING_LIT, FREQ.ABS.BNC.LOG2)

#literal
prop.table(table(RATING_LIT, FREQ.ABS.BNC.LOG2), margin = 2)*100

ggplot(ratings[complete.cases(ratings),], aes(x=FREQ.ABS.BNC.LOG2, y=RATING_LIT)) + geom_count()

#with higher frequency, higher literalness ratings occur more often

#actual motion
prop.table(table(RATING_MOTION, FREQ.ABS.BNC.LOG2), margin = 2)*100 # proportions do not change as frequency increases

ggplot(ratings[complete.cases(ratings),], aes(x=FREQ.ABS.BNC.LOG2, y=RATING_MOTION)) + geom_count()

#frequency does not have an effect on actual motion ratings

##frequency & condition

ggplot(ratings[complete.cases(ratings),], aes(x=FREQ.ABS.BNC.LOG2, y=RATING_LIT)) + ggtitle("Literalness") + facet_wrap(~CONDITION) + geom_count()

#literal: race (freq = 11.8) behaves odd, apart form that freq does not have an influence, i.e. literal is always rated as 5 no matter how high / low the verb frequency
#fictive: general trend with higher freq more literal, but "descend" falls out of that trend and is generally rated as more literal (freq = 10.7) (parly also meander and ascend, the two lowest in freq, both have relatively high literalness ratings)
#metaphor & music: seem to exhibit general trend that with higher frequencies higher literalness ratings occur

#ratings: literalness vs. actual motion
par(mfrow=c(1,2))
barplot(table(RATING_LIT), space = F, ylim = c(0,2000), main = "Literalness Ratings", ylab = "count", xlab = "Rating")
barplot(table(RATING_MOTION), space = F, ylim = c(0,2000), main = "Actual Motion Ratings", ylab = "count", xlab = "Rating")
par(mfrow=c(1,1))

#literalness ratings are more heterogeneous, actual motion rations tend to be more categorical, i.e. either motional (5) or not (1)

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
cor.test(AGE, as.numeric(as.character(RATING_LIT)), method = "kendall") # significant, p<.01 BUT the correlation coeffecint is .04 which means that the correlation is quasi non existant, so we can say it is signifcant that we have no correlation between age and the literalness ratings

#Result: both variables sex and age can be discarded from the data set
#delete non-significant metadata variables

qwe <- which(colnames(ratings)=="SEX"); qwe # identify SEX column
ratings <- ratings[,-qwe] # delete SEX column

#age also does not have a significant effect, so we will leave it out (cf. clmm_neu Kopie)
asd <- which(colnames(ratings)=="AGE"); asd
ratings <- ratings[,-asd]

# 1 participant did not provide an answer on his / her level of knowledge of theoretical linguistics. We well check whether the variable has a significant influence. If it doesn't, we can discard the variable and retain the data from that participant
#ling.edu
model <- clmm(RATING_LIT ~ LING.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # 0.846

cor.test(LING.EDU, as.numeric(as.character(RATING_LIT)), method = "kendall") # z = 0.32022, p-value = 0.7488

#Result: ling.edu is not significant so we can delete it
yxc <- which(colnames(ratings)=="LING.EDU"); yxc
ratings <- ratings[,-yxc]

#weitere Einzeltests
#lge.en
model <- clmm(RATING_LIT ~ LGE.EN + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # p = 0.937

wilcox.test(as.numeric(as.character(RATING_LIT[LGE.EN=="yes"])), as.numeric(as.character(RATING_LIT[LGE.EN=="no"])), paired = F, alternative = "two.sided") #W = 532680, p-value = 0.5551

#edu
model <- clmm(RATING_LIT ~ EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # we get more than on p-value
anova(model, model0) # model is not significantly better than the null model, EDU is not significant

#questinnaire
model <- clmm(RATING_LIT ~ QUESTNNR + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # more than one p-value, some factor levels are just not significantly different from the reference level
model0 <- clmm(RATING_LIT ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings)
anova(model, model0) # the model with questnnr is not significantly better than a null model, thus questnnr is not a significant predictor

#music.edu
model <- clmm(RATING_LIT ~ MUSIC.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # just not significant: 0.0633 but will be kept because interaction will be tested

#frequency
model <- clmm(RATING_LIT ~ FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # frequency is a significant predictor (positive slope: with higher freq, higher ratings are more likely)
anova(model, model0)

#condition
model <- clmm(RATING_LIT ~ CONDITION + (1|PARTICIPANT) + (1|VERB), data = ratings)
summary(model) # more than one p-value, all are signif diff from literal condition
anova(model,model0) # including condition makes the model significantly better

#Result: frequency and condition have significant main effect on the ratings now we test the interaction

#test interaction
model <- clmm(RATING_LIT ~ (CONDITION*MUSIC.EDU*FREQ.ABS.BNC.LOG2)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
summary(model)
drop1(model, test="Chisq") #result: 3-way interaction is not significant

#continue with testing two-way interactions
model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + MUSIC.EDU*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
summary(model)
drop1(model, test = "Chisq") # I tried with "LR", error message: test should be "none" or "Chisq"#Result: MUSIC.EDU:FREQ.ABS.BNC.LOG2 is not significant

#continue with the remaining interactions
model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
summary(model)
drop1(model, test = "Chisq")
#Result: both two-way interactions are significant: CONDITION*MUSIC.EDU & CONDITION*FREQ, no further factors can be dropped --> we have arrived at the final model

#Let's check whether the random effects are significant
#against a null model
model00 <- clm(RATING_LIT ~ 1, data = ratings[complete.cases(ratings),])
model01 <- clmm(RATING_LIT ~ 1 + (1|PARTICIPANT), data = ratings[complete.cases(ratings),])
anova(model00,model01) # model01 is significantly better --> PARTICIPANT as a random is significant

model01 <- clmm(RATING_LIT ~ 1 + (1|VERB), data = ratings[complete.cases(ratings),])
anova(model00, model01) # model01 is significantly better --> VERB as a random is significant

#against the model including significant predictor terms
#model <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
model1 <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|VERB), data = ratings[complete.cases(ratings),])
anova(model, model1) #model with PARTICIPANT as random effect is significantly better

model1 <- clmm(RATING_LIT ~ CONDITION*MUSIC.EDU + CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT), data = ratings[complete.cases(ratings),])
anova(model, model1) #model with VERBS as a random effect is significantly better

#Result: model with two 2-way interactions and two random effects is the final model for the literalness ratings

## clmm approach with maximal model ####

# quasi-maximal model #
#only includes interactions for those predictors that are theoretically motivated
modlit <- clmm(RATING_LIT ~ (CONDITION*MUSIC.EDU*FREQ.ABS.BNC.LOG2)^3+ LGE.EN + EDU + QUESTNNR + LING.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
summary(modlit)

drop1(modlit, test = "Chisq")
#Single term deletions

#Model:
#  RATING_LIT ~ (CONDITION * MUSIC.EDU * FREQ.ABS.BNC.LOG2)^3 + 
#  LGE.EN + EDU + QUESTNNR + LING.EDU + (1 | PARTICIPANT) + 
#  (1 | VERB)
#Df   AIC     LRT Pr(>Chi)
#<none>                                   10240                 
#LGE.EN                                 1 10238 0.48124   0.4879
#EDU                                    3 10236 1.82402   0.6097
#QUESTNNR                               2 10239 3.15927   0.2061
#LING.EDU                               1 10238 0.03859   0.8443
#CONDITION:MUSIC.EDU:FREQ.ABS.BNC.LOG2  3 10235 0.78258   0.8536

modlit1 <- update(modlit, ~. -CONDITION:MUSIC.EDU:FREQ.ABS.BNC.LOG2)
summary(modlit1)

drop1(modlit1, test = "Chisq")
#(I forgot to copy output)

modlit2 <- update(modlit1, ~. -MUSIC.EDU:FREQ.ABS.BNC.LOG2)
summary(modlit2)

drop1(modlit2, test = "Chisq")
#Single term deletions

#Model:
#  RATING_LIT ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + LGE.EN + 
#  EDU + QUESTNNR + LING.EDU + (1 | PARTICIPANT) + (1 | VERB) + 
#  CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2
#                           Df   AIC    LRT  Pr(>Chi)    
#<none>                         10234                     
#LGE.EN                       1 10233  0.470   0.49309    
#EDU                          3 10230  1.636   0.65121    
#QUESTNNR                     2 10233  3.164   0.20554    
#LING.EDU                     1 10232  0.037   0.84731    
#CONDITION:MUSIC.EDU          3 10236  8.012   0.04576 *  
#  CONDITION:FREQ.ABS.BNC.LOG2  3 10261 33.330 2.743e-07 ***

#both interactions are significant, so we continue dropping main effects

modlit3 <- update(modlit2, ~. - LING.EDU)
summary(modlit3)
anova(modlit2, modlit3)

drop1(modlit3, test = "Chisq")
#Single term deletions

#Model:
#  RATING_LIT ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + LGE.EN + 
#  EDU + QUESTNNR + (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2
#Df   AIC    LRT  Pr(>Chi)    
#<none>                         10232                     
#LGE.EN                       1 10231  0.526   0.46842    
#EDU                          3 10228  1.679   0.64155    
#QUESTNNR                     2 10231  3.212   0.20066    
#CONDITION:MUSIC.EDU          3 10234  8.011   0.04578 *  
#  CONDITION:FREQ.ABS.BNC.LOG2  3 10260 33.331 2.742e-07 ***

modlit4 <- update(modlit3, ~. -EDU)
summary(modlit4)
anova(modlit3, modlit4)

drop1(modlit4, test = "Chisq")
#Single term deletions

#Model:
#  RATING_LIT ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + LGE.EN + 
#  QUESTNNR + (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU + 
#  CONDITION:FREQ.ABS.BNC.LOG2
#Df   AIC    LRT  Pr(>Chi)    
#<none>                         10228                     
#LGE.EN                       1 10226  0.435   0.50945    
#QUESTNNR                     2 10227  3.285   0.19354    
#CONDITION:MUSIC.EDU          3 10230  8.009   0.04584 *  
#  CONDITION:FREQ.ABS.BNC.LOG2  3 10255 33.389 2.666e-07 ***

modlit5 <- update(modlit4, ~. -LGE.EN)
summary(modlit5)
anova(modlit4, modlit5)

drop1(modlit5, test = "Chisq")
#Single term deletions

#Model:
#  RATING_LIT ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + QUESTNNR +   (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2
#                           Df   AIC    LRT  Pr(>Chi)    
#<none>                         10226                     
#QUESTNNR                     2 10225  3.064   0.21605    
#CONDITION:MUSIC.EDU          3 10228  8.005   0.04592 *  
#  CONDITION:FREQ.ABS.BNC.LOG2  3 10254 33.381 2.677e-07 ***

modlit6 <- update(modlit5, ~. -QUESTNNR)
summary(modlit6)
anova(modlit5,modlit6)

drop1(modlit6, test = "Chisq")
#Single term deletions

#Model:
#  RATING_LIT ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + (1 |PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2
#                           Df   AIC    LRT  Pr(>Chi)    
#<none>                         10225                     
#CONDITION:MUSIC.EDU          3 10227  8.013   0.04575 *  
#  CONDITION:FREQ.ABS.BNC.LOG2  3 10253 33.426 2.618e-07 ***

## no further predictors can be dropped:
#### final model ####
modlit6 <- clmm(RATING_LIT~CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(modlit6)
#Cumulative Link Mixed Model fitted with the Laplace approximation

#formula: 
#  RATING_LIT ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + CONDITION:MUSIC.EDU +    CONDITION:FREQ.ABS.BNC.LOG2 + (1 | PARTICIPANT) + (1 | VERB) data:    ratings[complete.cases(ratings), ]

#link  threshold nobs logLik   AIC      niter       max.grad cond.H 
#logit flexible  4183 -5095.66 10225.32 2872(11495) 5.09e-03 2.5e+05

#Random effects:
#  Groups      Name        Variance Std.Dev.
#PARTICIPANT (Intercept) 0.9995   0.9998  
#VERB        (Intercept) 0.5046   0.7104  
#Number of groups:  PARTICIPANT 81,  VERB 13 

#Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
#CONDITIONfictive            -2.5859391  0.5258230  -4.918 8.75e-07 ***
#CONDITIONmusic              -4.1085992  0.5264819  -7.804 6.00e-15 ***
#CONDITIONmetaphorical        -5.2785235  0.5336219  -9.892  < 2e-16 ***
#MUSIC.EDU                    0.2298752  0.1104333   2.082  0.03738 *  
#FREQ.ABS.BNC.LOG2             0.2054469  0.0862177   2.383  0.01718 *  
#CONDITIONfictive:MUSIC.EDU  -0.0431699  0.0805502  -0.536  0.59200    
#CONDITIONmusic:MUSIC.EDU    -0.0005444  0.0805525  -0.007  0.99461    
#CONDITIONmetaphorical:MUSIC.EDU-0.1648233  0.0808624  -2.038  0.04152 *
#CONDITIONfictive:FREQ.ABS.BNC.LOG2-0.0428723  0.0407203  -1.053  0.29241    
#CONDITIONmusic:FREQ.ABS.BNC.LOG2 -0.0161755  0.0404611  -0.400  0.68932
#CONDITIONmetaphorical:FREQ.ABS.BNC.LOG2  0.1290140  0.0409761   3.149  0.00164 ** 

#Threshold coefficients:
#  Estimate Std. Error z value
#1|2  -2.3379     1.0977  -2.130
#2|3  -0.9442     1.0969  -0.861
#3|4   0.0882     1.0965   0.080
#4|5   1.5446     1.0962   1.409

##Result:
# COndition, music.edu and freq are significant predictors for the ratings. There is an interaction between condition and music.edu (as predicted) as well as an interaction between condition and frequency (freq was predicted to have a main effect, an interaction with condition was not predicted).

Anova(modlit6, type = "III")
#Analysis of Deviance Table (Type II tests)

#Response: RATING_LIT
#                               LR Chisq Df Pr(>Chisq)    
#CONDITION                      113.085  3  < 2.2e-16 ***
#  MUSIC.EDU                    0.000    1    0.99982    
#FREQ.ABS.BNC.LOG2              0.000    1    0.99976    
#CONDITION:MUSIC.EDU            8.013    3    0.04575 *  
#  CONDITION:FREQ.ABS.BNC.LOG2  33.426   3  2.618e-07 ***

Anova(modlit6, type = "II")
#Analysis of Deviance Table (Type II tests)

#Response: RATING_LIT
#                                 LR Chisq Df Pr(>Chisq)    
#CONDITION                        2182.00  3  < 2.2e-16 ***
#  MUSIC.EDU                      3.41     1    0.06461 .  
#FREQ.ABS.BNC.LOG2                6.19     1    0.01283 *  
#  CONDITION:MUSIC.EDU            8.01     3    0.04575 *  
#  CONDITION:FREQ.ABS.BNC.LOG2    33.43    3  2.618e-07 ***

modlit7 <- clmm(RATING_LIT~CONDITION + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

anova(modlit6, modlit7) # models have the same values
#for now we continue with modlit6

modlit8 <- clmm(RATING_LIT~CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

anova(modlit6, modlit8) # modlit6 (with condition as a main effect) is significantly better
#        no.par   AIC  logLik LR.stat df Pr(>Chisq)    
#modlit7     14 10332 -5152.2                          
#modlit6     17 10225 -5095.7  113.08  3  < 2.2e-16 ***


#### OUTLIERS ####
#Look at the predicted values to check whether there are outliers  (visually noticable predicted values)

#add fitted as another column
ratings.complete <- ratings[complete.cases(ratings),]
ratings.complete$FITTED.PROB.literal <- fitted(modlit6)

#plot fitted values
#all
ggplot(data = ratings.complete, aes(x=MUSIC.EDU, y=FITTED.PROB.literal)) + geom_point() + facet_wrap(~CONDITION) + geom_jitter()

#in individual ratings
#ggplot(data = ratings.complete[ratings.complete$RATING_LIT=="1",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal)) + geom_point() + facet_wrap(~CONDITION) + ggtitle("Rating 1") + geom_jitter()
#does that makes sense? in general it is weird that we find a rating of 1 in the literal condition

#ggplot(data = ratings.complete[ratings.complete$RATING_LIT=="2",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal)) + geom_point() + facet_wrap(~CONDITION) + ggtitle("Rating 2") + geom_jitter()

#ggplot(data = ratings.complete[ratings.complete$RATING_LIT=="3",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal)) + geom_point() + facet_wrap(~CONDITION) + ggtitle("Rating 3") + geom_jitter()

#ggplot(data = ratings.complete[ratings.complete$RATING_LIT=="4",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal)) + geom_point() + facet_wrap(~CONDITION) + ggtitle("Rating 4") + geom_jitter()

#ggplot(data = ratings.complete[ratings.complete$RATING_LIT=="5",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal)) + geom_point() + facet_wrap(~CONDITION) + ggtitle("Rating 5") + geom_jitter()

#all, color = ratings
ggplot(data = ratings.complete, aes(x=MUSIC.EDU, y=FITTED.PROB.literal, color=RATING_LIT)) + geom_point() + facet_wrap(~CONDITION) + geom_jitter()

#conditions individually
ggplot(data = ratings.complete[ratings.complete$CONDITION=="literal",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal, color=RATING_LIT)) + geom_point() + geom_jitter() + ggtitle("CONDITION literal")

ggplot(data = ratings.complete[ratings.complete$CONDITION=="literal",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal, color=RATING_LIT)) + geom_point() + facet_wrap(~RATING_LIT) + ggtitle("CONDITION literal")

ggplot(data = ratings.complete[ratings.complete$CONDITION=="fictive",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal, color=RATING_LIT)) + geom_point() + geom_jitter() + ggtitle("CONDITION fictive")

ggplot(data = ratings.complete[ratings.complete$CONDITION=="fictive",], aes(x=MUSIC.EDU, y=FITTED.PROB.literal, color=RATING_LIT)) + geom_point() + facet_wrap(~RATING_LIT) + ggtitle("CONDITION fictive")

# ??? How do I decide when a data point is far enough off ?? --> consultation with Anne:

ggplot(data = ratings.complete, aes(x=MUSIC.EDU, y=FITTED.PROB.literal)) + geom_point() + facet_grid(RATING_LIT~CONDITION)
#no peculiar points

ggplot(data = ratings.complete, aes(x=FREQ.ABS.BNC.LOG2, y=FITTED.PROB.literal)) + geom_point() + facet_grid(RATING_LIT~CONDITION)
#eventuell 1

ggplot(data = ratings.complete, aes(x=CONDITION, y= FITTED.PROB.literal)) + geom_point() + facet_grid(RATING_LIT~.)
#no outlier

ggplot(data = ratings.complete, aes(x=MUSIC.EDU, y= FITTED.PROB.literal)) + geom_point() + facet_grid(RATING_LIT~.)
#ok

ggplot(data = ratings.complete, aes(x=FREQ.ABS.BNC.LOG2, y= FITTED.PROB.literal)) + geom_point() + facet_grid(RATING_LIT~.)

#result: Based on (a coarse) manual inspection, there do not seem to be any peculiarly noticable outliers.

#### RELEVELING ####

#Do the factor levels of condition interact differntly with MUSIC.EDU in a significant way? & #Do the factor levels of condition interact differently with FREQUENCY in a significant way?

#literal as reference level (default)
summary(modlit6) # see file

#Result: only the slope of metaphorical:MUSIC.EDU is significantly different form literal:MUSIC.EDU. That means that the effect of music edu on the ratings in the literal condition is not significantly different from the slope of music.edu in the fictive and the music condition BUT it is significantly different from the slope in the metaphorical condition

#fictive as reference level
ratings$CONDITION <- relevel(ratings$CONDITION, "fictive")
modlit6 <- clmm(RATING_LIT~CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(modlit6)

#relevel to music
ratings$CONDITION <- relevel(ratings$CONDITION, "music")

modlit6 <- clmm(RATING_LIT~CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(modlit6)

#relevel to metaphorical
ratings$CONDITION <- relevel(ratings$CONDITION, "metaphorical")

modlit6 <- clmm(RATING_LIT~CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(modlit6)

#relevel to literal

ratings$CONDITION <- relevel(ratings$CONDITION, "literal")

modlit6 <- clmm(RATING_LIT~CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(modlit6)

#can factor levels of condition be collapsed?

ratings$CONDITION2 <- paste(ratings$CONDITION)
ratings$CONDITION2 <- gsub("fictive", "literal", ratings$CONDITION2)
ratings$CONDITION2 <- gsub("music", "literal", ratings$CONDITION2)
ratings$CONDITION2 <- as.factor(ratings$CONDITION2)
summary(ratings$CONDITION2)

attach(ratings)

modlit7 <- clmm(RATING_LIT~CONDITION2 + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + CONDITION2:MUSIC.EDU + CONDITION2:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

anova(modlit6, modlit7) # modlit6 is significantly better
#--> collapsing L, F, and Mu in one category does not make the model significantly better

#only L and Mu
ratings$CONDITION2 <- paste(ratings$CONDITION)
ratings$CONDITION2 <- gsub("music", "literal", ratings$CONDITION2)
ratings$CONDITION2 <- as.factor(ratings$CONDITION2)
summary(ratings$CONDITION2)

attach(ratings)

modlit7 <- clmm(RATING_LIT~CONDITION2 + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + CONDITION2:MUSIC.EDU + CONDITION2:FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

anova(modlit6, modlit7) # modlit6 is significantly better
#--> collapsing L, and Mu in one category does not make the model significantly better


#### predictions ####
##determine the nature of the effects numerically ##

#predict() does not work with clmm objects but we can use the effect() function instead (STG l. 2536ff.; cf. STG l. 3387), note, however, that effect() requires the threshold to be flexible (i.e. setting them to "equidistant" or "symmetric" is not possible despite the fact that setting them to "symmetric" makes the model significantly better)

#### the interaction with condition ##

pred.intact.cond <- effect("CONDITION", modlit6)
pred.intact.cond.data <- data.frame(pred.intact.cond$x, pred.intact.cond$prob)
pred.intact.cond.data1 <- data.frame(pred.intact.cond.data, RATING_LIT.pred=sub("^prob.X","",names(pred.intact.cond.data)[-1][max.col(pred.intact.cond.data[,-1])]))


#### the interaction CONDITION:MUSIC.EDU ##

pred.intact <- effect("CONDITION:MUSIC.EDU", modlit6)
pred.intact.data <- data.frame(pred.intact$x, pred.intact$prob)
pred.intact.data1 <- data.frame(pred.intact.data, RATING_LIT.pred=sub("^prob.X","",names(pred.intact.data)[-(1:2)][max.col(pred.intact.data[,-(1:2)])]))

# predicted probabilities and predicted rating for each of the four conditions
literal.pred <- pred.intact.data1[pred.intact.data1$CONDITION=="literal",] # music.edu does not influece literalness ratings
fictive.pred <- pred.intact.data1[pred.intact.data1$CONDITION=="fictive",] #music.edu influences fictive ratings, with higher music edu, fictive is rated as more literal (detail: with higher musical.edu the probabilities for rating 4, and 5 increase, the probability for rating 1,2,3 decrease)
music.pred <- pred.intact.data1[pred.intact.data1$CONDITION=="music",] # music.edu influeces music ratings, with higher music.edu, musical motion is rated as more literal (detail: with higher musical edu the probabilities for a rating of 3,4, or 5 increase, while the probability for a rating of 1,2 decreases)
metaphor.pred <- pred.intact.data1[pred.intact.data1$CONDITION=="metaphorical",] # music edu does not influece the metaphorical ratings

#### plot the predictions ##

#predicted ratings
ggplot(pred.intact.data1, aes(x=MUSIC.EDU, y=as.numeric(as.character(RATING_LIT.pred)), color = CONDITION)) + geom_line() + geom_point()  + ggtitle("CONDITION*MUSICAL BACKGROUND") + labs(y="Predicted rating (literalness)", x="knowledge of (classical) music")
  # (RATING.LIT.pred as numeric bc outwise rating of "3" does not appear     in plot bc it is never predicted)

#predicted probabilities
# a plot for each rating level
qplot(MUSIC.EDU, prob.X1, color=CONDITION, data = pred.intact.data1, ylim = c(0,1), ylab = "predicted probability for rating 1") + geom_line()
qplot(MUSIC.EDU, prob.X2, color=CONDITION, data = pred.intact.data1, ylim = c(0,1), ylab = "predicted probability for rating 2") + geom_line()
qplot(MUSIC.EDU, prob.X3, color=CONDITION, data = pred.intact.data1, ylim = c(0,1), ylab = "predicted probability for rating 3") + geom_line()
qplot(MUSIC.EDU, prob.X4, color=CONDITION, data = pred.intact.data1, ylim = c(0,1), ylab = "predicted probability for rating 4") + geom_line()
qplot(MUSIC.EDU, prob.X5, color=CONDITION, data = pred.intact.data1, ylim = c(0,1), ylab = "predicted probability for rating 5") + geom_line()

#all in one plot (par(mfrow) does not work with ggplot?)
aa <- pred.intact.data1$CONDITION
bb <- pred.intact.data1$MUSIC.EDU
cc <- unlist(append(pred.intact.data1[3], pred.intact.data1[c(4:7)]))

data.interim <- data.frame(condition=rep(aa,5), music.edu=rep(bb, 5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=20))

ggplot(data.interim, aes(x=music.edu, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION*MUSIC.EDU for individual rating levels") + labs(y="predicted probability", x="level of musical education")
# noch: Reihenfolge legend ??ndern (L, F, Mu, Met)

## area plot
data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (completely literal)","4", "3", "2", "1 (not literal at all)")) #re-organisers factor levels for the plot

colnames(data.interim)[5] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=rev(levels(data.interim$RATING))) + ggtitle("CONDITION*MUSICAL BACKGROUND") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

#legend reversed
ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=levels(data.interim$RATING)) + ggtitle("CONDITION*MUSICAL BACKGROUND") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

#safe
png("plot_area_literalness.png", width = 13, height = 8, units = "in", res = 400)
ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=levels(data.interim$RATING)) + xlab("Degree of musical knowledge") + ylab("Predicted probabilities") + theme(text = element_text(size = 20))+ theme(legend.key.size = unit(2, "lines"))
dev.off()

#Result: High level of musical education leads to rating the musical motion sentences as more literal. (And high level of musical education leads to rating the fictive motion sentences as more literal, same for literal); metaphorical condition is not effected by music.edu

 ##########################################

#### the interaction CONDITION:FREQ ##

pred.intact2 <- effect("CONDITION:FREQ.ABS.BNC.LOG2", modlit6)
pred.intact.data2 <- data.frame(pred.intact2$x, pred.intact2$prob)
pred.intact.data2 <- data.frame(pred.intact.data2, RATING_LIT.pred=sub("^prob.X","",names(pred.intact.data2)[-(1:2)][max.col(pred.intact.data2[,-(1:2)])]))

# predicted probabilities and predicted rating for each of the four conditions
literal.pred <- pred.intact.data2[pred.intact.data2$CONDITION=="literal",] # freq does not have an influence on the literal condition
fictive.pred <- pred.intact.data2[pred.intact.data2$CONDITION=="fictive",] # freq has an influence on the fictive condition, with higher frequencies, fictive motion sentences are rated as more literal
music.pred <- pred.intact.data2[pred.intact.data2$CONDITION=="music",] # frequency does not have an influence on the musical motion ratings
metaphor.pred <- pred.intact.data2[pred.intact.data2$CONDITION=="metaphorical",] # freq has an effect on the metaphorical motion ratings, with higher frequency of the motion verb, metaphorical motion is rated as more literal

#### plot the predictions ##

#predicted ratings
ggplot(pred.intact.data2, aes(x=FREQ.ABS.BNC.LOG2, y=as.numeric(as.character(RATING_LIT.pred)), color = CONDITION)) + geom_line() + geom_point()  + ggtitle("CONDITION*FREQUENCY") + labs(y="Predicted rating (literalness)", x="Absolute frequency of motion verb (BNC, logged)")
# (RATING.LIT.pred as numeric bc outwise rating of "3" does not appear     in plot bc it is never predicted)

#predicted probabilities
# a plot for each rating level
qplot(FREQ.ABS.BNC.LOG2, prob.X1, color=CONDITION, data = pred.intact.data2, ylim = c(0,1), ylab = "predicted probability for rating 1") + geom_line()
qplot(FREQ.ABS.BNC.LOG2, prob.X2, color=CONDITION, data = pred.intact.data2, ylim = c(0,1), ylab = "predicted probability for rating 2") + geom_line()
qplot(FREQ.ABS.BNC.LOG2, prob.X3, color=CONDITION, data = pred.intact.data2, ylim = c(0,1), ylab = "predicted probability for rating 3") + geom_line()
qplot(FREQ.ABS.BNC.LOG2, prob.X4, color=CONDITION, data = pred.intact.data2, ylim = c(0,1), ylab = "predicted probability for rating 4") + geom_line()
qplot(FREQ.ABS.BNC.LOG2, prob.X5, color=CONDITION, data = pred.intact.data2, ylim = c(0,1), ylab = "predicted probability for rating 5") + geom_line()

#all in one plot (par(mfrow) does not work with ggplot?)
aa <- pred.intact.data2$CONDITION
bb <- pred.intact.data2$FREQ.ABS.BNC.LOG2
cc <- unlist(append(pred.intact.data2[3], pred.intact.data2[c(4:7)]))

data.interim <- data.frame(condition=rep(aa,5), freq=rep(bb, 5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=28))

ggplot(data.interim, aes(x=freq, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION*FREQUENCY for individual rating levels") + labs(y="predicted probability", x="Absolute frequency of motion verb (BNC, logged)")
# noch: Reihenfolge legend ??ndern (L, F, Mu, Met)


#only display individual conditions
#literal
ggplot(data.interim[data.interim$condition=="literal",], aes(x=freq, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION:FREQUENCY for individual rating levels") + labs(y="predicted probability", x="frequency of motion verb")

#fictive
ggplot(data.interim[data.interim$condition=="fictive",], aes(x=freq, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION:FREQUENCY for individual rating levels") + labs(y="predicted probability", x="frequency of motion verb") + ylim(c(0,1))

#music
ggplot(data.interim[data.interim$condition=="music",], aes(x=freq, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION:FREQUENCY for individual rating levels") + labs(y="predicted probability", x="frequency of motion verb") + ylim(c(0,1))

#metaphorical
ggplot(data.interim[data.interim$condition=="metaphorical",], aes(x=freq, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION:FREQUENCY for individual rating levels") + labs(y="predicted probability", x="frequency of motion verb") + ylim(c(0,1))



#### area plot ####
data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (completely literal)","4", "3", "2", "1 (not literal at all)")) #re-organisers factor levels for the plot

colnames(data.interim)[5] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

ggplot(data.interim, aes(x=freq, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Reds", breaks=rev(levels(data.interim$RATING))) + ggtitle("CONDITION*FREQUENCY") + xlab("Absolute frequency of motion verb (BNC, logged)") + ylab("predicted probabilities (stacked)")

#legend reversed
ggplot(data.interim, aes(x=freq, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Reds", breaks=levels(data.interim$RATING)) + ggtitle("CONDITION*FREQUENCY") + xlab("Absolute frequency of motion verb (BNC, logged)") + ylab("predicted probabilities (stacked)")

#save
png("plot_area_literalness_freq.png", width = 13, height = 8, units = "in", res = 400)
ggplot(data.interim, aes(x=freq, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Reds", breaks=levels(data.interim$RATING)) + xlab("Absolute frequency of motion verb (BNC, logged)") + ylab("Predicted probabilities") + theme(text = element_text(size = 20)) + theme(legend.key.size = unit(2, "lines"))
dev.off()

##### only freq effect

pred.intact <- effect("FREQ.ABS.BNC.LOG2", modlit6)
pred.intact.data <- data.frame(pred.intact$x, pred.intact$prob)
pred.intact.data1 <- data.frame(pred.intact.data, RATING_LIT.pred=sub("^prob.X","",names(pred.intact.data)[-1][max.col(pred.intact.data[,-1])]))

#all (and super simple)
plot(pred.intact.data1$FREQ.ABS.BNC.LOG2, pred.intact.data1$RATING_LIT.pred)


#### accuracy of the model / Modelg??te ####

model <-clmm(RATING_MOTION ~ CONDITION * MUSIC.EDU + (1 | PARTICIPANT) + (1 | VERB), data = ratings)
model0 <- clmm(RATING_MOTION ~ 1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings$MUSIC.EDU),])
#model.base <- clmm(RATING_MOTION ~ (CONDITION*FREQ.ABS.BNC.LOG2*MUSIC.EDU)^3 + EDU + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),]) 
model.base1 <- clmm(RATING_MOTION ~ (CONDITION*FREQ.ABS.BNC.LOG2*MUSIC.EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings) 

anova(model, model0)
#anova(model,model.base) #model.base is better...
anova(model, model.base1) # no difference...

#AIC
modlit6$info[5] #10225.32

#comparision to null model with randoms
modlit0 <- clmm(RATING_LIT~1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

AIC(modlit0, modlit6)
#        df      AIC
#modlit0  6 12435.98
#modlit6 17 10225.32

anova(modlit0, modlit6)
#        no.par   AIC  logLik LR.stat df Pr(>Chisq)    
#modlit0      6 12436 -6212.0                          
#modlit6     17 10225 -5095.7  2232.7 11  < 2.2e-16 ***

#comparison to null model without randoms
modlit0.1 <- clm(RATING_LIT~1, data = ratings[complete.cases(ratings),])

AIC(modlit0.1, modlit6)
#          df      AIC
#modlit0.1  4 13207.88
#modlit6   17 10225.32

anova(modlit0.1, modlit6)
#          no.par   AIC  logLik LR.stat df Pr(>Chisq)    
#modlit0.1      4 13208 -6599.9                          
#modlit6       17 10225 -5095.7  3008.6 13  < 2.2e-16 ***

#Nagelkerke's Pseudo R-squared
#cf.http://rcompanion.org/handbook/G_12.html

#BIC
BIC(modlit6) #[1] 10333.08

#comparison to null model with random effects ("conditional")
nagel.a <- nagelkerke(fit = modlit6, null = modlit0)
nagel.a[[2]][3] # Nagelkerke = 0.435959
nagel.a[[3]][4] # p = 0
# The model performs significantly better than the null model (with randoms)

#comparison to null model without random effects ("marginal")
nagel.b <- nagelkerke(fit = modlit6, null = modlit0.1) 
nagel.b[[2]][3] # Nagelkerke = 0.535703 (vorher 0.56731)
nagel.b[[3]][4] # p = 0

nagel.b[[2]][3]-nagel.a[[2]][3] #pseudo R2 without randoms - pseudo R2 with randoms --> gives you info about the effect of randoms
#0.099744 (before: 0.540026)

#model only with randoms compated to naked null model to get R2 for model onyl with randoms
nagel.c <- nagelkerke(fit = modlit0, null = modlit0.1) # 0.17

#precent correct predictions


ratings2 <- ratings[complete.cases(ratings),]

ratings2$FITTED <- fitted(modlit6)
summary(ratings2$FITTED)

#### random intercepts ####
ranef(modlit6) #random effects (list, one dataframe for each random effect - calculates intercept for each participant, and for each verb)

#for the 2 random effects individually
randeffs <- ranef(modlit6)$PARTICIPANT
mean(randeffs[,1]); sd(randeffs[,1]) # mean = -0.000190982, sd=0.9621481
randeffs2 <- ranef(modlit6)$VERB
mean(randeffs2[,1]); sd(randeffs2[,1]) # -0.0006007638; 0.7214494

#2 ways of plotting the random interceps for each random effect

par(mfrow=c(1,2))
plot(randeffs, main="random: PARTICIPANT"); abline(v=mean(randeffs[,1]), col="red"); abline(v=mean(randeffs[,1])-sd(randeffs[,1])); abline(v=mean(randeffs[,1])+sd(randeffs[,1]))
plot(randeffs2, main="random: VERB"); abline(v=mean(randeffs2[,1]), col="blue"); abline(v=mean(randeffs2[,1])-sd(randeffs2[,1])); abline(v=mean(randeffs2[,1])+sd(randeffs2[,1]))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(randeffs[,1], main="random: PARTICIPANT") 
boxplot(randeffs2[,1], main="random: VERB")
par(mfrow=c(1,1))
#suggests outliers? ANNE says no because by having the random effects, the model adjusts for their variation

#do the random effects make the model significantly better?
modlit6.without <- clm(RATING_LIT~CONDITION*MUSIC.EDU+CONDITION*FREQ.ABS.BNC.LOG2, data = ratings[complete.cases(ratings),])
summary(modlit6.without)

anova(modlit6, modlit6.without) # the model with randoms is significantly better
#AIC modlit6 = 10225; AIC modlit6.without = 11249

#test for effect of individual randoms
modlit6.without.participant <- clmm(RATING_LIT~CONDITION*MUSIC.EDU+CONDITION*FREQ.ABS.BNC.LOG2 + (1|VERB), data = ratings[complete.cases(ratings),])
modlit6.witouht.verb <- clmm(RATING_LIT~CONDITION*MUSIC.EDU+CONDITION*FREQ.ABS.BNC.LOG2 + (1|PARTICIPANT), data = ratings[complete.cases(ratings),])

anova(modlit6, modlit6.without.participant) # modlit 6 is significantly better

anova(modlit6, modlit6.witouht.verb) # modlit 6 is significantly better
#so both randoms are significant

# --> both random effects make the model significantly better

#plot the randoms (cf. Christensen clmm2 tutorial)

#this plot shows differences in raters (I think in ratings[complete.cases], we have only 70 raters left); x-axis indicates ith rater, y-axis indicates what? random intercept adjustments?
ci <- modlit6$ranef[1:81] + qnorm(0.975) * sqrt(modlit6$condVar[1:81]) %o% c(-1, 1)
ord.re <- order(modlit6$ranef[1:81])
ci <- ci[order(modlit6$ranef[1:81]),]
plot(1:81, modlit6$ranef[1:81][ord.re], axes=FALSE, ylim=range(ci),
     xlab="ith Rater", ylab="Rater effect")
axis(1, at=1:81, labels = ord.re)
axis(2)
for(i in 1:81) segments(i, ci[i,1], i, ci[i, 2])
abline(h = 0, lty=2)

#ranef = the conditional modes of the random effects, sometimes referred to as "random effect estimates". (aus help)
#condVar = the conditional variances of the random effects at their conditional modes.
#(Ranef is estimates for random effect (here rater) and condVar is the varience of these estimates (something like how raters deviate from their mean estimate))


#for verb
ci2 <- modlit6$ranef[82:94] + qnorm(0.975) * sqrt(modlit6$condVar[82:94]) %o% c(-1, 1)
ord.re2 <- order(modlit6$ranef[82:94])
ci2 <- ci2[order(modlit6$ranef[82:94]),]
plot(1:13, modlit6$ranef[82:94][ord.re2], axes=FALSE, ylim=range(ci2),
     xlab="ith VERB", ylab="VERB effect")
axis(1, at=1:13, labels = ord.re2)
axis(2)
for(i in 1:13) segments(i, ci2[i,1], i, ci2[i, 2])
abline(h = 0, lty=2)

#ascend always rated higher?, and race rated lower?

#von Anne:

randeffs.pariticpant <- ranef(modlit6)$PARTICIPANT # storing random intercept adjustments in an object
randeffs.pariticpant$PARTICIPANT <- rownames(randeffs.pariticpant) #creates a new column "PARTICIPANT" which has as its contents the rownames of the original dataframe
colnames(randeffs.pariticpant) <- c("intercept", "participant") #change column names
randeffs.pariticpant$participant <- as.factor(randeffs.pariticpant$participant) # make "participant" a factor (it is not numeric! the numbers are indices for the respective participant!)
randeffs.pariticpant$participant <- factor(randeffs.pariticpant$participant, levels=randeffs.pariticpant$participant[order(randeffs.pariticpant$intercept)])
qplot(intercept, participant, data = randeffs.pariticpant, geom = "point")
#this is very similar to the plots derived from Christensen, the difference is that the plot here is transposed and what is nicer about it is that the labels on the y-axis directly give the participant index 8not the ith participant), however here we do not have confidence intervals

#for verb:
randeffs.verb <- ranef(modlit6)$VERB
randeffs.verb$verb <- rownames(randeffs.verb)
colnames(randeffs.verb) <- c("intercept", "verb")
randeffs.verb$verb <- as.factor(randeffs.verb$verb)
randeffs.verb$verb <- factor(randeffs.verb$verb, levels = randeffs.verb$verb[order(randeffs.verb$intercept)])
qplot(intercept, verb, data = randeffs.verb)

#### testing model assumptions ####

#testing the ordinality assumption (i.e. whether the dependent variable behaves in an ordinal fashion wrt each predictor) (STG p. 317)
#STG uses plot.xmean.ordinaly to test for model assumptions (cf. p. 320)
# plot should be in-/decreasing with the factor levels and be roughly parallel (Harrell 2001:335) (cf. l. 3106)
library(rms)
plot.xmean.ordinaly(RATING_LIT~CONDITION)# perfect
plot.xmean.ordinaly(RATING_LIT~MUSIC.EDU) # well, still ok, better than for motion (maybe because for motion "3" is rarely used)
plot.xmean.ordinaly(RATING_LIT~FREQ.ABS.BNC.LOG2) # super

#plot.xmean.ordinaly(RATING_LIT~CONDITION*MUSIC.EDU) # not possible




#### ACTUAL MOTION RATINGS ####

#### model selection ####
## clmm approach with maximal model ##

#without SEX and AGE (turned out to be insiginficant in a previous selection process)
modmot <- clmm(RATING_MOTION~(CONDITION*MUSIC.EDU*FREQ.ABS.BNC.LOG2)^3 + LGE.EN + EDU + QUESTNNR + LING.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings.complete[complete.cases(ratings.complete),])
summary(modmot)

drop1(modmot, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ (CONDITION * MUSIC.EDU * FREQ.ABS.BNC.LOG2)^3 +  LGE.EN + EDU + QUESTNNR + LING.EDU + (1 | PARTICIPANT) + (1 | VERB)
#                                       Df    AIC    LRT Pr(>Chi)  
#<none>                                     7947.4                  
#LGE.EN                                   1 7948.3 2.9128  0.08788 .
#EDU                                      3 7950.7 9.2622  0.02600 *
#  QUESTNNR                               2 7944.3 0.9297  0.62822  
#LING.EDU                                 1 7949.9 4.4627  0.03464 *
#  CONDITION:MUSIC.EDU:FREQ.ABS.BNC.LOG2  3 7944.6 3.2344  0.35687  

modmot1 <- update(modmot, ~. -CONDITION:MUSIC.EDU:FREQ.ABS.BNC.LOG2)
summary(modmot1)
anova(modmot, modmot1)

drop1(modmot1, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + LGE.EN + EDU + QUESTNNR + LING.EDU + (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2 + MUSIC.EDU:FREQ.ABS.BNC.LOG2
#                           Df    AIC    LRT  Pr(>Chi)    
#<none>                         7944.6                     
#LGE.EN                       1 7945.5  2.892   0.08902 .  
#EDU                          3 7947.8  9.215   0.02657 *  
#  QUESTNNR                     2 7941.6  0.928   0.62885    
#LING.EDU                     1 7947.1  4.488   0.03414 *  
 # CONDITION:MUSIC.EDU          3 7982.5 43.914 1.574e-09 ***
 # CONDITION:FREQ.ABS.BNC.LOG2  3 7941.8  3.172   0.36581    
#MUSIC.EDU:FREQ.ABS.BNC.LOG2  1 7943.4  0.798   0.37160 

modmot2 <- update(modmot1, ~. -MUSIC.EDU:FREQ.ABS.BNC.LOG2)
summary(modmot2)
anova(modmot1, modmot2)

drop1(modmot2, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + LGE.EN + EDU + QUESTNNR + LING.EDU + (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU + CONDITION:FREQ.ABS.BNC.LOG2
#                           Df    AIC    LRT  Pr(>Chi)    
#<none>                         7943.4                     
#LGE.EN                       1 7944.3  2.891   0.08906 .  
#EDU                          3 7946.6  9.199   0.02676 *  
#  QUESTNNR                     2 7940.4  0.925   0.62983    
#LING.EDU                     1 7945.9  4.497   0.03396 *  
#  CONDITION:MUSIC.EDU          3 7981.3 43.899 1.586e-09 ***
#  CONDITION:FREQ.ABS.BNC.LOG2  3 7940.7  3.310   0.34621 

modmot3 <- update(modmot2, ~. -CONDITION:FREQ.ABS.BNC.LOG2)
summary(modmot3)
anova(modmot2, modmot3)

drop1(modmot3, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + LGE.EN + EDU + QUESTNNR + LING.EDU + (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU
#                     Df    AIC    LRT  Pr(>Chi)    
#<none>                 7940.7                     
#FREQ.ABS.BNC.LOG2    1 7939.7  0.994   0.31885    
#LGE.EN               1 7941.6  2.890   0.08913 .  
#EDU                  3 7943.9  9.206   0.02668 *  
#  QUESTNNR             2 7937.7  0.921   0.63109    
#LING.EDU             1 7943.2  4.499   0.03391 *  
#  CONDITION:MUSIC.EDU  3 7978.7 43.973 1.529e-09 ***

modmot4 <- update(modmot3, ~. -QUESTNNR)
summary(modmot4)
anova(modmot3, modmot4)

drop1(modmot4, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ CONDITION + MUSIC.EDU + FREQ.ABS.BNC.LOG2 + LGE.EN + EDU + LING.EDU + (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU
#Df    AIC    LRT  Pr(>Chi)    
#<none>                 7937.7                     
#FREQ.ABS.BNC.LOG2    1 7936.7  0.993   0.31912    
#LGE.EN               1 7938.3  2.615   0.10586    
#EDU                  3 7940.3  8.640   0.03448 *  
#  LING.EDU             1 7940.1  4.424   0.03543 *  
#  CONDITION:MUSIC.EDU  3 7975.8 44.099 1.438e-09 ***

modmot5 <- update(modmot4, ~. -FREQ.ABS.BNC.LOG2)
summary(modmot5)
anova(modmot4, modmot5)

drop1(modmot5, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ CONDITION + MUSIC.EDU + LGE.EN + EDU + LING.EDU + (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU
#Df    AIC    LRT  Pr(>Chi)    
#<none>                 7936.7                     
#LGE.EN               1 7937.3  2.613   0.10600    
#EDU                  3 7939.3  8.628   0.03467 *  
#  LING.EDU             1 7939.1  4.422   0.03547 *  
#  CONDITION:MUSIC.EDU  3 7974.7 44.031 1.486e-09 ***

modmot6 <- update(modmot5, ~. -LGE.EN)
summary(modmot6)
anova(modmot5, modmot6)

drop1(modmot6, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ CONDITION + MUSIC.EDU + EDU + LING.EDU + (1 |  PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU
#Df    AIC    LRT  Pr(>Chi)    
#<none>                 7937.3                     
#EDU                  3 7940.8  9.531    0.0230 *  
#  LING.EDU             1 7938.7  3.438    0.0637 .  
#CONDITION:MUSIC.EDU  3 7975.3 44.006 1.505e-09 ***

modmot7 <- update(modmot6, ~. -LING.EDU)
#note that LING.EDU is just not significant
summary(modmot7)
anova(modmot6, modmot7)

drop1(modmot7, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ CONDITION + MUSIC.EDU + EDU + (1 | PARTICIPANT) + (1 | VERB) + CONDITION:MUSIC.EDU
#Df    AIC    LRT  Pr(>Chi)    
#<none>                 7938.7                     
#EDU                  3 7947.9 15.190  0.001661 ** 
#  CONDITION:MUSIC.EDU  3 7976.8 44.135 1.413e-09 ***

# all remaining predictors are significant

summary(modmot7)
#check for 3-way interactions
modmot8 <- clmm(RATING_MOTION~(CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings.complete[complete.cases(ratings.complete),])
summary(modmot8)
anova(modmot7, modmot8) #modmot 8 is significantly better

drop1(modmot8, test = "Chisq")
#Single term deletions

#Model:
#  RATING_MOTION ~ (CONDITION * MUSIC.EDU * EDU)^3 + (1 | PARTICIPANT) + (1 | VERB)
#Df    AIC    LRT  Pr(>Chi)    
#<none>                     7774.6                     
#CONDITION:MUSIC.EDU:EDU  9 7824.7 68.171 3.473e-11 ***

#### final model ####
modmot8 <- clmm(RATING_MOTION~(CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

modmot8 <- clmm(RATING_MOTION~(CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings)

summary(modmot8)

Anova(modmot8, type = "III")
#Analysis of Deviance Table (Type II tests)

#Response: RATING_MOTION
#LR Chisq Df Pr(>Chisq)    
#CONDITION                 98.055  3  < 2.2e-16 ***
#  MUSIC.EDU                  0.000  1          1    
#EDU                        0.000  3          1    
#CONDITION:MUSIC.EDU        0.000  3          1    
#CONDITION:EDU             85.578  9  1.250e-14 ***
#  MUSIC.EDU:EDU              0.000  3          1    
#CONDITION:MUSIC.EDU:EDU   68.171  9  3.473e-11 ***

#### final model without EDU ####

modmot9 <- clmm(RATING_MOTION~CONDITION*MUSIC.EDU + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])
summary(modmot9)

AIC(modmot9)

#### OUTLIERS ####

#add fitted as another column

ratings <- ratings[complete.cases(ratings),]

ratings$FITTED.PROB.motion <- fitted(modmot8)

ggplot(data = ratings[ratings$EDU=="school certificate",], aes(x=MUSIC.EDU, y=FITTED.PROB.motion)) + geom_point() + facet_grid(RATING_MOTION~CONDITION) + geom_jitter()
#looks ok

ggplot(data = ratings[ratings.complete$EDU=="BA",], aes(x=MUSIC.EDU, y=FITTED.PROB.motion)) + geom_point() + facet_grid(RATING_MOTION~CONDITION) + geom_jitter()

ggplot(data = ratings.complete[ratings.complete$EDU=="MA",], aes(x=MUSIC.EDU, y=FITTED.PROB.motion)) + geom_point() + facet_grid(RATING_MOTION~CONDITION) + geom_jitter()

ggplot(data = ratings.complete[ratings.complete$EDU=="PhD",], aes(x=MUSIC.EDU, y=FITTED.PROB.motion)) + geom_point() + facet_grid(RATING_MOTION~CONDITION) + geom_jitter()

#### RELEVELING ####

#literal
summary(modmot8)

#fictive
ratings$CONDITION <- relevel(ratings$CONDITION, "fictive")
attach(ratings)

modmot8 <- clmm(RATING_MOTION~(CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(modmot8)

#music
ratings$CONDITION <- relevel(ratings$CONDITION, "music")
attach(ratings)

modmot8 <- clmm(RATING_MOTION~(CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(modmot8)

#metaphorical
ratings$CONDITION <- relevel(ratings$CONDITION, "metaphorical")
attach(ratings)

modmot8 <- clmm(RATING_MOTION~(CONDITION*MUSIC.EDU*EDU)^3 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

summary(modmot8)

#### predictions ####

pred.intact3 <- effect("CONDITION:MUSIC.EDU:EDU", modmot8)
pred.intact3.data <- data.frame(pred.intact3$x, pred.intact3$prob) 
pred.intact3.data1 <- data.frame(pred.intact3.data, RATING_MOT.pred=sub("^prob.X","",names(pred.intact3.data)[-(1:3)][max.col(pred.intact3.data[,-(1:3)])]))

## plot the effect ##
#predicted ratings
ggplot(pred.intact3.data1, aes(x=MUSIC.EDU, y=as.numeric(as.character(RATING_MOT.pred)), color = CONDITION)) + geom_line() + geom_point() + ggtitle("CONDITION*MUSICAL BACKGROUND*EDUCATIONAL DEGREE") + labs(y="Predicted rating (actual motion)", x="knowledge of (classical) music") + facet_wrap(~EDU)+ geom_jitter(width = 0.1, height = 0.1)
# (RATING.LIT.pred as numeric bc outwise rating of "3" does not appear     in plot bc it is never predicted)


#predicted probabilities
ggplot(pred.intact3.data1, aes(x=MUSIC.EDU, y=prob.X1, color=CONDITION)) + geom_line() + geom_point() + ggtitle("COND:MUSIC.EDU:EDU Rating1") + facet_wrap(~EDU)

ggplot(pred.intact3.data1, aes(x=MUSIC.EDU, y=prob.X1, color=EDU)) + geom_line() + geom_point() + ggtitle("COND:MUSIC.EDU:EDU Rating1") + facet_wrap(~CONDITION)

ggplot(pred.intact3.data1, aes(x=MUSIC.EDU, y=prob.X2, color=EDU)) + geom_line() + geom_point() + ggtitle("COND:MUSIC.EDU:EDU Rating2") + facet_wrap(~CONDITION)

ggplot(pred.intact3.data1, aes(x=MUSIC.EDU, y=prob.X3, color=EDU)) + geom_line() + geom_point() + ggtitle("COND:MUSIC.EDU:EDU Rating3") + facet_wrap(~CONDITION)

ggplot(pred.intact3.data1, aes(x=MUSIC.EDU, y=prob.X4, color=EDU)) + geom_line() + geom_point() + ggtitle("COND:MUSIC.EDU:EDU Rating4") + facet_wrap(~CONDITION)

ggplot(pred.intact3.data1, aes(x=MUSIC.EDU, y=prob.X5, color=EDU)) + geom_line() + geom_point() + ggtitle("COND:MUSIC.EDU:EDU Rating5") + facet_wrap(~CONDITION)

#### is EDU correlated with MUSIC.EDU? ####

table(MUSIC.EDU,EDU)/52
chisq.test(table(MUSIC.EDU,EDU)/52)

cor.test(MUSIC.EDU, as.numeric(EDU),method = "kendall")
#no: correlation coeff is almost zero (Kendall's Tau = 0.09675197) and this is even significant (p<0.001), so we can even generalise that there is no correlation

#RESULT: EDU and MUSIC.EDU are not correlated

#all in one plot (par(mfrow) does not work with ggplot?)

aa <- pred.intact3.data$CONDITION
bb <- pred.intact3.data$MUSIC.EDU
dd <- pred.intact3.data$EDU
cc <- unlist(append(pred.intact3.data[4], pred.intact3.data[c(5:8)]))

data.interim <- data.frame(condition=rep(aa,5), music.edu=rep(bb, 5), edu=rep(dd,5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=80))

##area plot
data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1")) #re-organisers factor levels for the plot

colnames(data.interim)[6] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

ggplot(data.interim[data.interim$condition=="music",], aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~edu) + scale_fill_brewer(palette = "Greens", breaks=levels(data.interim$RATING)) + ggtitle("MUSICAL BACKGROUND*EDUCATION for MUSICAL MOTION") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

ggplot(data.interim[data.interim$condition=="fictive",], aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~edu) + scale_fill_brewer(palette = "Oranges", breaks=levels(data.interim$RATING)) + ggtitle("MUSICAL BACKGROUND*EDUCATION for FICTIVE MOTION") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

ggplot(data.interim[data.interim$condition=="literal",], aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~edu) + scale_fill_brewer(palette = "Greys", breaks=levels(data.interim$RATING)) + ggtitle("MUSICAL BACKGROUND*EDUCATION for LITERAL MOTION") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

ggplot(data.interim[data.interim$condition=="metaphorical",], aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~edu) + scale_fill_brewer(palette = "Purples", breaks=levels(data.interim$RATING)) + ggtitle("MUSICAL BACKGROUND*EDUCATION for METAPHORICAL MOTION") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

#### test 2-way interaction ####

pred.motion.2 <- effect("CONDITION:MUSIC.EDU", modmot8)
pred.motion.2 <- data.frame(pred.motion.2$x, pred.motion.2$prob)
pred.motion.2 <- data.frame(pred.motion.2, RATING_MOT.pred=sub("^prob.X","",names(pred.motion.2)[-(1:2)][max.col(pred.motion.2[,-(1:2)])]))

#probabilities in individual conditions
literal.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="literal",] # no influence of musical background
fictive.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="fictive",] # no influence of musical background
music.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="music",] # predicted rating stay 1 but probability for rating 1 (not at all related to motion) drops strongly (significantly?) with higher degree of musical knowledge
metaphorical.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="metaphorical",] # no influence of musical background

#plot the effect
#predicted ratings
ggplot(pred.motion.2, aes(x=MUSIC.EDU, y=as.numeric(as.character(RATING_MOT.pred)), color = CONDITION)) + geom_line() + geom_point() + ggtitle("CONDITION*MUSICAL BACKGROUND") + labs(y="Predicted rating (actual motion)", x="knowledge of (classical) music") + geom_jitter(width = 0.1, height = 0.1)
# (RATING.LIT.pred as numeric bc outwise rating of "3" does not appear     in plot bc it is never predicted)

#predicted probabilities
# a plot for each rating level
qplot(MUSIC.EDU, prob.X1, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 1") + geom_line()
qplot(MUSIC.EDU, prob.X2, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 2") + geom_line()
qplot(MUSIC.EDU, prob.X3, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 3") + geom_line()
qplot(MUSIC.EDU, prob.X4, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 4") + geom_line()
qplot(MUSIC.EDU, prob.X5, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 5") + geom_line()

#all in one plot (par(mfrow) does not work with ggplot?)
aa <- pred.motion.2$CONDITION
bb <- pred.motion.2$MUSIC.EDU
cc <- unlist(append(pred.motion.2[3], pred.motion.2[c(4:7)]))

data.interim <- data.frame(condition=rep(aa,5), music.edu=rep(bb, 5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=20))

ggplot(data.interim, aes(x=music.edu, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION*MUSICAL BACKGROUND for individual rating levels") + labs(y="predicted probability (actual motion)", x="knowledge of (classical) musical")
# noch: Reihenfolge legend ??ndern (L, F, Mu, Met)

#area plot
## area plot
data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (strongly associated\n   with actual motion)","4", "3", "2", "1 (not associated with\n   actual motion)")) #re-organisers factor levels for the plot

colnames(data.interim)[5] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=levels(data.interim$RATING)) + ggtitle("CONDITION*MUSICAL BACKGROUND") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

#safe
png("plot_area_actual.motion.png", width = 13, height = 8, units = "in", res = 400)
ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=levels(data.interim$RATING)) + xlab("Degree of musical knowledge") + ylab("Predicted probabilities") + theme(text = element_text(size = 20)) + theme(legend.key.size = unit(2, "lines"))
dev.off()


#### test CONDITION*MUSIC.EDU on model without EDU ####

modmot8.wo.EDU <- clmm(RATING_MOTION ~ CONDITION * MUSIC.EDU + (1 | PARTICIPANT) +  (1 | VERB), data = ratings)
summary(modmot8.wo.EDU)

pred.motion.2 <- effect("CONDITION:MUSIC.EDU", modmot8.wo.EDU)
pred.motion.2 <- data.frame(pred.motion.2$x, pred.motion.2$prob)
pred.motion.2 <- data.frame(pred.motion.2, RATING_MOT.pred=sub("^prob.X","",names(pred.motion.2)[-(1:2)][max.col(pred.motion.2[,-(1:2)])]))

#probabilities in individual conditions
literal.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="literal",] # no influence of musical background
fictive.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="fictive",] # no influence of musical background
music.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="music",] # predicted rating stay 1 but probability for rating 1 (not at all related to motion) drops strongly (significantly?) with higher degree of musical knowledge
metaphorical.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="metaphorical",] # no influence of musical background

#plot the effect
#predicted ratings
ggplot(pred.motion.2, aes(x=MUSIC.EDU, y=as.numeric(as.character(RATING_MOT.pred)), color = CONDITION)) + geom_line() + geom_point() + ggtitle("CONDITION*MUSICAL BACKGROUND") + labs(y="Predicted rating (actual motion)", x="knowledge of (classical) music") + geom_jitter(width = 0.1, height = 0.1)
# (RATING.LIT.pred as numeric bc outwise rating of "3" does not appear     in plot bc it is never predicted)

#predicted probabilities
# a plot for each rating level
qplot(MUSIC.EDU, prob.X1, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 1") + geom_line()
qplot(MUSIC.EDU, prob.X2, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 2") + geom_line()
qplot(MUSIC.EDU, prob.X3, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 3") + geom_line()
qplot(MUSIC.EDU, prob.X4, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 4") + geom_line()
qplot(MUSIC.EDU, prob.X5, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 5") + geom_line()

#all in one plot (par(mfrow) does not work with ggplot?)
aa <- pred.motion.2$CONDITION
bb <- pred.motion.2$MUSIC.EDU
cc <- unlist(append(pred.motion.2[3], pred.motion.2[c(4:7)]))

data.interim <- data.frame(condition=rep(aa,5), music.edu=rep(bb, 5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=20))

ggplot(data.interim, aes(x=music.edu, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION*MUSICAL BACKGROUND for individual rating levels") + labs(y="predicted probability (actual motion)", x="knowledge of (classical) musical")
# noch: Reihenfolge legend ??ndern (L, F, Mu, Met)

#area plot
## area plot
data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (strongly associated\n   with actual motion)","4", "3", "2", "1 (not associated with\n   actual motion)")) #re-organisers factor levels for the plot

colnames(data.interim)[5] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=levels(data.interim$RATING)) + ggtitle("CONDITION*MUSICAL BACKGROUND") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

#### test CONDITION*MUSIC.EDU on model without EDU, complete cases ####

modmot8.wo.EDU.cc <- clmm(RATING_MOTION ~ CONDITION * MUSIC.EDU + (1 | PARTICIPANT) +  (1 | VERB), data = ratings[complete.cases(ratings),])

pred.motion.2 <- effect("CONDITION:MUSIC.EDU", modmot8.wo.EDU.cc)
pred.motion.2 <- data.frame(pred.motion.2$x, pred.motion.2$prob)
pred.motion.2 <- data.frame(pred.motion.2, RATING_MOT.pred=sub("^prob.X","",names(pred.motion.2)[-(1:2)][max.col(pred.motion.2[,-(1:2)])]))

#probabilities in individual conditions
literal.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="literal",] # no influence of musical background
fictive.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="fictive",] # no influence of musical background
music.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="music",] # predicted rating stay 1 but probability for rating 1 (not at all related to motion) drops strongly (significantly?) with higher degree of musical knowledge
metaphorical.mot.pre <- pred.motion.2[pred.motion.2$CONDITION=="metaphorical",] # no influence of musical background

#plot the effect
#predicted ratings
ggplot(pred.motion.2, aes(x=MUSIC.EDU, y=as.numeric(as.character(RATING_MOT.pred)), color = CONDITION)) + geom_line() + geom_point() + ggtitle("CONDITION*MUSICAL BACKGROUND") + labs(y="Predicted rating (actual motion)", x="knowledge of (classical) music") + geom_jitter(width = 0.1, height = 0.1)
# (RATING.LIT.pred as numeric bc outwise rating of "3" does not appear     in plot bc it is never predicted)

#predicted probabilities
# a plot for each rating level
qplot(MUSIC.EDU, prob.X1, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 1") + geom_line()
qplot(MUSIC.EDU, prob.X2, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 2") + geom_line()
qplot(MUSIC.EDU, prob.X3, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 3") + geom_line()
qplot(MUSIC.EDU, prob.X4, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 4") + geom_line()
qplot(MUSIC.EDU, prob.X5, color=CONDITION, data = pred.motion.2, ylim = c(0,1), ylab = "predicted probability for rating 5") + geom_line()

#all in one plot (par(mfrow) does not work with ggplot?)
aa <- pred.motion.2$CONDITION
bb <- pred.motion.2$MUSIC.EDU
cc <- unlist(append(pred.motion.2[3], pred.motion.2[c(4:7)]))

data.interim <- data.frame(condition=rep(aa,5), music.edu=rep(bb, 5), pred.prob=cc, rating=rep(c("Rating.1", "Rating.2", "Rating.3", "Rating.4", "Rating.5"), each=20))

ggplot(data.interim, aes(x=music.edu, y=pred.prob, color=condition)) + geom_point() + geom_line() + facet_wrap(~rating) + ggtitle("CONDITION*MUSICAL BACKGROUND for individual rating levels") + labs(y="predicted probability (actual motion)", x="knowledge of (classical) musical")
# noch: Reihenfolge legend ??ndern (L, F, Mu, Met)

#area plot
## area plot
data.interim$rating2 <- factor(data.interim$rating, levels = c("Rating.5", "Rating.4", "Rating.3", "Rating.2", "Rating.1"), labels = c("5 (strongly associated\n   with actual motion)","4", "3", "2", "1 (not associated with\n   actual motion)")) #re-organisers factor levels for the plot

colnames(data.interim)[5] <- "RATING"
data.interim$condition <- factor(data.interim$condition, levels = c("literal", "fictive", "music", "metaphorical"))

ggplot(data.interim, aes(x=music.edu, y=pred.prob, fill=RATING)) + geom_area(colour="black", size=0.2, alpha=.4) + facet_wrap(~condition) + scale_fill_brewer(palette = "Blues", breaks=levels(data.interim$RATING)) + ggtitle("CONDITION*MUSICAL BACKGROUND") + xlab("degree of musical knowledge") + ylab("predicted probabilities (stacked)")

## RELEVEL




#### condition:edu ####

pred2 <- effect("CONDITION:EDU", modmot8)
pred2 <- data.frame(pred2$x, pred2$prob)
pred2 <- data.frame(pred2, RATING_MOT.pred=sub("^prob.X","",names(pred2)[-(1:2)][max.col(pred2[,-(1:2)])]))

#predicted ratings
#I will not plot them bc its 5 for literal and 1 for all the others

#predicted probabilities
lit.pred <- pred2[pred2$CONDITION=="literal",] # looks homogeneous
fic.pred <- pred2[pred2$CONDITION=="fictive",] # PhDs label fictive motion as more motional (esp. probability for rating "1" is much lower compared to school, BA, MA)
mus.pred <- pred2[pred2$CONDITION=="music",] # school certificate have lower probabilites for rating music as not at all related to motion (this is also true for PhDs but not as strong)
met.pred <- pred2[pred2$CONDITION=="metaphorical",] # PhDs exhibit lower probabilities for rating metaphorical motion as "1", i.e. they rate it as more motional

#plot the predicted probabilities
# a plot for each rating level
pred2$EDU <- relevel(pred2$EDU, "school certificate")
pred2$CONDITION <- factor(pred2$CONDITION, levels=c("literal", "fictive", "music", "metaphorical"))

ggplot(data = pred2, aes(x=CONDITION, y=prob.X1, color=EDU, group=EDU)) + geom_line() + geom_point()





#### accuracy of the model / Modelg??te ####

#AIC
modmot8$info[5] #7774.55 (the lower the better) / 7773.26 ohne 1|VERB

#comparision to null model with randoms
modmot0 <- clmm(RATING_MOTION~1 + (1|PARTICIPANT) + (1|VERB), data = ratings[complete.cases(ratings),])

AIC(modmot0, modmot8)
#df       AIC
#modmot0  6 11666.028
#modmot8 37  7774.553

anova(modmot0, modmot8) # modmot8 performs significantly better (< 2.2e-16 ***) 

#comparison to null model without randoms
modmot0.1 <- clm(RATING_MOTION~1, data = ratings[complete.cases(ratings),])

AIC(modmot0.1, modmot8)
#df       AIC
#modmot0.1  4 11846.429
#modmot8   37  7774.553

anova(modmot0.1, modmot8) # modmot8 performs significantly better (< 2.2e-16 ***) 

##BIC
BIC(modmot8)

#Nagelkerke's Pseudo R-squared
#cf.http://rcompanion.org/handbook/G_12.html

#comparison to null model with random effects ("conditional")
nagel.a <- nagelkerke(fit = modmot8, null = modmot0)
nagel.a[[2]][3] # Nagelkerke = 0.651548
nagel.a[[3]][4] # p = 0
# The model performs significantly better than the null model (with randoms)

#comparison to null model without random effects ("marginal")
nagel.b <- nagelkerke(fit = modmot8, null = modmot0.1) 
nagel.b[[2]][3] # Nagelkerke = 0.0.667518
nagel.b[[3]][4] # p = 0

nagel.b[[2]][3]-nagel.a[[2]][3] #pseudo R2 without randoms - pseudo R2 with randoms --> gives you info about the effect of randoms #0.01597 (literalness: 0.099744) - Does this mean that the variation of in participants and verb is not big, so the questions were well understood?

nagel.c <- nagelkerke(fit = modmot0, null = modmot0.1) # 0.04583 is Nagelkerke for model with only randoms

#### random intercepts ####

randeffs <- ranef(modmot8)$PARTICIPANT
mean(randeffs[,1]); sd(randeffs[,1]) # mean = 0.008037056 (lit: -0.000190982), sd= 0.9542014 (lit: 0.9621481)
randeffs2 <- ranef(modmot8)$VERB
mean(randeffs2[,1]); sd(randeffs2[,1]) # 0.0002982322 (lit: -0.0006007638); 0.04086876 (lit: 0.7214494)

#2 ways of plotting the random interceps for each random effect

par(mfrow=c(1,2))
plot(randeffs, main="random: PARTICIPANT"); abline(v=mean(randeffs[,1]), col="red"); abline(v=mean(randeffs[,1])-sd(randeffs[,1])); abline(v=mean(randeffs[,1])+sd(randeffs[,1]))
plot(randeffs2, main="random: VERB"); abline(v=mean(randeffs2[,1]), col="blue"); abline(v=mean(randeffs2[,1])-sd(randeffs2[,1])); abline(v=mean(randeffs2[,1])+sd(randeffs2[,1]))
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(randeffs[,1], main="random: PARTICIPANT") 
boxplot(randeffs2[,1], main="random: VERB")
par(mfrow=c(1,1))
#suggests outliers? ANNE says no because by having the random effects, the model adjusts for their variation

#do the random effects make the model significantly better?
modmot8.without <- clm(RATING_MOTION~CONDITION*MUSIC.EDU*EDU, data = ratings[complete.cases(ratings),])
summary(modmot8.without)

anova(modmot8, modmot8.without) # the model with randoms is significantly better

#test for effect of individual randoms
modmot.without.participant <- clmm(RATING_MOTION~CONDITION*MUSIC.EDU*EDU + (1|VERB), data = ratings[complete.cases(ratings),])
modmot.witouht.verb <- clmm(RATING_MOTION~CONDITION*MUSIC.EDU*EDU + (1|PARTICIPANT), data = ratings[complete.cases(ratings),])

anova(modmot8, modmot.without.participant) # modmot 8 is significantly better

anova(modmot8, modmot.witouht.verb) # modmot8 is not significantly better with verb as a random effect than without it

# --> only participant as a random effect makes the model significantly better

#plot the random intercepts

randeffs.pariticpant <- ranef(modmot8)$PARTICIPANT # storing random intercept adjustments in an object
randeffs.pariticpant$PARTICIPANT <- rownames(randeffs.pariticpant) #creates a new column "PARTICIPANT" which has as its contents the rownames of the original dataframe
colnames(randeffs.pariticpant) <- c("intercept", "participant") #change column names
randeffs.pariticpant$participant <- as.factor(randeffs.pariticpant$participant) # make "participant" a factor (it is not numeric! the numbers are indices for the respective participant!)
randeffs.pariticpant$participant <- factor(randeffs.pariticpant$participant, levels=randeffs.pariticpant$participant[order(randeffs.pariticpant$intercept)])
qplot(intercept, participant, data = randeffs.pariticpant, geom = "point")
#this is very similar to the plots derived from Christensen, the difference is that the plot here is transposed and what is nicer about it is that the labels on the y-axis directly give the participant index 8not the ith participant), however here we do not have confidence intervals

#for verb:
randeffs.verb <- ranef(modmot8)$VERB
randeffs.verb$verb <- rownames(randeffs.verb)
colnames(randeffs.verb) <- c("intercept", "verb")
randeffs.verb$verb <- as.factor(randeffs.verb$verb)
randeffs.verb$verb <- factor(randeffs.verb$verb, levels = randeffs.verb$verb[order(randeffs.verb$intercept)])
qplot(intercept, verb, data = randeffs.verb, xlim = c(-1,1))

#hardly any adjustment need for verb intercepts


#### testing model assumptions ####

#testing the ordinality assumption (i.e. whether the dependent variable behaves in an ordinal fashion wrt each predictor) (STG p. 317)
#STG uses plot.xmean.ordinaly to test for model assumptions (cf. p. 320)
# plot should be in-/decreasing with the factor levels and be roughly parallel (Harrell 2001:335) (cf. l. 3106)
library(rms)
plot.xmean.ordinaly(RATING_MOTION~CONDITION:MUSIC.EDU:EDU) # ok
plot.xmean.ordinaly(RATING_MOTION~CONDITION) # perfect
plot.xmean.ordinaly(RATING_MOTION~MUSIC.EDU) # well, rating for 3 falls out of the picture
plot.xmean.ordinaly(RATING_MOTION~EDU) #well, might be still

