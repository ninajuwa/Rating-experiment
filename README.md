# Rating-experiment

This is the repository for a metaphoricity rating experiment.

The repository includes the following files:

clmm_ratings.R – the R script for the analysis

data_ratings.csv (main input for R script) – the raw data for the rating expriment

data_meta.participants.csv – raw participant meta data (sex, age, educational degree etc.)

More info:

#Ordinal mixed-effects logistic regression with clmm for literalness / actual motion ratings of musical motion sentences

#In an online rating experiment 83 participants were presented with 52 stimulus sentences. There were 2 blocks. In the first block, participants had to rate the stimulus sentences according to the question "In your opinion, how literal is the sentence?" on a scale from 1 = "not literal at all" to 5 = "completely literal" (only the two extremes were labeled). In the second block, participants had to rate the same stimulus sentences according to the question "How strongly do you associate the sentence with actual motion?" on a scale from 1 = "not at all associated with acutal motion" to 5 = "very strongly associated with actual motion". The stimulus sentences all expressed motion events. The motion event was either literal ("Carl arrived at the concert hall"), fictive ("The road arrives at an intersection"), related to music ("The music arrives at the final chord") or metaphorical ("The two studies arrive at very different conclusions"). The stimulus sentencs contain 13 different verbs, each verb appears in all 4 conditions (13 verbs * 4 conditions = 52 stimulus sentences). Every participant rated every stimulus for both questions. The stimulus sentences were pseudo-randomised in that two successive stimuli neither contained the same verb nor expressed the same motion condition. The aim of the experiment is to find out whether musical motion is perceived as less metaphorical and/or perceived as closer to fictive motion (a previous corpus analysis suggests that musical motion is both structurally and functionally similar to fictive motion). 

#Explanation of the two blocks: Instead of asking participants "How metaphorical is the sentence?", the question "How LITERAL is the sentence?" was chosen because participants may have very different ideas about what counts as a metaphor in contrast to linguists. Moreover, participants may judge a sentence as more literal simply because the sentence is very frequent. For this reason a second question "How stronly do you associate the sentence with acutal motion" was added which explicily spells out the source domain to try to get participants to judge how similar source and target domain of the stimulus sentences are.
