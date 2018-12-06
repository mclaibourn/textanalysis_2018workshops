###################################################################################
# TEXT ANALYSIS WITH R, PART III
# 1. Topic models - lda
# 2. Lexicon-based classification - sentiment
# 3. Model-based classification - naive Bayes
# 4. Model-based classification - svm, logit w/regularization
# Michele Claibourn (mclaibourn@virginia.edu)
# Created: November 9, 2018
# Updated: November 12, 2018
###################################################################################

# 0. set up your workspace -----
library(quanteda) # main text package
library(tidyverse) # for dplyr, piping, gather, etc.
library(topicmodels) 
library(wordcloud)
# install.packages("devtools")
# library(devtools)
# install_github("kbenoit/quanteda.dictionaries")
library(quanteda.dictionaries)
library(caret) # for confusion matrix
library(readtext)
library(RTextTools) # for classification models

setwd("~/Box Sync/mpc/textanalysis2018_3/") # set this to where you put the workshop materials
load("text1.RData")
rm(list=ls(pattern="^tweets")) # remove tweet objects


# 1. Topic models - lda ---- 
# ours to shape comments
comments_stem_dfm <- dfm(comments_dfm, stem=TRUE) # stem and group
comments_stemtrim_dfm <- dfm_trim(comments_stem_dfm,   # reduce dimensionality by
                                  min_termfrea = 10,  # trim low freqeuncy words
                                  min_docfreq = 3,   # trim words that occur across few docs
                                  max_docfreq = 690)  # trim words that appear in almost all docs
comments_stemtrim_dfm

comments_stemtrim_dfm <- comments_stemtrim_dfm[which(rowSums(comments_stemtrim_dfm) > 0),] # remove empty rows
comments_stemtrim_dtm <- convert(comments_stemtrim_dfm, to="topicmodels") # convert to a tm corpus
comments_stemtrim_dtm                                                     #  topicmodels package expects the triplet matrix format used by tm

# initial topic model, ~ 41 seconds
seed1=823 # for reproducibility
t <- proc.time()
comments_tm1 <- LDA(comments_stemtrim_dtm, k=20, control=list(seed=seed1)) # estimate lda model with 20 topics
proc.time() - t
str(comments_tm1) # what's contained in the results?

# examine, evaluate, interpret: terms to topics
terms(comments_tm1, 15) # top 15 terms for each topic
terms(comments_tm1, threshold=.015) # surpassing a threshold probability
comments_topiclab <- as.data.frame(t(terms(comments_tm1, 5))) # save top five words as label
comments_probterms <- as.data.frame(t(posterior(comments_tm1)$terms)) # all the topic-term probabilities
wordcloud(rownames(comments_probterms), comments_probterms$`1`,  # make a wordcloud of topic x
          max.words = 100, rot.per = 0.25, scale = c(3, .33))

# examine, evaluate, interpret: topics to documents
topics(comments_tm1, threshold=.20) # topics composing 20% or more of article
comments_probtopic <- as.data.frame(posterior(comments_tm1)$topics) # all the document-topic probabilities
max_topic <- which(comments_probtopic$`19` > 0.9) # index of comments with high prob of topic x
comments$text[comments$doc_id %in% max_topic] # pull out comments with high prob of topic x

# visualization: topic prevalence in corpus
comments_topicsum <- comments_probtopic %>% summarize_all(funs(sum))
comments_topicsum <- as.data.frame(t(comments_topicsum))
comments_topicsum$topic <- as.integer(row.names(comments_topicsum))
comments_topicsum$terms <- paste0(rownames(comments_topiclab), ":", comments_topiclab$V1, "-", comments_topiclab$V2, "-", comments_topiclab$V3, "-", comments_topiclab$V4, "-", comments_topiclab$V5)
ggplot(comments_topicsum, aes(x=reorder(terms, V1), y=V1)) + 
  geom_bar(stat="identity") + coord_flip()

# visualization: topic prevalence by comment type
comments_probtopic <- cbind(comments_probtopic, 
                            doc_id = as.integer(comments$doc_id),  # add doc_id
                            type = comments$type)                  # add comment type

comments_topictype <- comments_probtopic %>% 
  select(c(1:20,22)) %>%   # remove doc_id
  group_by(type) %>%       # group by type
  summarize_all(funs(sum)) # sum topic probabilities
# plot prevalence of topic 1 by comment type
plottitle <- paste0("Topic 1:", comments_topicsum$terms[1])
ggplot(comments_topictype, aes(x=type, y=`1`)) + geom_bar(stat = "identity") + 
  ggtitle(plottitle)

# plot all topics (gather comes from tidyr)
comments_typelong <- gather(comments_topictype, topic, prevalence, -type)
ggplot(comments_typelong, aes(x=type, y=prevalence)) + 
  geom_bar(stat = "identity") + facet_wrap(~topic, ncol=5)
# add terms to data frame for facet labelling
comments_typelong <- comments_typelong %>% mutate(topic = as.integer(topic)) %>% 
  left_join(comments_topicsum, by = "topic")
ggplot(comments_typelong, aes(x=type, y=prevalence)) + 
  geom_bar(stat = "identity") + facet_wrap(~terms, ncol=5)



# 2. Lexicon-based classification - sentiment ---- 
# a. quanteda comes with the Lexicoder Sentiment Dictionary built in
#    http://lexicoder.com/
lsd <- data_dictionary_LSD2015

# dictionary can be applied directly to corpus, returns dfm for words in dictionary
comments_lsd <- dfm(comments_corpus, dictionary = lsd)
head(comments_lsd, 10)

# turn this into a dataframe, create tone=positive-negative
comments_lsd <- convert(comments_lsd, to = "data.frame")

comments_lsd <- comments_lsd %>% 
  mutate(tone = positive - negative)
summary(comments_lsd$tone)
ggplot(comments_lsd, aes(x=tone)) + geom_histogram()

# add feature to initial data frame and plot
comments$tone <- comments_lsd$tone

ggplot(comments, aes(x=type, y=tone)) + 
  geom_violin() +
  ggtitle("'Sentiment' of Ours to Shape Comments") + 
  labs(y = "Overall Tone (Negative to Positive)", x = "Comment Type")

# most positive comment
comments %>% filter(tone == max(tone)) %>% select(type, text) 
# most negative comment
comments %>% filter(tone == min(tone)) %>% select(type, text)


# b. Ken Benoit (quanteda's author) has made a variety of other
#    dictionaries available, including the NRC Word-Emotion Association Lexicon
#    http://saifmohammad.com/WebPages/AccessResource.htm
nrc <- data_dictionary_NRC
# dictionary can be applied to already processed dfm
comments_nrc <- dfm_lookup(comments_dfm, dictionary = nrc)
head(comments_nrc, 10)

# turn this into a dataframe, add ntoken, create proportions
comments_nrc <- convert(comments_nrc, to = "data.frame")
comments_nrc <- comments_nrc %>% 
  mutate(words = ntoken(comments_dfm),
         angerper = (anger/words)*100,
         anticper = (anticipation/words)*100,
         disgustper = (disgust/words)*100,
         fearper = (fear/words)*100,
         joyper = (joy/words)*100,
         sadnessper = (sadness/words)*100,
         surpper = (surprise/words)*100,
         trustper = (trust/words)*100,
         posper = (positive/words)*100,
         negper = (negative/words)*100)
summary(comments_nrc[13:22])

# add feature to initial data frame
comments[, ncol(comments)+1:10] <- comments_nrc[, 13:22]
ggplot(comments, aes(x = type, y = negper)) +
  geom_violin()

# highest anger comment
comments %>% filter(angerper == max(angerper)) %>% select(type, text) 

# create "long" dataframe with average affect by comment type and plot
commenttype_nrc <- comments %>% group_by(type) %>% 
  summarize(anger = mean(angerper), anticipation = mean(anticper), 
            disgust = mean(disgustper), fear = mean(fearper), 
            joy = mean(joyper), sadness = mean(sadnessper), 
            surprise = mean(surpper), trust = mean(trustper), 
            positive = mean(posper), negative = mean(negper)) 
commenttype_nrc_long <- commenttype_nrc %>% 
  gather(affect, value, -type)
ggplot(commenttype_nrc_long, aes(x = affect, y = value, fill = type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title = "'Affect' of Ours to Shape Comments", subtitle = "By Comment Type",
       x = "Affect", y = "Average Percent of Words Connoting Affect") + 
  scale_fill_manual(values = c("blue3", "orange3", "turquoise3"), name = "Type")
  
# Note: dictionary can also be applied directly to processed tokens via "tokens_lookup"
# Note: dictionaries exist (and can be built) for many non-sentiment concepts



# 3. Model-based classification - naive Bayes ---- 
# a. ours to shape and quanteda
# randomly sample 70% of comments for training set
set.seed(823)
samp_docs <- 1:nrow(comments_dfm) # index vector
id_train <- sample(samp_docs, .7*length(samp_docs), replace = FALSE) # training index
head(id_train, 10)
docvars(comments_dfm, "id_numeric") <- as.integer(docnames(comments_dfm)) # create docvar with numeric id
training_dfm <- dfm_subset(comments_dfm, id_numeric %in% id_train) # get training set
testing_dfm <- dfm_subset(comments_dfm, !id_numeric %in% id_train) # get testing set

# train the naive Bayes classifier
comments_nb <- textmodel_nb(training_dfm, docvars(training_dfm, "type"), prior = "docfreq")
summary(comments_nb)
comments_nb_feat <- as.data.frame(coef(comments_nb)) # save features/words and weights as data frame
comments_nb_feat$features <- rownames(comments_nb_feat) # add features in rownames as column
comments_nb_feat %>% filter(community > 0.85) # features with high weights for community

# model can only use features that occur in both training and testingset
testing_dfm <- dfm_select(testing_dfm, training_dfm) # make the features identical
predicted_class <- predict(comments_nb, testing_dfm)

# inspect the classification 
actual_class <- docvars(testing_dfm, "type")
class_table <- table(actual_class, predicted_class)
class_table
confusionMatrix(class_table, mode = "everything") # from caret package


# b. course listing and quanteda
classes <- readtext("data/louslist/*.csv", text_field = "Description")
table(classes$Type)
classes <- classes %>% 
  mutate(semester = str_sub(doc_id, 4,6),
         doc_id = paste(str_sub(classes$doc_id, 4, 6), str_sub(classes$doc_id, 16), sep = "-")) %>% 
  filter(Type %in% c("Lecture", "Seminar", "Workshop"))
table(classes$semester)

# create labeled set, add numeric id
classes <- classes %>% 
  mutate(quant = ifelse(Mnemonic=="STAT" | Mnemonic=="DS", 1, 
                        ifelse(grepl("^REL", Mnemonic) | Mnemonic=="AAS", 0, NA)),
         id_numeric = 1:nrow(classes))
table(classes$quant)

# create corpus and dfm
class_corpus <- corpus(classes)
class_dfm <- dfm(class_corpus, toLower = TRUE, removePunct = TRUE,
              stem = TRUE, remove = stopwords("english"), verbose = TRUE) 
class_dfm

# work with just the labeled/coded courses
quant_index <- !is.na(classes$quant)
labeled_courses <- classes[quant_index,] # data frame of "labeled" classes
unlabeled_courses <- classes[!quant_index,] # data frame of "unlabeled" classses

labeled_dfm <- class_dfm[quant_index,] # document-feature matrix for labeled classes
unlabeled_dfm <- class_dfm[!quant_index,] # document-feature matrix for unlabeled classes

# divide coded data into training and testing sets
samp_docs <- labeled_courses$id_numeric # vector of ids for labeled classes from which to sample
set.seed(777)
train_docs <- sample(samp_docs, .7*length(samp_docs), replace = FALSE) # sample ids for training set
training_dfm <- dfm_subset(labeled_dfm, id_numeric %in% train_docs) # create training dfm
testing_dfm <- dfm_subset(labeled_dfm, !id_numeric %in% train_docs) # create testing dfm

# train the naive Bayes classifier
class_nb <- textmodel_nb(training_dfm, docvars(training_dfm, "quant"), prior = "docfreq")
summary(class_nb)

# model can only use features that occur in both training and testing set to predict
testing_dfm <- dfm_select(testing_dfm, training_dfm) # make the features identical
predicted_class <- predict(class_nb, testing_dfm) # predict test set

# inspect the classification 
actual_class <- docvars(testing_dfm, "quant") 
class_table <- table(actual_class, predicted_class)
class_table
confusionMatrix(class_table, mode = "everything") # from caret package

# what features predict quant == 1?
class_nb_feat <- as.data.frame(coef(class_nb))
class_nb_feat$features <- rownames(class_nb_feat) 
class_nb_feat %>% filter(`1` > 0.9) 

# apply the model to unlabeled data, generating classification
predict_unlabeled <- predict(class_nb, unlabeled_dfm)
table(predict_unlabeled)
labeled_quant <- as.data.frame(predict_unlabeled[predict_unlabeled == 1]) # extract ids for predicted 1
labeled_quant_ids <- rownames(labeled_quant) # doc_ids for predicted 1s
classes_quant <- classes %>% filter(doc_id %in% labeled_quant_ids) # data frame of classes predicted as 1
table(classes_quant$Title)

# apply the model to unlabeled data, generating classification probability
predict_unlabeled <- as.data.frame(predict(class_nb, unlabeled_dfm, type = "probability"))
sum(predict_unlabeled$`1` > 0.95) # can choose our own threshold for classification
labeled_quant_ids <- rownames(predict_unlabeled)[predict_unlabeled$`1` > 0.95]
classes_quant <- classes %>% filter(doc_id %in% labeled_quant_ids)
table(classes_quant$Title)



# 4. Model-based classification - RTextTools ----
# RTextTools provides many classification algorithms in common framework

# create container/list to be used in algorithms
samp_docs <- 1:nrow(labeled_dfm)
set.seed(777)
train_docs <- sample(samp_docs, .7*length(samp_docs), replace = FALSE) # training set
test_docs <- samp_docs[-train_docs]
cont <- create_container(labeled_dfm, labeled_courses$quant,
                         trainSize = train_docs, testSize = test_docs, virgin = FALSE)
str(cont)

# a. Support Vector Machines
svmModel <- train_model(cont, "SVM") # train the model
svmResult <- classify_model(cont, svmModel) # apply to test set
svmAnalytic <- create_analytics(cont, svmResult) # generate summaries
summary(svmAnalytic)
svmDoc <- svmAnalytic@document_summary

# b. Logistic with lasso regularization
glmModel <- train_model(cont, "GLMNET")
glmResult <- classify_model(cont, glmModel)
glmAnalytic <- create_analytics(cont, glmResult)
summary(glmAnalytic)
glmDoc <- glmAnalytic@document_summary

# Additional models: 
# MAXENT (maximum entropy logistic)
# TREE (classification tree)
# RF (random forest)
# BOOSTING (boosted logit)
# SLDA (linear discriminant analysis)
# BAGGING (bootstrap aggregation, trees)
# NNET (feed-forward neural network)

# b. classify unlabeled cases
# re-train the model on all of the labeled data
# and apply the result to the unlabeled data
# create container/list to be used in classification algorithms
cont_labeled <- create_container(labeled_dfm, labeled_courses$quant,
                                trainSize = 1:613, virgin = FALSE)

# estimate model
models_labeled <- train_models(cont_labeled, algorithms=c("SVM", "GLMNET"))

# apply model to unlabeled courses
pred_size = nrow(unlabeled_dfm) # a vector for labels
cont_unlabeled <- create_container(unlabeled_dfm, labels=rep(0,pred_size), testSize=1:pred_size, virgin=TRUE)
classify_unlabeled <- classify_models(cont_unlabeled, models_labeled)
ensemble_unlabeled <- create_scoreSummary(cont_unlabeled, classify_unlabeled)

# attach predictions to initial (unlabeled) data, look at results
classes_predicted <- cbind(unlabeled_courses, classify_unlabeled)
table(classes_predicted$GLMNET_LABEL)
classes_predicted %>% filter(GLMNET_LABEL == 1) %>% 
  ggplot(aes(x = GLMNET_PROB)) + geom_histogram(bins = 20)


save.image("text3.RData")
