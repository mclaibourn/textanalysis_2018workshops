###################################################################################
# TEXT ANALYSIS WITH R, PART II
# 1. Review, dfm - bag-of-words, reducing dimensionality, including trim
# 2. Review, descriptives - frequencies, relative frequencies, keyness
# 3. Feature co-occurrence
# 4. Document similarity/groups
# 5. Document clustering - exploration
# 6. Document clustering - hierarchical
# 7. Document clustering - kmeans 
# 8. Topic models - lda
# 9. Topic models - stm
# Michele Claibourn (mclaibourn@virginia.edu)
# Created: October 18, 2018
# Updated: October 29, 2018
###################################################################################

# 0. set up your workspace -----
library(quanteda) # main text package
library(tidyverse) # for dplyr, stringr, piping, etc.
library(topicmodels) # for lda topic model
library(stm) # for structural topic model

setwd("~/Box Sync/mpc/textanalysis2018/") # set this to where you put the workshop materials
load("text1.RData")


# 1. Review, dfm (tokens, ngrams, reducing dimensionality)
# 2. Review, descriptives (frequencies, relative frequencies, keyness)


# 3. Feature co-occurrence (aka semantic networks) ---- 
# a. ours to shape comments
comments_dfm_trim <- dfm_trim(comments_dfm, min_termfreq=5) # reduce dfm
comments_fcm <- fcm(comments_dfm_trim) # generate feature co-occurrence
comment_top <- names(topfeatures(comments_fcm, 100)) # list of most frequently co-occurring words
comments_fcm <- fcm_select(comments_fcm, comment_top) # select these features from fcm
# visualize semantic network
textplot_network(comments_fcm)
size <- log(colSums(dfm_select(comments_dfm_trim, comment_top))) # scale for vertices
textplot_network(comments_fcm, min_freq = 0.75, vertex_size = size / max(size) * 3)

# b. trump tweets
# co-occuring hashtags 
tweets_tag <- dfm_select(tweets_dfm, ('#*')) # extract hashtags
tag_fcm <- fcm(tweets_tag) # generate feature co-occurrence
top_tags <- names(topfeatures(tweets_tag, 100)) # most frequently occurring tags
head(top_tags, 10)
tag_fcm <- fcm_select(tag_fcm, top_tags) # select these from fcm
size <- log(colSums(dfm_select(tweets_tag, top_tags))) # scale for vertices
textplot_network(tag_fcm, min_freq = 0.1, edge_color = 'orange3',
                 vertex_size = size / max(size) * 3)

# YOUR TURN: 
# co-occuring usernames (@) in tweets
tweets_user <- dfm_select(tweets_dfm, ('@*')) # extract hashtags
user_fcm <- fcm(tweets_user) # generate feature co-occurrence
top_user <- names(topfeatures(tweets_user, 100)) # most frequently occurring tags
head(top_user, 10)
user_fcm <- fcm_select(user_fcm, top_user) # select these from fcm
size <- log(colSums(dfm_select(tweets_user, top_user))) # scale for vertices
textplot_network(user_fcm, min_freq = 0.1, edge_color = 'orange3',
                 vertex_size = size / max(size) * 3)


# 4. Document similarity ---- 
# a. ours to shape comments: similarity by type
comments_stem_dfm <- dfm(comments_dfm, stem=TRUE, group="type") # stem and group
comments_simil <- textstat_simil(comments_stem_dfm,   # create similarity matrix
                                 method = "cosine", margin = "documents",
                                 upper = TRUE, diag = TRUE)
comments_simil

# YOUR TURN:
# b. trump tweets: similarity by source
tweets_stem_dfm <- dfm(tweets_dfm_trim, stem=TRUE, group="source") # stem and group
tweets_simil <- textstat_simil(tweets_stem_dfm,   # create similarity matrix
                                 method = "cosine", margin = "documents",
                                 upper = TRUE, diag = TRUE)
tweets_simil


# 5. Clustering documents - exploration ---- 
# a. ours to shape comments
# first a visual exploration
comments_stem_dfm <- dfm(comments_dfm, stem=TRUE) # stem and group
comments_mat <- as.matrix(dfm_tfidf(comments_stem_dfm))  # convert tf-idf weighted dfm to a matrix 
comments_mat <- comments_mat / apply(comments_mat, MARGIN=1, FUN=function(x) sum(x^2)^0.5)  # scale it
comments_pca <- prcomp(comments_mat) # principal components
comments_pca12 <- as.data.frame(comments_pca$x[,1:2]) # first two components
comments_pca12$doc_id = rownames(comments_pca12)
ggplot(comments_pca12, aes(x=PC1, y=PC2, label=doc_id)) +  # graph
  geom_text()
# what comments are standing apart?
comments_pc_group1 <- comments_pca12 %>% filter(PC1 > 0.75)
comments %>% filter(doc_id %in% comments_pc_group1$doc_id) %>% select(name, type, text)
comments_pc_group2 <- comments_pca12 %>% filter(PC1 > 0.09 & PC1 < 0.75)
comments %>% filter(doc_id %in% comments_pc_group2$doc_id) %>% select(type, text)


# 6. Clustering documents - hierarchical ----
# clustering: calculate distance, generate dendrogram, and label clusters
comments_stem_dfm <- dfm(comments_dfm, stem=TRUE) # stem
comments_dist <- textstat_dist(dfm_weight(comments_stem_dfm, "prop"),  # create distance matrix
                               method = "euclidean")
comments_ward <- hclust(comments_dist, method = "ward.D2")  # implement linkage method
plot(comments_ward, main="Euclidean Distance, Ward's Method",  # plot dendrogram
     xlab="", sub="", cex=.6)
rect.hclust(comments_ward, k=12, border="red")  # add helpful rectangles around clusters
comments_hc_12 <- cutree(comments_ward, k=12) # make cluster assignments for 12 groups
table(comments_hc_12)
split(docnames(comments_stem_dfm), comments_hc_12)

# examination: find most highly weighted words in each cluster
comment_stem_df <- convert(comments_stem_dfm, to = "data.frame") # coerce to dataframe
comment_stem_df <- data.frame(comment_stem_df, comments_hc_12) # append cluster labels
comment_cluster_list <- split(comment_stem_df[,-1], comment_stem_df$comments_hc_12) # split into lists by cluster
# apply function to each list: sum columns/weights, sort, save top 10
topwords <- lapply(comment_cluster_list, function(x) { 
  sort(apply(x[,-ncol(x)], 2, sum), decreasing=TRUE)[1:10]
})
topwords


# 7. Clustering documents - kmeans  ---- 
# a. ours to shape comments
clusterk10 <- kmeans(dfm_tfidf(comments_stem_dfm), 10)
split(docnames(comments_stem_dfm), clusterk10$cluster)

# how many clusters? plot the within group sum of squares: look for "elbow"
ssPlot <- function(data, maxCluster) { # 2 arguments: data and maximum k
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i-1] <- kmeans(data, centers=i)$tot.withinss
  }
  plot(2:maxCluster, SSw, type="b", xlab="# of Clusters", ylab="Within groups SS")
}
ssPlot(dfm_tfidf(comments_stem_dfm), 30)

set.seed(101706)
clusterk20 <- kmeans(dfm_tfidf(comments_stem_dfm), 20, iter.max = 25, nstart = 5)
split(docnames(comments_stem_dfm), clusterk20$cluster)

# make a dataframe with the first 2 PCs,  clusters from the kmeans, and metadata
comments_kmeans <- data.frame(comments, comments_pca12, cluster = as.factor(clusterk20$cluster))
# plot the first 2 PCs, colored by the cluster
ggplot(comments_kmeans, aes(x=PC1, y=PC2, color=cluster)) + 
#  scale_color_brewer(palette='Set1') +
  geom_point() +
  labs(title='K-Means Clusters on Principal Components (1,2)',
       x='PCA: Factor 1', y='PCA: Factor 2') +
  theme(legend.position='bottom', legend.title=element_blank())
# List obs in a cluster
table(comments_kmeans$cluster)
subset(comments_kmeans, cluster==2) 


# 8. Topic models - lda ---- 
# a. ours to shape comments
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

# examine, evaluate, interpret
terms(comments_tm1, 10) # top 10 terms for each topic
terms(comments_tm1, threshold=.015) # surpassing a threshold probability
comments_topiclab <- as.data.frame(t(terms(comments_tm1, 5))) # save top five words as label
comments_probterms <- as.data.frame(posterior(comments_tm1)$terms) # all the topic-term probabilities
topics(comments_tm1, threshold=.20) # topics composing 20% or more of article
comments_probtopic <- as.data.frame(posterior(comments_tm1)$topics) # all the document-topic probabilities

# visualization: topic prevalence in corpus
comments_topicsum <- comments_probtopic %>% summarize_all(funs(sum))
comments_topicsum <- as.data.frame(t(comments_topicsum))
comments_topicsum$topic <- as.integer(row.names(comments_topicsum))
comments_topicsum$terms <- paste0(comments_topiclab$V1, "-", comments_topiclab$V2, "-", comments_topiclab$V3, "-", comments_topiclab$V4, "-", comments_topiclab$V5)
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


# 9. Topic models - stm ---- 
# a. ours to shape comments
comments_stemtrim_stm <- convert(comments_stemtrim_dfm, to="stm") # convert to a stm corpus

# initial structural topic model, ~ 22 seconds
seed2=121 # for reproducibility
t <- proc.time()
comments_stm1 <- stm(comments_stemtrim_stm$documents, comments_stemtrim_stm$vocab, K = 20, 
                     max.em.its = 75, init.type = "Spectral", seed = seed2)
proc.time() - t
str(comments_stm1) # what's contained in the results?

# examine, evaluate, interpret
labelTopics(comments_stm1, n = 10) # associated words (prob, frex)
plot(comments_stm1, type = "summary") # topic prevalence
topicQuality(comments_stm1, comments_stemtrim_stm$documents) # topic quality

# metadata/topic relationships
type_effect <- estimateEffect(1:20 ~ type, comments_stm1, meta = comments_stemtrim_stm$meta)
plot(type_effect, covariate = "type", topics = 20, xlim = c(-0.02, 0.10),
     model = comments_stm1, labeltype = "prob", n = 5)



# Appendix: evaluating K ----
# a. stm's "searchK" function, ~ 2 minutes
t <- proc.time()
stmEval <- searchK(comments_stemtrim_stm$documents, comments_stemtrim_stm$vocab, K = c(10, 20, 30, 40, 50), 
                   init.type = "Spectral")
proc.time() - t
plot(stmEval)
# will estimate topic exclusivity and coherence, likelihood on a held-out set, and residuals for each model/k


# b. calculating perplexity: evaluating k on held-out data (test)
set.seed(121) # for reproducibility
train_size <- floor(0.75 * nrow(comments_stemtrim_dtm)) # 75% of sample
train_index <- sample(seq_len(nrow(comments_stemtrim_dtm)), size=train_size)

train <- comments_stemtrim_dtm[train_index,]
test <- comments_stemtrim_dtm[-train_index,]

# perplexity using a held-out set (from topicmodels), across a range of k
# run in parallel, ~ 2 minutes
library(parallel)
numCores <- detectCores() - 2

t <- proc.time()
trainK <- mclapply(seq(10,50,10), function(k){LDA(train, k)}, mc.cores=numCores)
perplexityK <- mclapply(trainK, function(k){perplexity(k, test)}, mc.cores=numCores)
proc.time() - t
# graph perplexity
px <- unlist(perplexityK)
perplexline <- as.data.frame(cbind(px, k=c(10,20,30,40,50))) # and add k values
ggplot(perplexline, aes(x=k, y=px)) + geom_line()
