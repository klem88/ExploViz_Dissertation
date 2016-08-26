#
.libPaths("C:/Users/clément/Documents/R/win-library/3.2")
setwd("C:/Users/clément/OneDrive/Dublin courses/Dissertation/R DICE")
#
library(topicmodels)
library(lsa)
library(vegan)
library(ggplot2)
library(tm)

#First scenario : local binary & inverse document frequency
w1.sparse <- lw_bintf(t(as.matrix(sparse.dtm))) * gw_idf(t(as.matrix(sparse.dtm)))
w1.sparse <- t(w1.sparse)
#Second scenario : local log term freq & global entropy
w2.sparse <- lw_logtf(t(as.matrix(sparse.dtm))) * gw_entropy(t(as.matrix(sparse.dtm)))
w2.sparse <- t(w2.sparse)
#tf * idf = term frequency * inverse document frequency = # times a word used in a doc * weight attributed to each term (the less used the bigger weight)
w3.sparse <- t(as.matrix(sparse.dtm)) * gw_idf(t(as.matrix(sparse.dtm)))
w3.sparse <- t(w3.sparse)
#log-tf * idf
w4.sparse <- lw_logtf(t(as.matrix(sparse.dtm))) * gw_idf(t(as.matrix(sparse.dtm)))
w4.sparse <- t(w4.sparse)

#w0.eucl <- dist(sparse.dtm)
w1.eucl <- dist(w1.sparse)
w2.eucl <- dist(w2.sparse)
w3.eucl <- dist(w3.sparse)
w4.eucl <- dist(w4.sparse)

#w0.cos <- cosine(as.matrix(t(sparse.dtm)))
#diag(w0.cos) <- 0
#w0.cos <- ifelse(w0.cos == 0, 0, 1 / w0.cos)

w1.cos <- cosine(t(w1.sparse))
diag(w1.cos) <- 0
w1.cos <- 1 - w1.cos

w2.cos <- cosine(t(w2.sparse))
diag(w2.cos) <- 0
w2.cos <- 1 - w2.cos

w3.cos <- cosine(t(w3.sparse))
diag(w3.cos) <- 0
w3.cos <- 1 - w3.cos

w4.cos <- cosine(t(w4.sparse))
diag(w4.cos) <- 0
w4.cos <- 1 - w4.cos

bin.sparse.dtm <- lw_bintf(as.matrix(sparse.dtm))
w0.jacc <- vegdist(bin.sparse.dtm, method = "jaccard")

#as.matrix(w0.eucl)[1:10, 1:10]
#as.matrix(w1.eucl)[1:10, 1:10]
#w0.cos[1:10, 1:10]
#as.matrix(w0.jacc)[1:10,1:10]

#w0.eucl.dimr <- cmdscale(w0.eucl, k = 2, eig = TRUE)
w1.eucl.dimr <- cmdscale(w1.eucl, k = 2, eig = TRUE)
w2.eucl.dimr <- cmdscale(w2.eucl, k = 2, eig = TRUE)
w3.eucl.dimr <- cmdscale(w3.eucl, k = 2, eig = TRUE)
w4.eucl.dimr <- cmdscale(w4.eucl, k = 2, eig = TRUE)

#w0.cos.dimr <- cmdscale(w0.cos, k = 2, eig = TRUE)
w1.cos.dimr <- cmdscale(w1.cos, k = 2, eig = TRUE)
w2.cos.dimr <- cmdscale(w2.cos, k = 2, eig = TRUE)
w3.cos.dimr <- cmdscale(w3.cos, k = 2, eig = TRUE)
w4.cos.dimr <- cmdscale(w4.cos, k = 2, eig = TRUE)

w0.jacc.dimr <- cmdscale(w0.jacc, k = 2, eig = TRUE)

bin.sparse.dtm <- lw_bintf(as.matrix(sparse.dtm))
adj <- as.matrix(bin.sparse.dtm) %*% t(as.matrix(bin.sparse.dtm))
diag(adj) <- 0

w0.adj.dimr <- cmdscale(adj, k = 2, eig = TRUE)

#k.means <- kmeans(adj, 5)
#k.means <- k.means$cluster
#k.means <- kmeans(as.matrix(sparse.dtm), 5)
#k.means <- node.list$comm

#######################################################
##HIERARCHICAL CLUSTERING##

#######################################################
##WRITE##
#points.w3.cos.dimr$docid <- as.numeric(dimnames(points.w3.cos.dimr)[[1]])
#points.w3.cos.dimr <- merge(points.w3.cos.dimr, d.books[ , c("docid", "title", "contributor1", "publishername")], by = "docid")

#con <- file("DICE/points.w3.cos.dimr.csv", encoding = "UTF-8")
#write.csv(points.w3.cos.dimr, con)
#remove(con)

#######################################################
##LDA##

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5
#Run LDA using Gibbs sampling
w0.lda.out <- LDA (sparse.dtm, k, method = "Gibbs",
                control = list(nstart = nstart,
                               seed = seed, best = best, 
                               burnin = burnin, iter = iter,
                               thin = thin))
k <- 10
#Run LDA using Gibbs sampling
w10.lda.out <- LDA (sparse.dtm, k, method = "Gibbs",
                   control = list(nstart = nstart,
                                  seed = seed, best = best, 
                                  burnin = burnin, iter = iter,
                                  thin = thin))

#docs to topics
w0.lda.topics <- data.frame(topicid = topics(w0.lda.out))
w0.lda.topics$docid <- as.numeric(dimnames(w0.lda.topics)[[1]])
w10.lda.topics <- data.frame(topicid = topics(w10.lda.out))

#top 6 terms in each topic
w0.lda.terms <- as.data.frame(terms(w0.lda.out,10))
w10.lda.terms <- as.data.frame(terms(w10.lda.out,10))

#w0.lda.terms <- t(as.matrix(w0.lda.terms))
#w0.lda.terms <- paste(w0.lda.terms[, 1], w0.lda.terms[, 2], w0.lda.terms[, 3], w0.lda.terms[, 4], w0.lda.terms[, 5], w0.lda.terms[, 6], w0.lda.terms[, 7], w0.lda.terms[, 8], w0.lda.terms[, 9], w0.lda.terms[, 10], sep = ", ")
#w0.lda.terms <- data.frame(topicid = 1:10, topic = w0.lda.terms)

#probabilities associated with each topic assignment per document
w0.topic.probabilities <- as.data.frame(w0.lda.out@gamma)
rownames(w0.topic.probabilities) <- w0.lda.out@documents

w10.topic.probabilities <- as.data.frame(w10.lda.out@gamma)
rownames(w10.topic.probabilities) <- w10.lda.out@documents

#Distance matrix of the topic.probabilities
w0.eucl.lda <- dist(w0.topic.probabilities)
w10.eucl.lda <- dist(w10.topic.probabilities)

w0.cos.lda <- cosine(t(as.matrix(w0.topic.probabilities)))
diag(w0.cos.lda) <- 0
w0.cos.lda <- 1 - w0.cos.lda

#as.matrix(eucl.lda)[1:10,1:10]
#cos.lda[1:10, 1:10]
w0.eucl.lda.dimr <- cmdscale(w0.eucl.lda, k = 2, eig = TRUE)
w10.eucl.lda.dimr <- cmdscale(w10.eucl.lda, k = 2, eig = TRUE)
w0.cos.lda.dimr <- cmdscale(w0.cos.lda, k = 2, eig = TRUE)

points.w0.eucl.lda <- data.frame(x = w0.eucl.lda.dimr$points[ , 1], y = w0.eucl.lda.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w10.eucl.lda <- data.frame(x = w10.eucl.lda.dimr$points[ , 1], y = w10.eucl.lda.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w0.cos.lda <- data.frame(x = w0.cos.lda.dimr$points[ , 1], y = w0.cos.lda.dimr$points[ , 2], topic = w0.lda.topics$topicid)

#test <- data.frame(k.m = as.numeric(names(k.means)), points = as.numeric(dimnames(points.w10.eucl.lda)[[1]]), cmds = as.numeric(dimnames(w0.eucl.lda.dimr$points)[[1]]))
#test$test <- ifelse(test$k.m == test$points && test$points == test$cmds, "ok", "erreur")
#table(test$test)

#w0.lda.topics <- merge(w0.lda.topics, w0.lda.terms, by = "topicid")

#points.w0.eucl.dimr <- data.frame(x = w0.eucl.dimr$points[ , 1], y = w0.eucl.dimr$points[ , 2], topic = k.means)
points.w1.eucl.dimr <- data.frame(x = w1.eucl.dimr$points[ , 1], y = w1.eucl.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w2.eucl.dimr <- data.frame(x = w2.eucl.dimr$points[ , 1], y = w2.eucl.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w3.eucl.dimr <- data.frame(x = w3.eucl.dimr$points[ , 1], y = w3.eucl.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w4.eucl.dimr <- data.frame(x = w4.eucl.dimr$points[ , 1], y = w4.eucl.dimr$points[ , 2], topic = w0.lda.topics$topicid)

#points.w0.cos.dimr <- data.frame(x = w0.cos.dimr$points[ , 1], y = w0.cos.dimr$points[ , 2], topic = k.means)
points.w1.cos.dimr <- data.frame(x = w1.cos.dimr$points[ , 1], y = w1.cos.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w2.cos.dimr <- data.frame(x = w2.cos.dimr$points[ , 1], y = w2.cos.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w3.cos.dimr <- data.frame(x = w3.cos.dimr$points[ , 1], y = w3.cos.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w4.cos.dimr <- data.frame(x = w4.cos.dimr$points[ , 1], y = w4.cos.dimr$points[ , 2], topic = w0.lda.topics$topicid)

points.w0.jacc.dimr <- data.frame(x = w0.jacc.dimr$points[ , 1], y = w0.jacc.dimr$points[ , 2], topic = w0.lda.topics$topicid)
points.w0.adj.dimr <- data.frame(x = w0.adj.dimr$points[ , 1], y = w0.adj.dimr$points[ , 2], topic = w0.lda.topics$topicid)

#test <- data.frame(k.m = as.numeric(names(k.means)), points = as.numeric(dimnames(points.w1.eucl.dimr)[[1]]), cmds = as.numeric(dimnames(w0.jacc.dimr$points)[[1]]))
#test$test <- ifelse(test$k.m == test$points && test$points == test$cmds, "ok", "erreur")
#table(test$test)


#######################################################
##WRITE##
#points.w0.eucl.lda$docid <- as.numeric(dimnames(points.w0.eucl.lda)[[1]])
#points.w0.eucl.lda <- merge(points.w0.eucl.lda, d.books[ , c("docid", "title", "contributor1", "publishername")], by = "docid")

#con <- file("DICE/points.w0.eucl.lda.csv", encoding = "UTF-8")
#write.csv(points.w0.eucl.lda, con)
#remove(con)

##############################################################
##VISUALISATION##

vis <- ggplot (points.w1.eucl.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) +
  scale_fill_discrete(labels = w0.lda.terms) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w1.eucl.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w2.eucl.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w2.eucl.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w3.eucl.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w3.eucl.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w4.eucl.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w4.eucl.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w1.cos.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w1.cos.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w2.cos.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w2.cos.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w3.cos.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w3.cos.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w4.cos.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w4.cos.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w0.jacc.dimr, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w0.jacc.dimr_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w0.cos.lda, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w0.cos.lda_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()


vis <- ggplot (points.w0.eucl.lda, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w0.eucl.lda_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()

vis <- ggplot (points.w10.eucl.lda, aes(x = x, y = y))
VIS <- vis + geom_point(aes(colour = as.character(topic), alpha = 0.8), shape = 15) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))

png(paste0("outR/points.w10.eucl.lda_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()

##################################################
##REMOVE UNNECESSARY HEAVY STUFF##
remove(adj)
remove(bin.sparse.dtm)
remove(w0.cos.lda)
remove(w0.eucl.lda)
remove(w0.jacc)
remove(w1.cos)
remove(w1.eucl)
remove(w1.sparse)
remove(w2.cos)
remove(w2.eucl)
remove(w2.sparse)
remove(w3.cos)
remove(w3.eucl)
remove(w3.sparse)
remove(w4.cos)
remove(w4.eucl)
remove(w4.sparse)
remove(w10.eucl.lda)

remove(bow.corpus)
remove(dtm)
