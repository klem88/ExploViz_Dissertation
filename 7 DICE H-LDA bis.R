#
.libPaths("C:/Users/clément/Documents/R/win-library/3.2")
setwd("C:/Users/clément/OneDrive/Dublin courses/Dissertation/R DICE")
#
library(topicmodels)
library(ggplot2)
library(tm)
library(class) #knn
library(lsa) #gw_idf function
library(dataQualityR)

########################################################
## RECONSTRUCTION oF MAN.CLASSIF FOR THESIS : SPLIT BOW1 FIELD ## 
man.classif.diss <- read.csv("Data/Manual_reclassification_2.csv", sep = "\t", quote = "", header = TRUE, fileEncoding = "UTF-8")
man.classif.diss <- merge(bow[, c("docid", "title.x", "calais", "concept")], man.classif.diss, by = "docid")
man.classif.diss$to.del <- rep(1432, 1)
#checkDataQuality(man.classif.diss, "outR/Num.csv", "outR/Cat.csv")

## MAN.CLASSIF with 1432 records ##
man.classif <- read.csv("Data/Manual_reclassification_2.csv", sep = "\t", quote = "", header = TRUE, fileEncoding = "UTF-8")
man.classif <- merge(bow[, c("docid", "title.x", "bow1")], man.classif, by = "docid")

## ADDING PRECISION AND ADDING HIGH LEVEL CLASSIFICATION

man.classif <- read.csv("Data/man.classif.csv", sep = ",", quote = '"', header = TRUE, fileEncoding = "UTF-8")
man.classif <- merge(bow[, c("docid", "title.x", "bow1")], man.classif, by = "docid")

filter <- read.csv("Data/filter1.csv", sep = ",", quote = '"',  header = TRUE, fileEncoding = "UTF-8")
to.delete <- which(man.classif$docid %in% filter$docid)
man.classif <- man.classif[-to.delete, ]

#input <- merge(man.classif, bow[ , c("docid", "semantic.input")], by = "docid")

bow.corpus <- VectorSource(paste(man.classif$Level1, man.classif$Level2, man.classif$bow1))
bow.corpus <- Corpus(bow.corpus)
#bow.corpus <- tm_map(bow.corpus,stemDocument)

#my.stopwords <- read.csv("outR/term_frequencies_to_delete_stemmed.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8", quote = '"')
#my.stopwords <- read.csv("outR/term_frequencies_to_delete_unstemmed.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8", quote = '"')
#bow.corpus <- tm_map(bow.corpus, removeWords, my.stopwords$term)
bow.corpus <- tm_map(bow.corpus, removeWords, stopwords("English"))

dtm <- DocumentTermMatrix(bow.corpus)
sparse.dtm <- removeSparseTerms(dtm, 0.98)
sparse.dtm$dimnames$Docs <- as.character(man.classif$docid)
sparse.dtm <- sparse.dtm[rowSums(as.matrix(sparse.dtm)) > 0, ]

###########################################################
## Analysis of books to filter
dtm$dimnames$Docs <- as.character(man.classif$docid)

dtm <- t(as.matrix(dtm)) * gw_entropy(t(as.matrix(dtm)))
#dtm <- t(as.matrix(dtm)) * gw_idf(t(as.matrix(dtm))) #the idf is 1+log2(nb tot doc/nb doc with term)
dtm <- t(dtm)

test <- colSums(as.matrix(dtm))
dtm <- t(as.matrix(dtm)) * test
dtm <- t(dtm)

test <- rowSums(dtm)
#head(sort(test, decreasing = FALSE), 100)
test <- bow[bow$docid %in% head(names(sort(test, decreasing = FALSE)),150), c("docid", "title.y")] #Not sorted inside the results BUT the group shows the first sorted books

con <- file("C:/Users/clément/OneDrive/Dublin courses/Dissertation/Experiment/Results/R/data/sparse_unstemmed_dtm.csv", encoding = "UTF-8")
write.csv(as.matrix(sparse.dtm), con)
remove(con)

test <- sparse.dtm[rowSums(as.matrix(sparse.dtm)) == 0, ]
bow[bow$docid %in% test$dimnames$Docs, "title.y"]
############################################################
remove(bow.corpus)
remove(my.stopwords)
remove(dtm)

#as.matrix(sparse.dtm)[1:1, ]
#ncol(dtm)
#nrow(dtm)
ncol(sparse.dtm)
nrow(sparse.dtm)
#colnames(as.matrix(sparse.dtm))
#########################################
##EXECUTE SPARSE DTM BEFORE## 

burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 9
#Run LDA using Gibbs sampling
w0.lda.out <- LDA (sparse.dtm, k, method = "Gibbs",
                   control = list(nstart = nstart,
                                  seed = seed, best = best, 
                                  burnin = burnin, iter = iter,
                                  thin = thin))

w0.lda.topics <- data.frame(topicid = topics(w0.lda.out))
w0.lda.topics$docid <- as.numeric(dimnames(w0.lda.topics)[[1]])

w0.lda.terms <- as.data.frame(terms(w0.lda.out,10))
w0.lda.terms
table(w0.lda.topics$topicid, man.classif$Level1)
table(w0.lda.topics$topicid, man.classif$Level2)
#man.classif$title.y[w0.lda.topics$topicid == 1 & man.classif$Level1 == "Business"]

w0.topic.probabilities <- as.data.frame(w0.lda.out@gamma)
rownames(w0.topic.probabilities) <- w0.lda.out@documents
w0.topic.probabilities$docid <- w0.lda.out@documents

top.correlation <- cor(w0.topic.probabilities[,1:length(unique(w0.lda.topics$topicid))])
dissimilarity.correlation <- as.dist(1 - top.correlation)
clust <- hclust(dissimilarity.correlation)
plot(clust, main = "Hierarchical clustering", sub = " based on the dissimilarity correlation matrix (1-correlation)")
#9 topics #Level1&2 and Bow1 
clust.map <- data.frame(topicid = clust$order, 
                        topicid.bis = c(1:7, rep(8, 2)),
                        topicid_orig = c(rep(1, 3), rep(2, 3), rep(3, 3)), 
                        label_x  = c(rep("Business", 3), rep("Finance", 3), rep("Economics", 3)), 
                        label_y = c("HR - Leadership", "Strategy - Production", "Marketing", "Market finance - Risk", "Corporate finance", "Corporate finance - Risk", "Economics theory", rep("Mathematical economics", 2)))

#6 topics #Level1&2 and Bow1 
#clust.map <- data.frame(topicid = clust$order, 
#                        topicid.bis = c(1, 2, 3, 4, 5, 6),
#                        topicid_orig = c(rep(1, 2), rep(2, 2), rep(3, 2)), 
#                        label_x  = c(rep("Business", 2), rep("Finance", 2), rep("Economics", 2)),
#                        label_y = c("Marketing", "Strategy - HR - Leadership", "Corporate finance", "Financial management", "Mathematical economics", "Economics theory"))

#6 topics #Level1 and semantic.input 
#clust.map <- data.frame(topicid = clust$order, 
#                        topicid.bis = c(1, 2, 3, 4, 5, 6),
#                        topicid_orig = c(rep(1, 2), rep(2, 2), rep(3, 2)),
#                        label_x  = c(rep("Business", 2), rep("Economics", 2), rep("Finance", 2)),
#                        label_y = c("Marketing - Strategy", "HR - Leadership", "Mathematical economics", "Economics theory", "Market finance", "Corporate finance"))

#8 topics #semantic.input only 
#clust.map <- data.frame(topicid = clust$order, 
#                        topicid.bis = 1:length(unique(w0.lda.topics$topicid)),
#                       topicid_orig = c(rep(1, 4), rep(2, 2), 3, 4), 
#                       label_x  = c(rep("Business", 4), rep("Finance", 2), "Economics", "Mathematical economics"),
#                       label_y = c("Human ressources", "Leadership and organisations", "Strategy", "Marketing", "Corporate finance", "Market finance - Risk", "Economics theory", "Mathematical economics"))

w0.lda.topics <- merge(w0.lda.topics, clust.map, by = "topicid")
w0.lda.topics <- w0.lda.topics[ , -1]
names(w0.lda.topics)[2] <- "topicid"

w0.eucl.lda <- dist(w0.topic.probabilities[ , -ncol(w0.topic.probabilities)])
#hist(w0.eucl.lda, main = "Histogram of topics' probabilities Euclidean distance")

w0.eucl.lda.dimr <- cmdscale(w0.eucl.lda, k = 2, eig = TRUE)

points.w0.eucl.lda <- data.frame(x = w0.eucl.lda.dimr$points[ , 1], y = w0.eucl.lda.dimr$points[ , 2], docid = as.numeric(dimnames(w0.eucl.lda.dimr$points)[[1]]))
points.w0.eucl.lda <- merge(points.w0.eucl.lda, w0.lda.topics, by = "docid")
#Wpoints.w0.eucl.lda <- merge(points.w0.eucl.lda, man.classif[ , c("docid", "Level1", "Level2")], by = "docid")

#i <- 1
#while (i < nrow(points.w0.eucl.lda)){
#points.w0.eucl.lda$topic.orig[i] <- if(points.w0.eucl.lda$Level1[i] == "Business"){1} else {
#  if(points.w0.eucl.lda$Level1[i] == "Finance") {2} else {3}
#}
#print(i)
#i <- i+1
#}


##Known issue. When a docid has the exact same word distribution than another book, the distance is 0. This is equal than the docid itself. 
##If there are more than 1 docid with the 0 distance, the docid are ordered increasingly. Therefore the docid can be placed in second position
## instead of inside the first column... which is a prerequisite for the record to be correct. There are 40 wrong records in the end, which create 
##duplicate docid.

j <- 1
sim.dist <- as.data.frame(matrix(nrow = 0, ncol = 11))
while (j <= nrow(w0.topic.probabilities))
{
  order.temp <- head(order(as.matrix(w0.eucl.lda)[j, ]), 11)
  order.temp.names <- as.numeric(w0.lda.out@documents[order.temp])
  sim.dist <- rbind(sim.dist, order.temp.names)
  print(j)
  j <- j + 1
}
names(sim.dist) <- c("docid", 1:10)

#points.eucl.lda.lev2.enriched[ grep("Ronin and Revolutionaries", points.eucl.lda.lev2.enriched$title), "docid" ]
#bow[bow$docid == 10276772, "title.x"]
#sim.dist[sim.dist$docid == 10276772, ]
#length(unique(sim.dist$docid))

#######################################################
##COMPUTE THE CLUSTERS POLYGONS AREA##

i <- 1
while (i <= length(unique(points.w0.eucl.lda$topicid_orig)))
{
  temp <- points.w0.eucl.lda[points.w0.eucl.lda$topicid_orig == i, c("x", "y")]
  chull <- chull(temp)
  name <- paste0("coords.", i)
  assign(name, temp[c(chull, chull[1]), ])
  print(i)
  i <- i + 1
}

#######################################################
##WRITE##
points.eucl.lda.lev2 <- merge(points.w0.eucl.lda, d.books[ , c("docid", "title", "contributor1", "publishername", "cover_url")], by = "docid")

points.eucl.lda.lev2.enriched <- merge(points.eucl.lda.lev2, concept.aggregated, by = "docid", all.x = TRUE)
points.eucl.lda.lev2.enriched <- merge(points.eucl.lda.lev2.enriched, abstracts, by = "docid")
points.eucl.lda.lev2.enriched <- merge(points.eucl.lda.lev2.enriched, opcalais, by = "docid", all.x = TRUE)
points.eucl.lda.lev2.enriched <- merge(points.eucl.lda.lev2.enriched, man.classif[ , c("docid", "Level1", "Level2")], by = "docid")

#points.eucl.lda.lev2.enriched <- merge(points.eucl.lda.lev2.enriched, legend, by.x = "topicid.orig", by.y = "topicid")
#points.eucl.lda.lev2.enriched <- merge(points.eucl.lda.lev2.enriched, legend, by = "topicid")
#points.eucl.lda.lev2.enriched <- merge(points.eucl.lda.lev2.enriched, bow[ , c("docid", "knn")], by = "docid")

con <- file("DICE/points.eucl.lda.lev2.csv", encoding = "UTF-8")
write.csv(points.eucl.lda.lev2.enriched, con)
remove(con)

remove(w0.eucl.lda)
remove(w0.lda.out)
remove(eucl.lda)
remove(vis)

#######################################################
##WRITE##
con <- file("DICE/coords.1.csv", encoding = "UTF-8")
write.csv(coords.1, con)
remove(con)

con <- file("DICE/coords.2.csv", encoding = "UTF-8")
write.csv(coords.2, con)
remove(con)

con <- file("DICE/coords.3.csv", encoding = "UTF-8")
write.csv(coords.3, con)
remove(con)

con <- file("DICE/coords.4.csv", encoding = "UTF-8")
write.csv(coords.4, con)
remove(con)

#################################################################"
## WRITE FOR JOHN##

bow.search <- merge(x = bow[ , c("docid", "semantic.input", "concept", "calais")], y = points.eucl.lda.lev2.enriched[ , c("docid", "title", "description", "contributor1", "cover_url", "topicid", "topicid_orig", "label_x", "label_y")], by = "docid")
con <- file("outR/bow_jma.csv", encoding = "UTF-8")
write.csv(bow.search, con)
remove(con)

con <- file("outR/similarity_jma.csv", encoding = "UTF-8")
write.csv(sim.dist, con)
remove(con)

##############################################################
##VISUALISATION

vis <- ggplot (points.w0.eucl.lda, aes(x = x, y = y))
vis + 
  geom_polygon(data = coords.1, aes(x = x, y = y), fill = rgb(1, 1, 1, 0.5)) +
  geom_polygon(data = coords.2, aes(x = x, y = y), fill = rgb(1, 1, 1, 0.5)) +
  geom_polygon(data = coords.3, aes(x = x, y = y), fill = rgb(1, 1, 1, 0.5)) + 
  #geom_polygon(data = coords.4, aes(x = x, y = y), fill = rgb(1, 1, 1, 0.5)) +
  #geom_polygon(data = coords.5, aes(x = x, y = y), fill = rgb(1, 1, 1, 0.5)) + # (for a border): colour = "grey"
  geom_point(aes(colour = as.character(topicid), alpha = 0.8), shape = 15) + #theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size = 15)))
#geom_point(data = points.w0.eucl.lda.medoids, aes(x = x.median, y = y.median), shape = 16, colour = "black")
#geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size = 1, data = voronoi$dirsgs, linetype = 1, color= "black")

png(paste0("outR/points.w0.eucl.lda_", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".png"), height = 1200, width = 1200)
print(VIS)
dev.off()

