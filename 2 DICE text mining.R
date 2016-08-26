#
.libPaths("C:/Users/clément/Documents/R/win-library/3.2")
setwd("C:/Users/clément/OneDrive/Dublin courses/Dissertation/R DICE")
#
library(tm)
library(class) #knn

bow.corpus <- VectorSource(paste(man.classif$Level1, man.classif$Level2, man.classif$bow1))
bow.corpus <- Corpus(bow.corpus)
#bow.corpus <- tm_map(bow.corpus,stemDocument)

#my.stopwords <- read.csv("outR/term_frequencies_to_delete_stemmed.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8", quote = '"')
#my.stopwords <- read.csv("outR/term_frequencies_to_delete_unstemmed.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8", quote = '"')
#bow.corpus <- tm_map(bow.corpus, removeWords, my.stopwords$term)

dtm <- DocumentTermMatrix(bow.corpus)
sparse.dtm <- removeSparseTerms(dtm, 0.98)
sparse.dtm$dimnames$Docs <- as.character(man.classif$docid) #bow$docid
sparse.dtm <- sparse.dtm[rowSums(as.matrix(sparse.dtm)) > 0, ]

remove(bow.corpus)
remove(my.stopwords)
remove(dtm)

#as.matrix(sparse.dtm)[1:1, ]
#ncol(dtm)
#nrow(dtm)
#ncol(sparse.dtm)
#nrow(sparse.dtm)
#colnames(as.matrix(sparse.dtm))
##########################
##Export the terms and their frequency
tag.frequency <- colSums(as.matrix(sparse.dtm))
tag.frequency <- sort(tag.frequency, decreasing = TRUE)
tag.frequency <- data.frame(term = names(tag.frequency), freq = as.vector(tag.frequency))

con <- file("outR/term_frequencies.csv", encoding = "UTF-8")
write.csv(tag.frequency, con)
remove(con)
#Choose the STEMMED words to delete and put it into "outR/term_frequencies_to_delete.csv"

##########################
## KNN BASED ON THE TRANSLATED CYBERLIBRIS DICETHEME LABELS ##
#  The w0.topic.probabilities below is based on bow$title.x, bow$description, bow$toc(semantic.input), bow$calais, bow$concept(bow1) and a sparese.dtm containing 200 terms
data <- w0.topic.probabilities
data <- merge(data, d.books[ , c("docid", "dicethemeid")], by = "docid")

test.prediction <- knn(train = data[!(data$dicethemeid == 0), 2:10], test = data[data$dicethemeid == 0, 2:10], cl = data[!(data$dicethemeid == 0), "dicethemeid"], k = 5)
data[data$dicethemeid == 0, "dicethemeid"] <- test.prediction
data <- merge(data, dicethemes, by = "dicethemeid")
colnames(data)[ncol(data)] <- "knn"
bow <- bow[ , -ncol(bow)]
bow <- merge(bow, data[ , c("docid", "knn")], by = "docid")

########################################################
## ADDING PRECISION AND ADDING HIGH LEVEL CLASSIFICATION

con <- file("outR/manual_clasification.csv", encoding = "UTF-8")
write.csv(bow[, c("docid", "title.y", "subject", 'knn')], con)
remove(con)

man.classif <- read.csv("Data/Manual_reclassification_2.csv", sep = "\t", quote = "", header = TRUE, fileEncoding = "UTF-8")
man.classif <- merge(bow[, c("docid", "title.x", "bow1")], man.classif, by = "docid")
man.classif$output <- paste(man.classif$title.x, man.classif$Level1, man.classif$Level2) 
man.classif$output <- tolower(man.classif$output)
man.classif$output <- gsub("NA", "", man.classif$output)

##########################
## KNN BASED ON THE DTM of Title + Level1 and part of Level2 manually reclassified FOR COMPLETING LEVEL2 ##
# 
data <- as.data.frame(as.matrix(sparse.dtm))
data$docid <- row.names(data)
data <- merge(data, man.classif[ , c("docid", "Level1", "Level2")], by = "docid")

test.prediction <- knn(train = data[!(data$Level2 == ""), 2:(ncol(data)-2)], test = data[data$Level2 == "", 2:(ncol(data)-2)], cl = data[!(data$Level2 == ""), "Level2"], k = 5)

data[data$Level2 == "", "Level2"] <- test.prediction
table(data$Level1, data$Level2)
str(data)
man.classif.enriched <- merge(data[, c("docid", "Level1", "Level2")], bow[, c("docid", "title.x", "bow1")], by = "docid")



