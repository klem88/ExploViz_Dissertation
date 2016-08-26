#
.libPaths("C:/Users/clément/Documents/R/win-library/3.2")
setwd("C:/Users/clément/OneDrive/Dublin courses/Dissertation/R DICE")
#
library(topicmodels)
library(ggplot2)
library(tm)
library(vegan)#Jaccard index
library(splancs)
library(outliers)
library(lsa) #gw_idf function

#################################################
## READ FILES ##

answers <- read.csv("Data/answers_corrected.csv", sep=",", header = TRUE, quote = '"', stringsAsFactors = FALSE, fileEncoding = "UTF-8")
## Remove viz records because too much viz (the first 4 + the hiteshan outlier which is a viz too)
answers <- answers[-c(33), ] #-c(1,2,4,6,33)
events <- read.csv("Data/events.csv", sep=",", quote = '"', header = TRUE, fileEncoding = "UTF-8")
events <- merge(events, points.eucl.lda.lev2.enriched[ , c("x", "y", "topicid_orig", "docid")], by.x = "book_docid", by.y = "docid")
explo.time <- read.csv("Data/eplotime.csv", sep=",", quote = '"', header = TRUE, fileEncoding = "UTF-8")

############################################
## One respondent per row - one topic per column - Bag of Words ##
users.answer <- data.frame(respondent_id = answers$respondent_id, task_type = answers$task_type, 
                           t1 = tolower(paste(answers$topic11, answers$topic12, answers$topic13, answers$topic14, answers$topic15)),
                           t2 = tolower(paste(answers$topic21, answers$topic22, answers$topic23, answers$topic24, answers$topic25)),
                           t3 = tolower(paste(answers$topic31, answers$topic32, answers$topic33, answers$topic34, answers$topic35)))

##############################################
## Execute once ##
result.bow <- merge(man.classif, bow[ , c("docid", "semantic.input", "concept", "calais")], by = "docid")
result.bow <- merge(result.bow, points.w0.eucl.lda, by = "docid")

topics.bow <- data.frame(respondent_id = 0, task_type = "topic")
## Manual loop i=1
input.temp <- result.bow[result.bow$topicid_orig == 1, ]
input.temp$bow <- paste(input.temp$semantic.input, input.temp$concept, input.temp$calais, input.temp$Level1, input.temp$Level2)
topics.bow$t1 <- paste(input.temp$bow, collapse = " ")
## Manual loop i=2
input.temp <- result.bow[result.bow$topicid_orig == 2, ]
input.temp$bow <- paste(input.temp$semantic.input, input.temp$concept, input.temp$calais, input.temp$Level1, input.temp$Level2)
topics.bow$t2 <- paste(input.temp$bow, collapse = " ")
## Manual loop i=3
input.temp <- result.bow[result.bow$topicid_orig == 3, ]
input.temp$bow <- paste(input.temp$semantic.input, input.temp$concept, input.temp$calais, input.temp$Level1, input.temp$Level2)
topics.bow$t3 <- paste(input.temp$bow, collapse = " ")
##############################################

users.answer <- rbind(users.answer, topics.bow)
#str(users.answer)
############################################
## Compute three DTM - One for each topic
i <- 1
sensemaking <- data.frame(topic = integer(), respondent_id = integer(), sensemaking = numeric())
while ( i <= 3){
  
  bow.corpus <- VectorSource(users.answer[, i + 2]) #topic 1 is the third column
  bow.corpus <- Corpus(bow.corpus)
  
  my.stopwords <- read.csv("outR/term_frequencies_to_delete_unstemmed.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8", quote = '"')
  bow.corpus <- tm_map(bow.corpus, removeWords, my.stopwords$term)
  bow.corpus <- tm_map(bow.corpus, removeWords, stopwords("English"))
    
  bow.corpus <- tm_map(bow.corpus,stemDocument)
  dtm <- DocumentTermMatrix(bow.corpus)
  row.names(dtm) <- users.answer$respondent_id
  
  #If 2 identical words in respondent's answer --> put 1 instead of 2 --> use of lw_bintf on the respondents' answers
  #as.matrix(dtm[-nrow(dtm) ,])
  sensemaking <- rbind(sensemaking, data.frame(topic = i, respondent_id = row.names(dtm[-nrow(dtm) ,]), sensemaking = lw_bintf(as.matrix(dtm[-nrow(dtm) ,])) %*% t(as.matrix(dtm[nrow(dtm) ,]))))
  
  #Euclidean distance
  #sparse.dtm <- dist(sparse.dtm)
  #Jaccard index
  #sparse.dtm <- vegdist(sparse.dtm, method = "jaccard")
  
  remove(bow.corpus)
  
  print(i)
  i <- i + 1
}

sensemaking <- merge(sensemaking, answers[, c("respondent_id", "task_type")], by = "respondent_id")

colnames(sensemaking) <- c("respondent_id","topic", "sensemaking", "task_type")

ggplot(data = sensemaking, aes(x = task_type, y = sensemaking)) + 
  geom_boxplot( ) + #outlier.colour = "red"
  facet_wrap( ~ topic) + #, scales = "free"
  xlab("Interface type") + ylab("Syntactic Sensemaking") + 
  ggtitle("Syntactic Sensemaking across Topics and Interfaces")
  #geom_text(aes(label = ifelse(((sensemaking < quantile(sensemaking)[1] - (3  * IQR(sensemaking))) | (sensemaking > quantile(sensemaking)[4] + (1.5 * IQR(sensemaking)))), respondent_id, "")), hjust = -0.3) 

ggplot(data = sensemaking, aes(x = as.character(task_type), y = sensemaking)) + 
  geom_boxplot( ) +
  xlab("Interface type") + ylab("Syntactic Sensemaking") + 
  ggtitle("Syntactic Sensemaking across Interfaces")

ggplot(data = sensemaking, aes(sensemaking)) + geom_histogram() + facet_wrap( ~ task_type)

sensemaking <- split(sensemaking, list(sensemaking$task_type, sensemaking$topic))
names(sensemaking)

#Normality test
i <- 1
while ( i <= length(sensemaking)){
  print(
    shapiro.test(sensemaking[[i]]$sensemaking)
        )
  print(i)
  i <- i + 1
}

#Assumption of homoscedasticity
var.test(sensemaking[[1]][, "sensemaking"], sensemaking[[2]][, "sensemaking"] )
# Parametric Independent T-test
t.test(sensemaking[[1]][, "sensemaking"], sensemaking[[2]][, "sensemaking"] , var.equal = TRUE, paired = FALSE)

# Non-parametric independent test
i <- 1
while ( i <= length(sensemaking)){
  print(
    wilcox.test(sensemaking[[i]]$sensemaking, sensemaking[[i+1]]$sensemaking, paired = FALSE)
        )
  print(
    median(sensemaking[[i]]$sensemaking)
  )
  print(
    median(sensemaking[[i+1]]$sensemaking)
  )
  print(i)
  i <- i + 2
}

#######################################################################
## Compute the area covered by the convex-hulls FOR TASK 1
######################################################################
events.1 <- merge(events[events$event_type == "HOVER", ], answers[, c("respondent_id", "task_type")])

events.1 <- events.1[, c("task", "respondent_id", "x", "y", "topicid_orig")]

events.1 <- split(events.1, list(events.1$task, events.1$respondent_id, events.1$topicid_orig), drop = TRUE)

i <- 1
area.explored <- data.frame(task = integer(), respondent_id = integer(), topicid_orig = integer(), area = numeric())
while ( i <= length(events.1)) {
  
  chull <- chull(events.1[[i]][, c("x", "y")])
  chull <- events.1[[i]][c(chull, chull[1]), c("x", "y")]
  area.explored <- rbind(area.explored, c(as.integer(unique(events.1[[i]]$task)), as.integer(unique(events.1[[i]]$respondent_id)), unique(events.1[[i]]$topicid_orig), as.numeric(areapl(cbind(chull[, "x"], chull[, "y"])))))
  
  #vis <- ggplot (events.1[[i]][, c("x", "y")], aes(x = x, y = y)) +
  #  geom_polygon(data = chull, aes(x = x, y = y), fill = rgb(0.9, 0.9, 0.9, 0.5)) +
  #  geom_point() + theme_bw()
  #print(vis)
  
  print(i)
  i <- i + 1
}
names(area.explored) <- c("task", "respondent_id", "topic", "area")
area.explored <- merge(area.explored, answers[, c("respondent_id", "task_type")], by = "respondent_id")

ggplot(data = area.explored, aes(x = task_type, y = area)) + 
  geom_boxplot( ) +
  facet_wrap( ~ task) +
  xlab("Interface type") + ylab("Area explored (polygon suface)") + 
  ggtitle("Area explored across Tasks and Interfaces")

area.explored <- split(area.explored, list(area.explored$task_type, area.explored$task))
names(area.explored)

#Normality test
i <- 1
while ( i <= length(area.explored)){
  print(
    shapiro.test(area.explored[[i]]$area)
  )
  print(i)
  i <- i + 1
}

# Non-parametric independent test
i <- 1
while ( i <= length(area.explored)){
  print(
    wilcox.test(area.explored[[i]]$area, area.explored[[i+1]]$area, paired = FALSE)
  )
  print(
    median(area.explored[[i]]$area)
  )
  print(
    median(area.explored[[i+1]]$area)
  )
  print(i)
  i <- i + 2
}


#######################################################################
##Number of hovers##
#######################################################################

#table(events$task, events$event_type)
number.hovers <- merge(events[events$event_type == "HOVER", ], answers[, c("respondent_id", "task_type")])
number.hovers <- data.frame(table(number.hovers$task, number.hovers$task_type, number.hovers$respondent_id))
number.hovers <- number.hovers[!(number.hovers$Freq == 0), ]
names(number.hovers) <- c("task", "task_type", "respondent_id", "freq")

ggplot(data = number.hovers, aes(x = task_type, y = freq)) + 
  geom_boxplot( ) + #outlier.colour = "red"
  facet_wrap( ~ task) + #, scales = "free"
  xlab("Interface type") + ylab("Number of hover events") + 
  ggtitle("Number of hover events across Tasks and Interfaces")

number.hovers <- split(number.hovers, list(number.hovers$task_type, number.hovers$task))
names(number.hovers)

#Normality test
i <- 1
while ( i <= length(number.hovers)){
  print(
    shapiro.test(number.hovers[[i]]$freq)
  )
  print(i)
  i <- i + 1
}

# Non-parametric independent test
i <- 1
while ( i <= length(number.hovers)){
  print(
    wilcox.test(number.hovers[[i]]$freq, number.hovers[[i+1]]$freq, paired = FALSE)
  )
  print(
    median(number.hovers[[i]]$freq)
  )
  print(
    median(number.hovers[[i+1]]$freq)
  )
  print(i)
  i <- i + 2
}

#######################################################################
##Distance between two chosen books##
#######################################################################

dist.books <- answers[, c("respondent_id", "task_type", "task2_book1")]
names(dist.books) <- c("respondent_id", "task_type", "task2_book2")
dist.books <- rbind(dist.books, answers[, c("respondent_id", "task_type", "task2_book2")])
names(dist.books) <- c("respondent_id", "task_type", "docid")
dist.books <- merge(dist.books, points.eucl.lda.lev2[, c("docid", "x", "y")], by = "docid")

dist.books <- split(dist.books, dist.books$respondent_id)

i <- 1
distances <- data.frame(respondent_id = integer(), task_type = character(), distance = numeric())
while ( i <= length(dist.books)) {

  temp <- as.numeric(dist(dist.books[[i]][,c("x", "y")]))
  distances <- rbind(distances, data.frame(respondent_id = names(dist.books[i]), task_type = dist.books[[i]][1, "task_type"], distance = temp))
  
  print(i)
  i <- i + 1
}

ggplot(data = distances, aes(x = as.character(task_type), y = distance)) + 
  geom_boxplot( ) + #outlier.colour = "red"
  #facet_wrap( ~ task) + #, scales = "free"
  xlab("Interface type") + ylab("Euclidean distance between two selected books") + 
  ggtitle("Distance between two selected books across Interfaces")

distances <- split(distances, distances$task_type)

#Normality test
i <- 1
while ( i <= length(distances)){
  print(
    shapiro.test(distances[[i]]$distance)
  )
  print(i)
  i <- i + 1
}

# Non-parametric independent test
i <- 1
while ( i <= length(distances)){
  print(
    wilcox.test(distances[[i]]$distance, distances[[i+1]]$distance, paired = FALSE)
  )
  print(
    median(distances[[i]]$distance)
  )
  print(
    median(distances[[i+1]]$distance)
  )
  print(i)
  i <- i + 2
}

#######################################################################
##Exploration time of the task 3##
#######################################################################
explo.time <- merge(explo.time, answers[ , c("respondent_id", "task_type")], by = "respondent_id")

ggplot(data = explo.time, aes(x = as.character(task_type), y = explotime)) + 
  geom_boxplot( ) + 
  #facet_wrap( ~ task) +
  xlab("Interface type") + ylab("Exploration time in seconds") + 
  ggtitle("Exploration time (in seconds) across Interfaces")

explo.time <- split(explo.time, explo.time$task_type)

#Normality test
i <- 1
while ( i <= length(explo.time)){
  print(
    shapiro.test(explo.time[[i]]$explotime)
  )
  print(i)
  i <- i + 1
}

# Non-parametric independent test
i <- 1
while ( i <= length(explo.time)){
  print(
    wilcox.test(explo.time[[i]]$explotime, explo.time[[i+1]]$explotime, paired = FALSE)
  )
  print(
    median(explo.time[[i]]$explotime)
  )
  print(
    median(explo.time[[i+1]]$explotime)
  )
  print(i)
  i <- i + 2
}
