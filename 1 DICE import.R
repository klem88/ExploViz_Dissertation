#
.libPaths("C:/Users/clément/Documents/R/win-library/3.2")
setwd("C:/Users/clément/OneDrive/Dublin courses/Dissertation/R DICE")
#
library(stringr)
library(tm)
library(httr)
library(jsonlite)
library(dataQualityR)

books <- read.csv("Data/books.csv", sep=";", header = TRUE, quote = "", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
dicethemes <- read.csv("Data/dicethemes.csv", sep=";", quote = "", header = TRUE, fileEncoding = "UTF-8")
doctype <- read.csv("Data/doctype.csv", sep=";", header = TRUE, fileEncoding = "UTF-8")
scores <- read.csv("Data/scores.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
tags <- read.csv("Data/tags.csv", sep = ";", quote = "", header = TRUE, fileEncoding = "UTF-8")
abstracts <- read.csv("Data/abstracts_Kettle.csv", sep = "|", quote = "", header = TRUE, fileEncoding = "UTF-8")
subjects <- read.csv("Data/subjects.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")
toc.clean <- read.csv("Data/a_toc.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8", quote = '"')

##########################
#### DATA CLEANING # BOOKS
a.books <- books[ , c("docid", "title")]
a.books[a.books == "(empty)"] = NA
to.delete <- which(is.na(as.numeric(as.character(a.books$docid))))
a.books <- a.books[-to.delete, ]

#Remove punctuation and numbers - inside each title
special <- unlist(str_extract_all(a.books$title, "[^[:alpha:][:blank:]]"))
to.delete <- unique(special)
to.delete <- to.delete[!(is.na(to.delete))]
i <- 1
while (i <= length(to.delete))
{
  a.books$title <- gsub(paste0("[", to.delete[i], "]"), " ", a.books$title)
  print (i)
  i <- i + 1
}
#Remove all 1-to-3 letters words
a.books$title <- gsub(" *\\b[[:alnum:]]{1,3}\\b *", " ", a.books$title)
a.books$title <- tolower(a.books$title)
a.books$title <- removeWords(a.books$title, stopwords("english"))
a.books$title <- stripWhitespace(a.books$title)
a.books$title <- trimws(a.books$title)

##########################
#### DATA CLEANING # TAGS
a.tags <- tags
to.delete <- which(is.na(as.numeric(as.character(a.tags$docid))))
a.tags <- a.tags[-to.delete, ]

#Remove punctuation and numbers - inside each title
special <- unlist(str_extract_all(a.tags$tag, "[^[:alpha:][:blank:]]"))
to.delete <- unique(special)
to.delete <- to.delete[!(is.na(to.delete))]
i <- 1
while (i <= length(to.delete))
{
  a.tags$tag <- gsub(paste0("[", to.delete[i], "]"), " ", a.tags$tag)
  print (i)
  i <- i + 1
}
#Remove all 1-to-3 letters words
a.tags$tag <- gsub(" *\\b[[:alnum:]]{1,3}\\b *", " ", a.tags$tag)
a.tags$tag <- tolower(a.tags$tag)
a.tags$tag <- removeWords(a.tags$tag, stopwords("english"))
a.tags$tag <- stripWhitespace(a.tags$tag)
a.tags$tag <- trimws(a.tags$tag)
#In the end because some tags are removed during the previous transformations
a.tags[a.tags == ""] <- NA
to.delete <- which(is.na(a.tags$tag) == TRUE)
a.tags <- a.tags[-to.delete, ]

a.tags$docid  <- as.character(a.tags$docid)
tags.aggregated <- aggregate(tag ~ docid, paste, collapse = " ", data = a.tags)

##########################
#### DATA CLEANING # ABSTRACTS
a.abstracts <- abstracts

#Remove punctuation and numbers - inside each title
special <- unlist(str_extract_all(a.abstracts$description, "[^[:alpha:][:blank:]]"))
to.delete <- unique(special)
to.delete <- to.delete[!(is.na(to.delete))]
i <- 1
while (i <= length(to.delete))
{
  a.abstracts$description <- gsub(paste0("[", to.delete[i], "]"), " ", a.abstracts$description)
  print (i)
  i <- i + 1
}
#Remove all 1-to-3 letters words
a.abstracts$description <- gsub(" *\\b[[:alnum:]]{1,3}\\b *", " ", a.abstracts$description)
a.abstracts$description <- tolower(a.abstracts$description)
a.abstracts$description <- removeWords(a.abstracts$description, stopwords("english"))
a.abstracts$description <- stripWhitespace(a.abstracts$description)
a.abstracts$description <- trimws(a.abstracts$description)

#The below transformation removes the words that contain special characters. 
#We keep those words like others BoW at the moment.
#special <- as.factor(unlist(str_extract_all(a.abstracts$description, "([0-9a-zA-Z]*[^0-9a-zA-Z[:blank:]]+[0-9a-zA-Z]*){1,}")))
#to.delete <- unique(special)
#a.abstracts$description <- removeWords(a.abstracts$description, to.delete)

##########################
#### DATA CLEANING # SUBJECTS

a.subjects <- subjects[, 2:3]
a.subjects[a.subjects == ""] = NA
a.subjects[a.subjects == "None"] = NA
to.delete <- which(is.na(a.subjects$subject) | is.na(a.subjects$subjectid) | is.na(as.numeric(as.character(a.subjects$subjectid))))
a.subjects <- a.subjects[-to.delete, ]
a.subjects <- unique(a.subjects)

#Remove punctuation and numbers - inside each title
special <- unlist(str_extract_all(a.subjects$subject, "[^[:alpha:][:blank:]]"))
to.delete <- unique(special)
to.delete <- to.delete[!(is.na(to.delete))]
i <- 1
while (i <= length(to.delete))
{
  a.subjects$subject <- gsub(paste0("[", to.delete[i], "]"), " ", a.subjects$subject)
  print (i)
  i <- i + 1
}
#Remove all 1-to-3 letters words
a.subjects$subject <- gsub(" *\\b[[:alnum:]]{1,3}\\b *", " ", a.subjects$subject)
a.subjects$subject <- tolower(a.subjects$subject)
a.subjects$subject <- stripWhitespace(a.subjects$subject)
a.subjects$subject <- trimws(a.subjects$subject)

b.subjects <- subjects[, 1:2]
to.delete <- which(is.na(as.numeric(as.character(b.subjects$subjectid))) | is.na(as.numeric(as.character(b.subjects$docid))))
b.subjects <- b.subjects[-to.delete, ]
c.subjects <- merge(x = a.subjects, y = b.subjects, by = "subjectid")

c.subjects$docid  <- as.character(c.subjects$docid)
subjects.aggregated <- aggregate(subject ~ docid, paste, collapse = " ", data = c.subjects)

##########################
#### ABT Bag Of Words

b.books <- merge(x = books, y = dicethemes , by = "dicethemeid", all.x = TRUE)
c.books <- merge(x = b.books, y = doctype, by = "doctypeid", all.x = TRUE)
d.books <- merge(x = c.books, y = scores, by = "docid", all.x = TRUE)

#########################################
## CAN BE HELPFUL TO FIND THE EXACT GET URL AND AUTHENTICATION PARAMETERS ##

library(curlconverter)
curlquery <- 'curl -X POST -H "Authorization:Token 3mZSz0BuR9gc" -H "Content-Type: application/json" --data "{\"texts\":[\"I am so happy today\"]}" https://api.uclassify.com/v1/klem88/Business Topics/classify'
curl <- make_req(straighten(curlquery))
#curl

############################################
## CONCEPTS FROM AYLIEN ## 

#i <- 1
#concept <- data.frame(docid = integer(), concept = character(), support = integer(), score = numeric())
#query <- "https://api.aylien.com/api/v1/concepts?text="

while (i <= nrow(bow))
{
  text.temp <- paste0('"',substr(bow$bow2[i], 0, 6000), '"')
  
  getdata <- GET(url = paste0(query,text.temp), query = list('text' = text.temp), add_headers(`X-AYLIEN-TextAPI-Application-Key` = "94f787c535fe6386b070465b36335ae8", `X-AYLIEN-TextAPI-Application-ID` = "a89295b8"))
  #getdata$status_code
  content <- fromJSON(content(getdata, type = "text"))
  #Deep Nested List structure
  support.temp1 <- as.data.frame(do.call(rbind, content$concepts))
  support.temp1$id <- rownames(support.temp1)
  support.temp1$support <- unlist(support.temp1$support)
  if (!(nrow(support.temp1) <= 1)) {
    support.temp2 <- as.data.frame(do.call(rbind, support.temp1$surfaceForms))
    support.temp2$id <- rownames(support.temp2)
    
    concept.temp <- merge(support.temp1, support.temp2, by = "id")
    
    concept.temp <- data.frame(docid = bow$docid[i], concept = concept.temp$string, support = concept.temp$support, score = concept.temp$score)
    #concept.temp <- data.frame(docid = bow$docid[i], concept = concept.temp[concept.temp$support == max(concept.temp$support), "string"])
    concept <- rbind(concept, concept.temp)
  } else { 
    if (nrow(support.temp1) == 1) {
      support.temp2 <- as.data.frame(do.call(rbind, support.temp1$surfaceForms))
      concept.temp <- data.frame(docid = bow$docid[i], concept = support.temp2$string, support = support.temp1$support, score = support.temp2$score)
      concept <- rbind(concept, concept.temp)
    } else {
      concept.temp <- data.frame(docid = bow$docid[i], concept = NA, support = NA, score = NA)
      concept <- rbind(concept, concept.temp)
    }
  }
  print(i)
  i <- i + 1
}
## CONCEPT ##
#sum(is.na(concept$concept))
concept.aggregated <- aggregate(concept ~ docid, paste, collapse = " ", data = concept)

############################################
## CLASSIFICATION FROM AYLIEN ## 

#i <- 1409
#classif <- data.frame(docid = integer(), classif = character(), confidence = integer())
#query <- "https://api.aylien.com/api/v1/classify?text="

while (i <= nrow(bow))
{
  text.temp <- paste0('"',substr(bow$semantic.input[i], 0, 6000), '"')
  
  getdata <- GET(url = paste0(query,text.temp), query = list('text' = text.temp), add_headers(`X-AYLIEN-TextAPI-Application-Key` = "94f787c535fe6386b070465b36335ae8", `X-AYLIEN-TextAPI-Application-ID` = "a89295b8"))
  #getdata$status_code
  content <- fromJSON(content(getdata, type = "text"))
  #Nested List structure
  temp1 <- as.data.frame(do.call(rbind, content$categories))
  if (ncol(temp1) == 1) {
    classif.temp <- data.frame(docid = bow$docid[i], classif = as.character(temp1[1,]), confidence = as.numeric(as.character(temp1[3,])))
    classif <- rbind(classif, classif.temp)
  } else {
    classif.temp <- data.frame(docid = bow$docid[i], classif = paste(as.character(temp1[1,1]), as.character(temp1[1,2])), confidence = as.numeric(as.character(temp1[3,1])))
    classif <- rbind(classif, classif.temp)
    
  } 
  print(i)
  i <- i + 1
}
## CLEANING CLASSIFICATION ##
#length(unique(classif$docid))
##REMARK: THE CLASSIFICATION FROM AYLIEN IS TOO WIDE AND THUS WRONG. 
##INDEED THE BIGGEST CLASSIF IS "UNREST CONFLICTS WAR CRISIS" (251) FOLLOWED BY "SCIENCE TECH MATH"(167) AND "ECONOMY BUSINESS FINANCE" (101) 
#sort(table(classif$classif))

#Remove punctuation and numbers - inside each classif
special <- unlist(str_extract_all(classif$classif, "[^[:alpha:][:blank:]]"))
to.delete <- unique(special)
to.delete <- to.delete[!(is.na(to.delete))]
i <- 1
while (i <= length(to.delete))
{
  classif$classif <- gsub(paste0("[", to.delete[i], "]"), " ", classif$classif)
  print (i)
  i <- i + 1
}

classif$classif <- tolower(classif$classif)
classif$classif <- removeWords(classif$classif, stopwords("english"))
classif$classif <- stripWhitespace(classif$classif)
classif$classif <- trimws(classif$classif)

##########################################################
## CLASSIFICATION FROM OPEN CALAIS ## 

#i <- 1
#opcalais <- data.frame(docid = integer(), calais = character())
#query <- "https://api.thomsonreuters.com/permid/calais"

attempt <- 1
while( attempt <= 30 ) {
  attempt <- attempt + 1
  try(
    while (i <= nrow(bow))
    {
      text.temp <- paste0('"',bow$semantic.input[i], '"')#substr(bow$semantic.input[i], 0, 6000)
      
      getdata <- POST(url = query, 
                      add_headers(`Content-Type` = "text/raw", `outputFormat` = "application/json", `x-ag-access-token` = "WIX0wCpTLecG4rNLlkRUMU4k4AR4GmoL", `x-calais-selectiveTags` = "industry, topic, socialtags"),
                      body = text.temp)
      #getdata$status_code
      content <- fromJSON(content(getdata, type = "text"))
      #Nested List structure
      temp1 <- as.data.frame(do.call(rbind, content))
      if (!(length(as.character(temp1$name)[-c(1,2)]) == 0)){
        calais.temp <- data.frame(docid = bow$docid[i], calais = as.character(temp1$name)[-c(1,2)])
        calais.temp <- calais.temp[-nrow(calais.temp), ]
        calais.temp <- aggregate(calais ~ docid, paste, collapse = " ", data = calais.temp)
        opcalais <- rbind(opcalais, calais.temp)
      } else {
        calais.temp <- data.frame(docid = bow$docid[i], calais = NA)
        opcalais <- rbind(opcalais, calais.temp)
      }
      
      print(i)
      i <- i + 1
    }
  )
} 
opcalais[opcalais$docid == 10040355, ]
a.calais <- opcalais
a.calais$calais <- tolower(a.calais$calais)
a.calais$calais <- removeWords(a.calais$calais, stopwords("english"))

#Remove punctuation and numbers - inside each calais
special <- unlist(str_extract_all(a.calais$calais, "[^[:alpha:][:blank:]]"))
to.delete <- unique(special)
to.delete <- to.delete[!(is.na(to.delete))]
i <- 1
while (i <= length(to.delete))
{
  a.calais$calais <- gsub(paste0("[", to.delete[i], "]"), " ", a.calais$calais)
  print (i)
  i <- i + 1
}

a.calais$calais <- stripWhitespace(a.calais$calais)
a.calais$calais <- trimws(a.calais$calais)

##########################################################
## TOPICS FROM OPEN CALAIS ## 
test <- merge(bow, toc.clean, by = "docid")
#i <- 1
#topcalais <- data.frame(docid = integer(), calais = character())
#query <- "https://api.thomsonreuters.com/permid/calais"

attempt <- 1
while( attempt <= 20 ) {
  attempt <- attempt + 1
  try(
    while (i <= nrow(bow))
    {
      text.temp <- paste0('"',bow$title.y[i], '"')
      
      getdata <- POST(url = query, 
                      add_headers(`Content-Type` = "text/raw", `outputFormat` = "application/json", `x-ag-access-token` = "WIX0wCpTLecG4rNLlkRUMU4k4AR4GmoL", `x-calais-selectiveTags` = "industry, topic"),
                      body = text.temp)
      #getdata$status_code
      content <- fromJSON(content(getdata, type = "text"))
      #Nested List structure
      temp1 <- as.data.frame(do.call(rbind, content))
      if (!(length(as.character(temp1$name)[-c(1,2)]) == 0)){
        calais.temp <- data.frame(docid = test$docid[i], calais = as.character(temp1$name)[-c(1,2)])
        calais.temp <- calais.temp[-nrow(calais.temp), ]
        calais.temp <- aggregate(calais ~ docid, paste, collapse = " ", data = calais.temp)
        topcalais <- rbind(topcalais, calais.temp)
      } else {
        calais.temp <- data.frame(docid = test$docid[i], calais = NA)
        topcalais <- rbind(topcalais, calais.temp)
      }
      
      print(i)
      i <- i + 1
    }
  )
} 


##########################################################
## BOW ##

#Only "Manuel" and "Guide" are extracted -> when the filter is removed, add back the left outer join
bow <- merge(x = a.books, y = d.books[which(d.books$doctype == "Manuel") , c("docid", "title", "color", "dicetheme", "doctype", "score")], by = "docid")
#bow <- merge(x = a.books, y = d.books[ , c("docid", "title", "color", "dicetheme", "doctype", "score")], by = "docid")
bow <- merge(x = bow, y = tags.aggregated, by = "docid", all.x = TRUE)
bow <- merge(x = bow, y = a.abstracts, by = "docid", all.x = TRUE)
bow <- merge(x = bow, y = subjects.aggregated, by = "docid", all.x = TRUE)
bow <- merge(x = bow, y = toc.clean[, 2:3], by = "docid", all.x = TRUE)

bow$semantic.input <- paste(bow$title.x, bow$description, bow$toc, sep = " ")
bow$semantic.input <- gsub("NA", "", bow$semantic.input)
bow$semantic.input <- removeWords(bow$semantic.input, stopwords("english"))
my.stopwords <- read.csv("outR/term_frequencies_to_delete_unstemmed.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8", quote = '"')
bow$semantic.input <- removeWords(bow$semantic.input, my.stopwords$term)
bow$semantic.input <- stripWhitespace(bow$semantic.input)
bow$semantic.input <- trimws(bow$semantic.input)

#SEMANTIC INFORMATION FROM AYLIEN AND OPEN CALAIS
bow <- merge(x = bow, y = concept.aggregated, by = "docid", all.x = TRUE)
#bow <- merge(x = bow, y = classif[, c("docid", "classif")], by = "docid") #, all.x = TRUE
bow <- merge(x = bow, y = a.calais, by = "docid", all.x = TRUE)

bow$bow1 <- paste(bow$title.x, bow$calais, bow$concept, sep = " ")
bow$bow1 <- gsub("NA", "", bow$bow1)
bow$bow1 <- stripWhitespace(bow$bow1)
bow$bow1 <- trimws(bow$bow1)

bow$bow2 <- paste(bow$semantic.input, bow$bow1, sep = " ")
bow$bow2 <- gsub("NA", "", bow$bow2)
bow$bow2 <- stripWhitespace(bow$bow2)
bow$bow2 <- trimws(bow$bow2)


sum(is.na(points.eucl.lda.lev2.enriched$cover_url))
as.matrix(unique(dicethemes$dicetheme_en))
as.matrix(unique(points.eucl.lda.lev2.enriched$Level1))
#summary(d.books$doctype)
#checkDataQuality(bow, "outR/Num.csv", "outR/Cat.csv")

