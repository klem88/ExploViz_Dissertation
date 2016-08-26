#
.libPaths("C:/Users/clément/Documents/R/win-library/3.2")

library(reshape2)

#############################################################
## Cognitive tasks ##
#############################################################
cogtasks <- read.csv("Data/cogtasks.csv", sep=",", quote = '"', header = TRUE, fileEncoding = "UTF-8")

cogtasks.old.18 <- cogtasks[cogtasks$respondent_id <= 122, ] ##id 122 corresponds to the 18th respondent 
#Remove old question 1 and duplicate old question 2 which corresponds to the new question 1 and 2
cogtasks.old.18$question1 <- cogtasks.old.18$question2
#Inverse the 5 points Likert scake old question 3 (very hard and very easy have been switched for more consistency)
cogtasks.old.18$question3 <- (cogtasks.old.18$question3 * -1) + 6

cogtasks <- cogtasks[cogtasks$respondent_id > 122, ]
cogtasks <- rbind(cogtasks.old.18, cogtasks)
remove(cogtasks.old.18)

cogtasks <- merge(cogtasks, answers[, c("respondent_id", "task_type")], by = "respondent_id")
#BEcause the Q3 and Q4 are aggregated and they are in the opposite direction (Very easy - Very hard)
cogtasks$question4 <- (cogtasks$question4 * -1) + 6

cogtasks.germane <- melt(cogtasks, id.vars = c("respondent_id", "task", "task_type"), measure.vars = c("question1", "question2"))
cogtasks.germane <- merge(data.frame(table(cogtasks.germane$task, cogtasks.germane$task_type, cogtasks.germane$value)), data.frame(table(cogtasks.germane$task, cogtasks.germane$task_type)), by = c("Var1", "Var2"))
cogtasks.germane$value <- cogtasks.germane$Freq.x / cogtasks.germane$Freq.y * 100

cogtasks.extraneous <- melt(cogtasks, id.vars = c("respondent_id", "task", "task_type"), measure.vars = c("question3", "question4"))
cogtasks.extraneous <- merge(data.frame(table(cogtasks.extraneous$task, cogtasks.extraneous$task_type, cogtasks.extraneous$value)), data.frame(table(cogtasks.extraneous$task, cogtasks.extraneous$task_type)), by = c("Var1", "Var2"))
cogtasks.extraneous$value <- cogtasks.extraneous$Freq.x / cogtasks.extraneous$Freq.y * 100

ggplot(data = cogtasks.germane, aes(x = Var2, y = value)) + 
  geom_bar(stat = "identity") +
  facet_grid(Var1 ~ Var3) +
  xlab("Interface type") + ylab("Percentage of answers per task and per interface type") + 
  ggtitle("Germane Cognitive Load on a 5 Likert scale across the Tasks and Interfaces")

ggplot(data = cogtasks.extraneous, aes(x = Var2, y = value)) + 
  geom_bar(stat = "identity") +
  facet_grid(Var1 ~ Var3) +
  xlab("Interface type") + ylab("Percentage of answers per task and per interface type") +
  ggtitle("Extraneous Cognitive Load on a 5 Likert scale across Tasks and Interfaces")
  
## First execute back to the first version of cogtasks.germane and cogtasks.extraneous ##
cogtasks.germane <- split(cogtasks.germane, list(cogtasks.germane$task_type, cogtasks.germane$task))
names(cogtasks.germane)
cogtasks.extraneous <- split(cogtasks.extraneous, list(cogtasks.extraneous$task_type, cogtasks.extraneous$task))
names(cogtasks.extraneous)

############Germane cognitive load
#Normality test
i <- 1
while ( i <= length(cogtasks.germane)){
  print(
    shapiro.test(cogtasks.germane[[i]]$value)
  )
  print(i)
  i <- i + 1
}


# Non-parametric independent test
i <- 1
while ( i <= length(cogtasks.germane)){
  print(
    wilcox.test(cogtasks.germane[[i]]$value, cogtasks.germane[[i+1]]$value, paired = FALSE)
  )
  print(
    median(cogtasks.germane[[i]]$value)
  )
  print(
    median(cogtasks.germane[[i+1]]$value)
  )
  print(i)
  i <- i + 2
}

############Extraneous cognitive load
#Normality test
i <- 1
while ( i <= length(cogtasks.extraneous)){
  print(
    shapiro.test(cogtasks.extraneous[[i]]$value)
  )
  print(i)
  i <- i + 1
}


# Non-parametric independent test
i <- 1
while ( i <= length(cogtasks.extraneous)){
  print(
    wilcox.test(cogtasks.extraneous[[i]]$value, cogtasks.extraneous[[i+1]]$value, paired = FALSE)
  )
  print(
    median(cogtasks.extraneous[[i]]$value)
  )
  print(
    median(cogtasks.extraneous[[i+1]]$value)
  )
  print(i)
  i <- i + 2
}
