library(dplyr)
library(lubridate)
library(arules)
library(arulesViz)
library(tidyr)


meetup <- read.csv('meetup.csv', stringsAsFactors = FALSE)
meetup$X <- NULL
meetup$Location <- NULL


##### DATA CLEANING #####

# Convert Founded to date
meetup$Founded <- mdy(meetup$Founded)

# Delete rows with empty group titles
meetup <- meetup[meetup$Title != '',]

# Clean titles
meetup$Title <- gsub("\"", "", meetup$Title)
meetup$Title <- gsub("#", "", meetup$Title)
meetup$Title <- gsub("\\(.*\\) ", "", meetup$Title)



##### FEATURE ENGINEERING #####

meetup <- meetup %>%
  mutate(Days_Since_Creation = as.integer(ymd('2018-02-09') - Founded), # Calculated from Feb 09, 2018
         Members_Per_Day =  Members/Days_Since_Creation,
         Year = year(Founded),      # Year Founded
         Month = factor(months(Founded),      # Month Founded
                        levels = month.name),
         Topic_List = strsplit(Topics, split =  ', '), # Topics tokenized list
         Num_Topics = sapply(meetup$Topics, FUN = function(x){length(strsplit(x, split = ', ')[[1]])})) 




# Creating transaction object of topics
topic_list <- meetup$Topic_List
names(topic_list) <- meetup$Title
topic_tranx <- as(topic_list, "transactions")



# Setting apriori rules
# Topic needs to show up atleast 1% of all groups
# One topic must show up 50% more with another topic
rules <- apriori(topic_tranx, parameter=list(support=0.01, confidence=0.5))



# Create rules Data Frame and sort by lift
rules_df <- as(rules, 'data.frame')
rules_df <- rules_df %>% 
  separate(rules, c('LHS','RHS'), '=>') %>%
  arrange(desc(lift))



# Clean and tokenize LHS and RHS
rules_df$LHS <- gsub('\\{', '',  rules_df$LHS)
rules_df$LHS <- gsub('\\} ', '',  rules_df$LHS)
rules_df$RHS <- gsub(' \\{', '',  rules_df$RHS)
rules_df$RHS <- gsub('\\}', '',  rules_df$RHS)
rules_df$LHS <- strsplit(rules_df$LHS, split = ",")



# Create function to be used for filtering in server
# Finds all rules where LHS is a subset of interested list of topics
is_subset <- function(LL, L){
  any(LL[[1]] %in% L)
}







