setwd('C:/Users/moyke/Desktop/GitHub/Recommendation_System_For_Meetup.com')
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(iterators)

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



##### FEATURE ENGINEERING #####

meetup <- meetup %>%
  mutate(Days_Since_Creation = as.integer(ymd('2018-02-09') - Founded), # Calculated from Feb 09, 2018
         Members_Per_Day =  Members/Days_Since_Creation,
         Year = year(Founded),      # Year Founded
         Month = factor(months(Founded),      # Month Founded
                        levels = month.name),
         Topic_List = strsplit(Topics, split =  ', '), # Topics tokenized list
         Num_Topics = sapply(meetup$Topics, FUN = function(x){length(strsplit(x, split = ', ')[[1]])})) 


# 2017 data
meetup_2017 <- meetup %>% filter(Year == 2017)




##### EXPLORATORY DATA ANALYSIS #####

# 1718 Tech groups within 5 mile radius of NYC were created between 2002-10-08 - 2018-02-07
min(meetup$Founded)
max(meetup$Founded)



# Tech groups are consistently increasing per Year, 2017 had 77% growth
ggplot(meetup %>% group_by(Year) %>%
         summarise(Groups_Created = n()) %>%
         mutate(Growth = (Groups_Created - lag(x = Groups_Created, n = 1))/ lag(x = Groups_Created, n = 1)),
       aes(x = Year, y = Groups_Created)
       ) +
  geom_bar(stat = 'identity') + 
  ggtitle('Groups Created by Year (growth rate labeled)') +
  geom_text(aes(label = round(Growth,2) , vjust = -0.5))



# Groups are being created mainly in January and the Fall months OVERALL
ggplot( meetup %>% group_by(Month) %>%
          summarise(Groups_Created = n()),
        aes(x = Month, y = Groups_Created)
) +
  geom_bar(stat = 'identity')
# In 2017, the Fall months also have the most groups created
ggplot( meetup_2017 %>% group_by(Month) %>%
          summarise(Groups_Created = n()),
        aes(x = Month, y = Groups_Created)
) +
  geom_bar(stat = 'identity')



# Fastest growing tech groups created in 2017 mainly deal with cryptocurrency:
# 1) CryptoMondays, 2) BlockChain NYC, 3) Reach NYC, 4) Crypto2020, 5) Build with Code
View(meetup_2017 %>% arrange(desc(Members_Per_Day)))


# In 2017, of course the longer the group existed, the more members they have
ggplot(meetup_2017, aes(x = Days_Since_Creation, y = Members)) +
  geom_point()

# But rate at which each group grows is consistent and growth doesnt seem to depend on how long a group has existed within the year.
# Probably has to deal with the topics?
ggplot(meetup_2017, aes(x = Days_Since_Creation, y = Members_Per_Day)) +
  geom_point()



# Most groups are beings created with 15 topics listed under them
ggplot( meetup %>% group_by(Num_Topics) %>%
          summarise(Groups_Created = n()),
        aes(x = Num_Topics, y = Groups_Created)
        ) +
  geom_bar(stat = 'identity')



# Groups with 15 topics also seem to have higher member growth rates
ggplot( meetup,
        aes(x = factor(Num_Topics), y = Members_Per_Day)
) +
  geom_boxplot() 



#######################################################################################################

install.packages('arules')
install.packages('arulesViz')
library(arules)
library(arulesViz)

# Creating transaction object
topic_list <- meetup$Topic_List
names(topic_list) <- meetup$Title
topic_tranx <- as(topic_list, "transactions")



# Exploring transaction data
inspect(topic_tranx[1:5])
image(topic_tranx)
itemFrequencyPlot(topic_tranx, support = 0.1) # show only items that were bough in atleast 10% of ALL transactions
itemFrequencyPlot(topic_tranx, topN = 20) # Top 20 topics



# Top most frequent topics listed were 
# 1) Software Dev, 2) New Tech, 3) Computer Programming, 4) Web Tech, 5) Web Dev.
# Number of topics listed range from 1 to 18, but average amount of topics listed is 11
summary(topic_tranx)
size(topic_tranx)
hist(size(topic_tranx))



# Setting apriori rules
# Topic needs to show up atleast 1% of all groups
# One topic must show up 50% more with another topic
rules <- apriori(topic_tranx, parameter=list(support=0.01, confidence=0.5))



# Visualize
plot(rules)
plot(rules, method="graph", control=list(type="items"))



# Create rules Data Frame and sort by lift
rules_df <- as(rules, 'data.frame')
rules_df <- rules_df %>% 
  separate(rules, c('LHS','RHS'), '=>') %>%
  arrange(desc(lift))

View(rules_df)


# Clean and tokenize LHS and RHS
rules_df$LHS <- gsub('\\{', '',  rules_df$LHS)
rules_df$LHS <- gsub('\\} ', '',  rules_df$LHS)
rules_df$RHS <- gsub(' \\{', '',  rules_df$RHS)
rules_df$RHS <- gsub('\\}', '',  rules_df$RHS)
rules_df$LHS <- strsplit(rules_df$LHS, split = ",")


#########################################################################################################

##### GROUP RECOMMENDER #####

# Enter Group name that you like
groupInput <- 'CryptoMondays NYC'

# Find topics related to the group we know you like
interested_topics <- meetup[meetup$Title==groupInput,]$Topic_List[[1]]
interested_topics


# Find all rules where LHS is a subset of interested list of topics
# Function to find subset
is_subset <- function(LL, L){
  any(LL[[1]] %in% L)
}

# Boolean of whether LHS in rules_df row is a subset
filter_rules <- unlist(lapply(rules_df$LHS, is_subset, interested_topics))
#View(rules_df[filter_rules == TRUE,])
filter_rules


# Filter rules_df to only include LHS that is susbset of interested topics
# These will be sorted by topics that have the highest Lift
recommended_topics <- unique(rules_df[filter_rules,]$RHS)
recommended_topics



# Find other groups that have the same recommended_topics
recommendation_list <- list()

for (topic in recommended_topics) {
  for (row in 1:nrow(meetup)){
    if (any(meetup$Topic_List[[row]] %in% topic)){ # see if any topics for this group match any topics under the group of interest
      if (!(meetup$Title[row] %in% recommendation_list)) { # make sure group name is not already in the list
        if (meetup$Title[row] != groupInput) { # make sure group name is not the group of interest
          recommendation_list = c(recommendation_list, meetup$Title[row])
        } # close if statement - group name is not the same as groupInput
      } # close if statement - group name not in list
    } # closing if statement - if any group topics are in the interested topics
  } # closing row for loop
} # closing topic for loop

# Turn list of recommendation list into a vector
# These are sorted by groups that contain the RHS topic with the highest lift from the LHS topics in our rules_df
recommendation_list <- unlist(recommendation_list)
View(recommendation_list)
class(recommendation_list)


iter_recommendations <- iter(recommendation_list)
nextElem(iter_recommendations)

iter_recommendations$length

####################################################################################################################



nchar(meetup[meetup$Title == groupInput,]$About)


















