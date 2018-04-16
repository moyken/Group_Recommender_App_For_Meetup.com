library(dplyr)
library(iterators)
library(shiny)


function(input, output) {

  
  
  # Find all topics associated with the group user selected
  interested_topics <- reactive({
    meetup[meetup$Title == input$groupInput,]$Topic_List[[1]]
  })

  
  
  # Boolean of whether LHS in rules_df row is a subset
  filter_rules <- reactive({
    unlist(lapply(rules_df$LHS, is_subset, interested_topics()))
  })
  
  
  
  # Filter rules_df to only include LHS that is subset of interested topics (antecedents)
  # Then take the topics on the RHS (consequents)
  # These will be sorted by topics that have the highest Lift
  recommended_topics <- reactive({
    unique(rules_df[filter_rules(),]$RHS)
  })
  
 

  # Find groups that have those conseequent topics

  recommended_groups <- reactive({

    recommendation_list <- list() # initialize recommendation list

    for (topic in recommended_topics()) {
      for (row in 1:nrow(meetup)){
        if (any(meetup$Topic_List[[row]] %in% topic)){ # see if any topics for this group match any topics under the group of interest
          if (!(meetup$Title[row] %in% recommendation_list)) { # make sure group name is not already in the list
            if (meetup$Title[row] != input$groupInput) { # make sure group name is not the group of interest
              recommendation_list = c(recommendation_list, meetup$Title[row])
            } # close if statement - group name is not the same as groupInput
          } # close if statement - group name not in list
        } # closing if statement - if any group topics are in the interested topics
      } # closing row for loop
    } # closing topic for loop

    recommendation_list = unlist(recommendation_list)
    
    iter_recommendations = iter(recommendation_list) # Make recommendation list iterable

    return(iter_recommendations)
  })
  
  
  
  # Hit button to see next recommended group
  nextGroup <- eventReactive({input$anotherButton
                              input$groupInput},
                             {nextElem(recommended_groups())
                               })
  
  output$showNext <- renderText({
   validate(
    need(recommended_groups()$length > 0 ,
    "Sorry! There are no recommendations given the selected group :/ Please select another."
    )
   )
    nextGroup()
  })
  
  
  # Show the About Me page for recommended group
  output$aboutMe <- renderText({
    validate(
      need(recommended_groups()$length > 0 ,
           ""
      )
    )
    paste('[Group Description]: ', meetup[meetup$Title == nextGroup(),]$About)
  })
  
  
  # Show topics for recommended group
  output$topics <- renderText({
    validate(
      need(recommended_groups()$length > 0 ,
           ""
      )
    )
    paste('[Topics]: ', meetup[meetup$Title == nextGroup(),]$Topics)
  })
  
  
  
  #########################################################################################################
  
  # FOR DEBUGGING
 
  
  
  # Save selected groupInput
  output$groupSelected <- renderText({ input$groupInput })
  
  

   output$clicks <- renderText({
    input$anotherButton
  })
  
}












