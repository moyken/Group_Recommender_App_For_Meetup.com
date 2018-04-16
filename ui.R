library(shinythemes)
library(shiny)
library(shinydashboard)

navbarPage(theme =  shinytheme('simplex'),
           
           title = 'Meetup.com - Tech Group Recommendations',
           
           
           tabPanel(title = 'Find A Group To Join',
                    
                    sidebarPanel(h3('Steps To Find A Group'),
                                 '1) Select Tech Group of interest for recommendation', br(),
                                 '2) Click button for more recommendations!', br(),
                                 
                                 selectizeInput("groupInput",
                                             label = h4("Select Group Below"), 
                                             choices = sort(meetup$Title),
                                             selected = 1
                                             ), 
                                 
                                 actionButton("anotherButton", label = "Recommend Me Another"),
                                 width = 4
                                 ), # close side bar panel
                    
                    mainPanel(h2(paste('You May Also Like:')),
                              # textOutput('clicks'), # For debugging,
                              h4(textOutput("showNext")),
                              textOutput('aboutMe'),
                              br(),
                              textOutput('topics')
                              ) # Close main Panel
                    ) # Close tabPanel
)