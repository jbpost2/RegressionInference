###########################################################################
##R Shiny App to plot confidence intervals and find their coverage rate
###########################################################################

#Load package
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)


dashboardPage(skin="red",
              #add title
              dashboardHeader(title = "Sampling Distribution of Regression Estimators Visualization", titleWidth = 750),
              
              #define sidebar items
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("archive")),
                menuItem("Application", tabName = "app", icon = icon("laptop"))
              )),
              
              #define the body of the app
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "about",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            
                            #two columns for each of the two items
                            column(6,
                                   #Description of App
                                   h1("What does this app do?"),
                                   #box to contain description
                                   box(background="red",width=12,
                                       h4("This app is meant to help visualize the variability of the fitted intercept and slope parameters of a simple linear regression line.   The user can specify the true line to generate data from as well as the samples size and standard deviation on the errors"),#, and the rough pattern in which the predictor (x) is observed. "),
                                       h4("As data sets are generated, the most recent has its scatterplot shown with the true and fitted regression lines.  Past regression lines remain as greyed out lines (up to 100 of them) to help understand the variability in the fitted lines."),
                                       h4("The sampling distributions of the sample intercept and slope are visualized in the histograms on the lower left and right, respectively.  Summary values about the estimates are also provided.  ")
                                   )
                            ),
                            
                            column(6,
                                   #How to use the app
                                   h1("How to use the app?"),
                                   #box to contain description
                                   box(background="red",width=12,
                                       h4("The controls for the app are located on the top left and top right.  The top left controls allow for changing the parameters of the simulated datasets.  Any time one of these is changed the number of datasets created resets to 0.  The controls on the top right allow for creation of more data sets and to reset the number of data sets."),
                                       h4("As you generate each sample, the information corresponding to the most recent sample is plotted and values are added to each histogram on the bottom.")
                                   )
                            )
                          )
                  ),
                  
                  #actual app layout      
                  tabItem(tabName = "app",  
                          
                          fluidRow(
                            column(2,
                              h2("True Regression Parameters"),
                              sliderInput("Param1", h4("Intercept:"), value = 1, step = 0.25, min = -10, max = 10),
                              sliderInput("Param2", h4("Slope:"), value = 0, min = -10,max = 10, step = 0.25),
                              sliderInput("sd", h4("Standard Deviation:"), value = 1, min = 0.1, max = 10, step = 0.5),
                              sliderInput("sampleSize", h4("Sample size:"), value = 10, min = 2, max = 50, step = 1)
                              #selectizeInput("xDist", h4("Pattern for predictor (x):"), choices = c("Uniform", "Skewed-Left", "Skewed-Right", "Bell-Shaped"))#,
                      #selectInput("intervalType","One Sample Interval Method:",choices=c("Normal","T"))
                            ), #end column

                            #Plots and summary stats
                             column(10,
                                    fluidRow(
                                      h1("Visual of True and Fitted Regression Lines")
                                      ),
                                    fluidRow(
                                      column(8,
                                        fluidRow(
                                          box(width = 12, background = "red",
                                             h3("Blue indicates the most recent dataset's fitted regression line.  Black the 'true' regression line."),
                                              plotOutput("fitPlots") %>% withSpinner(color="#0dc5c1")
                                          )
                                        )
                                      ),
                                      column(4,
                                           h2("Generate some data sets!"),
                                           actionButton("newData", h4("New Data Set")),
                                         div(br(), style = "font-size:10%"),
                                           actionButton("newData10", h4("10 New Data Sets")),
                                         div(br(), style = "font-size:10%"),
                                           actionButton("newData100", h4("100 New Data Sets")),         div(br(), style = "font-size:10%"),
                                           actionButton("newData1000", h4("1000 New Data Sets")),         div(br(), style = "font-size:10%"),
                                           actionButton("reset", h4("Reset the # of datasets to 0"))
                                        )
                                    )
                             ) #end column
                          ),
                          fluidRow(
                            column(6, 
                              box(width = 12, background = "red",
                                  title = h3("Sampling Distribution of Intercept"),
                                  column(8,
                                         plotOutput("interceptHist")
                                  ),
                                  column(4,
                                         div(tableOutput("interceptSummaryStats"), style = "font-size:125%")
                                  )
                              )
                            ),
                            column(6, 
                                   box(width = 12, background = "red",
                                       title = h3("Sampling Distribution of Slope"),
                                       column(8,
                                              plotOutput("slopeHist")
                                       ),
                                       column(4,
                                              div(tableOutput("slopeSummaryStats"), style = "font-size:125%")
                                       )
                                   )
                            )
                          )
                  )
                )
              )
)

