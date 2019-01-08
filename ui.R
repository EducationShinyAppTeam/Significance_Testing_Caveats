#MTC

library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)

dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Caveats of Significance Testing",titleWidth=300),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(id="tabs",
                  menuItem("Overview", tabName = "over", icon = icon("dashboard")),
                  menuItem("Multiple Testing Caution", tabName = "mtc", icon = icon("wpexplorer")),
                  menuItem("Large Sample Caution", tabName = "LargeSizeCaution", icon = icon("wpexplorer")),
                  menuItem("Small Sample Caution", tabName = "SmallSizeCaution", icon = icon("wpexplorer"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
                ),
                tags$style(
                  type = "text/css",
                  ".content-wrapper,.right-side {
                  background-color: white;
                  }"
                ),
                tabItems(
                  tabItem(tabName = "over",
                          tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                          br(),br(),br(),
                          h3(strong("About:")),
                          h4("In this app you will explore the most important caveats of significance testing ."),
                          h4("Statistical hypothesis tests are a very useful tool but it is important to use them properly and avoid certain caveats. "),
                          br(),
                          h3(strong("Instructions:")),
                          h3("This app explores three caveats:"),
                          h4(tags$li("The first caveat is the “Multiple Testing Caution” where you investigate how the number of significant results changes under the null as the number of significance tests changes.")),
                          h4(tags$li("The second caveat is the “Large Sample Caution” where you investigate how the number of significant results changes under an alternative close to the null as the sample size changes.")),
                          h4(tags$li("The third caveat is the “Small Sample Caution” where you investigate how the number of significant results changes under an alternative of practical importance as the sample size changes.")),
                          
                          div(style = "text-align: center",bsButton("explore", "Explore", icon("bolt"), size = "large")),
                          br(),
                          h3(strong("Acknowledgements:")),
                          h4("This app was developed and coded by David Robinson.")
                          ),
                    
                  tabItem(tabName = "mtc",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          fluidRow(
                            #Include LaTeX functioality although I don't think I used it for this
                            withMathJax(),
                            column(4,
                                   h3("The Problem:"),
                                   box(width =12,background = "blue",
                                       "When the null hypothesis is true there is a small chance of getting a low p-value and declaring the result highly significant (from the definition of the p-value).  So if you look at a lot significance tests, finding one that is highly significant is expected."
                                       
                                   ),
                                   #h3("Notice how there tend to be more 'significant' pvalues on the plot when you increase the number of tests"),
                                   br(),
                                   tags$style(type = "text/css",
                                              "
                                              .irs-slider {width: 8px; height: 20px; top: 22px;}
                                              "),
                                   sliderInput(inputId = "days", "Adjust how many significance tests you are running here:", min = 0, max = 500, val = 5,step = 10)
                                   
                            ),
                            column(8,
                                   h3("Plot"),
                                   plotOutput("pplotMTC"),
                                   bsPopover(id = "pplotMTC", " ", content = "What happens to the number of significant pvalues when you increase the number of tests?", placement = "top"
                                   ),
                                   h4("The points above the green line on the plot are all p-values you might consider high. In other words the points above the greeen line represent the tests where you might conclude the null hypothesis provides a reasonable explanation for the data")
                            )
                            
                          )
                  ),
                  
                  tabItem(tabName = "LargeSizeCaution",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          #Large
                          fluidRow(
                            #Include LaTeX functioality although I don't think I used it for this
                            withMathJax(),
                            column(4,
                                   h3("The Problem:"),
                                   box(width =12,background = "blue",
                                       "With a sufficiently large sample size, one can detect the smallest of departures from the null hypothesis.  For studies with large sample sizes, ask yourself if the magnitude of the observed difference from the null hypothesis is of any practical importance."
                                       
                                   ),
                                   #h3("Notice how there tend to be more 'significant' pvalues on the plot when you increase the number of tests"),
                                   br(),
                                   tags$style(type = "text/css",
                                              "
                                              .irs-slider {width: 8px; height: 20px; top: 22px;}
                                              "),
                                   sliderInput(inputId = "sizeForLargeCaution", "Input a Sample Size ", min = 1, max = 1000, value = 10)
                            ),
                            column(8,
                                   h3("Plot"),
                                   plotOutput("LargeCautionPlot"),
                                   bsPopover(id = "LargeCautionPlot", " ", content = "The points below the green line on the plot are all pvalues you might consider low.", placement = "bottom"),
                                   h4("This plot shows the p-values for simulations of alternative hypothesis values that are not practically different from the null hypothesis. See what happens to the number of significant simulations when the sample size gets very large. ")
                            )
                            
                          )),
                  
                  tabItem(tabName = "SmallSizeCaution",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          #Small
                          fluidRow(
                            #Include LaTeX functioality although I don't think I used it for this
                            withMathJax(),
                            column(4,
                                   h3("The Problem:"),
                                   box(width =12,background = "blue",
                                       "For very small sample sizes, a very large departure of the sample results from the null hypothesis may not be statistically significant (although it may be of practical concern).  This should motivate one to do a better study with a larger sample size."
                                   ),
                                   br(),
                                   tags$style(type = "text/css",
                                              "
                                              .irs-slider {width: 8px; height: 20px; top: 22px;}
                                              "),
                                   sliderInput(inputId = "sizeForSmallCaution", "Input a Sample Size ", min = 1, max = 200, value = 15)
                            ),
                            column(8,
                                   h3("Plot"),
                                   plotOutput("SmallCautionPlot"),
                                   bsPopover(id = "SmallCautionPlot", " ", content = "The points below the green line on the plot are all pvalues you might consider low.", placement = "bottom"),
                                   h4("This plot shows the p-values for simulations of alternative hypothesis values that are practically different from the null hypothesis. See what happens to the number of significant simulations when the sample size gets small. ")
                            )
                            
                          )
                  ),
                  tabItem(tabName = "CVQ",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          fluidRow(
                            column(6,
                                   numericInput(inputId = "mtcQ1", "Q1: If the null hypothesis is true with a significance level of 5%, how many tests would you expect to be significant out of 100?", min = 0 , max = 100, val = 0 ),
                                   numericInput(inputId = "mtcQ2", "Q2: If the null hypothesis is true with a significance level of 10%, how many tests would you expect to be NOT significant out of 100?", min = 0 , max = 100, val = 0 )
                            ),
                            column(6,
                                   br(),
                                   textOutput("mtcQ1check"),br(),br(),br(),br(),br(),br(),
                                   textOutput("mtcQ2check")
                            )
                            
                          )
                  )
                )
              )
)




