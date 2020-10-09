library(shiny)
library(shinydashboard)
library(shinyBS)
library(ggplot2)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Significance Testing Canveats"
APP_DESCP  <<- paste(
  "This app explores three caveats to null hypothesis significance testing.
  Each of these caveats holds true regardless of what type of hypothesis test 
  you're conducting."
)
## End App Meta Data------------------------------------------------------------

dashboardPage(
  skin = "purple",
  #Title
  dashboardHeader(
    title ="Significance Testing Canveats",
    titleWidth = 250,
    tags$li(class="dropdown",
            actionLink("info", icon("info"), class="myClass")),
    tags$li(
      class = "dropdown",
      tags$a(target = "_blank", icon("comments"),
             href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Significance_Testing_Caveats"
      )
    ),
    tags$li(class = "dropdown",
            tags$a(href='https://shinyapps.science.psu.edu/',
                   icon("home")))
  ),
  #Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Overview", tabName = "over", icon = icon("dashboard")),
      menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
      menuItem("Multiple Testing Caveat", tabName = "mtc", icon = icon("wpexplorer")),
      menuItem("Large Sample Caveat", tabName = "lsc", icon = icon("wpexplorer")),
      menuItem("Small Sample Caveat", tabName = "ssc", icon = icon("wpexplorer")),
      menuItem("References",tabName = "Ref",icon = icon("leanpub"))
    ),
    #PSU logo
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
  
  #Content within the tabs
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    tabItems(
      #Overview Page
      tabItem(
        tabName = "ap1",
        uiOutput("markdown")
      ),
      tabItem(
        tabName = "over",
        h1("Significance Testing Canveats"),
        p(
          "In this app you will explore three important issues to keep in mind 
          when you are engaged in null hypothesis significance testing. 
          Null hypothesis tests are useful tools when you want to confirm an 
          underlying model using your data. However, just as with every tool, 
          you need to know how to use them properly and be aware of any limitations 
          or issues with their use.  This app explores three caveats to null
          hypothesis significance testing. Each of these caveats holds true 
          regardless of what type of hypothesis test (e.g., one/two sample 
          proportions, one/two sample location, independence, etc.) you're 
          conducting. This app will help you understand:"
        
          ),
        
        tags$ol(
          tags$li(
            "Multiple Testing Caveat: As you increase the number of significance 
            tests you conduct, the number of results you declare as 'significant' 
            will increase...even when the null hypothesis is true. Explore the 
            relationship between the number of hypothesis tests you conduct and 
            the number of 'statistically significant' results when the null hypothesis models reality."
          ),
          tags$li(
            "Large Sample Size Caveat: As you increase the size of your sample,
            you will call negligible differences (i.e., there is no practical difference) 
            significant. Explore the relationship between the size of your sample 
            and and the chance of getting a 'statistically significant' result
            when there is no practical difference between groups."
          ),
          tags$li(
            "Small Sample Size Caveat:  As you decrease the size of your sample,
            you will call even large practical differences insignificant.
            Explore the relationship between the size of your sample and the 
            chance of getting a 'statistically significant' result when there 
            is practical difference between groups."
          )
        ),
        br(),
        h2("Instructions"),
        p("To use this app, you will need to"),
        tags$ol(
          tags$li(
            "Verify that you have mastered the pre-requisite material.",
            div(style = "text-align: center", 
                bsButton(inputId = "prereqs", "Pre-requisites", icon("bolt"), size = "large",class = "circle grow"))
          ),
          tags$li(
            "When you're ready to begin, click to start with the 
            Multiple Testing Caveat, or use the left-hand menu to jump to a particular caveat.",
            div(style = "text-align: center", 
                bsButton(inputId = "explore", "Explore", icon("bolt"), size = "large",class = "circle grow"))
          )
        ),
        br(),
        h2("Acknowledgements"),
        p(
          "This version of the app was developed and coded by Neil J. Hatfield 
          and was based upon the work of David Robinson, then maintained by Zhuolin Luo.",
          br(),
          "Text content was written by Neil J. Hatfield."
          ),
        br(),
        p("We would like to extend a special thanks to the Shiny Program Students.",
          br(),
          br(),
          br(),
          div(class = 'updated', "Last Update: 08/28/20 by ZL."))
      ),
      
      #Pre-requisites Page
      tabItem(
        tabName = "prereq",
        withMathJax(),
        h2("Prerequisite Meanings"),
        p(
          "In order to get the most out of this applet, you need to already be 
          familiar with the following concepts. Write down how you think about 
          each one. When you're finished, press the plus signs (on the right edge)
          to expand each one to compare what you've written with what is here."
        ),
        box(
          title = strong("Null Hypothesis Significance Tests (NHSTs)"),
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          width = '100%',
          "In the Confirmatory Data Analysis tradition, null hypothesis significance 
          tests serve as a critical tool to confirm that a particular theoretical 
          model describes our data and to make a generalization from our sample to
          the broader population (i.e., make an inference). The null hypothesis 
          often reflects the simpler of two models (e.g., 'no statistical difference', 
          'there is an additive difference of 1', etc.) that we will use to build a 
          sampling distribution for our chosen estimator.  These methods let us test 
          whether our sample data are consistent with this simple model (null hypothesis)."
        ),
        box(
          title = strong(tags$em("p"), "-values"),
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          width = '100%',
          "The probability that our selected estimator takes on a value at least
          as extreme as what we observed given our null hypothesis. If we were
          to carry out our study infinitely many times and the null hypothesis 
          accurately modeled what we're studying, then we would expect for our 
          estimator to produce a value at least as extreme as what we have seen 
          100*(p-value)% of the time. The larger the p-value, the more often we
          would expect our estimator to take on a value at least as extreme as 
          what we've seen; the smaller, the less often."
        ),
        box(
          title = strong("Unusualness Threshold/Level of Significance"),
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          width = '100%',
          "The p-value gives us the long-run relative frequency of our estimator 
          taking on certain behavior given the null hypothesis's model. 
          Using our sample, our estimator has taken on a specific value. 
          Is this value 'typical' given the null hypothesis? We make this 
          call by stating that we're going to set a threshold for what qualifies 
          as 'unusual'. This Unusualness Threshold, \\(\\alpha\\), is the upper
          probability limit for events we take as being 'unusual' given the null
          hypothesis. If we see events whose probability is no more than this 
          threshold (i.e., p-value \\(\\leq \\alpha\\) ), we say that we've 
          observed an unusual event given the null and we take our sample as 
          being inconsistent with the model in the null hypothesis. 
          If our p-value is greater than this threshold, then we have usual or 
          typical event given the null hypothesis. This threshold is also known 
          as the 'Level of Significance'."
        ),
        box(
          title = strong("Statistically Significant Result"),
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          width = '100%',
          "When you conduct a null hypothesis significance test and the p-value 
          is less than your threshold, you state your decision to reject the null
          hypothesis as a model for what's going on. We call this event 'statistically significant' and indicates that we should continue investigating the underlying processes."
        ),
        box(
          title = strong("Practical Significance"),
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          width = '100%',
          "Statistical significance only sends up a flag that there is something 
          going on. Practical significance provides a way to measure the tangible 
          impacts of the alternative model. Null hypothesis significances tests 
          do not measure practical significance."
        ),
        p(
          "When you're ready to explore the caveats, press the Explore button 
          or use the options on the left."
        ),
        div(style = "text-align: center", bsButton(
          "explore1", "Explore", icon("bolt"), size = "large"
        ))
      ),
      
      #Multiple Testing Caveat Page
      tabItem(
        tabName = "mtc",
        withMathJax(),
        h2("The Multiple Testing Caveat"),
        p(
          "In this portion, you'll explore the relationship between the number 
          of hypothesis tests you conduct and the number of results that would 
          be declared as 'statistically significant'. You are able to control 
          two aspects: 1) the number of hypothesis tests you want to simulate 
          doing, and 2) the threshold for determining whether or not you would 
          declare a test as 'statistically significant' (i.e., setting the value of \\(\\alpha\\))."
        ),
        p(
          "Underlying this simulation is the notion that the null hypotheis 
          is true. Thus, any p-value that is less than or equal to \\(\\alpha\\) 
          would lead a researcher to claim statistical significance."
        ),
        p(
          "Use the controls to explore the relationship that exists between the 
          number of hypothesis tests you conduct and the number of tests that
          would be declared 'statistically significant'."
        ),
        fluidRow(
          column(
            4,
            h3("Controls"),
            sliderInput(
              inputId = "mtcAlpha",
              label = "Set your significance level, \\(\\alpha\\):",
              min = 0.01,
              max = 0.25,
              value = 0.1,
              step = 0.01
            ),
            br(),
            sliderInput(
              inputId = "mtcTests",
              label = "Set the number of hypothesis tests conducted:",
              min = 0,
              max = 500,
              value = 5,
              step = 10
            )
          ),
          column(
            8,
            h3("Test Result"),
            plotOutput("pplotMTC"),
            tags$script(HTML(
              "$(document).ready(function() {
              document.getElementById('pplotMTC').setAttribute('aria-label',
              `This plot displays the change of the number of statistically significant 
              tests when we increase the number of tests`)
              })"
            )),
            bsPopover(
              id = "pplotMTC",
              title = "Investigate!",
              content = "What happens to the number of statistically significant
              tests when you increase the number of tests?",
              placement = "top"
            )
          )
        ),
        br(),
        p(
          tags$em("Note"),
          ": The points above the horizontal line are all p-values that exceed 
          your selected threshold. In other words, the points above the line 
          represent the tests where you would decided that the null hypothesis
          provides a reasonable explanation for the data (i.e., 'fail to reject the null').  
          The points below or on the horizontal line are all p-values that are at or below your selected threshold. 
          These points represent tests where you would decide that the null 
          hypothesis doesn't adequately explain the data (i.e., 'reject the null')."
        )
      ),
      
      #Large Sample Caveat Page
      tabItem(
        tabName = "lsc",
        withMathJax(),
        h2("The Large Sample Caveat"),
        p(
          "There is no cure for the common cold caused by a rhinovirus, so medical 
          advice is to get plenty of rest and drink clear liquids to stay hydrated. 
          The duration of the disease then averages 9 days.  Suppose a researcher 
          claims to have developed a medication that will shorten the average duration 
          of a cold.  But if you had a cold you might consider shortening its duration
          by a very small amount to be worthless. Here, you'll explore the relationship 
          between the size of the samples used in a hypothesis test and the chance that
          results would be declared as 'statistically significant'. You are able to
          control three aspects: 1) the actual effect of the medication that you 
          don't see as showing any practical difference from the normal situation 
          (how much shorter a cold would last) , 2) the threshold for determining
          whether or not you would declare a test as 'statistically significant' 
          (i.e., setting the value of α), and 3) the total sample size used in the hypothesis test."
        ),
        p(
          "The app will simulate 100 hypothesis tests using your selected sample 
          size. Use the controls to explore the relationship that exists between 
          the sample size and the chance that a test would be declared 'statistically significant'."
        ),
        fluidRow(
          column(
            4,
            h3("Controls"),
            sliderInput(
              inputId = "lscDiff",
              label = "Pick how much shorter a cold might last that you would 
              still consider worthless: (days)",
              min = 0,
              max = 0.2,
              value = 0.1,
              step = 0.01
            ),
            sliderInput(
              inputId = "lscAlpha",
              label = "Set your significance level, \\(\\alpha\\):",
              min = 0.01,
              max = 0.25,
              value = 0.1,
              step = 0.01
            ),
            sliderInput(
              inputId = "lscSize",
              label = "Input a total sample size:",
              min = 0,
              max = 1000,
              value = 10,
              step = 2
            )
          ),
          column(
            8,
            h3("Test Result"),
            plotOutput("pplotLSC"),
            tags$script(HTML(
              "$(document).ready(function() {
  document.getElementById('pplotLSC').setAttribute('aria-label',
  `This plot displays the change of the number of statistically significant 
  tests as we increase sample size. (Large)`)
  })"
            )),
            bsPopover(
              id = "pplotLSC",
              title = "Investigate!",
              content = "What happens to the number of statistically significant 
              tests as you increase your sample size?",
              placement = "top"
            )
          )
        ),
        br(),
        p(
          tags$em("Note"),
          ": The points above the horizontal line are all p-values that exceed 
          your selected threshold. The points below or on the horizontal line are
          all p-values that are at or below your selected threshold.  
          Keep in mind that you've stipulated that any actual effect of the medication 
          has no practical implication."
        )
      ),
      
      #Small Sample Caveat Page
      tabItem(
        tabName = "ssc",
        withMathJax(),
        h2("The Small Sample Caveat"),
        p(
          "There is no cure for the common cold caused by a rhinovirus, so medical
          advice is to get plenty of rest and drink clear liquids to stay hydrated. 
          The duration of the disease then averages 9 days.  Suppose a researcher 
          claims to have developed a medication that will shorten the average duration
          of a cold.  If you had a cold you might consider shortening its duration
          by a good amount to be of practical importance. Here, you'll explore 
          the relationship between the size of the samples used in a hypothesis 
          test and the chance that results would be declared as 'statistically 
          significant'. You are able to control three aspects: 1) the actual 
          effect of the medication that you view as being important to detect, 
          2) the threshold for determining whether or not you would declare a test
          as 'statistically significant' (i.e., setting the value of α), and 3) 
          the total sample size used in the hypothesis test."
        ),
        p(
          "The app will simulate 100 hypothesis tests using your selected sample 
          size. Use the controls to explore the relationship that exists between 
          the sample size and the chance that a test would be declared 'statistically significant'."
        ),
        fluidRow(
          column(
            4,
            h3("Controls"),
            sliderInput(
              inputId = "sscDiff",
              label = "Pick how much shorter a cold might last that you view as
              being important to detect: (days)",
              min = 1,
              max = 5,
              value = 1,
              step = 0.05
            ),
            sliderInput(
              inputId = "sscAlpha",
              label = "Set your significance level, \\(\\alpha\\):",
              min = 0.01,
              max = 0.25,
              value = 0.1,
              step = 0.01
            ),
            sliderInput(
              inputId = "sscSize",
              label = "Input a total sample size:",
              min = 4,
              max = 250,
              value = 250,
              step = 2
            )
          ),
          column(
            8,
            h3("Test Result"),
            plotOutput("pplotSSC"),
            tags$script(HTML(
              "$(document).ready(function() {
  document.getElementById('pplotSSC').setAttribute('aria-label',
  `This plot displays the change of the number of statistically significant tests 
  as we increase sample size. (Small)`)
  })"
            )),
            bsPopover(
              id = "pplotSSC",
              title = "Investigate!",
              content = "What happens to the number of statistically significant tests 
              as you decrease the sample size?",
              placement = "top"
            )
          )
        ),
        p(
          tags$em("Note"),
          ": The points above the horizontal line are all p-values that exceed
          your selected threshold. The points below or on the horizontal line are all p-values 
          that are at or below your selected threshold.  Keep in mind that you've 
          stipulated that the actual effect of the medication has practical importance."
        )
      ),
      tabItem(
        tabName = "Ref",
        withMathJax(),
        h2("References"),
        p(class = "hangingindent",
          "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R package.
          Available from https://CRAN.R-project.org/package=shinyBS"),
        p(class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create dashboards with 'Shiny', R Package. 
          Available from https://CRAN.R-project.org/package=shinydashboard"),
        p(class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019), 
          shiny: Web application framework for R, R Package. 
          Available from https://CRAN.R-project.org/package=shiny"),
        p(class = "hangingindent",
          "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis, R Package, 
          New York: Springer-Verlag. Available from https://ggplot2.tidyverse.org")
      )
    )
  )
)