# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(ggplot2)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Significance Testing Caveats"
APP_DESCP  <<- paste(
  "This app explores three caveats to null hypothesis significance testing.
  Each of these caveats holds true regardless of what type of hypothesis test
  you're conducting."
)
## End App Meta Data------------------------------------------------------------

# Global constants, functions, and data ----

# Define the ui ----
ui <- list(
  dashboardPage(
    skin = "purple",
    #Header ----
    dashboardHeader(
      title ="Significance Testing Caveats",
      titleWidth = 250,
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
    #Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
        menuItem("Large Sample Caveat", tabName = "lsc", icon = icon("wpexplorer")),
        menuItem("Small Sample Caveat", tabName = "ssc", icon = icon("wpexplorer")),
        menuItem("Multiple Testing Caveat", tabName = "mtc", icon = icon("wpexplorer")),
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
      tabItems(
        #Overview Page ----
        tabItem(
          tabName = "overview",
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
              strong("Large Sample Size Caveat: "),
              "As you increase the size of your sample, you will call negligible
              differences (i.e., there is no practical difference) significant.
              Explore the relationship between the size of your sample and how
              often you get a 'statistically significant' result when there is
              no practical difference between groups."
            ),
            tags$li(
              strong("Small Sample Size Caveat: "),
              "As you decrease the size of your sample, you will call even large
              practical differences insignificant. Explore the relationship between
              the size of your sample and how offten you get a 'statistically
              significant' result when there is practical difference between groups."
            ),
            tags$li(
              strong("Multiple Testing Caveat: "),
              "As you increase the number of significance tests you conduct, the
              number of results you declare as 'significant' will increase...even
              when the null hypothesis is true. Explore the relationship between
              the number of hypothesis tests you conduct and the number of
              'statistically significant' results when the null hypothesis models
              reality."
            )
          ),
          br(),
          h2("Instructions"),
          p("To use this app, you will need to"),
          tags$ol(
            tags$li(
              "Verify that you have mastered the pre-requisite material.",
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "prereqs",
                  label = "Pre-requisites",
                  icon = icon("bolt"),
                  size = "large"
                )
              )
            ),
            tags$li(
              "When you're ready to begin, use the left-hand menu to select which
              caveat/caution you wish to exlore or press the button to start with
              the Large Sample Caveat.",
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "explore",
                  label = "Explore",
                  icon = icon("bolt"),
                  size = "large",
                )
              )
            )
          ),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J. Hatfield
            and was based upon the work of David Robinson, then maintained by
            Zhuolin Luo.",
            br(),
            br(),
            br(),
            div(class = 'updated', "Last Update: 11/03/20 by NJH."))
        ),
        #Pre-requisites Page ----
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
            "In the Confirmatory Data Analysis tradition, null hypothesis
            significance tests (NHSTs) serve as a critical tool to confirm that
            a particular theoretical model describes our data and to make a
            generalization from our sample to the broader population (i.e., make
            an inference). The null hypothesis often reflects the simpler of two
            models (e.g., 'no statistical difference', 'there is an additive
            difference of 1', etc.) that we will use to build a sampling
            distribution for our chosen estimator. These methods let us test
            whether our sample data are consistent with this simple model (null
            hypothesis)."
          ),
          box(
            title = strong(tags$em("p"), "-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The probability that our selected estimator takes on a value at
            least as extreme as what we observed given our null hypothesis. If
            we were to carry out our study infinitely many times and the null
            hypothesis accurately modeled what we're studying, then we would
            expect for our estimator to produce a value at least as extreme as
            what we have seen 100*(p-value)% of the time. The larger the p-value,
            the more often we would expect our estimator to take on a value at
            least as extreme as what we've seen; the smaller, the less often."
          ),
          box(
            title = strong("Unusualness Threshold/Level of Significance"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The p-value gives us the long-run relative frequency of our estimator
            taking on certain behavior given the null hypothesis's model. Using
            our sample, our estimator has taken on a specific value. Is this value
            'typical' or 'usual' given our null hypothesis? We make this call by
            stating that we're going to set a threshold for what qualifies as
            'unusual'. This Unusualness Threshold, \\(\\alpha\\), is the upper
            probability limit for events we take as being 'unusual' given the
            null hypothesis. If we see events whose probability is no more than
            this threshold (i.e., p-value \\(\\leq \\alpha\\) ), we say that
            we've observed an unusual event given the null and we take our sample
            as being inconsistent with the model in the null hypothesis. If our
            p-value is greater than this threshold, then we have usual or typical
            event given the null hypothesis. This threshold is also known as the
            'Level of Significance'."
          ),
          box(
            title = strong("Statistically Significant Result"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "When you conduct a null hypothesis significance test and the p-value
            is less than your threshold, you state your decision to reject the
            null hypothesis as a model for what's going on. We call this event
            'statistically significant' and indicates that we should continue
            investigating the underlying processes."
          ),
          box(
            title = strong("Practical Significance"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Statistical significance only sends up a flag that there is something
            going on. Practical significance provides a way to measure the
            tangible impacts of the alternative model. Null hypothesis
            significances tests do not measure practical significance."
          ),
          p(
            "When you're ready to explore the caveats, press the Explore button
            or use the options on the left."
          ),
          div(
            style = "text-align: center;",
            bsButton(
            inputId = "explore1",
            label = "Explore",
            icon = icon("bolt"),
            size = "large"
            )
          )
        ),
        #Large Sample Caveat Page ----
        tabItem(
          tabName = "lsc",
          withMathJax(),
          h2("The Large Sample Caveat"),
          p(
            "There is no cure for the common cold caused by a rhinovirus, so
            medical advice is to get plenty of rest and drink clear liquids to
            stay hydrated. Following this advice the duration of the cold is
            typically around 9 days. Suppose a researcher claims to have developed
            a medication that will shorten the duration. If you had a cold, you
            might consider shortening the duration by a very small amount to be
            worthless (especially if the medication is expensive)."
          ),
          p(
            "Here, you'll explore the relationship between the size of the samples
            used in a hypothesis test and how often results would be declared as
            'statistically significant'. You are able to control three aspects:
            1) the actual effect of the medication that you don't see as showing
            any practical difference from the normal situation (i.e., how much
            shorter a cold would last), 2) the threshold for determining whether
            or not you would declare a test as 'statistically significant' (i.e.,
            setting the value of \\(\\alpha\\)), and 3) the total sample size used
            in the hypothesis test."
          ),
          p(
            "The app will simulate 100 hypothesis tests using your selected
            sample size. Use the controls to explore the relationship that exists
            between the sample size and the how often a hypothesis test would be
            declared 'statistically significant'."
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Controls"),
                sliderInput(
                  inputId = "lscDiff",
                  label = "Decrease in duration seen as worthless",
                  min = 0,
                  max = 0.2,
                  value = 0.1,
                  step = 0.01,
                  post = " days"
                ),
                sliderInput(
                  inputId = "lscAlpha",
                  label = "Set your significance level, \\(\\alpha\\)",
                  min = 0.01,
                  max = 0.25,
                  value = 0.1,
                  step = 0.01
                ),
                sliderInput(
                  inputId = "lscSize",
                  label = "Input a total sample size",
                  min = 0,
                  max = 1000,
                  value = 10,
                  step = 5
                )
              )
            ),
            column(
              width = 8,
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
                content = paste("What happens to the number of statistically",
                                "significant tests as you increase your sample",
                                "size?"),
                placement = "top"
              )
            )
          ),
          br(),
          p(
            tags$em("Note"),
            ": The points above the horizontal line are all p-values that exceed
            your selected threshold. The points below or on the horizontal line
            are all p-values that are at or below your selected threshold. Keep
            in mind that you've stipulated that any actual effect of the
            medication has no practical implication (i.e., is worthless)."
          )
        ),
        # Small Sample Caveat Page ----
        tabItem(
          tabName = "ssc",
          withMathJax(),
          h2("The Small Sample Caveat"),
          p(
            "There is no cure for the common cold caused by a rhinovirus, so
            medical advice is to get plenty of rest and drink clear liquids to
            stay hydrated. Following this advice the duration of the cold is
            typically around 9 days. Suppose a researcher claims to have developed
            a medication that will shorten the duration. If you had a cold you
            might consider shortening the duration by a good amount to be of
            practical importance."
          ),
          p(
            "Here, you'll explore the relationship between the size of the samples
            used in a hypothesis test and how often results would be declared as
            'statistically significant'. You are able to control three aspects:
            1) the actual effect of the medication that you view as being
            important to detect (i.e., worthwhile decreases in duration), 2) the
            threshold for determining whether or not you would declare a test as
            'statistically significant' (i.e., setting the value of \\(\\alpha\\)),
            and 3) the total sample size used in the hypothesis test."
          ),
          p(
            "The app will simulate 100 hypothesis tests using your selected
            sample size. Use the controls to explore the relationship that exists
            between the sample size and the chance that a test would be declared
            'statistically significant'."
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Controls"),
                sliderInput(
                  inputId = "sscDiff",
                  label = "Decrease in duration seen as worthwhile",
                  min = 1,
                  max = 5,
                  value = 1,
                  step = 0.05,
                  post = " days"
                ),
                sliderInput(
                  inputId = "sscAlpha",
                  label = "Set your significance level, \\(\\alpha\\)",
                  min = 0.01,
                  max = 0.25,
                  value = 0.1,
                  step = 0.01
                ),
                sliderInput(
                  inputId = "sscSize",
                  label = "Input a total sample size",
                  min = 0,
                  max = 250,
                  value = 250,
                  step = 5
                )
              )
            ),
            column(
              width = 8,
              h3("Test Result"),
              plotOutput("pplotSSC"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('pplotSSC').setAttribute('aria-label',
                `This plot displays the change of the number of statistically
                significant tests as we increase sample size.`)
                })"
              )),
              bsPopover(
                id = "pplotSSC",
                title = "Investigate!",
                content = paste("What happens to the number of statistically",
                                "significant tests as you decrease the sample size?"),
                placement = "top"
              )
            )
          ),
          p(
            tags$em("Note"),
            ": The points above the horizontal line are all p-values that exceed
            your selected threshold. The points below or on the horizontal line
            are all p-values that are at or below your selected threshold. Keep
            in mind that you've stipulated that the actual effect of the
            medication has practical importance."
          )
        ),
        # Multiple Testing Caveat Page ----
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
            declare a test as 'statistically significant' (i.e., setting the
            value of \\(\\alpha\\))."
          ),
          p(
            "Underlying this simulation is the notion that the null hypotheis is
            true. Thus, any p-value that is less than or equal to \\(\\alpha\\)
            would lead a researcher to claim statistical significance."
          ),
          p(
            "Use the controls to explore the relationship that exists between
            the number of hypothesis tests you conduct and the number of tests
            that would be declared 'statistically significant'."
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Controls"),
                sliderInput(
                  inputId = "mtcAlpha",
                  label = "Set your significance level, \\(\\alpha\\)",
                  min = 0.01,
                  max = 0.25,
                  value = 0.1,
                  step = 0.01
                ),
                br(),
                sliderInput(
                  inputId = "mtcTests",
                  label = "Set the number of hypothesis tests conducted",
                  min = 0,
                  max = 500,
                  value = 5,
                  step = 1
                )
              )
            ),
            column(
              width = 8,
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
                content = paste("What happens to the number of statistically",
                                "significant tests when you increase the number",
                                "of tests?"),
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
            provides a reasonable explanation for the data (i.e., 'fail to reject
            the null'). The points below or on the horizontal line are all
            p-values that are at or below your selected threshold. These points
            represent tests where you would decide that the null hypothesis
            doesn't adequately explain the data (i.e., 'reject the null')."
          )
        ),
        # References ----
        tabItem(
          tabName = "Ref",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
            R package. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020) boastUtils: BOAST Utilities,
            R package. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package.
            Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019),
            shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis,
            R Package,New York: Springer-Verlag.
            Available from https://ggplot2.tidyverse.org"
          )
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "lsc")
  })
  #Pre-req Button
  observeEvent(input$prereqs, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "prereq"
    )
  })
  #Explore1 Button
  observeEvent(input$explore1, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "lsc")
  })

  ## Create reactive values for the dynamic plots
  ## n is sample size, a is alpha, d is difference; caps are which caveat
  nMTC <- reactive({
    return(input$mtcTests)
  })
  aMTC <- reactive({
    return(input$mtcAlpha)
  })

  dLSC <- reactive({
    return(input$lscDiff)
  })
  aLSC <- reactive({
    return(input$lscAlpha)
  })
  nLSC <- reactive({
    return(input$lscSize)
  })

  dSSC <- reactive({
    return(input$sscDiff)
  })
  aSSC <- reactive({
    return(input$sscAlpha)
  })
  nSSC <- reactive({
    return(input$sscSize)
  })

  # Plot for the Multiple Testing Caveat ----
  #General Logic: Create a user-specified number of hypothesis tests' p-values.
  #Compare those p-values to a user-specified threshold for significance.
  output$pplotMTC <- renderPlot({
    validate(need(input$mtcTests > 0,
                  message = "Please input a valid number of tests"))
    validate(need(input$mtcAlpha > 0,
                  message = "Please input a valid threshold"))
    a1 = aMTC() #Get threshold
    n1 = nMTC() #Get sample size
    x1 = 1:n1 #Create sample ids
    bp = 0 #Set counters
    rp = 0
    r = numeric(n1) #Create pvalue vector
    sim1 = rbinom(n = n1,
                  size = 1,
                  prob = 1 - a1) #Use Binomial to generate n2 Bernoulli trials;
    #Success means p-value will be greater than threshold
    for (w in 1:n1) {
      if (sim1[w] == 1) {
        r[w] = runif(1, (a1 + 0.001) , 0.999)
        bp = bp + 1
      }
      else{
        r[w] = runif(1, 0.0001, a1)
        rp = rp + 1
      }
    } #generate p-values
    #Generate the plot
    ggplot(
      data = data.frame(x1, r),
      mapping = aes(x = x1, y = r)) +
      geom_point(
        color = ifelse(r <= a1, psuPalette[7], psuPalette[1]),
        shape = 19,
        size = 3
      ) +
      geom_line(y = a1,
                color = boastPalette[3],
                size = 1) +
      ylim(0, 1) +
      labs(
        title = bquote(
          "The p-values for " ~ .(n1) ~ " hypothesis tests at " ~ alpha == .(a1)
        ),
        y = "p-value",
        x = "Test Number",
        caption = paste("There are", bp, "blue points and", rp, "red points")
      ) +
      theme(
        panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.caption = element_text(size = 18),
        text = element_text(size = 18)
      )
  })

  #Plot for the Large Sample Caveat ----
  #General Logic: Under a null hypothesis of no additive difference,
  #the SAM's distribution converges in law to N(0,Var[X]/N).
  #Using a user-specified no-practical-difference value, generate 100 observed differences.
  #Treating these differences as values of the SAM, under the null,
  #generate the one-tail p-values. Compare the p-values to a user-specified
  #threshold for determining significance.
  output$pplotLSC <- renderPlot({
    validate(need(input$lscSize > 0,
                  message = "Please input a valid sample size"))
    validate(need(input$lscAlpha > 0,
                  message = "Please input a valid threshold"))
    delta2 = dLSC() #Get the no-practical difference
    n2 = nLSC() #Get the sample size
    a2 = aLSC() #Get threshold
    #Assuming two, equally sized groups, and Hedge's g effect size
    nc2 = sqrt((n2 / 2) * (n2 / 2) / n2) * delta2 #Calculate non-centrality parameter
    odiffs2 = rt(n = 100, df = n2 - 2, ncp = nc2) #Generate observed differences
    ps2 = pnorm(
      abs(odiffs2),
      mean = 0,
      sd = (15 / sqrt(n2)),
      lower.tail = FALSE
    )  #Calculate the associated p-values
    bp = 0
    rp = 0
    x1 = 1:100
    for (i in 1:100) {
      if (ps2[i] > a2) {
        bp = bp + 1
      }
      else{
        rp = rp + 1
      }
    }
    #Generate the plot
    ggplot(
      data = data.frame(x1, ps2),
      mapping = aes(x = x1, y = ps2)) +
      geom_point(
        color = ifelse(ps2 <= a2, psuPalette[7], psuPalette[1]),
        shape = 19,
        size = 3
      ) +
      geom_line(y = a2,
                color = boastPalette[3],
                size = 1) +
      ylim(0, 0.5) +
      labs(
        title = bquote("The p-values for 100 hypothesis tests at " ~ alpha == .(a2)),
        y = "p-value",
        x = "Simulation",
        caption = paste("There are", bp, "blue points and", rp, "red points")
      ) +
      theme(
        panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.caption = element_text(size = 18),
        text = element_text(size = 18)
      )
  })

  #Plot for the Small Sample Size Caveat ----
  #General Logic: Under a null hypothesis of no additive difference,
  #the SAM's distribution converges in law to N(0,Var[X]/N).
  #Using a user-specified level of practical difference, generate 100 observed
  #differences. Treating these differences as values of the SAM, under the null,
  #generate the one-tail p-values. Compare the p-values to a user-specified
  #threshold for determining significance.
  output$pplotSSC <- renderPlot({
    validate(need(input$sscSize > 3,
                  message = "Please input a valid sample size"))
    validate(need(input$sscAlpha > 0,
                  message = "Please input a valid threshold"))
    delta3 = dSSC() #Get the important effect size
    n3 = nSSC() # Get the sample size
    a3 = aSSC() # Get threshold
    #Assuming two, equally sized groups, and Hedge's g effect size
    nc3 = sqrt((n3 / 2) * (n3 / 2) / n3) * delta3 #Calculate non-centrality parameter
    odiffs3 = rt(n = 100, df = n3 - 2, ncp = nc3) #Generate observed differences
    ps3 = pnorm(
      abs(odiffs3),
      mean = 0,
      sd = (15 / sqrt(n3)),
      lower.tail = FALSE
    ) #Calculate the associated p-values
    bp = 0
    rp = 0
    x1 = 1:100
    for (i in 1:100) {
      if (ps3[i] > a3) {
        bp = bp + 1
      }
      else{
        rp = rp + 1
      }
    }
    #Generate the plot
    ggplot(
      data = data.frame(x1, ps3),
      mapping = aes(x = x1, y = ps3)) +
      geom_point(
        color = ifelse(ps3 <= a3, psuPalette[7], psuPalette[1]),
        shape = 19,
        size = 3
      ) +
      geom_line(y = a3,
                color = boastPalette[3],
                size = 1) +
      ylim(0, 0.5) +
      labs(
        title = bquote("The p-values for 100 hypothesis tests at " ~ alpha == .(a3)),
        y = "p-value",
        x = "Simulation",
        caption = paste("There are", bp, "blue points and", rp, "red points")
      ) +
      theme(
        panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.caption = element_text(size = 18),
        text = element_text(size = 18)
      )
  })
}

# Boast app call ----
boastUtils::boastApp(ui = ui, server = server)