# Multiple Testing Caution

library(shiny)
library(shinydashboard)
library(shinyBS)
library(ggplot2)
library(boastUtils)

APP_TITLE <- "Significance Testing Caveats"

ui <- dashboardPage(
  skin = "blue",

  # Title
  dashboardHeader(title = "Caveats of Significance Testing", titleWidth = 300),

  # Sidebar
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Multiple Testing Caution", tabName = "MultipleTestingCaution", icon = icon("wpexplorer")),
      menuItem("Large Sample Caution", tabName = "LargeSizeCaution", icon = icon("wpexplorer")),
      menuItem("Small Sample Caution", tabName = "SmallSizeCaution", icon = icon("wpexplorer"))
    ),
    tags$div(class = "sidebar-logo", boastUtils::psu_eberly_logo("reversed"))
  ),

  # Content within the tabs
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        h3("About:"),
        p("In this app you will explore the most important caveats of significance testing."),
        p("Statistical hypothesis tests are a very useful tool but it is important to use them properly and avoid certain caveats. "),
        br(),
        h3("Instructions:"),
        p("This app explores three caveats:"),
        tags$ul(
          tags$li("The first caveat is the “Multiple Testing Caution” where you investigate how the number of significant results changes under the null as the number of significance tests changes."),
          tags$li("The second caveat is the “Large Sample Caution” where you investigate how the number of significant results changes under an alternative close to the null as the sample size changes."),
          tags$li("The third caveat is the “Small Sample Caution” where you investigate how the number of significant results changes under an alternative of practical importance as the sample size changes."),
        ),
        div(style = "text-align: center", bsButton("explore", "Explore", icon("bolt"), size = "large")),
        br(),
        h3("Acknowledgements:"),
        p("This app was developed and coded by David Robinson.")
      ),

      tabItem(
        tabName = "MultipleTestingCaution",
        div(
          style = "display: inline-block;vertical-align:top;",
          tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
        ),
        fluidRow(
          withMathJax(),
          column(
            4,
            h3("The Problem:"),
            box(
              width = 12, background = "blue",
              "When the null hypothesis is true there is a small chance of getting a low p-value and declaring the result highly significant (from the definition of the p-value).  So if you look at a lot significance tests, finding one that is highly significant is expected."
            ),
            br(),
            sliderInput(inputId = "days", "Adjust how many significance tests you are running here:", min = 0, max = 500, val = 5, step = 10)
          ),
          column(
            8,
            h3("Plot"),
            plotOutput("pplotMTC"),
            bsPopover(id = "pplotMTC", " ", content = "What happens to the number of significant pvalues when you increase the number of tests?", placement = "top"),
            p("The points above the green line on the plot are all p-values you might consider high. In other words the points above the greeen line represent the tests where you might conclude the null hypothesis provides a reasonable explanation for the data")
          )
        )
      ),

      tabItem(
        tabName = "LargeSizeCaution",
        div(
          style = "display: inline-block;vertical-align:top;",
          tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
        ),
        # Large
        fluidRow(
          # Include LaTeX functioality although I don't think I used it for this
          withMathJax(),
          column(
            4,
            h3("The Problem:"),
            box(
              width = 12, background = "blue",
              "With a sufficiently large sample size, one can detect the smallest of departures from the null hypothesis.  For studies with large sample sizes, ask yourself if the magnitude of the observed difference from the null hypothesis is of any practical importance."
            ),
            # h3("Notice how there tend to be more 'significant' pvalues on the plot when you increase the number of tests"),
            br(),
            sliderInput(inputId = "sizeForLargeCaution", "Input a Sample Size ", min = 1, max = 1000, value = 10)
          ),
          column(
            8,
            h3("Plot"),
            plotOutput("LargeCautionPlot"),
            bsPopover(id = "LargeCautionPlot", " ", content = "The points below the green line on the plot are all pvalues you might consider low.", placement = "bottom"),
            p("This plot shows the p-values for simulations of alternative hypothesis values that are not practically different from the null hypothesis. See what happens to the number of significant simulations when the sample size gets very large. ")
          )
        )
      ),

      tabItem(
        tabName = "SmallSizeCaution",
        div(
          style = "display: inline-block;vertical-align:top;",
          tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
        ),
        # Small
        fluidRow(
          withMathJax(),
          column(
            4,
            h3("The Problem:"),
            box(
              width = 12, background = "blue",
              "For very small sample sizes, a very large departure of the sample results from the null hypothesis may not be statistically significant (although it may be of practical concern).  This should motivate one to do a better study with a larger sample size."
            ),
            br(),
            sliderInput(inputId = "sizeForSmallCaution", "Input a Sample Size ", min = 1, max = 200, value = 15)
          ),
          column(
            8,
            h3("Plot"),
            plotOutput("SmallCautionPlot"),
            bsPopover(id = "SmallCautionPlot", " ", content = "The points below the green line on the plot are all pvalues you might consider low.", placement = "bottom"),
            p("This plot shows the p-values for simulations of alternative hypothesis values that are practically different from the null hypothesis. See what happens to the number of significant simulations when the sample size gets small. ")
          )
        )
      )
      
      # This code is not being used.
      #
      # tabItem(
      #   tabName = "CVQ",
      #   div(
      #     style = "display: inline-block;vertical-align:top;",
      #     tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
      #   ),
      #   fluidRow(
      #     column(
      #       6,
      #       numericInput(inputId = "mtcQ1", "Q1: If the null hypothesis is true with a significance level of 5%, how many tests would you expect to be significant out of 100?", min = 0, max = 100, val = 0),
      #       numericInput(inputId = "mtcQ2", "Q2: If the null hypothesis is true with a significance level of 10%, how many tests would you expect to be NOT significant out of 100?", min = 0, max = 100, val = 0)
      #     ),
      #     column(
      #       6,
      #       br(),
      #       textOutput("mtcQ1check"), br(), br(), br(), br(), br(), br(),
      #       textOutput("mtcQ2check")
      #     )
      #   )
      # )
    )
  )
)

server <- function(input, output, session) {
  
  PLOT_DETAILS <- list(
    red = 0,
    blue = 0
  )
  
  # Learning Locker Statement Generation
  .generateStatement <- function(session, verb = NA, object = NA, description = NA, value = NA) {
    if(is.na(object)){
      object <- paste0("#shiny-tab-", session$input$tabs)
    } else {
      object <- paste0("#", object)
    }
    
    stmt <- list(
      verb =  verb,
      object = list(
        id = paste0(boastUtils::getCurrentAddress(session), object),
        name = paste0(APP_TITLE),
        description = description
      )
    )
    
    if(!is.na(value)){
      stmt$result <- list(
        response = value,
        extensions = list(
          ref = "https://shinyapps.science.psu.edu/details", value = paste0("{ \"red\": ", PLOT_DETAILS$red, ", \"blue\": ", PLOT_DETAILS$blue, " }")
        )
      ) 
    }
    
    statement <- rlocker::createStatement(stmt)
    response <- rlocker::store(session, statement)
    
    print(statement)
    
    return(response)   
  }

  # Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "MultipleTestingCaution")
  })
  
  observeEvent(input$tabs, {
    .generateStatement(session, verb = "experienced", description = paste0("Navigated to ", input$tabs, " tab."))
  }, ignoreInit = TRUE)
  
  observeEvent(input$days, {
    # generateStatement belongs here
  }, ignoreInit = TRUE)
  
  observeEvent(input$sizeForLargeCaution, {
    # generateStatement belongs here
  }, ignoreInit = TRUE)
  
  observeEvent(input$sizeForSmallCaution, {
    # generateStatement belongs here
  }, ignoreInit = TRUE)
  
  nMTC <- reactive({
    return(input$days)
  })

  nLCV <- reactive({
    return(input$sizeForLargeCaution)
  })

  nSCV <- reactive({
    return(input$sizeForSmallCaution)
  })

  sample.size <- reactive({
    return(input$samp.size)
  })

  # I think this way is better
  # Second thought on simulating this
  # Binomial might not be the best
  # Idea is to randomly generate x number of data points (days)
  #       and each day has 5% chance of having a small pvalue

  # Plot for the first idea
  output$pplotMTC <- renderPlot({
    validate(
      need(input$days > 0,
        message = "Please input a valid number of tests"
      )
    )
    n2 <- nMTC()
    x1 <- 1:n2
    bp <- 0
    rp <- 0
    r <- numeric(n2)
    sim1 <- rbinom(n = n2, size = 1, prob = 0.95)
    for (w in 1:n2) {
      if (sim1[w] == 1) {
        r[w] <- runif(1, 0.05001, 0.999)
        bp <- bp + 1
      }
      else {
        r[w] <- runif(1, 0.0001, 0.05)
        rp <- rp + 1
      }
    }
    r <- r
    # plot(r,ylim = c(0,1), ylab = "p-value",main = "p-values of multiple tests", xlab = paste("There are", bp, "blue points and",rp,"red points" ), xlim = c(0,500),col=ifelse(r<=0.05, "red", "blue"))
    # abline(h = 0.05, col = "green", lwd = 2)
    # ggplot(data.frame(x1,r),aes(x=x1, y=r))+geom_point(color=ifelse(r<=0.05, "red", "blue"))+ geom_line(y = 0.05, color = "green") + ylim(0,1) + xlab("Test") + ylab("p-value") + ggtitle("p- values of multiple tests")
    PLOT_DETAILS <<- list(
      red = rp,
      blue = bp
    )
    
    .generateStatement(session, object = "days", verb = "interacted", description = "Adjust how many significance tests you are running here", value = input$days)
    
    ggplot(data.frame(x1, r), aes(x = x1, y = r)) + geom_point(color = ifelse(r <= 0.05, "red", "blue")) +
      geom_line(y = 0.05, color = "green") + ylim(0, 1) +
      labs(title = "p - values of multiple tests", y = "p-value", x = "Test", caption = paste("There are", rp, "red points and", bp, "blue points")) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(plot.caption = element_text(size = 14))
  })

  # This code is not being used.
  # 
  # output$mtcQ1check <- renderText({
  #   if (input$mtcQ1 == 5) {
  #     paste("Correct!")
  #   }
  # })
  # 
  # output$mtcQ2check <- renderText({
  #   if (input$mtcQ2 == 90) {
  #     paste("Correct!")
  #   }
  # })

  output$LargeCautionPlot <- renderPlot({
    validate(
      need(input$sizeForLargeCaution > 0,
        message = "Please input a valid sample size"
      )
    )

    meanCV <- 100
    sdCV <- 15

    n3 <- nLCV()

    seCV <- sdCV / sqrt(n3)

    sims1 <- 100

    vectorOfArrivalTimes <- numeric(sims1)
    vectorOfZ <- numeric(sims1)
    vectorOfPvalues <- numeric(sims1)

    for (i in 1:sims1) {
      vectorOfArrivalTimes[i] <- rnorm(1, mean = meanCV, sd = 2)
    }

    for (j in 1:sims1) {
      vectorOfZ[j] <- (vectorOfArrivalTimes[j] - meanCV) / seCV
    }

    for (k in 1:sims1) {
      if (vectorOfZ[k] < 0) {
        vectorOfPvalues[k] <- pnorm(vectorOfZ[k])
      }
      else {
        vectorOfPvalues[k] <- pnorm(vectorOfZ[k], lower.tail = FALSE)
      }
    }

    bp <- 0
    rp <- 0
    r <- vectorOfPvalues
    x1 <- 1:sims1
    for (m in 1:sims1) {
      if (vectorOfPvalues[m] > 0.05) {
        bp <- bp + 1
      }
      else {
        rp <- rp + 1
      }
    }
    
    PLOT_DETAILS <<- list(
      red = rp,
      blue = bp
    )
    
    .generateStatement(session, object = "sizeForLargeCaution", verb = "interacted", description = "Input a Sample Size", value = input$sizeForLargeCaution)
    
    ggplot(data.frame(x1, r), aes(x = x1, y = r)) + geom_point(color = ifelse(r <= 0.05, "red", "blue")) +
      geom_line(y = 0.05, color = "green") + ylim(0, 0.5) +
      labs(title = "p - values of multiple tests", y = "p-value", x = "Simulation", caption = paste("There are", rp, "red points and", bp, "blue points")) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(plot.caption = element_text(size = 14))
  })

  output$SmallCautionPlot <- renderPlot({
    validate(
      need(input$sizeForSmallCaution > 0,
        message = "Please input a valid sample size"
      )
    )

    meanCV <- 100
    sdCV <- 15

    n3 <- nSCV()

    seCV <- sdCV / sqrt(n3)

    sims1 <- 100

    vectorOfArrivalTimes <- numeric(sims1)
    vectorOfZ <- numeric(sims1)
    vectorOfPvalues <- numeric(sims1)

    for (i in 1:sims1) {
      vectorOfArrivalTimes[i] <- rnorm(1, mean = meanCV, sd = 10)
    }

    for (j in 1:sims1) {
      vectorOfZ[j] <- (vectorOfArrivalTimes[j] - meanCV) / seCV
    }

    for (k in 1:sims1) {
      if (vectorOfZ[k] < 0) {
        vectorOfPvalues[k] <- pnorm(vectorOfZ[k])
      }
      else {
        vectorOfPvalues[k] <- pnorm(vectorOfZ[k], lower.tail = FALSE)
      }
    }

    bp <- 0
    rp <- 0
    r <- vectorOfPvalues
    x1 <- 1:sims1
    for (m in 1:sims1) {
      if (vectorOfPvalues[m] > 0.05) {
        bp <- bp + 1
      }
      else {
        rp <- rp + 1
      }
    }
    
    PLOT_DETAILS <<- list(
      red = rp,
      blue = bp
    )
    
    .generateStatement(session, object = "sizeForSmallCaution", verb = "interacted", description = "Input a Sample Size", value = input$sizeForSmallCaution)

    ggplot(data.frame(x1, r), aes(x = x1, y = r)) + geom_point(color = ifelse(r <= 0.05, "red", "blue")) +
      geom_line(y = 0.05, color = "green") + ylim(0, 0.5) +
      labs(title = "p - values of multiple tests", y = "p-value", x = "Simulation", caption = paste("There are", rp, "red points and", bp, "blue points")) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(plot.caption = element_text(size = 14))
  })
}

boastApp(ui = ui, server = server)
