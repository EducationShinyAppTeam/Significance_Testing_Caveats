library(shiny)
library(ggplot2)


shinyServer(function(input, output, session) {
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "pages", "mtc")
  })
  #Pre-req Button
  observeEvent(input$prereqs, {
    updateTabItems(session, "pages", "prereq")
  })
  #Explore1 Button
  observeEvent(input$explore1, {
    updateTabItems(session, "pages", "mtc")
  })
  
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
  
  #Plot for the Multiple Testing Caveat
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
    ggplot(data.frame(x1, r), aes(x = x1, y = r)) +
      geom_point(
        color = ifelse(r <= a1, "#F2665E", "#1E407C"),
        shape = 19,
        size = 3
      ) +
      geom_line(y = a1,
                color = "forestgreen",
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
      ) +
      #scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = expansion(mult = 0, add = c(0,0.05)),limits = c(0,1))
  })
  
  #Plot for the Large Sample Caveat
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
    ggplot(data.frame(x1, ps2), aes(x = x1, y = ps2)) +
      geom_point(
        color = ifelse(ps2 <= a2, "#F2665E", "#1E407C"),
        shape = 19,
        size = 3
      ) +
      geom_line(y = a2,
                color = "forestgreen",
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
  
  #Plot for the Small Sample Size Caveat
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
    ggplot(data.frame(x1, ps3), aes(x = x1, y = ps3)) +
      geom_point(
        color = ifelse(ps3 <= a3, "#F2665E", "#1E407C"),
        shape = 19,
        size = 3
      ) +
      geom_line(y = a3,
                color = "forestgreen",
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
})