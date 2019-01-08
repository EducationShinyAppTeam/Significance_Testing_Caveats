#Multiple Testing Caution

library(shiny)
library(ggplot2)

shinyServer(function(input, output,session) {
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "mtc")
  })
  nMTC <-reactive({
    return(input$days)
  })
  
  nLCV <- reactive({
    return(input$sizeForLargeCaution)
  })
  
  nSCV <- reactive({
    return(input$sizeForSmallCaution)
  })
  
  sample.size <-reactive({
    return(input$samp.size)
  })
  
  #I think this way is better
  #Second thought on simulating this
  #Binomial might not be the best
  #Idea is to randomly generate x number of data points (days)
  #       and each day has 5% chance of having a small pvalue
  
  #Plot for the first idea
  output$pplotMTC<-renderPlot({
    validate(
      need(input$days>0,
           message = "Please input a valid number of tests")
    )
    n2 = nMTC()
    x1 = 1:n2
    bp = 0
    rp = 0
    r = numeric(n2)
    sim1 = rbinom(n = n2,size = 1, prob = 0.95)
    for(w in 1:n2){
      if(sim1[w]==1){
        r[w] = runif(1, 0.05001,0.999)
        bp = bp + 1 
      }
      else{
        r[w] = runif(1, 0.0001, 0.05)
        rp = rp + 1
      }
    }
    r = r
    #plot(r,ylim = c(0,1), ylab = "p-value",main = "p-values of multiple tests", xlab = paste("There are", bp, "blue points and",rp,"red points" ), xlim = c(0,500),col=ifelse(r<=0.05, "red", "blue"))
    #abline(h = 0.05, col = "green", lwd = 2)
    #ggplot(data.frame(x1,r),aes(x=x1, y=r))+geom_point(color=ifelse(r<=0.05, "red", "blue"))+ geom_line(y = 0.05, color = "green") + ylim(0,1) + xlab("Test") + ylab("p-value") + ggtitle("p- values of multiple tests")
    ggplot(data.frame(x1,r),aes(x=x1, y=r))+geom_point(color=ifelse(r<=0.05, "red", "blue"))+ 
      geom_line(y = 0.05, color = "green") + ylim(0,1) +
      labs(title = "p - values of multiple tests",y = "p-value", x = "Test", caption = paste("There are", bp, "blue points and",rp,"red points"))+
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      theme(plot.caption = element_text(size=14)) 
  })
  
  output$mtcQ1check <- renderText({
    if(input$mtcQ1 == 5){
      paste("Correct!")
    }
  })
  
  output$mtcQ2check <- renderText({
    if(input$mtcQ2 == 90){
      paste("Correct!")
    }
  })
  
  output$LargeCautionPlot <- renderPlot({
    validate(
      need(input$sizeForLargeCaution>0,
           message = "Please input a valid sample size")
    )
    
    meanCV= 100
    sdCV = 15
    
    n3 = nLCV()
    
    seCV = sdCV/sqrt(n3)
    
    sims1 = 100
    
    vectorOfArrivalTimes = numeric(sims1)
    vectorOfZ = numeric(sims1)
    vectorOfPvalues = numeric(sims1)
    
    for(i in 1:sims1){
      vectorOfArrivalTimes[i] = rnorm(1, mean = meanCV, sd = 2)
    }
    
    for(j in 1:sims1){
      vectorOfZ[j] = (vectorOfArrivalTimes[j] - meanCV)/seCV
    }
    
    for(k in 1:sims1){
      if(vectorOfZ[k]<0){
        vectorOfPvalues[k] = pnorm(vectorOfZ[k])
      }
      else{
        vectorOfPvalues[k] = pnorm(vectorOfZ[k], lower.tail = FALSE)
      }
    }
    
    bp = 0
    rp = 0
    r = vectorOfPvalues
    x1 = 1:sims1
    for(m in 1:sims1){
      if(vectorOfPvalues[m]>0.05){
        bp = bp + 1 
      }
      else{
        rp = rp + 1
      }
    }
    
    
    ggplot(data.frame(x1,r),aes(x=x1, y=r))+ geom_point(color=ifelse(r<=0.05, "red", "blue"))+ 
      geom_line(y = 0.05, color = "green") + ylim(0,0.5) +
      labs(title = "p - values of multiple tests",y = "p-value", x = "Simulation", caption = paste("There are", bp, "blue points and",rp,"red points"))+
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      theme(plot.caption = element_text(size=14)) 
    
    
  })
  
  output$SmallCautionPlot <- renderPlot({
    validate(
      need(input$sizeForSmallCaution>0,
           message = "Please input a valid sample size")
    )
    
    meanCV= 100
    sdCV = 15
    
    n3 = nSCV()
    
    seCV = sdCV/sqrt(n3)
    
    sims1 = 100
    
    vectorOfArrivalTimes = numeric(sims1)
    vectorOfZ = numeric(sims1)
    vectorOfPvalues = numeric(sims1)
    
    for(i in 1:sims1){
      vectorOfArrivalTimes[i] = rnorm(1, mean = meanCV, sd = 10)
    }
    
    for(j in 1:sims1){
      vectorOfZ[j] = (vectorOfArrivalTimes[j] - meanCV)/seCV
    }
    
    for(k in 1:sims1){
      if(vectorOfZ[k]<0){
        vectorOfPvalues[k] = pnorm(vectorOfZ[k])
      }
      else{
        vectorOfPvalues[k] = pnorm(vectorOfZ[k], lower.tail = FALSE)
      }
    }
    
    bp = 0
    rp = 0
    r = vectorOfPvalues
    x1 = 1:sims1
    for(m in 1:sims1){
      if(vectorOfPvalues[m]>0.05){
        bp = bp + 1 
      }
      else{
        rp = rp + 1
      }
    }
    
    
    ggplot(data.frame(x1,r),aes(x=x1, y=r))+geom_point(color=ifelse(r<=0.05, "red", "blue"))+ 
      geom_line(y = 0.05, color = "green") + ylim(0,0.5) +
      labs(title = "p - values of multiple tests",y = "p-value", x = "Simulation", caption = paste("There are", bp, "blue points and",rp,"red points"))+
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      theme(plot.caption = element_text(size=14)) 
    
    
  })
  
  
  
})

