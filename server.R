library(shiny)
require(plotrix)
library(ggplot2)

D <- 5000
set.seed(110)

genData <- function(intercept, slope, n, xDist, sd){
  if(xDist == "Uniform"){
    xseq <- rbeta(n, shape1 = 1, shape2 = 1)
  } else if(xDist == "Left-Skewed"){
    xseq <- rbeta(n, shape1 = 6, shape2 = 1)
  } else if(xDist == "Right-Skewed"){
    xseq <- rbeta(n, shape1 = 1, shape2 = 6)
  } else if(xDist == "Bell-Shaped"){
    xseq <- rbeta(n, shape1 = 16, shape2 = 16)
  }
  data.frame(ys = intercept + slope * xseq +rnorm(n, mean = 0, sd = sd), xs = xseq)
}

# Define server logic required to draw the plots
shinyServer(function(input, output, session) {

  
  #number of data set we are one
  v <- reactiveValues(valueButton = 0)
  
  observeEvent(input$newData, {
    v$valueButton <- v$valueButton + 1
    if(v$valueButton > D) {
      v$valueButton <- D
    }
  })
  
  observeEvent(input$newData10, {
    v$valueButton <- v$valueButton + 10
    if(v$valueButton > D) {
      v$valueButton <- D
    }
  })
  
  observeEvent(input$newData100, {
    v$valueButton <- v$valueButton + 100
    if(v$valueButton > D) {
      v$valueButton <- D
    }
  })
  
  observeEvent(input$newData1000, {
    v$valueButton <- v$valueButton + 1000
    if(v$valueButton > D) {
      v$valueButton <- D
    }
  })
  
  observeEvent(input$reset | input$Param1 | input$Param2 | input$sd | input$sampleSize, {
    v$valueButton <- 0
  })
  

  simData <- reactive({

    #sample size
    n <- input$sampleSize
    intercept <- input$Param1
    slope <- input$Param2
    sd <- input$sd
    
    #get preds
    xDist <- "Uniform"
    samples <- replicate(D, genData(intercept, slope, n, xDist, sd))
    samples
    
  })
  
  fits <- reactive({
    samples <- simData()
    
    #create fits
    fits <- apply(X = samples, MARGIN = 2, FUN = function(i){
      lm(i$`y` ~ i$`x`)$coef
    })
    
    #each column is one fit
    fits
  })
  


  output$interceptSummaryStats <- renderTable({
    if(v$valueButton == 0){
      sumstat <- data.frame(
        Name = c("Min", "Q1", "Median", "Q3", "Max", "Mean", "SD"),
        Value = rep(NA, 7),
        stringsAsFactors = FALSE)
    } else {
      #Get data
      fits <- fits()    
  
      #just the last row of the data set
      fits <- fits[1, 1:v$valueButton, drop = FALSE]
      #summary stats
      sumstat <- data.frame(
        Name = c("Min", "Q1", "Median", "Q3", "Max", "Mean", "SD"),
        Value = as.character(c(round(quantile(fits, c(0, 0.25, 0.5, 0.75, 1)), 3), round(mean(fits), 3), round(sd(fits), 3))),
        stringsAsFactors = FALSE)
    }
  })


  output$slopeSummaryStats <- renderTable({
    if(v$valueButton == 0){
      sumstat <- data.frame(
        Name = c("Min", "Q1", "Median", "Q3", "Max", "Mean", "SD"),
        Value = rep(NA, 7),
        stringsAsFactors = FALSE)
    } else {
      #Get data
      fits <- fits()    
      
      #just the last row of the data set
      fits <- fits[2, 1:v$valueButton, drop = FALSE]
      #summary stats
      sumstat <- data.frame(
        Name = c("Min", "Q1", "Median", "Q3", "Max", "Mean", "SD"),
        Value = as.character(c(round(quantile(fits, c(0, 0.25, 0.5, 0.75, 1)), 3), round(mean(fits), 3), round(sd(fits), 3))),
        stringsAsFactors = FALSE)
    }
  })
  
    output$interceptHist<-renderPlot({
      if(v$valueButton == 0){
        NULL
      } else {
        #Get data
        fits <- fits()
        fits <- fits[1, 1:v$valueButton]
        #plot
        hist(fits, main = "Histogram of Sample Intercepts")
      }
    })  
    
    output$slopeHist<-renderPlot({
      if(v$valueButton == 0){
        NULL
      } else {
        #Get data
        fits <- fits()
        fits <- fits[2, 1:v$valueButton]
        #plot
        hist(fits, main = "Histogram of Sample Slopes")
      }
    })     

  output$fitPlots<-renderPlot({
    #Get data
    samples <- simData()    
    fits <- fits()
    it <- v$valueButton
    #put down base plot 
    intercept <- input$Param1
    slope <- input$Param2
    sd <- input$sd
    n <- input$sampleSize
    
    baseX <- seq(from = 0, to = 1, length = 100)
    trueLine <- data.frame(x = baseX, y = intercept + slope * baseX)
    #g <- ggplot(data = as.data.frame(samples[, it]), aes(x = xs, y = ys)) + coord_cartesian(xlim = c(0, 1), ylim = c(min(trueLine$y) - 3*sd, max(trueLine$y) + 3*sd)) + geom_abline(intercept, slope)
    g <- ggplot(trueLine, aes(x = x, y = y)) +  coord_cartesian(ylim = c(min(trueLine$y) - 2*sd*sqrt(1+1/n+0.08/sd(seq(from = 0, to = 1, by = 1/n))), max(trueLine$y) + 2*sd*sqrt(1+1/n+0.08/sd(seq(from = 0, to = 1, by = 1/n)))))

    #add points and line corresponding to current data set
    if(it > 0){
      i <- 1
      while(i < it & i < 150) {
        curLine <- data.frame(x = baseX, y = baseX*fits[,i][2] + fits[,i][1])
        g <- g + geom_line(data = curLine, aes(x = x, y = y), col = "Grey", lwd = 1) 
        i <- i + 1
      }
      #add baseX for plotting fits and add true line on top so it isn't covered by other lines
      curLine <- data.frame(x = baseX, y = baseX*fits[,it][2] + fits[,it][1])
      g <- g + geom_line(lwd = 2) + geom_point(data = as.data.frame(samples[, it]), aes(x = xs, y = ys), size = 2) + geom_line(data = curLine, aes(x = x, y = y), col = "Blue", lwd = 1.25) 
      g
    }
  })

  
})