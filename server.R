library(shiny)
library(ggplot2)

function(input, output) {
  Beers <- read.csv("Beers.csv", header = TRUE, strip.white = TRUE)
  Beers <- as.data.frame(Beers)
  Breweries <-
    read.csv("Breweries.csv", header = TRUE, strip.white = TRUE)
  Breweries <- as.data.frame(Breweries)
  colnames(Beers)[5] <- "Brew_ID"
  
  # Merge the datasets on the Brewery Id Column ---
  MergeData <- merge(Breweries, Beers, by = "Brew_ID", all = TRUE)
  
  # Change the column names of teh new dataset ---
  colnames(MergeData)[2] <- "Brewery"
  colnames(MergeData)[5] <- "Beer_Name"
  tempData <- mice(
    MergeData,
    m = 1,
    maxit = 0,
    meth = 'fastpmm',
    seed = 500
  )
  dataset <- complete(tempData, 1)
  
  output$dataset <- renderTable({
    if (input$state != "All") {
      dataset <- dataset[dataset$State == input$state, ]
    } else {
      dataset  
    }
    
  })
  
  output$plot <- renderPlot({
    if (input$state != "All") {
      dataset <- dataset[dataset$State == input$state, ]
    }
    
    if (input$radio == "1") {
      if (input$var == "ABV") {
        hist(dataset$ABV, ylab = "Count", xlab="Alcohol Content", col = "#75AADB", border = "white", main = "ABV Histogram")
      }
      if (input$var == "IBU") {
        hist(dataset$IBU, ylab = "Count", xlab="Bitternes Unit", col = "#75AADB", border = "white",  main = "IBU Histogram")
      }
      
    }
    
    if (input$radio == "2") {
      if (input$var == "ABV") {
        boxplot(dataset$ABV, ylab = "ABV",col = "#75AADB", main = "ABV Box plot")
      }
      if (input$var == "IBU") {
        boxplot(dataset$IBU, ylab = "IBU",col = "#75AADB", main = "IBU Box plot")
      }
      
    }
    
    if (input$radio == "3") {
      ## plot(dataset$IBU,dataset$ABV)
      
      if (input$regressionline == "Yes") {
        ggplot(dataset, aes(x = dataset$ABV, y = dataset$IBU )) +
          geom_point(position = "jitter", col = "#75AADB") + ylab("IBU") + xlab("ABV") + ggtitle("Relationship Between Alcohol content and Bitterness") +
          xlim(c(0.02, 0.1)) +
          theme_bw() + geom_smooth(method = lm, se = FALSE, col="red")
      }
      else {
        ggplot(dataset, aes(x = dataset$ABV, y = dataset$IBU)) +
          geom_point(position = "jitter", col = "#75AADB") + ylab("IBU") + xlab("ABV") +
          ggtitle("Relationship Between Alcohol content and Bitterness") +
          xlim(c(0.02, 0.1)) +
          theme_bw()
      }
      
    }
    
  })
}
