library(shiny)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(jsonlite)
library(RCurl)
library(class)
library(httr)
library(caret)
library(e1071)
library(ggplot2)
library(magrittr)
library(plyr)
library(dplyr)
library(tm)
library(tidyr)
library(tidyverse)
library(maps)
library(mapproj)
library(stringr)
library(VIM)
library(mice)
library(forcats)
library(MASS)
library(GGally)


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


fluidPage(
  titlePanel("Brewery analysis"),
  
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(12,
           selectInput("state",
                       "State:",
                       c(
                         "All",
                         sort(unique(as.character(dataset$State)))
                       ))),
    
    column(12,
           selectInput(
             "var",
             "Variable Selection",
             c("All", "IBU", "ABV")
           )),
    
    column(
      12,
      radioButtons(
        "radio",
        label = "Plot Selection",
        choices = list(
          "Histogram" = 1,
          "Box Plot" = 2,
          "Scatter Plot" = 3
        ),
        selected = 1,
        inline = "TRUE"
      )
    ),
    
    
    column(
      12,
      selectInput(
        "regressionline",
        "Add Regression line to Scatter Plot",
        c("Yes", "No")
      )
    )
    
  ),
  # Create a new row for the table.
  
  mainPanel(# Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(
              type = "tab",
              tabPanel("Data", tableOutput("dataset")),
              tabPanel("Plot", plotOutput("plot"))
            ))
  
)
