library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

df <- readRDS("cleaned_graduate_survey.rds")

ui <- fluidPage(
  titlePanel("Eduvos Graduate Survey Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("study_field", "Select Study Field:", choices = unique(df$StudyField), selected = unique(df$StudyField)[1]),
      selectInput("campus", "Select Campus:", choices = unique(df$Campus), selected = unique(df$Campus)[1])
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Programming Languages", plotOutput("progPlot", height = "700px")),
        tabPanel("Databases", plotOutput("databasePlot", height = "700px")),
        tabPanel("AI Search Tools", plotOutput("aiSearchPlot", height = "700px")),
        tabPanel("AI Developer Tools", plotOutput("aiToolPlot", height = "700px")),
        tabPanel("Web Frameworks", plotOutput("webFrameworkPlot", height = "700px")),
        tabPanel("Platforms", plotOutput("platformPlot", height = "700px")),
        tabPanel("Industries", plotOutput("industryPlot", height = "700px")),
        tabPanel("Employment Rate", plotOutput("employmentPlot", height = "700px")),
        tabPanel("Job Roles", plotOutput("jobRolePlot", height = "700px"))
      )
    )
  )
)