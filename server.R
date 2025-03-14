library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

df <- readRDS("cleaned_graduate_survey.rds")

server <- function(input, output) {
  
  # Reactive to filter data
  filtered_data <- reactive({ 
    df |>
      filter(StudyField == input$study_field, Campus == input$campus)
  })
  
  # Custom theme for plots
  custom_theme <- theme_minimal() +
    theme(
      text = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  # Programming Languages Plot
  output$progPlot <- renderPlot({
    prog_langs <- filtered_data() |>
      separate_rows(ProgLang, sep = ";") |>
      count(ProgLang, sort = TRUE)
    
    ggplot(prog_langs, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
      geom_bar(stat = "identity", width = 0.8) +
      coord_flip() +
      custom_theme +
      labs(title = "Most Popular Programming Languages", x = "Programming Language", y = "Count") +
      theme(axis.text.y = element_text(size = 12, margin = margin(r = 10)))
  })
  
  # Databases Plot
  output$databasePlot <- renderPlot({
    databases <- filtered_data() |>
      separate_rows(Databases, sep = ";") |>
      count(Databases, sort = TRUE)
    
    ggplot(databases, aes(x = reorder(Databases, n), y = n, fill = Databases)) +
      geom_bar(stat = "identity", width = 0.8) +
      coord_flip() +
      custom_theme +
      labs(title = "Top Databases", x = "Database", y = "Count")
  })
  
  # AI Search Tools Plot
  output$aiSearchPlot <- renderPlot({
    ai_search <- filtered_data() |>
      separate_rows(AISearch, sep = ";") |>
      count(AISearch, sort = TRUE)
    
    ggplot(ai_search, aes(x = reorder(AISearch, n), y = n, fill = AISearch)) +
      geom_bar(stat = "identity", width = 0.8) +
      coord_flip() +
      custom_theme +
      labs(title = "Top AI Search Tools", x = "AI Search Tool", y = "Count")
  })
  
  # AI Developer Tools Plot
  output$aiToolPlot <- renderPlot({
    ai_tool <- filtered_data() |>
      separate_rows(AITool, sep = ";") |>
      count(AITool, sort = TRUE)
    
    ggplot(ai_tool, aes(x = reorder(AITool, n), y = n, fill = AITool)) +
      geom_bar(stat = "identity", width = 0.8) +
      coord_flip() +
      custom_theme +
      labs(title = "Top AI Developer Tools", x = "AI Developer Tool", y = "Count")
  })
  
  # Web Frameworks Plot
  output$webFrameworkPlot <- renderPlot({
    web_framework <- filtered_data() |>
      separate_rows(WebFramework, sep = ";") |>
      count(WebFramework, sort = TRUE)
    
    ggplot(web_framework, aes(x = reorder(WebFramework, n), y = n, fill = WebFramework)) +
      geom_bar(stat = "identity", width = 0.8) +
      coord_flip() +
      custom_theme +
      labs(title = "Top Web Frameworks", x = "Web Framework", y = "Count") +
      theme(axis.text.y = element_text(size = 12, margin = margin(r = 10)))
  })
  
  # Platforms Plot
  output$platformPlot <- renderPlot({
    platform <- filtered_data() |>
      separate_rows(Platform, sep = ";") |>
      count(Platform, sort = TRUE)
    
    ggplot(platform, aes(x = reorder(Platform, n), y = n, fill = Platform)) +
      geom_bar(stat = "identity", width = 0.8) +
      coord_flip() +
      custom_theme +
      labs(title = "Top Platforms", x = "Platform", y = "Count")
  })
  
  # Industries Plot
  output$industryPlot <- renderPlot({
    industries <- filtered_data() |>
      separate_rows(Industry, sep = ";") |>
      count(Industry, sort = TRUE)
    
    ggplot(industries, aes(x = reorder(Industry, n), y = n, fill = Industry)) +
      geom_bar(stat = "identity", width = 0.8) +
      coord_flip() +
      custom_theme +
      labs(title = "Top Industries", x = "Industry", y = "Count")
  })
  
  # Employment Rate Plot
  output$employmentPlot <- renderPlot({
    employment_rate <- filtered_data() |>
      separate_rows(Employment, sep = ";") |>
      mutate(Employment = str_trim(Employment)) |>
      count(StudyField, Employment) |>
      group_by(StudyField) |>
      mutate(total = sum(n)) |>
      filter(Employment %in% c("Employed, full-time", "Not employed, but looking for work")) |>
      mutate(employment_rate = n / total * 100)
    
    ggplot(employment_rate, aes(x = StudyField, y = employment_rate, fill = Employment)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.8) +
      custom_theme +
      labs(title = "Employment Rate", x = "Study Field", y = "Employment Rate (%)")
  })
  
  # Job Roles Plot
  output$jobRolePlot <- renderPlot({
    job_roles <- filtered_data() |>
      count(StudyField, Role) |>
      arrange(StudyField, desc(n))
    
    # Plotting
    ggplot(job_roles, aes(x = reorder(Role, n), y = n, fill = StudyField)) +
      geom_bar(stat = "identity", width = 0.8) +
      coord_flip() +
      facet_wrap(~ StudyField, scales = "free_y") +
      custom_theme +
      labs(title = "Top Job Roles by Study Fields", x = "Job Role", y = "Count")
  })
}