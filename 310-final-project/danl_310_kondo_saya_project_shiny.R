library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)

df <- read_excel("410-df_panel_selected_90.xlsx") %>%
  mutate(
    year = as.character(year),
    total_text_language =
      kw_pollution_ecosystem_lag +
      kw_climate_emissions_lag +
      kw_action_investment_lag +
      kw_sustainability_core_lag +
      kw_governance_reporting_lag +
      kw_energy_resources_lag
  )

keyword_choices <- c(
  "Pollution / Ecosystem" = "kw_pollution_ecosystem_lag",
  "Climate / Emissions" = "kw_climate_emissions_lag",
  "Action / Investment" = "kw_action_investment_lag",
  "Sustainability Core" = "kw_sustainability_core_lag",
  "Governance / Reporting" = "kw_governance_reporting_lag",
  "Energy / Resources" = "kw_energy_resources_lag",
  "Total Sustainability Language" = "total_text_language"
)

ui <- fluidPage(
  
  titlePanel("Sustainability Talk vs. ESG Performance"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        inputId = "sector",
        label = "Choose sector:",
        choices = c("All", sort(unique(df$GICSSector))),
        selected = "All"
      ),
      
      selectInput(
        inputId = "year",
        label = "Choose year:",
        choices = c("All", sort(unique(df$year))),
        selected = "All"
      ),
      
      selectInput(
        inputId = "keyword",
        label = "Choose sustainability language variable:",
        choices = keyword_choices,
        selected = "total_text_language"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Overview",
          h3("Project Question"),
          p("Do companies that use more sustainability-related language also show stronger environmental performance?"),
          
          h3("Why This Matters"),
          p("This app explores the gap between sustainability communication and ESG performance. If a company talks a lot about sustainability but has a low environmental score, that may suggest a possible greenwashing concern.")
        ),
        
        tabPanel(
          "Summary",
          h3("Descriptive Statistics"),
          DTOutput("summary_table"),
          
          h3("Average Environmental Score by Sector"),
          plotlyOutput("sector_plot")
        ),
        
        tabPanel(
          "Talk vs. Walk",
          h3("Sustainability Language vs. Environmental Score"),
          plotlyOutput("scatter_plot")
        ),
        
        tabPanel(
          "Keyword Comparison",
          h3("Average Sustainability Language by Sector"),
          plotlyOutput("keyword_plot")
        ),
        
        tabPanel(
          "Data",
          h3("Filtered Dataset"),
          DTOutput("data_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_df <- reactive({
    data <- df
    
    if (input$sector != "All") {
      data <- data %>%
        filter(GICSSector == input$sector)
    }
    
    if (input$year != "All") {
      data <- data %>%
        filter(year == input$year)
    }
    
    data
  })
  
  output$summary_table <- renderDT({
    filtered_df() %>%
      summarise(
        observations = n(),
        firms = n_distinct(Ticker),
        average_environmental_score = mean(e_score, na.rm = TRUE),
        median_environmental_score = median(e_score, na.rm = TRUE),
        average_social_score = mean(s_score, na.rm = TRUE),
        average_governance_score = mean(g_score, na.rm = TRUE),
        average_total_esg_score = mean(total_score, na.rm = TRUE),
        average_sustainability_language = mean(total_text_language, na.rm = TRUE)
      ) %>%
      mutate(
        across(where(is.numeric), ~ round(.x, 3))
      ) %>%
      datatable(rownames = FALSE)
  })
  
  output$sector_plot <- renderPlotly({
    plot_data <- filtered_df() %>%
      group_by(GICSSector) %>%
      summarise(
        average_e_score = mean(e_score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(average_e_score)
    
    p <- ggplot(
      plot_data,
      aes(
        x = average_e_score,
        y = reorder(GICSSector, average_e_score),
        text = paste(
          "Sector:", GICSSector,
          "<br>Average Environmental Score:", round(average_e_score, 2)
        )
      )
    ) +
      geom_col() +
      labs(
        x = "Average Environmental Score",
        y = NULL,
        title = "Average Environmental Score by Sector"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$scatter_plot <- renderPlotly({
    p <- ggplot(
      filtered_df(),
      aes(
        x = .data[[input$keyword]],
        y = e_score,
        color = GICSSector,
        text = paste(
          "Ticker:", Ticker,
          "<br>Year:", year,
          "<br>Sector:", GICSSector,
          "<br>Environmental Score:", round(e_score, 2),
          "<br>Language Value:", round(.data[[input$keyword]], 4)
        )
      )
    ) +
      geom_point(size = 3, alpha = 0.75) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(
        x = "Sustainability Language",
        y = "Environmental Score",
        title = "Sustainability Language vs. Environmental Score"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$keyword_plot <- renderPlotly({
    plot_data <- filtered_df() %>%
      group_by(GICSSector) %>%
      summarise(
        average_keyword = mean(.data[[input$keyword]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(average_keyword)
    
    p <- ggplot(
      plot_data,
      aes(
        x = average_keyword,
        y = reorder(GICSSector, average_keyword),
        text = paste(
          "Sector:", GICSSector,
          "<br>Average language value:", round(average_keyword, 4)
        )
      )
    ) +
      geom_col() +
      labs(
        x = "Average Sustainability Language",
        y = NULL,
        title = "Average Sustainability Language by Sector"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$data_table <- renderDT({
    filtered_df() %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
}

shinyApp(ui, server)




