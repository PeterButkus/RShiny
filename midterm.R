library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(treemapify)


# Load data
data <- read.csv("PLDefensedata.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Premier League Defensive Stats through week 33 2023"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select a team", choices = unique(data$Squad))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tackles Def", plotlyOutput("tackles_plotly")),
        tabPanel("Tackles Mid", plotlyOutput("challenges")),
        tabPanel("Tackles Att", plotlyOutput("tackleatt")),
        tabPanel("Compare Tackles Defensive 3rd", plotlyOutput("scatterdef")),
        tabPanel("Compare Tackles Middle 3rd", plotlyOutput("scatterchallanges")),
        tabPanel("Compare Tackles Attacking 3rd", plotlyOutput("scatteratt")),
        tabPanel("Tackle success rate", plotlyOutput("barplot")),
        tabPanel("Compare Tackles and Blocks", plotlyOutput("comparison")),
        tabPanel("Compare Blocks + interceptions", plotlyOutput("scatterblock")),
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter data by selected team
  team_data <- reactive({
    filter(data, Squad == input$team)
  })
  
  # Tackles plot
  output$tackles_plotly <- renderPlotly({
    ggplotly(
      ggplot(team_data(), aes(x = Def_3rd, y = Tkl, color = Squad)) +
        geom_point(size = 4) +
        labs(x = "Tackles in defensive third", y = "Total Tackles", title = "Total tackles vs tackles only in defensive third") +
        scale_color_manual(values = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694", "Crystal Palace" = "#1B458F",
                                      "Everton" = "#003399",
                                        "Leeds United" = "#FFCD00",
                                          "Leicester City" = "#0053A0",
                                            "Liverpool" = "#C8102E",
                                              "Manchester City" = "#6CABDD",
                                                "Manchester Utd" = "#DA291C",
                                                  "Newcastle Utd" = "#241F20",
                                                    "Norwich City" = "#00A650",
                                                      "Southampton" = "#D71920",
                                                        "Tottenham" = "#132257",
                                                          "Watford" = "#FBEE23",
                                                            "West Ham" = "#7A263A",
                                                              "Wolves" = "#FDB913",
                                      "Nottingham Forest" = "#D71920",
                                      "Fulham" = "#000000"
                                      )  ))
  })
  
  output$challenges <- renderPlotly({
    ggplotly(
      ggplot(team_data(), aes(x = Mid_3rd, y = Tkl, color = Squad)) +
        geom_point(size = 4) +
        labs(x = "Tackles in Middle 3rd", y = "Total Tackles", title = "Total tackles vs Tackles only in middle Third") +
        scale_color_manual(values = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694", "Crystal Palace" = "#1B458F",
                                      "Everton" = "#003399",
                                      "Leeds United" = "#FFCD00",
                                      "Leicester City" = "#0053A0",
                                      "Liverpool" = "#C8102E",
                                      "Manchester City" = "#6CABDD",
                                      "Manchester Utd" = "#DA291C",
                                      "Newcastle Utd" = "#241F20",
                                      "Norwich City" = "#00A650",
                                      "Southampton" = "#D71920",
                                      "Tottenham" = "#132257",
                                      "Watford" = "#FBEE23",
                                      "West Ham" = "#7A263A",
                                      "Wolves" = "#FDB913",
                                      "Nottingham Forest" = "#D71920",
                                      "Fulham" = "#000000"
        )  ))
  })
  

  output$tackleatt <- renderPlotly({
    ggplotly(
    ggplot(team_data(), aes(x = `Att_3rd`, y = Tkl, color = Squad)) +
      geom_point(size = 4) +
      labs(x = "Tackles in attacking 3rd", y = "Total Tackles", title = "Total tackles vs Tackles only in Attacking Third") +
      scale_color_manual(values = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694",
                                    "Crystal Palace" = "#1B458F",
                                    "Everton" = "#003399",
                                    "Leeds United" = "#FFCD00",
                                    "Leicester City" = "#0053A0",
                                    "Liverpool" = "#C8102E",
                                    "Manchester City" = "#6CABDD",
                                    "Manchester Utd" = "#DA291C",
                                    "Newcastle Utd" = "#241F20",
                                    "Norwich City" = "#00A650",
                                    "Southampton" = "#D71920",
                                    "Tottenham" = "#132257",
                                    "Watford" = "#FBEE23",
                                    "West Ham" = "#7A263A",
                                    "Wolves" = "#FDB913",  "Nottingham Forest" = "#D71920",
                                    "Fulham" = "#000000"             ))
    )
  })
  
  # Comparison plot
  output$comparison <- renderPlotly({
    # Create a summary data frame for all teams
    summary_data <- data %>% 
      group_by(Squad) %>% 
      summarize(Tkl = sum(Tkl), TklW = sum(TklW), Blocks = sum(Blocks)) %>%
      pivot_longer(cols = c("Tkl", "TklW", "Blocks"), names_to = "variable", values_to = "value")
    
    
    # Filter the summary data frame to exclude the selected team
    comparison_data <- summary_data %>% 
      filter(Squad != input$team)
    
    # Create a stacked bar chart to compare the selected team to other teams
    plot_ly(comparison_data, x = ~Squad, y = ~value, color = ~variable, type = "bar") %>%
      layout(xaxis = list(title = "Team"), yaxis = list(title = "Total"), title = "Comparison to Other Teams")
  })
  
  # Scatter plot
  output$scatterplot <- renderPlotly({
    # Scatter plot comparing tackles and tackles won for all teams
      plot_ly(data, x = ~Tkl, y = ~TklW, color = ~Squad, colors = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694","Crystal Palace" = "#1B458F", "Everton" = "#003399", "Leeds United" = "#FFCD00",
                                                                    "Leicester City" = "#0053A0",
                                                                    "Liverpool" = "#C8102E",
                                                                    "Manchester City" = "#6CABDD",
                                                                    "Manchester Utd" = "#DA291C",
                                                                    "Newcastle Utd" = "#241F20",
                                                                    "Norwich City" = "#00A650",
                                                                    "Southampton" = "#D71920",
                                                                    "Tottenham" = "#132257",
                                                                    "Watford" = "#FBEE23",
                                                                    "West Ham" = "#7A263A",
                                                                    "Wolves" = "#FDB913",
                                                                    "Nottingham Forest" = "#D71920",
                                                                    "Fulham" = "#000000" ), type = "scatter", mode = "markers") %>%
        layout(xaxis = list(title = "Tackles"), yaxis = list(title = "Tackles Won"), title = "Tackles vs. Tackles Won for All Teams")
    })
  # Scatter plot
  output$scatterdef <- renderPlotly({
    # Scatter plot comparing total tackles and tackles in middle third for all teams
    plot_ly(data, x = ~Def_3rd, y = ~Tkl, color = ~Squad, colors = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694", "Crystal Palace" = "#1B458F", "Everton" = "#003399", "Leeds United" = "#FFCD00", "Leicester City" = "#0053A0", "Liverpool" = "#C8102E", "Manchester City" = "#6CABDD", "Manchester Utd" = "#DA291C", "Newcastle Utd" = "#241F20", "Norwich City" = "#00A650", "Southampton" = "#D71920", "Tottenham" = "#132257", "Watford" = "#FBEE23", "West Ham" = "#7A263A", "Wolves" = "#FDB913",  "Nottingham Forest" = "#D71920",
                                                                      "Fulham" = "#000000"), type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = "Tackles in Defensive Third"), yaxis = list(title = "Total Tackles"), title = "Total Tackles vs. Tackles in defensive Third for All Teams")
  })
  
  # Scatter plot
  output$scatterchallanges <- renderPlotly({
    # Scatter plot comparing total tackles and tackles in middle third for all teams
    plot_ly(data, x = ~Mid_3rd, y = ~Tkl, color = ~Squad, colors = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694", "Crystal Palace" = "#1B458F", "Everton" = "#003399", "Leeds United" = "#FFCD00", "Leicester City" = "#0053A0", "Liverpool" = "#C8102E", "Manchester City" = "#6CABDD", "Manchester Utd" = "#DA291C", "Newcastle Utd" = "#241F20", "Norwich City" = "#00A650", "Southampton" = "#D71920", "Tottenham" = "#132257", "Watford" = "#FBEE23", "West Ham" = "#7A263A", "Wolves" = "#FDB913",  "Nottingham Forest" = "#D71920",
                                                                      "Fulham" = "#000000"), type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = "Tackles in Middle Third"), yaxis = list(title = "Total Tackles"), title = "Total Tackles vs. Tackles in Middle Third for All Teams")
  })
  # Scatter plot
  output$scatteratt <- renderPlotly({
    # Scatter plot comparing total tackles and tackles in middle third for all teams
    plot_ly(data, x = ~Att_3rd, y = ~Tkl, color = ~Squad, colors = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694", "Crystal Palace" = "#1B458F", "Everton" = "#003399", "Leeds United" = "#FFCD00", "Leicester City" = "#0053A0", "Liverpool" = "#C8102E", "Manchester City" = "#6CABDD", "Manchester Utd" = "#DA291C", "Newcastle Utd" = "#241F20", "Norwich City" = "#00A650", "Southampton" = "#D71920", "Tottenham" = "#132257", "Watford" = "#FBEE23", "West Ham" = "#7A263A", "Wolves" = "#FDB913",  "Nottingham Forest" = "#D71920",
                                                                      "Fulham" = "#000000"), type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = "Tackles in Attacking Third"), yaxis = list(title = "Total tackles"), title = "Total Tackles vs. Tackles in Attacking Third for All Teams")
  })
  
  # Scatter plot
  output$scatterblock <- renderPlotly({
    # Scatter plot comparing blocks and  in attacking third
    plot_ly(data, x = ~Blocks, y = ~Int, color = ~Squad, colors = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694", "Crystal Palace" = "#1B458F", "Everton" = "#003399", "Leeds United" = "#FFCD00", "Leicester City" = "#0053A0", "Liverpool" = "#C8102E", "Manchester City" = "#6CABDD", "Manchester Utd" = "#DA291C", "Newcastle Utd" = "#241F20", "Norwich City" = "#00A650", "Southampton" = "#D71920", "Tottenham" = "#132257", "Watford" = "#FBEE23", "West Ham" = "#7A263A", "Wolves" = "#FDB913",  "Nottingham Forest" = "#D71920",
                                                                      "Fulham" = "#000000"), type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = "Blocks"), yaxis = list(title = "Interceptions"), title = "Blocks vs. Interceptions for All Teams")
  })
  
  # Create a new factor variable with levels ordered by TklPercent
  data$Squad_ordered <- factor(data$Squad, levels = data$Squad[order(data$TklPercent, decreasing = TRUE)])
  
  # Create a bar graph of Tkl% by Squad_ordered
  output$barplot <- renderPlotly({
    plot_ly(data, x = ~Squad_ordered, y = ~TklPercent, color = ~Squad, colors = c("Arsenal" = "#EF0107", "Aston Villa" = "#95BFE5", "Bournemouth" = "#DA291C", "Brentford" = "#FFD700", "Brighton" = "#0057B8", "Chelsea" = "#034694", "Crystal Palace" = "#1B458F", "Everton" = "#003399", "Leeds United" = "#FFCD00", "Leicester City" = "#0053A0", "Liverpool" = "#C8102E", "Manchester City" = "#6CABDD", "Manchester Utd" = "#DA291C", "Newcastle Utd" = "#241F20", "Norwich City" = "#00A650", "Southampton" = "#D71920", "Tottenham" = "#132257", "Watford" = "#FBEE23", "West Ham" = "#7A263A", "Wolves" = "#FDB913",  "Nottingham Forest" = "#FF0000", "Fulham" = "#000000"), type = "bar") %>%
      layout(xaxis = list(title = "Team"), yaxis = list(title = "Tackles Won %"), title = "Tackles Won % by Team")
  })
  
#  output$bplot <- renderPlotly({
    # Create a summary data frame for all teams
 #     sum_data <- data %>% 
  #      group_by(Squad) %>% 
   #     summarize(Def_3rd = sum(Def_3rd), Mid_3rd = sum(Mid_3rd), Att_3rd = sum(Att_3rd), Tkl = Tkl) %>%
  #      pivot_longer(cols = c("Def_3rd", "Mid_3rd", "Att_3rd", "Tkl"), names_to = "variable", values_to = "value")
      
    
    # Filter the summary data frame to exclude the selected team
 #   comparison_data <- sum_data %>% 
 #     filter(Squad != input$team)
    
    # Create a stacked bar chart to compare the selected team to other teams
#    plot_ly(comparison_data, x = ~Squad, y = ~value, color = ~variable, type = "bar") %>%
 #     layout(xaxis = list(title = "Team"), yaxis = list(title = "Total"), title = "Comparison to Other Teams", barmode = "stack") %>%
#      add_trace(data = sum_data, x = ~Squad, y = ~Tkl, name = "Total Tackles", type = "scatter", mode = "lines", line = list(color = "black", width = 2), marker = list(color = "black"))
 # })
  

}

# Run app
shinyApp(ui, server)
