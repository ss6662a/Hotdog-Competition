

library(shiny)
library(tidyverse)
library(readxl)
library(bslib)


dog <- read_excel("../data/hotdog_tracker.xlsx") %>% 
  janitor::clean_names()



ui <- fluidPage(
  
  tags$style(HTML("
    body {
      background-image: url('hotdog_background.jpg');
      background-size: cover;
      background-attachment: fixed;
      background-repeat: repeat;
      font-family: 'Comic Sans MS', cursive;
    }
    
    .container-fluid {
      margin-top: 30vh !important;
      background-color: transparent !important;
    }
    .well {
      background-color: rgba(255, 255, 0, 0.6) !important;
      border: 3px dashed red !important;
    }
    .container-fluid {
      background-color: transparent !important;
    }
    
    h2 {
      color: red;
      text-shadow: 3px 3px yellow;
      font-size: 48px !important;
    }
  ")),

  titlePanel("Hotdog Competition 2026"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "name",
                         label = "Select Person(s)",
                         choices = unique(dog$person),
                         selected = unique(dog$person))
      
    ),
    
    mainPanel(
      plotOutput(outputId = "cum_plt")
    )
  )
)

server <- function(input, output, session) {
 
  output$cum_plt <- renderPlot({
    
    if (length(input$name) == 0) {
      
      df <- dog
      
    } else {
      
      df <- dog %>% filter(person %in% input$name)

    }
      
    df <- df %>% 
      group_by(person) %>% 
      mutate(cum = cumsum(number_of_dogs))
    
    

    plt <- ggplot(df, aes(x = date, y = cum, 
                            color = person))
    
    plt <- plt + geom_line(size = 1) +
      labs(
        x = "Date",
        y = "Number of cummulative hot dogs",
        title = paste0(
          paste(input$name, collapse = ", "),
          if_else(length(input$name) == 1, "'s", 
                  if_else(length(input$name) == 0, "Everyone's", "'s")),
          " 2026 Hot Dogs (so far)"
        )
      ) +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "lightyellow"),
        plot.background = element_rect(fill = "lightyellow"),
        text = element_text(family = "Comic Sans MS", color = "red"),
        axis.text = element_text(color = "red"),
        axis.line = element_line(color = "red"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.background = element_rect(fill = "yellow")
        )
    
    plt # display built plot
    
  })
   
}

shinyApp(ui, server)



