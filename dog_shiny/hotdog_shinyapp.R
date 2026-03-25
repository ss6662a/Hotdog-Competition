

library(shiny)
library(tidyverse)
library(readxl)
library(DT)


dog <- read_excel("../data/hotdog_tracker.xlsx") %>% 
  janitor::clean_names()

dog_theme <- function() {
  theme_classic() +
    theme(
      panel.background = element_rect(fill = "lightyellow"),
      plot.background  = element_rect(fill = "lightyellow"),
      text             = element_text(family = "Comic Sans MS", color = "red"),
      axis.text        = element_text(color = "red"),
      axis.line        = element_line(color = "red"),
      plot.title       = element_text(size = 18, face = "bold"),
      legend.background = element_rect(fill = "yellow")
    )
}

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
      margin-top: 25vh !important;
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
    /* inactive tab text */
    .nav-tabs > li > a {
       color: red !important;
       font-family: 'Comic Sans MS', cursive;
       }

    /* active tab text */
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      color: red !important;
    }
    /* wraps the entire DataTable */
    .dataTables_wrapper {
      background-color: rgba(255, 255, 0, 0.6) !important;
      border: 3px dashed red !important;
      padding: 15px;
      border-radius: 4px;
    }

    
  ")),

  titlePanel("Hotdog Competition 2026"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "name",
                         label = "Select Person(s)",
                         choices = unique(dog$person),
                         selected = unique(dog$person)
                         ),
      
      conditionalPanel(
        condition = "input.tabs == 'Dog Time'",
        radioButtons(inputId = "time_plt_switch",
                     label = "Density or Radar Plot?",
                     choices = c("Density", "Radar"),
                     selected = "Density",
                     inline = TRUE)
      )

      
    ),
    
    mainPanel(
      
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Cumulative Dogs",
                 plotOutput(outputId = "cum_plt")
        ),
        tabPanel("Dog Type",
                 plotOutput(outputId = "type_plt")
        ),
        tabPanel("Dog Time",
                 plotOutput(outputId = "time_plt")
        ),
        tabPanel("Dog Data",
                 DTOutput(outputId = "dog_data")
                 )
        
      )
      
    )
  )
)

server <- function(input, output, session) {
 
  
  filtered <- reactive({
    
    if (length(input$name) == 0) {
      dog
      } else {
      df <- dog %>% filter(person %in% input$name)
      }
    
  })
  
  output$cum_plt <- renderPlot({
    
    df <- filtered() %>% 
      group_by(person) %>% 
      mutate(cum = cumsum(number_of_dogs))

    plt <- ggplot(df, aes(x = date, y = cum, color = person)) + 
      geom_line(size = 1) +
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
      dog_theme()
    
    plt
    
  })
  
  
  output$type_plt <- renderPlot({
    
    df <- filtered() %>%
      select(person, number_of_dogs, 6:15) %>%
      pivot_longer(cols = -c(person, number_of_dogs), 
                   names_to = "type", values_to = "flag") %>%
      filter(flag == 1) %>%
      group_by(person, type) %>%
      summarise(total = sum(number_of_dogs), .groups = "drop") %>%
      group_by(person) %>%
      mutate(
        person_total = sum(total),
        pct = total / person_total,
        label = ifelse(pct > 0.10, paste0(round(pct * 100), "%"), NA)  # only label >10%
      ) %>%
      ungroup() %>%
      mutate(type = str_replace_all(type, "_", " ") %>% str_to_title())

    
    plt <- ggplot(df, aes(x = reorder(person, total), y = total, fill = type)) +
      geom_bar(stat = "identity", color = "red") +
      labs(x = NULL, 
           y = "Total Dogs",
           fill = "Type",
           title = "Dog Type Breakdown by Person") +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),  # centers label in each segment
        color = "black",
        fontface = "bold",
        size = 4,
        na.rm = TRUE  # silently skips the NA labels (≤10%)
      ) +
      dog_theme()
    
    plt
    
  })
  
  output$time_plt <- renderPlot({
    
    
    if (input$time_plt_switch == "Density") {
      df <- filtered() %>%
        mutate(hour = as.numeric(format(strptime(time_et, "%I:%M %p"), "%H")) +
                 as.numeric(format(strptime(time_et, "%I:%M %p"), "%M")) / 60)
          
      
      plt <- ggplot(df, aes(x = hour, color = person, fill = person)) +
        geom_density(alpha = 0.3, linewidth = 1.2) +
        scale_x_continuous(
          breaks = seq(0, 23, by = 2),
          labels = format(strptime(paste0(seq(0, 23, by = 2), ":00"), "%H:%M"), "%I %p")
          ) +
        labs(x = "Time of Day", 
             y = "Density",
             title = "Time of Dog Consumption") +
        dog_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
      } else if (input$time_plt_switch == "Radar") {
      
        df <- filtered() %>%
          mutate(hour = as.integer(format(strptime(time_et, "%I:%M %p"), "%H"))) %>%
          group_by(person, hour) %>%
          summarise(total = sum(number_of_dogs), .groups = "drop")
        
        plt <- ggplot(df, aes(x = hour, y = person, fill = total)) +
          geom_tile(color = "white", linewidth = 0.8) +
          scale_fill_gradient(low = "yellow", high = "red") +
          scale_x_continuous(breaks = 0:23,
                             labels = format(strptime(paste0(0:23, ":00"), "%H:%M"), "%I %p")) +
          labs(x = "Hour of Day", y = NULL, fill = "Total Dogs",
               title = "Time of Dog Consumption") +
          dog_theme() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        }
  
    plt
    
  })
  
  output$dog_data <- renderDT({
    
    filtered() %>%
      select(-other_notes) %>%
      datatable(options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle(
        columns = names(dog %>% select(-other_notes)),
        color = "red",
        fontFamily = "Comic Sans MS"
      )
    
  })
   
}

shinyApp(ui, server)



