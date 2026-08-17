#
# SDS 313 Project 3
# Ryan Chin
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(rlang)
library(bslib)

#reads in dataset
#This data was originally gathered by web-scraping the public industry tracking website vgchartz.com. 
#The specific, clean file for this project is sourced from the [Maven Analytics Data Playground]
#(https://mavenanalytics.io/data-playground/video-game-sales).
data = read.csv("vgchartz-2024.csv")

#Removes img column(its just a jpg or png) and games with less that 100,000 sales
cleaned_data = data |> select(-(img)) |> filter(total_sales >= 0.1)

#Change dates from a string to a Date object
cleaned_data = cleaned_data |> mutate(release_date = ymd(release_date), last_update  = ymd(last_update))

#Changes the title column to have the console if that specific video game is on multiple consoles(ex. GTA V - PS4, GTA V - PS3)
cleaned_data = cleaned_data |>
  group_by(title) |>
  #Create a new temporary column to store the count of platforms
  mutate(n_platforms = n_distinct(console)) |>
  ungroup() |>
  # Use if_else() to conditionally create the new title
  mutate(title = if_else(n_platforms > 1, paste(title, console, sep = " - "), title)) |>
  select(-n_platforms)

# Removes data with missing values
cleaned_data <- cleaned_data |>
  filter(!console %in% c("2600", "NG", "PCE", "SCD", "VC", "WS", "WW", "XBL", "GEN","GBC","PSN"))
cleaned_data <- cleaned_data |>
  filter(!genre %in% c("Visual Novel", "Board Game", "Education", "Sandbox"))

# Ensure categorical variables are factors and numeric
cleaned_data = cleaned_data |>
  mutate(
    console = as.factor(console),
    genre = as.factor(genre),
    critic_score = as.numeric(critic_score),
    total_sales = as.numeric(total_sales)
  )

# Define variable types for multivariate plotting 
Var_Choices = list(
  "Console" = "console", 
  "Genre" = "genre", 
  "Critic Score" = "critic_score", 
  "Total Sales (Millions)" = "total_sales" 
)

# Define UI
ui = fluidPage(
  # Changes font
  theme = bs_theme(base_font = font_google("Ubuntu")),
  
  # Application title
  titlePanel("Video Game Sales Data Explorer"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      # Introduction 
      h3("Welcome to the Video Game Sales Data Explorer!"),
      p("This tool allows you to visualize key variables from a dataset about video game sales."),
      p("Use the controls below to filter the data and customize your charts."),
      hr(),
      
      # Download Button
      div(
        style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
        downloadButton("downloadPlot", "Download Graph")
      ),
      
      # Filter by Total Sales (Slider)
      sliderInput(
        inputId = "sales_range_filter",
        label = h4("Filter by Total Sales (in millions):"),
        min = min(cleaned_data$total_sales, na.rm = TRUE),
        max = max(cleaned_data$total_sales, na.rm = TRUE),
        value = c(min(cleaned_data$total_sales, na.rm = TRUE), 
                  max(cleaned_data$total_sales, na.rm = TRUE)),
        step = 0.1,
        dragRange = TRUE
      ),
      p("Only games within the selected sales range will be included in the graphs and statistics."),
      hr(),
      
      # The main selector
      p("Choose either Univariate (one variable) or Multivariate (two variables) analysis."),
      radioButtons(
        inputId = "graph_type",
        label = "Choose Graph Type:",
        choices = c("Univariate", "Multivariate"),
        selected = "Univariate"
      ),
      
      # Select box for variable if univariate:
      conditionalPanel(
        condition = "input.graph_type == 'Univariate'",
        selectInput("var1", label = h3("Choose a Variable"), 
                    choices=list("Console"="console", "Critic Score"="critic score", "Genre"="genre", "Total Sales (Millions)"="total sales"), 
                    selected = "console")
      ),
      
      # Select box for variable if multivariate:
      conditionalPanel(
        condition = "input.graph_type == 'Multivariate'",
        selectInput(
          inputId = "var_x",
          label = "Select X Variable:",
          choices = Var_Choices,
          selected = "console"
        ),
        uiOutput("var_y_ui")
      ),
      
      hr(),
      p("These are options to choose the color of the graph and whether or not to show the descriptive statistics of the graph."),
      #option to chose colors
      selectInput("color_var", label = h3("Choose a Color"), 
                  choices=list("Red"='red', "Green"='green', "Blue"='blue', "Purple"='purple',"Orange"='orange'), 
                  selected = 'red'),
      
      #option to show statistics
      checkboxInput("checkbox1", label="Display Statistics", value=FALSE)
    ),

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      verbatimTextOutput("statsOutput")
    )
  )
)

server = function(input, output) {
  
  # Helper function to check if a variable is categorical 
  is_categorical = function(var_name) {
    var_class = class(cleaned_data[[var_name]])
    return(var_class %in% c("factor", "character"))
  }
  
  # Data Filtering 
  filtered_data = reactive({
    data = cleaned_data
    
    # Check if the sales range filter input exists and has two values
    if (!is.null(input$sales_range_filter) && length(input$sales_range_filter) == 2) {
      
      min_sales = input$sales_range_filter[1]
      max_sales = input$sales_range_filter[2]
      
      data = data |>
        # Filter the data to include only games within the selected sales range
        filter(total_sales >= min_sales & total_sales <= max_sales)
    }
    
    return(data)
  })
  
  # Dynamic UI for the Y-Variable Selector
  output$var_y_ui = renderUI({
    all_choices = Var_Choices
    selected_x = input$var_x
    
    filtered_choices = all_choices[all_choices != selected_x]
    
    selectInput(
      inputId = "var_y",
      label = "Select Y Variable:",
      choices = filtered_choices,
      selected = filtered_choices[1] 
    )
  })
  
  # Create the plot as a reactive object
  current_plot = reactive({
    # Get the filtered data
    data_to_plot = filtered_data()
    
    # Create an empty variable to hold the plot
    p = NULL 
    
    # Univariate Logic
    if(input$graph_type == "Univariate") {
      if(input$var1 == "console"){
        p = ggplot(data_to_plot) +
          geom_bar(aes(x = fct_infreq(console)), fill = input$color_var, color = "black") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
          labs(x = "Console", y = "Count", title = "Console Counts") 
      }
      else if(input$var1 == "critic score"){
        p = ggplot(data_to_plot) +
          geom_histogram(aes(x = critic_score), fill = input$color_var, color = "black", binwidth = 1) +
          theme_minimal() +
          scale_x_continuous(breaks = breaks_width(1)) +
          labs(x = "Critic Score", y = "Number of Games", title = "Distribution of Critic Scores")
      }
      else if(input$var1 == "genre"){
        p = ggplot(data_to_plot) +
          geom_bar(aes(x = fct_infreq(genre)), fill = input$color_var, color = "black") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
          labs(x = "Genre", y = "Count", title = "Genre Counts") 
      }
      else if(input$var1 == "total sales"){
        p = ggplot(data_to_plot) +
          geom_histogram(aes(x = total_sales), fill = input$color_var, color = "black", binwidth = 1) +
          theme_minimal() +
          scale_x_continuous(breaks = breaks_width(2)) +
          labs(x = "Sales(in millions)", y = "Number of Games", title = "Distribution of Sales")
      }
    }
    # Multivariate Logic
    else {
      x_var = input$var_x
      y_var = input$var_y
      
      x_is_cat = is_categorical(x_var)
      y_is_cat = is_categorical(y_var)
      
      x_title = tools::toTitleCase(gsub("_", " ", x_var))
      y_title = tools::toTitleCase(gsub("_", " ", y_var))
      
      if (!x_is_cat && !y_is_cat) {
        title_text = paste("Relationship between", y_title, "and", x_title, "across different video games")
        p = ggplot(data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_var))) +
          geom_point(color = input$color_var, alpha = 0.6) +
          theme_minimal() +
          labs(title = title_text, x = x_title, y = y_title)
      }  
      else if (x_is_cat && !y_is_cat) {
        title_text = paste0(y_title, " Distribution Across Different ", x_title, "s")
        p = ggplot(data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_var))) +
          geom_boxplot(aes(x = fct_reorder(!!sym(x_var), !!sym(y_var), .fun=median)), 
                       fill = input$color_var, color = "black", alpha = 0.7) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
          labs(title = title_text, x = x_title, y = y_title)
      }  
      else if (!x_is_cat && y_is_cat) {
        title_text = paste0(x_title, " Distribution Across Different ", y_title, "s")
        p = ggplot(data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_var))) +
          geom_boxplot(aes(y = fct_reorder(!!sym(y_var), !!sym(x_var), .fun=median)), 
                       fill = input$color_var, color = "black", alpha = 0.7) +
          theme_minimal() +
          labs(title = title_text, x = x_title, y = y_title) +
          coord_flip() 
      }  
      else if (x_is_cat && y_is_cat) {
        title_text = paste("Frequency of", x_title, "and", y_title, "Combinations across different video games")
        plot_data = data_to_plot |>
          group_by(!!sym(x_var), !!sym(y_var)) |>
          summarise(Count = n(), .groups = 'drop')
        
        p = ggplot(plot_data, aes(x = !!sym(x_var), y = !!sym(y_var), fill = Count)) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "white", high = input$color_var) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
          labs(title = title_text, x = x_title, y = y_title)
      }
    }
    
    # Return the final plot object
    return(p)
  })
  
  output$distPlot = renderPlot({
    current_plot()
  })
  
  # The download handler
  output$downloadPlot <- downloadHandler(
    
    # Name the file
    filename = function() {
      paste("game_graph_", input$genre, ".png", sep = "")
    },
    
    # Save the plot using ggsave()
    content = function(file) {
      ggsave(file, plot = current_plot(), device = "png", width = 8, height = 6)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
