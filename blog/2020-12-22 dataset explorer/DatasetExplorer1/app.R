library(shiny)
library(tidyverse)

# Dataset to be explored. You can replace `diamonds` with your own dataset.
raw_df <- diamonds

# Pre-compute some variables to be used by app
not_numeric <- sapply(names(raw_df), function(x) !is.numeric(raw_df[[x]]))
df <- raw_df

# Define UI ----
ui <- fluidPage(
  
  titlePanel("Dataset Explorer"),
  
  p("A simple Shiny app for exploratory data analysis. The output in the
    'Summary' and 'str() Output tabs is based on the full dataset. The 
    controls in the sidebar are for the 'Plot' and 'Data snippet' tabs. The
    'Data snippet' tab shows a maximum of 15 rows of the dataset."),
  
  sidebarPanel(
    
    sliderInput("sampleSize", "Plot sample size (n)", min = 1, max = nrow(df),
                value = min(1000, nrow(df)), step = nrow(df) / 50, round = 0),
    radioButtons("sampleType", "Plot sample type",
                 choices = list("Random n" = "random", "First n" = "first")),
    numericInput("sampleSeed", "Sample seed", value = 1),
    
    selectInput("x", "X", names(df)),
    selectInput("y", "Y", c("None", names(df)), names(df)[[2]]),
    
    # only allow non-numeric variables for color
    selectInput("color", "Color", c("None", names(df)[not_numeric])),
    
    p("Jitter and smoothing are only available when two numeric variables 
      are selected."),
    checkboxInput("jitter", "Jitter"),
    checkboxInput("smooth", "Smooth")
  ),
  
  mainPanel(
    
    # Output: Tabset
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Data Snippet", verbatimTextOutput("snippet")),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("str() Output", verbatimTextOutput("str"))
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  # Generate data summaries
  output$summary <- renderPrint({
    summary(raw_df)
  })
  output$str <- renderPrint({
    str(raw_df)
  })
  
  # get new dataset sample for plotting
  idx <- reactive({
    if (input$sampleType == "first") {
      1:input$sampleSize
    } else {
      set.seed(input$sampleSeed)
      sample(nrow(raw_df), input$sampleSize)
    }
  })
  df <- reactive(raw_df[idx(), , drop = FALSE])

  # Get head of selected data
  output$snippet <- renderPrint({
    head(df(), n = 15)
  })
  
  # get plot type
  # * 2: both numeric variables
  # * 1: one numeric, one non-numeric variable
  # * 0: both non-numeric variables
  # * -1: only one variable provided
  plot_type <- reactive({
    if (input$y != "None")
      is.numeric(raw_df[[input$x]]) + is.numeric(raw_df[[input$y]])
    else
      -1
  })
    
  # Create plot
  output$plot <- renderPlot({
    if (plot_type() == 2) {
      # both numeric variables: scatterplot
      # also allow for color, jitter & smoothing
      p <- ggplot(df(), aes_string(x = input$x, y = input$y))
      
      if (input$jitter)
        p <- p + geom_jitter(alpha = 0.5)
      else
        p <- p + geom_point(alpha = 0.5)
      
      if (input$smooth)
        p <- p + geom_smooth()
      
      # color change
      if (input$color != "None")
        p <- p + aes_string(color = input$color)
    } else if (plot_type() == 1) {
      # one numeric var, one character var: boxplot
      # allow color, don't allow jitter or smoothing
      p <- p <- ggplot(df(), aes_string(x = input$x, y = input$y)) + 
        geom_boxplot()
      
      # fill change
      if (input$color != "None")
        p <- p + aes_string(fill = input$color)
    } else if (plot_type() == 0) {
      # two character variables: heatmap
      # don't allow color, jitter or smoothing
      temp_df <- reactive(df()[, c(input$x, input$y), drop = FALSE] %>%
                            group_by(across()) %>%
                            summarize(count = n())
      )
      p <- ggplot(temp_df(), 
                  mapping = aes_string(x = input$x, y = input$y, fill = "count")) +
        geom_tile() +
        scale_fill_gradient(low = "#e7e7fd", high = "#1111dd")
    } else {
      # only one variable: univariate plot
      # allow color, don't allow jitter or smoothing
      p <- ggplot(df(), aes_string(x = input$x))
      
      if (is.numeric(raw_df[[input$x]]))
        p <- p + geom_histogram()
      else
        p <- p + geom_bar()
      
      # fill change
      if (input$color != "None")
        p <- p + aes_string(fill = input$color)
    }
    
    # add title
    if (plot_type() >= 0) {
      p <- p + labs(title = paste(input$y, "vs.", input$x))
    } else {
      p <- p + labs(title = paste("Distribution of", input$x))
    }
    
    # add styling
    p <- p + 
      theme_bw() +
      theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
            axis.title = element_text(size = rel(1.5)))
    
    print(p)
    
  }, height=500)
}

# Run the app ----
shinyApp(ui = ui, server = server)