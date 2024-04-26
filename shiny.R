
################################### Shiny Application ############################################

################################## Build a babynames explorer Shiny app
install.packages("tidyverse")
install.packages("broom")
install.packages("shiny")
install.packages("bslib")

library(tidyverse)
library(broom)
library(shiny)
library(bslib)
x<-babynames

## Sketch your App ------------------------

## Add inputs (UI) :
install.packages("babynames")

library(babynames)
library(ggplot2)


# To make UI : 
ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  textInput('name', 'Enter Name', 'David')
)


server <- function(input, output, session){
}


shinyApp(ui = ui, server = server)

########################

## Add outputs (UI/server) : --------------------------------


ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  textInput('name', 'Enter Name', 'David'),
  plotOutput('trend')
)


server <- function(input, output, session){
  output$trend <- renderPlot({
    ggplot()
  })
}


shinyApp(ui = ui, server = server)
install.packages("textshaping")
library(textshaping)
########################

# Update layout (UI) ---------------------------------------

ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter Name', 'David')
    ),
    mainPanel(
      plotOutput('trend')
    )
  )
)


server <- function(input, output, session){
  output$trend <- renderPlot({ggplot()})
}


shinyApp(ui = ui, server = server)
###########################
## Final : 

## Update output (server) ----------------------------------------

ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter Name', 'David')
    ),
    mainPanel(
      plotOutput('trend')
    )
  )
)


server <- function(input, output, session){
  output$trend <- renderPlot({
    data_name <- subset(
      babynames, name == input$name
    )
    ggplot(data_name) +
      geom_line(
        aes(x = year, y = prop, color = sex)
      )
  })
}


shinyApp(ui = ui, server = server)

###################################################################################################################################
############################ Life Expectation vs. GDP Per Capital #############################

##########   Step 1: Add inputs (UI) ---------------------------------------------

# Input :

# gapminder : GDP & Life info data

install.packages("gapminder")
library(gapminder)

u<-gapminder

# Step 1 : Add Input ( UI)

ui <- fluidPage(
  titlePanel("Life Expectation vs. GDP Per Capital"),
  selectInput('continent', 'Select Continent', unique(gapminder$continent)),
  sliderInput('year', 'Select Year', 1952, 2007, 1992, step = 5)
)

# output
server <- function(input, output, session){
}

# Server ( Rendering )
shinyApp(ui = ui, server = server)


######### Step 2: Add outputs (UI) ---------------------------------------------

ui <- fluidPage(
  titlePanel("Life Expectation vs. GDP Per Capita"),
  selectInput('continent', 'Select Continent', unique(gapminder$continent)),
  sliderInput('year', 'Select Year', 1952, 2007, 1990, step = 5),
  plotOutput('plot'),
  DT::DTOutput('table')
)

## Step 2: Add outputs (Server)
server <- function(input, output, session){
  output$plot <- renderPlot({
    ggplot()
  })
  output$table <- DT::renderDT({
    gapminder
  })
}


shinyApp(ui = ui, server = server)

########### Step 3: Update layout (UI)
# Adding : Plot + Table 

ui <- fluidPage( titlePanel(
  "Life Expectation vs. GDP Per Capita"
),
sidebarLayout(
  sidebarPanel( selectInput(
    'continent'
    ,
    'Select Continent'
    , unique(gapminder$continent)), sliderInput(
      'year'
      ,
      'Select Year'
      ,
      1952
      ,
      2007
      ,
      1990
    ) ),
  mainPanel( tabsetPanel( tabPanel(
    "Plot"
    , plotOutput(
      'plot'
    )), tabPanel(
      "Table"
      , DT::DTOutput(
        'table'
      )) ) )
) )


server <- function(input, output, session){
  output$plot <- renderPlot({
    ggplot()
  })
  output$table <- DT::renderDT({
    gapminder
  })
}


shinyApp(ui = ui, server = server)

########### Step 4: Update outputs (Server) --------------------------------------------
####### Final : --------------------------

ui <- fluidPage( titlePanel(
  "Life Expectation vs. GDP Per Capita"
),
sidebarLayout(
  sidebarPanel( selectInput(
    'continent'
    ,
    'Select Continent'
    , unique(gapminder$continent)), sliderInput(
      'year'
      ,
      'Select Year'
      ,
      1952
      ,
      2007
      ,
      1990
    ) ),
  mainPanel( tabsetPanel( tabPanel(
    "Plot"
    , plotOutput(
      'plot'
    )), tabPanel(
      "Table"
      , DT::DTOutput(
        'table'
      )) ) )
) )

server <- function(input, output, session){
  output$plot <- renderPlot({
    data <- gapminder %>%
      filter(year == input$year) %>%
      filter(continent == input$continent)
    print(data)
    ggplot(data, aes(x = gdpPercap, y = lifeExp)) +
      geom_point()
  })
  output$table <- DT::renderDT({
    gapminder %>%
      filter(year == input$year) %>%
      filter(continent == input$continent)
  })
}
shinyApp(ui = ui, server = server)
