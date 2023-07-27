## app.R ##
library(shinydashboard)
library(shiny)
library(quantmod)
library(ggplot2)
library(dplyr)
library(purrr)
library(plotly)

#plotting theme for ggplot2
.theme<-theme(
  axis.line = element_line(colour = "gray", size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)

#User interface ---
ui <- dashboardPage(
  dashboardHeader(title = "Exploring Your Biofouling Data"),
    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload Your Own Data", tabName = "yours", icon = icon("th")),
        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        # Input: Select separator ----
        selectInput("xcol", "Predictor variable", ""),
        selectInput("ycol", "Outcome variable", "", selected = ""),
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        # Horizontal line ----
        tags$hr(),
        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"),
    menuItem("SK Data Exploration", tabName = "skdata", icon = icon("dashboard")))),
dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "skdata",
                fluidRow(
                  column(width = 12,
                  box(plotOutput("plot1"), width = 12),
                  box(
                    title = "Controls", width = 12,
                    selectInput("species", "Choose a species to display", choices = c("Species 1", "Species 2", "Species 3"), selected = "Species 1"),
                    selectInput("depth", "Choose a depth to display", choices = c("Shallow", "Deep"), selected = "Shallow")
                  )
                  )
                )
        ),
        # Second tab content
        tabItem(tabName = "yours",
                h3("Upload and explore your own biofouling data"),
                fluidRow(
                    # Main panel for displaying outputs ----
                    mainPanel(
                      #Output: Box plot of data ---
                      plotlyOutput("MyPlot"),
                      # Output: Data file ----
                      tableOutput("contents")
                    )
                    
                  )
                )
)))

server <- function(input, output, session) {
  # added "session" because updateSelectInput requires it
  options(warn = -1)
  #options(encoding="UTF-8")
  state <- reactiveValues()
  mycols = c("blue", "green", "red", "orange", "purple")
  fouling <- read.csv("data/data.csv")

  output$plot1 <- renderPlot({
    if(input$depth == "Deep"){
      data2 <- switch(input$species,
                     "Species 1" = fouling$SP1_Deep,
                     "Species 2" = fouling$SP2_Deep,
                     "Species 3" = fouling$SP3_Deep)
      #Render a barplot

      barplot(height = data2,
              ylab="Percent Cover",
              xlab="Date",
              main = "Deep",
              col = mycols[as.factor(fouling$Site)],
              names.arg = fouling$Date, ylim = c(0,100))
      legend = TRUE
      legend(x=0, y=110,bty="n", ncol=5, legend = as.factor(unique(fouling$Site)), fill=mycols[as.factor(unique(fouling$Site))],xpd = TRUE)
    }
    else{
      data2 <- switch(input$species,
                     "Species 1" = fouling$SP1_Shallow,
                     "Species 2" = fouling$SP2_Shallow,
                     "Species 3" = fouling$SP3_Shallow)
          barplot(height = data2,
              ylab="Percent Cover",
              xlab="Date",
              main = "Shallow",
              col = mycols[as.factor(fouling$Site)],
              names.arg = fouling$Date, ylim = c(0,100))
      legend = TRUE
      legend(x=0, y=110, bty="n", ncol=5, legend = as.factor(unique(fouling$Site)), fill=mycols[as.factor(unique(fouling$Site))], xpd = TRUE)
    }
  })
  data<- reactive({
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    for(i in seq_along(df)){
      df[,i] <- as.factor(df[,i])
      
    }
    df[df=="NA"] <- NA
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    updateSelectInput(session, inputId = "ycol", label = "Predictor variable",
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = "xcol", label = "Outcome variable",
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  output$MyPlot <- renderPlotly({
    p <- ggplot(na.omit(data()), aes(x=na.omit(data())[,input$ycol],y=na.omit(data())[,input$xcol], fill =na.omit(data())[,input$ycol]))+
      geom_boxplot(na.rm = TRUE)+
      xlab(input$ycol)+ylab(input$xcol) +scale_fill_discrete(name=input$ycol) +coord_flip()+theme(legend.position = "none")
    ggplotly(p) %>% layout(autosize=TRUE)# %>% style(hoverinfo = "none")
  })
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}

shinyApp(ui, server)