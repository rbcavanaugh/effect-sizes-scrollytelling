#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scrollytell)

source('functions_p.R')

# Define UI for application that draws a histogram

ui <- fluidPage(
  # Application title
  titlePanel("Scrolly Telling"),
  fluidRow(scrolly_container(width = 4,
                              "scr",
                             scrolly_graph(#textOutput("section"),
                                           plotOutput("distPlot")),
                             scrolly_sections(
                               scrolly_section(id = "1",
                                               h3("The Responder"),
                                               p("a paragraph of all kinds of information goes here")),
                               scrolly_section(id = "2", h3("The level change"),
                                               p("a paragraph of all kinds of information goes here")),
                               scrolly_section(id = "3", h3("The slope change"),
                                               p("a paragraph of all kinds of information goes here")),
                               scrolly_section(id = "4", h3("Low baseline Variability"),
                                               p("a paragraph of all kinds of information goes here")),
                               scrolly_section(id = "5", h3("Not a responder"),
                                               p("a paragraph of all kinds of information goes here")),
                               scrolly_section(id = "6", h3("The rising baseline"),
                                               p("a paragraph of all kinds of information goes here")),
                               scrolly_section(id = "7", h3("Summary"),
                                               p("a paragraph of all kinds of information goes here")),
                            )
   )),
  #HTML('<center><img src="fig3_top_8-31.png"></center>', width="300px", height="300px"),
  div(img(src="fig3_top_8-31.png", heigth = 500, width = 500), style="text-align: right;"),
  div("Footer")
)

# Define server logic required to draw a histogram   input$scr
server <- function(input, output) {
  
  # df_reactive <- reactive(
  #   df %>%
  #     filter()
  # )
  
  output$distPlot <- renderPlot({
    t <- as.numeric(input$scr)
    tmp <- colr[as.numeric(input$scr)]
    
    if(t<7 & !is.na(t)) df %>%
      filter(new_id == input$scr) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase)) +
      geom_point(size = 5, color = tmp) +
      geom_line(size = 2, color = tmp) + 
      geom_vline(aes(xintercept = 5.5), alpha = .5) +
      scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      scale_x_continuous(labels = seq(1,15,1), breaks = seq(1,15,1)) +
      theme_modern(base_size = 14) +
      theme(legend.position = 'none') +
      ylab('Accuracy') +
      xlab("Session") +
      ggtitle("Probe Performance")
    else NULL

  })
  
  output$img <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath("fig3_top_8-31.png")
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  
  output$scr <- renderScrollytell({
    scrollytell()
  })
  
  #output$section <- renderText(paste0("Section: ", input$scr))
  
  observe({
    cat("section:", input$scr, "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)