library(shiny)
library(shinyjs)
library(shticky)
#library(sigmajs)
library(waypointer)
library(plotly)
#source("./data/network.R")
source("functions.R")

OFFSET <- "50%"
ANIMATION <- "slideInUp"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.8.1/css/all.css", 
      integrity = "sha384-50oBUHEmvpQ+1lW4y57PTFmhCaXp0ML5d60M1M7uH2+nqUivzIebhndOJK28anvf",
      crossorigin = "anonymous"
    )
  ),
  use_shticky(),
  use_waypointer(),
  div(
    id = "bg",
    div(
      id = "stick",
      style = "position:fixed;width:100%;",
      fluidRow(
        column(4),
        column(8) #, sigmajsOutput("graph",
      )
    ),
    longdiv(
      h1("Nine Months of #tidytuesday", class = "title"),
      br(),
      br(),
      h1(
        class = "subtitle",
        "Each", tags$i(class = "fas fa-circle sg"), "node is a twitter user,",
        "and each", tags$i(class = "fas fa-slash sg"), "is one tweet or more."
      ),
      br(),
      p(
        style = "text-align:center;",
        "Using the first dataset of #tidytuesday: #Rstats & #TidyTuesday Tweets", 
        tags$a(
          class = "sg",
          tags$i(class = "fas fa-external-link-alt"),
          target = "_blank",
          href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-01"
        )
      ),
      br(),
      br(),
      br(),
      p(
        style = "text-align:center;",
        tags$i(class = "fas fa-chevron-down fa-3x")
      )
    ),
    longdiv(
      div(
        id = "m1",
        uiOutput("1"),
        plotlyOutput("p1")
      )
    ),
    longdiv(
      div(
        id = "m2",
        uiOutput("2"),
        plotlyOutput("p1")
      )
    ),
    longdiv(
      div(
        id = "m3",
        uiOutput("3"),
        plotlyOutput("p3")
      )
    ),
    longdiv(
      div(
        id = "m4",
        uiOutput("4"),
        plotlyOutput("p4")
      )
    ),
    longdiv(
      div(
        id = "m5",
        uiOutput("5"),
        plotlyOutput("p5")
      )
    ),
    longdiv(
      div(
        id = "m6",
        uiOutput("6"),
        plotlyOutput("p6")
      )
    ),
    longdiv(
      id = "m7",
      class = "light",
      style = "text-align:center;",
      h1("Thank you", class = "title"),
      h1(
        class = "subtitle",
        tags$a(
          "Read the blog post",
          target = "_blank",
          class = "sg",
          href = "https://john-coene.com/post/scrollytell/"
        )
      )
    )
  )
)







server <- function(input, output, session) {

  w1 <- Waypoint$
    new("m1", offset = OFFSET, animate = F)$ #, animation = ANIMATION
    start()
  w2 <- Waypoint$
    new("m2", offset = OFFSET, animate = F)$
    start()
  w3 <- Waypoint$
    new("m3", offset = OFFSET, animate = F)$
    start()
  w4 <- Waypoint$
    new("m4", offset = OFFSET, animate = F)$
    start()
  w5 <- Waypoint$
    new("m5", offset = OFFSET, animate = F)$
    start()
  w6 <- Waypoint$
    new("m6", offset = OFFSET, animate = F)$
    start()
  w7 <- Waypoint$
    new("m7", offset = "80%", animate = F)$
    start()

  # renders info on left
  output$`1` <- renderUI({
    req(w1$get_triggered())
    if(w1$get_triggered() == TRUE) render_month(1)
  })

  output$`2` <- renderUI({
    req(w2$get_triggered())
    if(w2$get_triggered() == TRUE) render_month(2)
  })
  
  output$`3` <- renderUI({
    req(w3$get_triggered())
    if(w3$get_triggered() == TRUE) render_month(3)
  })
  
  output$`4` <- renderUI({
    req(w4$get_triggered())
    if(w4$get_triggered() == TRUE) render_month(4)
  })
  
  output$`5` <- renderUI({
    req(w5$get_triggered())
    if(w5$get_triggered() == TRUE) render_month(5)
  })
  
  output$`6` <- renderUI({
    req(w6$get_triggered())
    if(w6$get_triggered() == TRUE) render_month(6)
  })

  # Our sticky plot
  shtick <- Shtick$
    new("#stick")$
    shtick()
  
  
  df_reac<-reactive({
    ids = seq(1,6)
    df%>%
      filter(new_id %in% ids)
  })
  
  output$p1 <- renderPlotly({
    req(w1$get_triggered())
    if(w1$get_triggered() == TRUE) 
      ggplotly(
        df_reac() %>%
        ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id)) +
        geom_point() +
        geom_line() + 
        geom_vline(aes(xintercept = 5.5), alpha = .5) +
        scale_y_continuous(limits = c(0,1), labels = scales::percent) +
        scale_x_continuous(labels = NULL, breaks = NULL) +
        theme_grey(base_size = 12) +
        theme(legend.position = 'none') +
        ylab('Accuracy') +
        xlab(NULL)
      )
  })
  
   eventReactive(w1$get_direction(), {
     if(w1$get_direction() == "down") ids = 1
   })
  
  observeEvent(w1$get_direction(), {
    if(w1$get_direction() == "down")
      plotlyProxy("p1", session) 
    })

  # 
  # observeEvent(w2$get_direction(), {
  #   if(w2$get_direction() == "down") ids = 2
  # })
  # 
  # observeEvent(w3$get_direction(), {
  #   if(w3$get_direction() == "down") ids = 3
  # })
  # 
  # observeEvent(w4$get_direction(), {
  #   if(w4$get_direction() == "down") ids = 4
  # })
  # 
  # observeEvent(w5$get_direction(), {
  #   if(w5$get_direction() == "down") ids = 5
  # })
  # 
  # observeEvent(w6$get_direction(), {
  #   if(w6$get_direction() == "down") ids = 6
  # })

  observeEvent(w7$get_direction(), {
    if(w7$get_direction() == "down") shtick$unshtick()
  })
  
}

shinyApp(ui, server)