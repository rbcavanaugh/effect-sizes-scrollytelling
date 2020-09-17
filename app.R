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
library(curl)

source('functions.R')
source('text.R')

# Define UI for application that draws a histogram



######################################################################################################
############################################## ui ################################################
######################################################################################################


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$meta(name = "viewport", content = "width=1600, initial-scale = 1")
  ),
  longdiv(h1("Are all effect sizes created equal?"),
          h4("Rob Cavanaugh", style = "text-align:center"),
           h5("Ph.D Student, University of Pittsburgh", style = "text-align:center"),
           h5("last updated: 9-12-20", style = "text-align:center"),
          h5(br(),
             br(),
             br(),
             br(),
             br(),
             img(src="chevron.png", heigth = "10%", width = "10%"), style = "text-align:center")
  ),
  
  
  ########################################### scrolly sections ###########################################################                      
  
  
  fluidRow(scrolly_container(width = 4,
                              "scr",
                             scrolly_graph(#textOutput("section"),
                                           plotOutput("distPlot")),
                             scrolly_sections(
                               scrolly_section(id = "1", h3("Why you should care about effect sizes"),
                                               p(text1)),
                               scrolly_section(id = "2", h3("Effect Sizes in Aphasiology"),
                                               p(text2)),
                               scrolly_section(id = "3", h3("Standardized Mean Difference"),
                                               p(SMD1)),
                               scrolly_section(id = "4", h3("Standardized Mean Difference"),
                                               p(SMD2)),
                               scrolly_section(id = "5", h3("Non-overlap of All Pairs"),
                                               p(NAP1)),
                               scrolly_section(id = "6", h3("Non-overlap of All Pairs"),
                                               p(NAP2)),
                               scrolly_section(id = "7", h3("Tau-U"),
                                               p(TAU)),
                               scrolly_section(id = "8", h3("Tau-U"),
                                               p(TAU)),
                               scrolly_section(id = "9", h3("Proportion of Potential Maximal Gain"),
                                               p(PMG1)),
                               scrolly_section(id = "10", h3("Proportion of Potential Maximal Gain"),
                                               p(PMG2)),
                               scrolly_section(id = "11", h3("Generalized linear mixed-effects models"),
                                               p(GLMM1),
                                               br(),
                                               p(GLMM2)),
                               scrolly_section(id = "12", h3("Generalized linear mixed-effects models"),
                                               p(GLMM3)),
                               scrolly_section(id = "13", h3("Bayesian GLMMs"),
                                               p(BMEM2),
                                               br(),
                                               p(BMEM3)),
                              # br(), # this section is to provide some space
                               scrolly_section(id = "14", h3(""),
                                               p(" ")),
                            )

########################################### summary ###########################################################                      

   )),
  div(h3("Summary"),
    p("Weve walked through six different types of tretment response that effect sizes should be sensitive to. In our larger study, we simulated 100 different participants in this hypothetical treatment study and used concordance correlation coefficients to evaluate the agreement between these different effect size measures across the range of treatment responses."),
    br(),
  p(
    "In the plot below, scatterplots visualize the relationship between standardized effect sizes (lower triangle) and concordance correlation coefficients are visualized by color (upper triangle). Concordance correlation coefficients less than 0.40 are considered poor. Coefficients between 0.40 and 0.75 are considered good to fair. Coefficients greater than 0.75 are considered excellent. Scatterplots also reveal cases where agreement is strong or poor across all effect sizes, increases as effect sizes increase (i.e. heterskedasticity), or the relationship between two different effect size measures is non-linear"
  ), style="text-align: left;"),
  br(),
  br(),
  div(img(src="shiny_plot.svg", heigth = "50%", width = "50%"), style="text-align: center;"),
  div(h2(
    style = "text-align:center; face:bold",
    tags$a(href = "https://osf.io/6x5pd/", "Interest piqued? Skeptical? Explore the methods and data here")),
    class = "container",
    style = "height:40vh; line-height:40vh; padding:20%"),

########################################### methods ########################################################### 

div(h3("Birefly, the nitty gritty"),
    p("Data were simulated for 100 hypothetical people with aphasia participating in a multiple baseline study. Each participant reflected performance on 30 treated items at 5 baseline and 10 treatment probe timepoints.Data were simulated using the R package SimStudy (Goldfeld, 2019), following the general modeling approach described by Manolov and Solanas (2008), in which the probability of a given correct response is a function of the baseline slope (β1), level change (β2) between baseline and treatment phases, and slope change (β3) between baseline and treatment phases. Beta-coefficients for the baseline slope, level change, and slope change variables were set at 0.06, 0.3, and 0.15 respectively (Manolov and Solanas, 2008). The intercept, β0, was defined by a normal distribution with a mean of -1.75 and variance of .25, randomly assigned to each time series. Participant level variance was characterized by a uniform distribution between 0 and 2. Item level effects were modeled by adding a term to describe item difficulty, approximating a normal distribution with a variance of 0.6. A logistic link function was used to calculate the probability of a correct response for each participant, item, session, and condition. Binomial responses were probabilistically simulated with a lag-1 autocorrelation of 0.5. A multi-level model recovered the parameteres effectively."),
p("SMD was calculated per the methods of Busk and Serlin (1992), NAP and Tau-U were calculated per the methods of Parker and Vannest (2011); Tau-U with a correction for baseline trend: Tau UA VS. B – TREND A. PMG was calculated as described by Lambon Ralph and colleagues (2010), but using the final session as post-treatment performance. Effect sizes estimated through GLMM replicated the approach described by Meier (2019). BMEM effect sizes mirrored the approach used by Evans et al (2020)."),
  style = "text-align:left",
  style = "padding:10%"),

########################################### references ########################################################### 

div(h3("Selected References"),
    p("Beeson, P. M., & Robey, R. R. (2006). Evaluating single-subject treatment research: Lessons learned from the aphasia literature. Neuropsychology Review, 16(4), 161–169. https://doi.org/10.1007/s11065-006-9013-7"),
    p("Bürkner, P. C. (2018). Advanced Bayesian multilevel modeling with the R package brms. R Journal. https://doi.org/10.32614/rj-2018-017"),
    p("Creet, E., Morris, J., Howard, D., & Nickels, L. (2019). Name it again! Investigating the effects of repeated naming attempts in aphasia. Aphasiology, 33(10), 1202–1226. https://doi.org/10.1080/02687038.2019.1622352"),
    p("Evans, W. S., Cavanaugh, R., Quique, Y., Boss, E., Dickey, M. W., Doyle, P. J., Starns, J. J., & Hula, W. D. (2020). BEARS - Balancing Effort, Accuracy, and Response Speed in semantic feature verification anomia treatment. Abstract for Platform Presentation, Annual Clinical Aphasiology Conference (Conference Cancelled)."),
    p("Goldfeld, K. (2019). simstudy: Simulation of Study Data (R package). https://cran.r-project.org/package=simstudy"),
    p("Jaeger, T. F. (2008). Categorical data analysis: Away from ANOVAs (transformation or not) and towards logit mixed models. Journal of Memory and Language, 59(4), 434–446. https://doi.org/10.1016/j.jml.2007.11.007"),
    p("King, T. S., & Chinchilli, V. M. (2001). A generalized concordance correlation coefficient for continuous and categorical data. Statistics in Medicine, 20(14), 2131–2147. https://doi.org/10.1002/sim.845"),
    p("Lambon Ralph, M. A., Snell, C., Fillingham, J. K., Conroy, P., & Sage, K. (2010). Predicting the outcome of anomia therapy for people with aphasia post CVA: both language and cognitive status are key predictors. Neuropsychological Rehabilitation, 20(2), 289–305. https://doi.org/10.1080/09602010903237875"),
    p("Landis, J. R., & Koch, G. G. (1977). An Application of Hierarchical Kappa-type Statistics in the Assessment of Majority Agreement among Multiple Observers. Biometrics, 33(2), 363–374. JSTOR. https://doi.org/10.2307/2529786"),
    p("Lin, L. I.-K. (1989). A Concordance Correlation Coefficient to Evaluate Reproducibility. Biometrics, 45(1), 255–268. JSTOR. https://doi.org/10.2307/2532051"),
    p("Manolov, R., & Solanas, A. (2008). Comparing N = 1 Effect Size Indices in Presence of Autocorrelation. Behavior Modification, 32(6), 860–875. https://doi.org/10.1177/0145445508318866"),
    p("Parker, R. I., & Vannest, K. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. Behavior Therapy, 40(4), 357–367. https://doi.org/10.1016/j.beth.2008.10.006"),
    p("Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011). Combining nonoverlap and trend for single-case research: Tau-U. Behavior Therapy, 42(2), 284–299. https://doi.org/10.1016/j.beth.2010.08.006"),
    p("R Core Team. (2020). R: A language and environment for statistical computing (4.0.2). R Foundation for Statistical Computing. https://www.r-project.org/"),
    p("Wiley, R. W., & Rapp, B. (2018). Statistical analysis in Small-N Designs: Using linear mixed-effects modeling for evaluating intervention effectiveness. Aphasiology, 33(1), 1–30. https://doi.org/10.1080/02687038.2018.1454884"),
    style = "text-align:left",
    style = "padding:10%")
)



######################################################################################################
############################################## server ################################################
######################################################################################################



# Define server logic required to draw a histogram   input$scr
server <- function(input, output) {
  
  # df_reactive <- reactive(
  #   df %>%
  #     filter()
  # )
  
  output$distPlot <- renderPlot({
    
    t = as.numeric(input$scr) # defines what plot we want....(what section we're on)
    
    ls <- list(
        t1 = c(31, 34, 12, 15, 51, 56, 57, 84, 93, 99),
        t2 = c(31, 34, 12, 15, 51, 56, 57, 84, 93, 99),
        t3 =  c(51),#SMD
        t4 =  c(12),#SMD
        t5  = c(15), #NAP
        t6  = c(51, 84, 93), #NAP
        t7 =  c(84), #TAU
        t8  = c(56, 57), #TAU
        t9  = c(34), # PMG
        t10  = c(31, 34), #PMG
        t11  = c(99), #GLMM
        t12  = c(84, 99), #GLMMM
        t13  = c(12) #BMEM
    )
    
    if(between(t, 1,13)) sel = ls[[t]]


    if(t==1) df %>% # plots all
      ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id,
                 alpha = ifelse(sub_id %in% sel, 0.975, 0.01))) +
      geom_point(size = 4, shape = 16) +
      geom_line(size = 1.5) + 
      geom_vline(aes(xintercept = 5.5), alpha = .5) +
      scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      scale_x_continuous(labels = seq(1,15,1), breaks = seq(1,15,1)) +
      theme_modern(base_size = 14) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "transparent",colour = NA), 
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA),
            axis.title.x = element_text(size = 16, family = 'roboto'),
            axis.title.y = element_text(size = 16, family = 'roboto')) + #,
      scale_colour_viridis_d(option = "plasma", begin = 0, end = .8) +
      ylab('Accuracy') +
      xlab("Session") 
    else if(t==2) df %>%
      filter(sub_id %in% ls[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id)) +
      geom_point(size = 4, shape = 16) + #, color = tmp
      geom_line(size = 1.5) + 
      geom_vline(aes(xintercept = 5.5), alpha = .5) +
      scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      scale_x_continuous(labels = seq(1,15,1), breaks = seq(1,15,1)) +
      theme_modern(base_size = 14) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "transparent",colour = NA), 
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA),
            axis.title.x = element_text(size = 16, family = 'roboto'),
            axis.title.y = element_text(size = 16, family = 'roboto')) +
      scale_colour_viridis_d(option = "plasma", begin = 0, end = .8) +
      ylab('Accuracy') +
      xlab("Session") 
    else if(t>2 & t<=13 & t !=5) df %>%
      filter(sub_id %in% ls[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      geom_point(size = 4, shape = 16) + #, color = tmp
      geom_line(size = 1.5) + 
      geom_vline(aes(xintercept = 5.5), alpha = .5) +
      scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      scale_x_continuous(labels = seq(1,15,1), breaks = seq(1,15,1)) +
      theme_modern(base_size = 14) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "transparent",colour = NA), 
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA),
            axis.title.x = element_text(size = 16, family = 'roboto'),
            axis.title.y = element_text(size = 16, family = 'roboto')) +
      scale_colour_viridis_d(option = "plasma", begin = 0, end = .8) +
      ylab('Accuracy') +
      xlab("Session") 
    else if(t==5) df %>%
      filter(sub_id %in% ls[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      geom_point(size = 4, shape = 16) + #, color = tmp
      geom_line(size = 1.5) + 
      geom_vline(aes(xintercept = 5.5), alpha = .5) +
      scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      scale_x_continuous(labels = seq(1,15,1), breaks = seq(1,15,1)) +
      theme_modern(base_size = 14) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill = "transparent",colour = NA), 
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA),
            axis.title.x = element_text(size = 16, family = 'roboto'),
            axis.title.y = element_text(size = 16, family = 'roboto')) +
      scale_colour_viridis_d(option = "plasma", begin = 0, end = .8) +
      ylab('Accuracy') +
      xlab("Session") +
      geom_segment(aes(x = 3, y = .1666, xend = 6, yend = .0666), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 7, yend = .1666), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 8, yend = .233), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 9, yend = .166), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 10, yend = .300), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 11, yend = .500), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 12, yend = .6), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 13, yend = .5), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 14, yend = .566), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm"))) +
      geom_segment(aes(x = 3, y = .1666, xend = 15, yend = .6), color = 'black',size = .25,
                   arrow = arrow(type = 'closed',length = unit(0.25, "cm")))
    else NULL

  }, bg="transparent")
  
  # output$img <- renderImage({
  #   # When input$n is 1, filename is ./images/image1.jpeg
  #   filename <- normalizePath("fig3_top_8-31.png")
  #   # Return a list containing the filename
  #   list(src = filename)
  # }, deleteFile = FALSE)
  
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



# # # test =
# # # 
# df %>% # plots all
#   filter(sub_id == 15) %>%
#   ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id)) +
#   geom_point(size = 2) +
#   geom_line(size = 1.5) +
#   geom_vline(aes(xintercept = 5.5), alpha = .5) +
#   scale_y_continuous(limits = c(0,1), labels = scales::percent) +
#   scale_x_continuous(labels = seq(1,15,1), breaks = seq(1,15,1)) +
#   facet_wrap(~sub_id) +
#   theme_modern(base_size = 14) +
#   theme(legend.position = 'none',
#         panel.background = element_rect(fill = "transparent",colour = NA),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         plot.background = element_rect(fill = "transparent",colour = NA),
#         axis.title.x = element_text(size = 16, family = 'roboto'),
#         axis.title.y = element_text(size = 16, family = 'roboto')) + #,
#   # plot.title = element_text(size = 18, family = 'roboto', hjust = 0.5)) +
#   #scale_color_brewer(palette = "Dark2") +
#   ylab('Accuracy') +
#   xlab("Session") +
#   geom_segment(aes(x = 3, y = .1666, xend = 6, yend = .0666), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 7, yend = .1666), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 8, yend = .233), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 9, yend = .166), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 10, yend = .300), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 11, yend = .500), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 12, yend = .6), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 13, yend = .5), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 14, yend = .566), color = 'black',
#                arrow = arrow()) +
#   geom_segment(aes(x = 3, y = .1666, xend = 15, yend = .6), color = 'black',
#                arrow = arrow())
#   
# 
# 
# 
# 
# df %>% # plots all
#   filter(sub_id == 15) 

