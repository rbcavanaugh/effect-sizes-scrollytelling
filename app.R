#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# to do:
# hard numbers for SMD
# more plot annotations
# bigger font for left side
# more concise paragraphcs
# name to bottom
# 'other considerations' section: credible/confidence intervals. other DVs etc.

library(shiny)
library(scrollytell)
library(curl)
library(shinyjs)

source('functions.R')
source('text.R')
options(gganimate.dev_args = list(bg = 'transparent'))
# Define UI for application that draws a histogram

######################################################################################################
############################################## ui ################################################
######################################################################################################


ui <- fluidPage(  
  mobileDetect('isMobile'),
  shinyjs::useShinyjs(),
  includeScript('www/scrolldown.js'),
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(rel="stylesheet", media="screen and (max-device-width: 767px)", href="style_mobile2.css"),
    includeHTML("www/analytics.html")
  ),
  withMathJax(),
  div(class="demo_wrap",
          h1("Effect Sizes in Aphasia Single-Case Designs"),
          h2("Rob Cavanaugh"),
          h5("Ph.D Student, University of Pittsburgh"),
          h5(htmlOutput('isItMobile')),
          img(src = "outfile2.gif"),
          ),
  br(),
  br(),
  div(img(id = "scrll", src="chevron.png", heigth = "10%", width = "10%", style="cursor:pointer;"),
      style = "text-align:center; opacity:60%; padding:10%",
      type = "button"
      ),
  longdiv(
      h3("Introduction: (note: currently under revision)", style = "text-align: center; padding-bottom: 3%"),
      p(ls$text1a), #style = 'text-align: center;'),
      p(ls$text1a2), #style = 'text-align: center;'),
      p(ls$text1b) #style = 'text-align: center;')
  ),

  ########################################### scrolly sections ###########################################################                      
  fluidRow(scrolly_container(width = 4,
                              "scr",
                             scrolly_graph(imageOutput("distPlot")),
                             scrolly_sections(
                               scrolly_section(id = "1", h3("Challenges in effect size measurement"),
                                               p(ls$text2a),
                                               p(ls$text2b)),
                               scrolly_section(id = "2", h3("Comparing effect sizes in aphasiology"),
                                               p(ls$text3a),
                                               p(ls$text3b)),
                               scrolly_section(id = "3", h3("Standardized Mean Difference"),
                                               p(ls$SMD1),
                                               p(ls$SMD1a),
                                               h4(ls$SMD_eq),
                                               p(ls$SMD1b)),
                               scrolly_section(id = "4", h3("Standardized Mean Difference"),
                                               p(ls$SMD2),
                                               p(ls$SMD2a)),
                               scrolly_section(id = "5", h3("Standardized Mean Difference"),
                                               p(ls$SMD3),
                                               h4(ls$SMD_eq2),
                                               p(ls$SMD3a)),
                               scrolly_section(id = "6", h3("Standardized Mean Difference"),
                                               p(ls$SMD4a),
                                               h4(ls$SMD_eq3),
                                               p(ls$SMD4b)),
                               scrolly_section(id = "7", h3("Non-overlap of All Pairs"),
                                               p(ls$NAP1),
                                               p(ls$NAP2),
                                               p(ls$NAP3)),
                               scrolly_section(id = "8", h3("Non-overlap of All Pairs"),
                                               p(ls$NAP4)),
                               scrolly_section(id = "9", h3("Tau-U"),
                                               p(ls$TAU),
                                               p(ls$TAUa)),
                               scrolly_section(id = "10", h3("Tau-U"),
                                               p(ls$TAU1),
                                               p(ls$TAU2)),
                               scrolly_section(id = "11", h3("Proportion of Potential Maximal Gain"),
                                               p(ls$PMG1),
                                               h4(ls$PMG_eq)),
                               scrolly_section(id = "12", h3("Proportion of Potential Maximal Gain"),
                                               p(ls$PMG2),
                                               p(ls$PMG3)),
                               scrolly_section(id = "13", h3("Generalized linear mixed-effects models"),
                                               p(ls$GLMM1)),
                               scrolly_section(id = "14", h3("Generalized linear mixed-effects models"),
                                               p(ls$GLMM2),
                                               p(ls$GLMM3)),
                               scrolly_section(id = "15", h3("Generalized linear mixed-effects models"),
                                               p(ls$GLMM4)),
                               scrolly_section(id = "16", h3("Bayesian GLMMs"),
                                               p(ls$BMEM1),
                                               p(ls$BMEM_eq),
                                               p(ls$BMEM1a)),
                               scrolly_section(id = "17", h3("Bayesian GLMMs"),
                                               p(ls$BMEM2),
                                               br(),
                                               br()),
                            )),

########################################### summary ###########################################################         
  ),
  div(h3("Correspondance between effect size measures"),
    p(sum1),
    p(sum2),style="text-align: left; padding-left:10%; padding-right:10%; padding-top:15%"),
  div(img(src="fig2.png", heigth = "70%", width = "70%"), style="text-align: center;padding:5%"),
  div(
    h3("Trends worth point out:"),
    p(sum3),
    p(sum4),
    p(sum5),
    p(sum6),
    style="text-align: left; padding-left:10%; padding-right:10%; padding-top:2%;"
  ),
  div(h2(
    style = "text-align:center;",
    tags$a(href = "https://osf.io/6x5pd/", "Interest piqued? Skeptical? Explore the methods and data here")),
    class = "container",
    style = "padding:5%"),

########################################### methods ########################################################### 

# div(h3("The nitty gritty"),
#     p("A Systematic Apprasial of Individual Effect Sizes in Aphasia Rehabilitation", style = "font-weight: bold;"),
#     p("Robert Cavanaugh, Lauren Terhorst, Alexander M. Swiderski, William D. Hula, William S. Evans"),
#     p("Poster: Academy of Aphasia 2020"),
#     br(),
#     p(methods1),
#     p(methods2),
#     style = "text-align:left; padding-left:10%; padding-right:10%"),

########################################### references ########################################################### 

div(h3("Selected References"),
    p("Antonucci, S., & Gilmore, N. (2019). Do aphasia core outcome sets require core analysis sets: Where do we go from here in single subject design research? 49th Clinical Aphasiology Conference."),
    p("Beeson, P. M., & Robey, R. R. (2006). Evaluating single-subject treatment research: Lessons learned from the aphasia literature. Neuropsychology Review, 16(4), 161–169. https://doi.org/10.1007/s11065-006-9013-7"),
    p("Bürkner, P. C. (2018). Advanced Bayesian multilevel modeling with the R package brms. R Journal. https://doi.org/10.32614/rj-2018-017"),
    p("Creet, E., Morris, J., Howard, D., & Nickels, L. (2019). Name it again! Investigating the effects of repeated naming attempts in aphasia. Aphasiology, 33(10), 1202–1226. https://doi.org/10.1080/02687038.2019.1622352"),
    p("Evans, W. S., Cavanaugh, R., Quique, Y., Boss, E., Dickey, M. W., Doyle, P. J., Starns, J. J., & Hula, W. D. (2020). BEARS - Balancing Effort, Accuracy, and Response Speed in semantic feature verification anomia treatment. Abstract for Platform Presentation, Annual Clinical Aphasiology Conference (Conference Cancelled)."),
    p("Goldfeld, K. (2019). simstudy: Simulation of Study Data (R package). https://cran.r-project.org/package=simstudy"),
    p("Jaeger, T. F. (2008). Categorical data analysis: Away from ANOVAs (transformation or not) and towards logit mixed models. Journal of Memory and Language, 59(4), 434–446. https://doi.org/10.1016/j.jml.2007.11.007"),
    p("King, T. S., & Chinchilli, V. M. (2001). A generalized concordance correlation coefficient for continuous and categorical data. Statistics in Medicine, 20(14), 2131–2147. https://doi.org/10.1002/sim.845"),
    p("Lambon Ralph, M. A., Snell, C., Fillingham, J. K., Conroy, P., & Sage, K. (2010). Predicting the outcome of anomia therapy for people with aphasia post CVA: both language and cognitive status are key predictors. Neuropsychological Rehabilitation, 20(2), 289–305. https://doi.org/10.1080/09602010903237875"),
    p("Landis, J. R., & Koch, G. G. (1977). An Application of Hierarchical Kappa-type Statistics in the Assessment of Majority Agreement among Multiple Observers. Biometrics, 33(2), 363–374. JSTOR. https://doi.org/10.2307/2529786"),
    p("Lee, J. B., & Cherney, L. R. (2018). Tau-U: A Quantitative Approach for Analysis of Single-Case Experimental Data in Aphasia. American Journal of Speech-Language Pathology, 27(1S), 495–503. https://doi.org/10.1044/2017_AJSLP-16-0197"),
    p("Lin, L. I.-K. (1989). A Concordance Correlation Coefficient to Evaluate Reproducibility. Biometrics, 45(1), 255–268. JSTOR. https://doi.org/10.2307/2532051"),
    p("Manolov, R., & Solanas, A. (2008). Comparing N = 1 Effect Size Indices in Presence of Autocorrelation. Behavior Modification, 32(6), 860–875. https://doi.org/10.1177/0145445508318866"),
    p("Parker, R. I., & Vannest, K. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. Behavior Therapy, 40(4), 357–367. https://doi.org/10.1016/j.beth.2008.10.006"),
    p("Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011). Combining nonoverlap and trend for single-case research: Tau-U. Behavior Therapy, 42(2), 284–299. https://doi.org/10.1016/j.beth.2010.08.006"),
    p("R Core Team. (2020). R: A language and environment for statistical computing (4.0.2). R Foundation for Statistical Computing. https://www.r-project.org/"),
    p("Wiley, R. W., & Rapp, B. (2018). Statistical analysis in Small-N Designs: Using linear mixed-effects modeling for evaluating intervention effectiveness. Aphasiology, 33(1), 1–30. https://doi.org/10.1080/02687038.2018.1454884"),
    style = "text-align:left; padding-left:10%; padding-right:10%"),
br(),
div(p("This work was inspired by the 2019 CAC roundtable led by Natalie Gilmore and Sharon Antonucci. Thanks to Natalie and Sam Harvey (La Trobe University) for their extremely helpful feedback on this vignette.", style = "text-align:center; padding-left:10%; padding-right:10%"),
    p("Did I goof somewhere? Do you have recommendations or questions? Contact me here:", style = "text-align:center;"),
    h2(tags$a(href = "https://github.com/rbcavanaugh/effect-sizes-scrollytelling/",
            icon("github")),
        tags$a(href = "https://robcavanaugh.com",
               icon("globe-americas")),
        tags$a(href = "https://twitter.com/Littlejohnsband",
               icon("twitter")),
       tags$a(href = "mailto:rob.cavanaugh@pitt.edu",
              icon("envelope")),
       style = "padding-left:20%; padding-right:20%"),
    p(icon('copyright'), "2021 Robert Cavanaugh"),
    h6("last updated: 1-9-21"),style = "text-align:center; padding:2.5%")
)



######################################################################################################
############################################## server ################################################
######################################################################################################

server <- function(input, output) {
  
    output$isItMobile <- renderText({
      ifelse(input$isMobile, "Not optimized for mobile: Recommend viewing in landscape", "")
    })
  

    ############# This is for the plots.... ########################
  
  output$distPlot <- renderImage({
    
    # defines what plot we want....(based on what section we're on)
    t = as.numeric(input$scr) 
    
    # defintes how subs are selected
    ls2 <- list(
        t1 = c(31, 34, 12, 15, 51, 56, 57, 84, 93, 99),
        t2 = c(31, 34, 12, 15, 51, 56, 57, 84, 93, 99),
        t3 =  c(51),#SMD
        t4 =  c(12, 51),#SMD
        t5 =  c(12, 51),#SMD
        t6 =  c(12, 51),#SMD
        t7  = c(15), #NAP
        t8  = c(51, 84, 93), #NAP
        t9 =  c(84), #TAU
        t10  = c(56, 57, 84), #TAU
        t11  = c(34), # PMG
        t12  = c(31, 34), #PMG
        t13  = c(99), #GLMM
        t14  = c(84, 99), #GLMMM
        t15  = c(84, 99), #GLMMM
        t16  = c(12),
        t17  = c(84, 99) #GLMMM#BMEM
    )
    
    # create variable sel for alpha
    if(!is.na(t) && t>0 && t<18){sel = ls2[[t]]}else{sel<-NA}
    
    p <- if(isTruthy(t==1)) df %>% # plots all 100
      ggplot(aes(x = session, y = mean_correct, shape = phase, color = sub_id,
                 alpha = ifelse(sub_id %in% sel, 0.975, 0.01))) +
      theme_scrolly()
    else if(isTruthy(t==2)) df %>% # plots all 9
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id)) +
      theme_scrolly()
    else if(isTruthy(t==3)) df %>%
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_smd1()
    else if(isTruthy(t==4)) df %>% #SMD 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_smd2()
    else if(isTruthy(t==5)) df %>% # SMD 3
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_smd3()
    else if(isTruthy(t==6)) df %>% #SMD 5.1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_smd4()
    else if(isTruthy(t==7)) df %>% # NAP 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_nap()
    else if(isTruthy(t==8)) df %>% # NAP 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_nap2()
    else if(isTruthy(t==9)) df %>% # tau 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_tau1()
    else if(isTruthy(t==10)) df %>% # tau 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_tau2()
    else if(isTruthy(t==11)) df %>% # pmg 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_pmg1()
    else if(isTruthy(t==12)) df %>% # pmg 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_pmg2()
    else if(isTruthy(t==13)) df %>% # glmm 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, #shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_glmm1()
    else if(isTruthy(t==14)) df %>% # glmm 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, #shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_glmm2()
    else if(isTruthy(t==15)) df %>% # glmm 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, #shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_glmm3()
    else if(isTruthy(t==16)) df %>% # bmem 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_bmem1()
    else if(isTruthy(t==17)) df %>%
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      theme_bmem2()
    else NULL
    
    # temporary output file output
     outfile <- tempfile(fileext='.png')
    # render image
     ggsave(filename = here("www", "outfile.png"), plot = p, bg = "transparent", device = "png",
                height = 4.5, width = 6, units = 'in', dpi = 150)
    # unlink image
     unlink(p)
    # refer to image
     list(src = "www/outfile.png",
          contentType = 'image/svg',
          alt = "This is also alternate text")

  },  deleteFile = F) # don't keep file
  
  # output
  output$scr <- renderScrollytell({
    scrollytell()
  })
  
  shinyjs::onclick("scrll", runjs("(window.scroll(0,findPos(document.getElementById('scr'))))"))
}


# Run the application
shinyApp(ui = ui, server = server)
