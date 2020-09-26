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
    tags$link(rel="stylesheet", media="screen and (max-device-width: 767px)", href="style_mobile2.css")
  ),
  withMathJax(),
  div(class="demo_wrap",
          h1("Effect size ScrollyTelling: In development..."),
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
      h2("Effect sizes in Aphasia single-case design", style = "text-align: center; padding-bottom: 3%"),
      p(ls$text1a, style = 'text-align: center;')
  ),

  ########################################### scrolly sections ###########################################################                      
  fluidRow(scrolly_container(width = 4,
                              "scr",
                             scrolly_graph(imageOutput("distPlot")),
                             scrolly_sections(
                               scrolly_section(id = "1", h3("Effect sizes in single-case design"),
                                               p(ls$text2a),
                                               p(ls$text2b),
                                               p(ls$text2c)),
                               scrolly_section(id = "2", h3("Effect Sizes in Aphasiology"),
                                               p("Some notes about this data")),
                               scrolly_section(id = "3", h3("Standardized Mean Difference"),
                                               p(ls$SMD1),
                                               p(ls$SMD1a),
                                               h4(ls$SMD_eq)),
                               scrolly_section(id = "4", h3("Standardized Mean Difference"),
                                               p(ls$SMD2)),
                               scrolly_section(id = "5", h3("Standardized Mean Difference"),
                                               p(ls$SMD3),
                                               h4(ls$SMD_eq2)),
                               scrolly_section(id = "6", h3("Non-overlap of All Pairs"),
                                               p(ls$NAP1),
                                               p(ls$NAP2),
                                               p(ls$NAP3)),
                               scrolly_section(id = "7", h3("Non-overlap of All Pairs"),
                                               p(ls$NAP4)),
                               scrolly_section(id = "8", h3("Tau-U"),
                                               p(ls$TAU)),
                               scrolly_section(id = "9", h3("Tau-U"),
                                               p(ls$TAU1),
                                               p(ls$TAU2)),
                               scrolly_section(id = "10", h3("Proportion of Potential Maximal Gain"),
                                               p(ls$PMG1),
                                               h4(ls$PMG_eq)),
                               scrolly_section(id = "11", h3("Proportion of Potential Maximal Gain"),
                                               p(ls$PMG2),
                                               p(ls$PMG3)),
                               scrolly_section(id = "12", h3("Generalized linear mixed-effects models"),
                                               p(ls$GLMM1),
                                               p(ls$GLMM2),
                                               code(ls$GLMM_eq)),
                               scrolly_section(id = "13", h3("Generalized linear mixed-effects models"),
                                               p(ls$GLMM3),
                                               p(ls$GLMM4)),
                               scrolly_section(id = "14", h3("Bayesian GLMMs"),
                                               p(ls$BMEM1),
                                               p(ls$BMEM_eq),
                                               p(ls$BMEM1a)),
                               scrolly_section(id = "15", h3("Bayesian GLMMs"),
                                               p(ls$BMEM2),
                                               p(ls$BMEM3),
                                               br(),
                                               br()),
                            )),

########################################### summary ###########################################################         

  div(h3("Summary"),
    p("It's clear that these 6 effect sizes each have their own strengths and weaknesses. Knowing that there is no 'gold standard,' we sought to detemine how interchangable these effect size measures are in single case design reserach. To calculate an index of agreement between all measures, we z-scored each effect size measure and and calculated concordance correlation coefficients (Lin 1989)."),
    br(),
  p("In the plot below, scatterplots visualize the relationship between effect sizes (lower triangle) and concordance correlation coefficients (upper triangle). Concordance correlation coefficients less than 0.40 are considered poor. Coefficients between 0.40 and 0.75 are considered good to fair. Coefficients greater than 0.75 are considered good to excellent. Scatterplots also reveal cases where agreement is inconsistent across effect sizes."
  ), style="text-align: left; padding-left:10%; padding-right:10%; padding-top:10%")
),
  div(img(src="shiny_plot.svg", heigth = "70%", width = "70%"), style="text-align: center;padding:5%"),
  div(h2(
    style = "text-align:center; face:bold",
    tags$a(href = "https://osf.io/6x5pd/", "Interest piqued? Skeptical? Explore the methods and data here")),
    class = "container",
    style = "height:40vh; line-height:40vh;padding:5%"),

########################################### methods ########################################################### 

div(h3("The nitty gritty"),
    p("Data were simulated for 100 hypothetical people with aphasia participating in a multiple baseline study. Each participant reflected performance on 30 treated items at 5 baseline and 10 treatment probe timepoints.Data were simulated using the R package SimStudy (Goldfeld, 2019), following the general modeling approach described by Manolov and Solanas (2008), in which the probability of a given correct response is a function of the baseline slope (β1), level change (β2) between baseline and treatment phases, and slope change (β3) between baseline and treatment phases. Beta-coefficients for the baseline slope, level change, and slope change variables were set at 0.06, 0.3, and 0.15 respectively (Manolov and Solanas, 2008). The intercept, β0, was defined by a normal distribution with a mean of -1.75 and variance of .25, randomly assigned to each time series. Participant level variance was characterized by a uniform distribution between 0 and 2. Item level effects were modeled by adding a term to describe item difficulty, approximating a normal distribution with a variance of 0.6. A logistic link function was used to calculate the probability of a correct response for each participant, item, session, and condition. Binomial responses were probabilistically simulated with a lag-1 autocorrelation of 0.5. A multi-level model recovered the parameteres effectively."),
p("SMD was calculated per the methods of Busk and Serlin (1992), NAP and Tau-U were calculated per the methods of Parker and Vannest (2009; 2015); Tau-U with a correction for baseline trend: Tau UA VS. B – TREND A. PMG was calculated as described by Lambon Ralph and colleagues (2010), but using the final session as post-treatment performance. Effect sizes estimated through GLMM replicated the approach described by Meier (2019). BMEM effect sizes mirrored the approach used by Evans et al (2020). In the larger study, calculating SMD similar to Beeson & Robey (2006) did not meaningfully change the results."),
  style = "text-align:left; padding-left:10%; padding-right:10%"),

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
    style = "text-align:left; padding-left:10%; padding-right:10%"),
div(p(icon('copyright'), "2020 Robert Cavanaugh"),
    h6("last updated: 9-19-20"),style = "text-align:center; padding:5%")
)



######################################################################################################
############################################## server ################################################
######################################################################################################

server <- function(input, output) {
  
    output$isItMobile <- renderText({
      ifelse(input$isMobile, "Recommend viewing in landscape on mobile", "")
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
        t6  = c(15), #NAP
        t7  = c(51, 84, 93), #NAP
        t8 =  c(84), #TAU
        t9  = c(56, 57, 84), #TAU
        t10  = c(34), # PMG
        t11  = c(31, 34), #PMG
        t12  = c(99), #GLMM
        t13  = c(84, 99), #GLMMM
        t14  = c(12),
        t13  = c(84, 99) #GLMMM#BMEM
    )
    
    # create variable sel for alpha
    if(between(t, 1,15)) sel = ls2[[t]]
    
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
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_smd1()
    else if(isTruthy(t==4)) df %>% #SMD 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_smd2()
    else if(isTruthy(t==5)) df %>% # SMD 3
      mutate(mean_correct = ifelse(sub_id == 12 & phase == 'baseline', 0, mean_correct)) %>%
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_smd3()
    else if(isTruthy(t==6)) df %>% # NAP 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_nap()
    else if(isTruthy(t==7)) df %>% # NAP 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_nap2()
    else if(isTruthy(t==8)) df %>% # tau 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_tau1()
    else if(isTruthy(t==9)) df %>% # tau 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_tau2()
    else if(isTruthy(t==10)) df %>% # pmg 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_pmg1()
    else if(isTruthy(t==11)) df %>% # pmg 2
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() +
      theme_pmg2()
    else if(isTruthy(t==12)) df %>% # glmm 1
      filter(sub_id %in% ls2[[1]]) %>%
      # mutate(preds = ifelse(sub_id == 84, NA, preds),
      #        lci = ifelse(sub_id == 84, NA, lci),
      #        uci = ifelse(sub_id == 84, NA, uci)) %>%
      ggplot(aes(x = session, y = mean_correct, #shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      geom_ribbon(data = df %>% filter(sub_id == 99),
                  aes(x = session, ymin = lci, ymax = uci),  alpha = .2, linetype = 0,) + #linetype = 0,
      geom_line(data = df %>% filter(sub_id == 99),
                aes(y = preds, x = session),size = .75)
    else if(isTruthy(t==13)) df %>% # glmm 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, #shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      geom_ribbon(data = df %>% filter(sub_id == 84 | sub_id == 99),
                  aes(x = session, ymin = lci, ymax = uci),  alpha = .1, linetype = 0,) + #
      geom_line(data = df %>% filter(sub_id == 99),
                aes(y = preds, x = session),size = .75) +
      annotate(geom = "text", x = 6, y = .85,
               label = "Magenta = 1.40x/session", size = 5, family = 'roboto', hjust = "left",
               color = "violetred3") +
      annotate(geom = "text", x = 9, y = .35,
             label = "Orange: 1.45x/session", size = 5, family = 'roboto', hjust = "left",
             color = "orangered3")
    else if(isTruthy(t==14)) df %>% # glmm 1
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.05))) +
      theme_scrolly() +
      geom_smooth(data = df %>% filter(sub_id == 12), method = "lm", se = FALSE, formula = y~x) +
      annotate(geom = 'segment', x = 5, xend = 6.02, y = .23, yend = .39, size = .75, color = 'darkblue',
               linetype = 'dashed') +
      annotate(geom = "text", x = 1, y = .15,
               label = TeX('$\\beta_1: BaselineSlope$', output = 'character'),
               size = 5, family = 'roboto', hjust = "left",
               color = "darkblue", parse = T,fontface = "bold")  + 
      annotate("segment", x = 7.5, y = 0.2, xend = 5.7, yend = .32,
               color = 'darkblue',
               arrow = arrow(ends = 'last', type = 'closed',length = unit(0.25, "cm"))) + 
      annotate(geom = "text", x = 7.7, y = .2,
               label = TeX('$\\beta_2: LevelChange$', output = 'character'),
               size = 5, family = 'roboto', hjust = "left",
               color = "darkblue", parse = T, fontface = "bold")   + 
      annotate("segment", x = 6, y = 0.39, xend = 14.5, yend = .42,
               color = 'darkblue')  + 
      annotate("segment", x = 11.5, y = 0.64, xend = 11.5, yend = .42,
               color = 'darkblue',
               arrow = arrow(ends = 'both', type = 'closed',length = unit(0.25, "cm"))) +
      annotate(geom = "text", x = 9, y = .37,
               label = TeX('$\\beta_3: SlopeChange$', output = 'character'),
               size = 5, family = 'roboto', hjust = "left",
               color = "darkblue", parse = T, fontface = "bold") +
      annotate("segment", x = 5, y = 0.255, xend = 5, yend = .81,
               color = 'red', arrow = arrow(ends = 'both', type = 'closed',length = unit(0.25, "cm")))+
      annotate(geom = "text", x = 1, y = .85,
               label = "difference between\nposterior samples:\n+16 words",
               size = 4, family = 'roboto', hjust = "left",
               color = "red") +
      annotate(geom = 'segment', x = 5, xend = 15, y = .82, yend = .82, size = .75, color = 'darkblue',
               linetype = 'dotted')
    else if(isTruthy(t==15)) df %>%
      filter(sub_id %in% ls2[[1]]) %>%
      ggplot(aes(x = session, y = mean_correct, shape = phase,
                 color = sub_id, alpha = ifelse(sub_id %in% sel, .85, 0.25))) +
      theme_scrolly() + 
      annotate(geom = "text", x = 6, y = .85,
               label = "Magenta = +17.5 words", size = 5, family = 'roboto', hjust = "left",
               color = "violetred3") +
      annotate(geom = "text", x = 9, y = .35,
               label = "Orange: +19.5 words", size = 5, family = 'roboto', hjust = "left",
               color = "orangered3")
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
