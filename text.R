#### Paragraphs ####

ls <- list()
# p1 ####
ls$text1a <- "Single-case designs are common aphasia research. They can be more cost-effective than larger trials while providing a close look at individual differences in treatment response. Effect sizes describe the magnitude of treatment response and are essential to single-case design research. They can validate the clinical relevance of treatment. They are used as dependent variables in studies predicting treatment response. In both cases, effect size accuracy and precision is important"

# p2 ####
ls$text2a <- "However, estimating effect sizes in aphasia single-case designs can be challenging. Response to treatment is noisy and variable. Baseline performancae is not always stable, masking treatment effects. Effect sizes are not likely to be equal in their ability to describe the magnitude of change"


ls$text2b <- "In this study, we set out to compare the effect size measures used in aphasia signle-case design reserach. First, we simulated performance for 100 hypothetical participants with aphasia during a multiple-baseline anomia study. Then, we derived effect sizes using 6 measures used in the aphasia single-case design literature. Last, we calculated the agreement between these measures."

ls$text2c <- "This vignette provides a brief walk-through of each measure and summarizes the results of our findings." 

# SMD ####
ls$SMD1 <- "Standardized mean difference (Busk & Serlin, 1992) and it's variant published by Beeson & Robey (2006) is commonly used in aphasia single-case reserach."

ls$SMD1a <- 'SMD is the difference in average performance between treatment (or post-treatment) and baseline divided by the variability during baseline. It is typically interpreted based on established benchmarks in terms of “small,” “medium,” and “large."'

ls$SMD2 <- "Intuitively, SMD is highly influenced by the variability in baseline performance. However, the degree to which this is the case can be startling."

ls$SMD3 <- "When the baseline variability approaches zero, one option is to pool variance between baseline and treatment phases. But variability may not be even across time - it's not clear we should do so"

ls$SMD_eq <- "\\[{smd} = \\frac{\\bar{x}_{treatment}-\\bar{x}_{baseline}}{\\sigma_{baseline}}\\]"
ls$SMD_eq2 <- "\\[{smd} = \\frac{\\bar{x}_{treatment}-\\bar{x}_{baseline}}{\\sigma_{pooled}}\\]"

# NAP and Tau-U
ls$NAP1 <- "Non-overlap of all pairs (NAP; Parker & Vannest, 2009) is a non-parametric effect sizes."

ls$NAP2 <- "NAP characterizes the degree of overlap for all pair-wise comparisons of data points between two phases of treatment. Thus, NAP is the proportion of all comparisons in which the second phase exceeds the first. It can be interpreted as “the probability that a score drawn at random from a treatment phase will exceed (overlap) that of a score drawn at random from a baseline phase” (Parker & Vannest, 2009 p. 359). It ranges from 0 to 1"

ls$NAP3 <- "Here, only one treatment probe (session 6) is less than sessions 1, 2, and 3. Sessions 7 and 9 are equal to session 3 (ties count as .5). There are 50 total comparisons. Thus there are a total of 4 overlapping datapoints, and 46 non-overlapping data points. NAP = 46/50 or 0.92."

ls$NAP4 <- "NAP is susceptible to ceiling effects and struggles to differentiate between treatment responders if there is a significant and lasting improvement from the first or second day of treatment or low variability at baseline. For example, NAP = 1 for each participant here, even though there are clear differences in treatment response"

ls$TAU <- "Tau-U Parker et al. (2011) is an extension of NAP (and the original Tau, which rescaled NAP to [-1,1] that also accounts for baseline trends in performance (shown right). Effectively, it represents NAP or Tau, corrected for any monotonic trends in the baseline phase. Conceptually, values range from -1 to 1 suggesting increasing independence between study phases."


ls$TAU1 <- "However, Tau-U is not mathematically constrained between -1 and 1, and may notably exceed 1 when correcting for downward trends in the baseline phase (Tarlow, 2017) or in cases where the baseline phase has a similar or longer length than the treatment phase."

ls$TAU2 <- "For example, in the red and purple cases, the treatment effect appears equivalent, but Tau-U for pink = 1.06 whil Tau-U for red = 0.96. In these cases it may be prudent not to correct for the downward baseline trend. On the otherhand, for the orange case, Tau-U = 0.80, which reflects the baseline trend in performance."

ls$PMG1 <- "Lambon Ralph and colleagues (2010) proposed the proportion of potential maximal gain (PMG). PMG is bounded between 0 and 1 and is intuitively interpreted as the proportion of improvement relative to the amount of improvement possible after the baseline phase."

ls$PMG_eq <- "\\[{pmg} = \\frac{\\bar{x}_{post-treatment}-\\bar{x}_{baseline}}{N_{items} - \\bar{x}_{baseline}}\\]"
  
ls$PMG2 <- "However, PMG has been noted to be susceptible to ceiling effects. PMG can be affected by the level of baseline performance and the number of items treated. For example, a participant who improves from 4/20 to 8/20 items (PMG = 0.25)  is half that of a participant who improves from 12/20 to 16/20 items (PMG  = 0.50), but equivalent to the participant who improves from 8/40 to 16/40 (PMG = 0.25)."

ls$PMG3 <- "This behavior is illustrated here, where both cases have improved by approximately 20 words, but PMG for dark purple = 1 while pmg for light purple = .76. In other words, decreasing the number of items and item difficulty can bias PMG upwards by improving baseline performance."

ls$GLMM1 <- "Generalized linear mixed effects models (GLMMs) can be used to calculate individual effect sizes. GLMMs can handle unbalanced and missing data, account for autocorrelation, model non-linear trends, test between-condition effects, and account for baseline trends in performance." 

ls$GLMM2 <- "Here, we've replicated the approach from Meier et al., (2019). This technique models trial-level responses (correct or incorrect) as a function of session (time) for each participant. The time coefficient is interpreted as the increase in the odds of a correct response per session. For instance, an odds ratio coefficient of 2 suggests that the odds of a correct response double with every treatment session. The lme4 syntax for these models is:"

ls$GLMM_eq <-"glmer(response ~ session + (session | item), family = binomial)"

ls$GLMM3 <- "GLMMs have a number of general drawbacks (not discussed here). Specific to estimating effect sizes, it’s not clear whether this GLMM approach adequately accounts for baseline trends. A single linear slope though the entire time series still may not adjust effect sizes in the presence of a baseline slope and may underestimate effect sizes when the baseline slope is relatively flat."

ls$GLMM4 <- "In this case, the GLMM effect sizes are roughly equivalent (red = 1.37, orange 1.39) even though red has a significant rising baseline trend which needs to be accounted for. Adding a phase and phase-session interaction and evaluating the interaction term can account for the baseline change here, but assumes that the linear baseline trend will continue without intervention and may under-estimate the treatment effect"

ls$BMEM1 <- "Bayesian mixed effects models (BMEMs) have been used in single-case design research in fields outside of aphasiology and now recently in aphasiology (Evans et al., 2020). In this study, we implemented an interrupted time series modeling approach (Hutiema, 2000) which models performance as a function of the slope during the baseline phase, level change between the last baseline and first treatment session, and the change in slope between baseline and treatment."

ls$BMEM1a <- "This approach includes all participants in the same model and derives individual effect sizes by subtracting model’s posterior predictons for each participant at the last baseline probe from the final treatment probe. The median of the posterior predictors thus describes the number of items improved during treatment. Because any trends in baseline performance are characterized in the model structure, this approach should reasonably account for improvement due to repeated testing during the baseline phase."

ls$BMEM_eq <- "\\[Y_t=\\beta_0+\\beta_1T_t+\\beta_2D_t+\\beta_3[T_t-(n_1+1)]D_t+\\epsilon_t\\]"

ls$BMEM2 <- "We see slightly more divergence between these cases using the BMEM approach compared to GLMM. the BMEM size suggests that the red case gained about 17.5 items in response to treatment while the orange case gained about 19.5."

ls$BMEM3 <- "It's worth noting that other factors can have small influences on the BMEM effect sizes described here. Because they are derived from a group model (in this study, 10 models of n = 10), the estimates are subject to shrinkage, or the tendence for individual estimates to bias towards the mean performance from individuals in the group. This can be advantageous in that it may reduce the risk of interpreting treatment effects which are not representative of the general population. However, it is also a likely source of some disagreement between BMEM and other measures."

texts <- c('text1a', 'text1b', 'text2a', 'text2b', 'text2c', 'SMD1', 'SMD1a', 'SMD2', 'SMD3',
           'SMD_eq', 'SMD_eq2', 'NAP1', 'NAP2', 'NAP3', 'NAP4', 'TAU', 'TAU1', 'TAU2', 'PMG1',
           'PMG_eq', 'PMG2', 'PMG3', 'GLMM1', 'GLMM2', 'GLMM_eq', 'GLMM3', 'GLMM4', 'BMEM1', 'BMEM1a',
           'BMEM_eq', 'BMEM2', 'BMEM3')

