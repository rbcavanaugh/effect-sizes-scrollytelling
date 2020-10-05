#### Paragraphs ####

ls <- list()
# p1 #### before scrolly sections
ls$text1a <- "Single-case designs are common aphasia research. They can be more cost-effective than larger trials while providing a close look at individual differences in treatment response. Effect sizes describe the magnitude of treatment response and are essential to single-case design research. They can validate the clinical relevance of treatment. They are used as dependent variables in studies predicting treatment response. In both cases, effect size accuracy and precision is important"

ls$text1b <- "Minimally, effect size measures should 1) be interpretable, 2) distinguish non-responders from responders, from great responders, 3) account for trends in the baseline phase, and 4) provide an estimate of uncertainty around the estimated effect size."


# p2 #### first scrolly section
ls$text2a <- "Estimating effect sizes in aphasia single-case designs can be challenging. Response to treatment is noisy and variable. Baseline performance is not always stable and can trend upwards. Performance is typically autocorrelated and variance is rarely consistent across all time points."

ls$text2b <- "Effect sizes are not equal in their ability to account for these issues (e.g. Archer et al., 2019; Howard et al., 2015). Understanding the methods, strengths, and weaknesses of each effect size measure and how they are related can guide the selection and deployment of effect size measures in single-case design studies. "


ls$text3a <- "To systematically evaluate and compare effect sizes used in aphasia single case design reserach, we simulated trial-level performance for 100 hypothetical participants with aphasia during a multiple-baseline anomia study. Our simulated study described performance on 30 words across 5 baseline and 10 treatment probes."

ls$text3b <- "We calculated effect sizes using 6 methods from the aphasia single-case design literature and the agreement between these methods. This vignette provides a brief walk-through of each measure and summarizes the results of our findings." 

# SMD ####
ls$SMD1 <- "Standardized mean difference (Busk & Serlin, 1992) and it's variant published by Beeson & Robey (2006) is commonly used in aphasia single-case reserach."

ls$SMD1a <- 'SMD is the difference in average performance between treatment (or post-treatment) and baseline divided by the variability during baseline. It is typically interpreted based on established benchmarks in terms of “small,” “medium,” and “large."'

ls$SMD2 <- "Intuitively, SMD is influenced by the variability in baseline performance. However, the degree to which this is the case can be startling. According to existing benchmarks, blue would be considered a very large effect size (large = 10.1) while purple is equivalent to the benchmark for small (4.0). "

ls$SMD2a <- "Across our entire sample, baseline sd accounted for 33% of the variance in SMD scores."

ls$SMD3 <- "In these cases where baseline variability approaches zero, one option is to pool variance between baseline and treatment phases. But variability may not be even across time and it's not clear that pooling is appropriate"

ls$SMD3a <- "SMD now does not meet the benchmark for 'small' even though the participant demonstrates marked improvement. Whether researchers can elect to pool baseline variability on a case-by-case basis or must pool across all participants is not clear. Furthermore, how benchmarks might shift based on pooled standard deviation has not been studied evaulated."

ls$SMD_eq <- "\\[{smd} = \\frac{\\bar{x}_{treatment}-\\bar{x}_{baseline}}{\\sigma_{baseline}}\\]"
ls$SMD_eq2 <- "\\[{smd} = \\frac{\\bar{x}_{treatment}-\\bar{x}_{baseline}}{\\sigma_{pooled}}\\]"
ls$SMD_eq3 <- "\\[{smd} = \\frac{\\bar{x}_{post-treatment}-\\bar{x}_{baseline}}{\\sigma_{baseline}}\\]"

ls$SMD4a <- "Technically, it's not clear that these benchmarks from Beeson and Robey (2006) apply to the original SMD, as they were calculated based off of post-treatment scores instead of the treatment phase."

ls$SMD4b <- "Re-calculating SMD approximating this method (averaging over the last 2 treatment sessions in lieu of post-treatment), we see that the SMD scores increase significantly for blue, such that the result is not interpretable given the current benchmarks. Pooling the standard deviation in this case is unlikely to alleviate the problem, as performance appears to be relatively stable after session 11."

# NAP and Tau-U
ls$NAP1 <- "Non-overlap of all pairs (NAP; Parker & Vannest, 2009) is a non-parametric effect size."

ls$NAP2 <- "NAP characterizes the degree of overlap for all pair-wise comparisons of data points between two treatment phases. It can be interpreted as “the probability that a score drawn at random from a treatment phase will exceed (overlap) that of a score drawn at random from a baseline phase” (Parker & Vannest, 2009 p. 359). It ranges from 0 to 1"

ls$NAP3 <- "10 out of 50 total comparisons are shown here. In total, there are 4 overlapping and 46 non-overlapping data points. NAP = 46/50 or 0.92."

ls$NAP4 <- "NAP is susceptible to ceiling effects and struggles to differentiate between treatment responders if there is a meaningful improvement from the first day treatment (Wolery, 2010). For example, NAP = 1 for each participant, even though there are clear differences in treatment response"

ls$TAU <- "Tau-U (Parker et al. 2011) is an extension of NAP that also accounts for baseline trends in performance. Effectively, it represents NAP, corrected for any monotonic trends in the baseline phase. Conceptually, values should range from -1 to 1. Larger values indicate increasing independence between study phases."

ls$TAU1 <- "However, Tau-U is not mathematically constrained between -1 and 1, and may notably exceed 1 when correcting for downward trends in the baseline phase or in cases where the baseline phase has a similar or longer length than the treatment phase (Tarlow, 2017). Some authors have gone so far as to recommend discontinuing the use of Tau-U until these issues can be resolved (Brossart et al. 2018)."

ls$TAU2 <- "For example, in the red and purple cases, the treatment effect appears equivalent, but Tau-U for purple = 1.06 whil Tau-U for red = 0.96. In the case of downward trends, it may be prudent not to correct for the baseline trend, such that Tau-U (purple) = 1.0. On the other hand, in this case Tau-U does clearly adjust for the rising baseline trend (pink)."

ls$PMG1 <- "Lambon Ralph and colleagues (2010) proposed the proportion of potential maximal gain (PMG). PMG is intuitive - it is the proportion of improvement gained (red arrow) to the amount of improvement possible after the baseline phase (blue arrow)."

ls$PMG_eq <- "\\[{pmg} = \\frac{\\bar{x}_{post-treatment}-\\bar{x}_{baseline}}{N_{items} - \\bar{x}_{baseline}}\\]"
  
ls$PMG2 <- "Unfortunately, there is no confidence interval available for PMG and PMG is known to be susceptible to ceiling effects. Additionally, decreasing the number of items and baseline item difficulty can bias PMG upwards by improving baseline performance. For example, a participant who improves from 4/20 to 8/20 items (PMG = 0.25) is half that of a participant who improves from 12/20 to 16/20 items (PMG  = 0.50), but equivalent to the participant who improves from 8/40 to 16/40 (PMG = 0.25). It's not clear that this is a desirable behavior for an effect size"

ls$PMG3 <- "This concern is illustrated here, where both cases have improved by approximately 20 words, but PMG for dark purple = 1 while pmg for light purple = 0.76."

ls$GLMM1 <- "Generalized linear mixed effects models (GLMMs) can be used to calculate individual effect sizes. Here, we've replicated the approach from Meier et al., (2019). This technique models each item-level response (correct or incorrect) as a function of session (time) for each participant." 

ls$GLMM2 <- "The effect size is calculated from the session coefficient, exponentiated from logits to an odds ratio. It is interpreted as the increase in the odds of a correct response per session. For instance, a session coefficient of 0.7 indicates that the odds of a correct response doubles with each treatment session (exp(0.7) = 2.0). The lme4 syntax for these models is:"

ls$GLMM_eq <-"glmer(response ~ session + (session | item), family = binomial)"

ls$GLMM3 <- "GLMMs have a number of general drawbacks (not discussed here). In this case, it’s not clear whether this GLMM approach adequately accounts for baseline trends. A single slope though the entire time series still may not adequately adjust effect sizes in the presence of a baseline slope and may underestimate effect sizes when the baseline slope is relatively flat."

ls$GLMM4 <- "In this example, GLMM effect sizes are roughly equivalent, even though red has a significant rising baseline trend which needs to be accounted for. More complex models which include a phase (baseline vs. treatment) variable may help to resolve these concerns." 


ls$BMEM1 <- "Bayesian mixed effects models (BMEMs) have been used in single-case design research in fields outside of aphasiology. Our reserach group recently deployed a bayesian approach (Evans et al., 2020) to calculating effect sizes. In this study, we used an interrupted time series approach (Hutiema, 2000) which models performance as a function of the slope during the baseline phase, level change between the last baseline and first treatment session, and the change in slope between baseline and treatment:"

ls$BMEM1a <- "This approach includes multiple participants in the same model and derives individual effect sizes by subtracting model’s posterior predictons for each participant at the last baseline probe from the final treatment probe. The median of the difference in posterior predictors thus describes the number of items improved during treatment. Because any trends in baseline performance are characterized in the model structure, this approach may reasonably account for improvement due to repeated testing during the baseline phase."

ls$BMEM_eq <- "\\[Y_t=\\beta_0+\\beta_1T_t+\\beta_2D_t+\\beta_3[T_t-(n_1+1)]D_t+\\epsilon_t\\]"

ls$BMEM2 <- "We see slightly more divergence between these cases using the BMEM approach compared to GLMM. the BMEM size suggests that the red case gained about 17.5 items in response to treatment while the orange case gained about 19.5. However, it's again not clear whether this approach adequately accounts for the baseline trend"


#################### summary ################

sum1 <- "These 6 effect sizes each have their own strengths and weaknesses. Knowing that there is no 'gold standard,' we sought to detemine how interchangable these effect size measures are in single case design reserach. To calculate an index of agreement between all measures, we z-scored each effect size measure and and calculated concordance correlation coefficients (Lin 1989) using a non-parametic method to account for non-linear relationships."

sum2 <- "In the plot below, scatterplots visualize the relationship between effect sizes (lower triangle) and concordance correlation coefficients (upper triangle). Concordance correlation coefficients less than 0.40 are considered poor. Coefficients between 0.40 and 0.75 are considered good to fair. Coefficients greater than 0.75 are considered good to excellent. Scatterplots also reveal cases where agreement is inconsistent across effect sizes."

sum3 <- "Given its susceptibility to commonly observed changes in variance and a number of straightforward alternatives, we suggest that it's time to retire standardized mean difference in aphasia single-case designs. Generalized linear mixed effects models and their Bayesian extensions satisfy most criteria for effect sizes in that they can account for baseline trends, are relatively unsusceptible to ceiling effects, and provide a clear estimate of treatment effect magnitude and an estimate of uncertainty."

sum4 <- "We acknowledge there are cases when mixed effects models are too complex. In this situations, non-overlap of all pairs and Tau-U appear to provide similar results when treatments effects are expected to be gradual and/or small but lose sensitivity for characterizing differences between participants when effect sizes are medium to large. The proportion of potential maximal gain provides an interpretable, reasonable approximation of the mixed effects modeling approaches but doesn’t provide a measure of certainty. PMG may be best utilized when combined with a method that is powered to indicate whether or not changes are significant (e.g. weighted statistics)."

sum5 <- "No methods appear to provde a satisfactory solution to the problem of rising baseline. this problem is likely best addressed with careful experimental design, which can minimize the risk of baseline trends."


#################### methods ####################

methods1 <- "Data were simulated for 100 hypothetical people with aphasia participating in a multiple baseline study. Each participant reflected performance on 30 treated items at 5 baseline and 10 treatment probe timepoints.Data were simulated using the R package SimStudy (Goldfeld, 2019), following the general modeling approach described by Manolov and Solanas (2008), in which the probability of a given correct response is a function of the baseline slope (β1), level change (β2) between baseline and treatment phases, and slope change (β3) between baseline and treatment phases. Beta-coefficients for the baseline slope, level change, and slope change variables were set at 0.06, 0.3, and 0.15 respectively (Manolov and Solanas, 2008). The intercept, β0, was defined by a normal distribution with a mean of -1.75 and variance of .25, randomly assigned to each time series. Participant level variance was characterized by a uniform distribution between 0 and 2. Item level effects were modeled by adding a term to describe item difficulty, approximating a normal distribution with a variance of 0.6. A logistic link function was used to calculate the probability of a correct response for each participant, item, session, and condition. Binomial responses were probabilistically simulated with a lag-1 autocorrelation of 0.5. A multi-level model recovered the parameteres effectively."
  
methods2 <- "SMD was calculated per the methods of Busk and Serlin (1992), NAP and Tau-U were calculated per the methods of Parker and Vannest (2009; 2015); Tau-U with a correction for baseline trend: Tau UA VS. B – TREND A. PMG was calculated as described by Lambon Ralph and colleagues (2010), but using the final session as post-treatment performance. Effect sizes estimated through GLMM replicated the approach described by Meier (2019). BMEM effect sizes mirrored the approach used by Evans et al (2020). In the larger study, calculating SMD similar to Beeson & Robey (2006) did not meaningfully change the results."


