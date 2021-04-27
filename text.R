#### Paragraphs ####

ls <- list()
# p1 #### before scrolly sections
ls$text1a <- "Single-case experimental design studies are common in aphasia research. These studies (also referred to as single-subject experimental design) focus on treatment response at the individual level and establish experimental control within each participant rather than using a control group. Single-case experimental design studies typically include at least 2-4 participants but methods employed in these studies are often extended to within-subject case-series designs with upwards of thirty participants (e.g., Gilmore et al., 2018), which can be used to test theories and explore individual differences in treatment response. While not a replacement for group-level clinical trials, single-case designs and their case-series extensions are a cost-effective method for establishing preliminary treatment efficacy in early phase research."

ls$text1a2 <- "Effect sizes are an essential measure of treatment efficacy in these studies, describing the magnitude of treatment response. They validate the clinical relevance of treatment and are often used in within-subject case-series design studies to explore underlying cognitive mechansisms. In both cases, effect size accuracy and precision are important."


# p2 #### first scrolly section
ls$text2a <- "Estimating effect sizes in aphasia single-case designs can be challenging. Response to treatment is noisy and variable. Baseline performance is not always stable and can trend upwards or downwards. Performance across time is autocorrelated and variance is rarely consistent across all time points. These issues make it difficult to isolate and specify the true effects of an intervention."

ls$text2b <- "Effect sizes are not equal in their ability to account for these issues (e.g. Archer et al., 2019; Howard et al., 2015). Understanding the methods, strengths, and weaknesses of each effect size measure and how they are related can guide the selection and deployment of effect size measures in single-case design studies."


ls$text3a <- "To compare effect sizes used in aphasia single case design reserach, we simulated trial-level performance for 500 participants with aphasia during a hypothetical single-case design anomia study. Our simulated study described performance on 30 words across 5 baseline and 10 treatment probes."

ls$text3b <- "We calculated effect sizes using 6 methods identified in the aphasia single-case design literature (Gilmore & Antonucci, 2019) and the agreement between these methods. The methods are: Standardized Mean Difference (Busk & Serlin, 1992), Non-overlap of All Pairs (Parker & Vannest., 2009), Tau-U (Parker et al., 2011), Proportion of Maximal Gain (Lambon Ralph et al., 2010), Generalized linear mixed-effects models (Wiley & Rapp 2018) and Bayesian mixed-effects models (based on Hutiema, 2000)." 

# SMD ####
ls$SMD1 <- "Standardized mean difference (SMD; Busk & Serlin, 1992) and its variant published by Beeson & Robey (2006) is commonly used in aphasia single-case design reserach."

ls$SMD1a <- 'SMD is the difference in average performance between treatment (or post-treatment) and baseline divided by the variability during baseline.'

ls$SMD1b <- 'SMD is typically interpreted based on established benchmarks in terms of “small,” “medium,” and “large."'

ls$SMD2 <- "SMD can be greatly influenced by variability in baseline performance. According to existing benchmarks for anomia treatments, the effect size corresponding to the blue trace would be considered very large (large ≥ 10.1; Beeson & Robey 2006) while the ES corresponding to the purple trace would be considered small (≥ 4.0; Beeson & Robey 2006)."

ls$SMD2a <- "Across our entire sample, baseline standard deviation accounted for 33% of the variance in SMD scores."

ls$SMD3 <- "In these cases where baseline variability approaches zero, one option is to pool variance between baseline and treatment phases. But variability may not be even across time and it's not clear that pooling the standard deviation is appropriate (it assumes that variability is constant during baseline and treatment phases)."

ls$SMD3a <- "SMD does not meet the benchmark for 'small' even though the participant demonstrates marked improvement. Whether researchers can elect to pool baseline variability on a case-by-case basis or must pool across all participants is not clear. Furthermore, how benchmarks might shift based on pooled standard deviation has not been evaulated."

ls$SMD_eq <- "\\[{smd} = \\frac{\\bar{x}_{treatment}-\\bar{x}_{baseline}}{\\sigma_{baseline}}\\]"
ls$SMD_eq2 <- "\\[{smd} = \\frac{\\bar{x}_{treatment}-\\bar{x}_{baseline}}{\\sigma_{pooled}}\\]"
ls$SMD_eq3 <- "\\[{smd} = \\frac{\\bar{x}_{post-treatment}-\\bar{x}_{baseline}}{\\sigma_{baseline}}\\]"

ls$SMD4a <- "Technically, it's not clear that these benchmarks from Beeson and Robey (2006) apply to the original SMD, as they were calculated based off of post-treatment scores instead of the treatment phase."

ls$SMD4b <- "Re-calculating SMD approximating this method (averaging over the last 2 treatment sessions in lieu of post-treatment), we see that the SMD scores increase significantly for the blue trace, such that the result is not interpretable given the current benchmarks. Pooling the standard deviation in this case is unlikely to alleviate the problem, as performance appears to be relatively stable after session 11."

# NAP and Tau-U
ls$NAP1 <- "Non-overlap of all pairs (NAP; Parker & Vannest, 2009) is a non-parametric effect size."

ls$NAP2 <- "NAP characterizes the degree of overlap for all pair-wise comparisons of data points between two adjacent phases. It can be interpreted as “the probability that a score drawn at random from a treatment phase will exceed that of a score drawn at random from a baseline phase” (Parker & Vannest, 2009 p. 359). It ranges from 0 to 1, with values greater than 0.5 indicating less overlap between the treatment and baseline phases and a larger treatment response."

ls$NAP3 <- "10 out of 50 total comparisons are shown here. In total, there are 3 overlapping data points, two equal data points (ties count as 0.5) and 46 non-overlapping data points between phases. NAP = 46/50 or 0.92."

ls$NAP4 <- "NAP (and Tau-U below) are standardized effect size measures and are not intended to capture the magnitude of treatment effects (Wolery, 2010). As a result, NAP does not distinguish between participants who clearly demonstrate different treatment responses when there is no overlap between phases."

ls$TAU <- "Tau-U (Parker et al. 2011) is an extension of NAP. Unlike NAP, Tau-U can account for baseline trends in performance. Conceptually, values should range from -1 to 1. Larger values indicate increasing independence between study phases."

ls$TAUa <- "In this case, Tau-U clearly adjusts for the rising baseline trend in the case of the pink trace."

ls$TAU1 <- "However, Tau-U is not mathematically constrained between -1 and 1, and may notably exceed 1 when correcting for downward trends in the baseline phase or in cases where the baseline phase has a similar or longer length than the treatment phase (Tarlow, 2017). It is not clear how to interpret values of Tau-U that exceed 1."

ls$TAU2 <- "For example, in the red trace and purple trace, the treatment effect appears equivalent, but Tau-U for purple trace = 1.06 while Tau-U for the red trace = 0.96. Parker et al., (2011) recommend only correcting for the baseline trend in cases where a baseline trend is visually apparent and exceeds an a priori benchmark (in Parker et al., baseline slope = 0.4). Without correcting, Tau-U for the purple trace = 1.0." 

ls$PMG1 <- "Lambon Ralph and colleagues (2010) proposed the proportion of potential maximal gain (PMG). PMG reflects is the proportion of improvement gained (red arrow) to the amount of improvement possible after the baseline phase (blue arrow)."

ls$PMG_eq <- "\\[{pmg} = \\frac{\\bar{x}_{post-treatment}-\\bar{x}_{baseline}}{N_{items} - \\bar{x}_{baseline}}\\]"
  
ls$PMG2 <- "Unfortunately, there is no confidence interval available for PMG. Additionally, PMG depends largely on baseline performance rather than amount of change. For example, a participant who improves from 4/20 to 8/20 items (PMG = 0.25) is half that of a participant who improves from 12/20 to 16/20 items (PMG  = 0.50), but equivalent to the participant who improves from 8/40 to 16/40 (PMG = 0.25).  It's not clear that this is a desirable behavior for an effect size metric."

ls$PMG3 <- "This concern is illustrated here, where both cases have improved by approximately 20 words, but PMG for dark purple = 1 while PMG for light purple = 0.76."

ls$GLMM1 <- "Generalized linear mixed effects models (GLMMs) can be used to calculate individual effect sizes. Here, we've replicated the approach from Wiley & Rapp (2018). This technique models each item-level response (correct or incorrect) as a function of session (time), phase (baseline vs. treatment) and their interaction for each participant. Session is mean-centered and phase is sum coded (-1,1)." 

ls$GLMM2 <- "A GLMM effect size can be estimated from exponentiating the sum of the session and interaction coefficeints to an odds ratio. For instance, an odds ratio of 2 indicates that the odds of a correct response doubled with each treatment session. Furthermore, the p-value for the interaction coefficient indicates whether this trend is significantly different from the baseline phase."

ls$GLMM3 <- "One drawback to this approach is that the GLMM effect size is only estimated based on the treatment phase, thus the effect size estimates are similar even though red has a significant rising baseline trend which needs to be accounted for."

ls$GLMM4 <- "Alternatively, estimating the change in slope from baseline to treatment will account for baseline trends, but assumes that any baseline trends will continue at the same rate without treatment, which may be unrealistic in the case of steep baseline trends. Here, if the GLMM effect size were estimated based on the change in slope from baseline to treatment, it would indicate that treatement had a negative effect on performance for pink (GLMM = 0.89) because of the steep baseline trend, but a similar effect for orange (GLMM = 1.36)." 

ls$BMEM1 <- "Bayesian mixed effects models (BMEMs) have been used in single-case design research in fields outside of aphasiology. Our research group recently deployed a bayesian approach (Evans et al., 2020) to calculating effect sizes. In this study, we used an interrupted time series approach (Hutiema, 2000) which models performance as a function of the slope during the baseline phase, level change between the last baseline and first treatment session, and the change in slope between baseline and treatment:"

ls$BMEM1a <- "This approach can include one or multiple participants in the same model and derives individual effect sizes by subtracting model’s posterior predictions for each participant at the last baseline probe from the final treatment probe. The median of the difference in posterior predictors thus describes the number of items improved during treatment. Because any trends in baseline performance are characterized in the model structure, this approach may reasonably account for improvement due to repeated testing during the baseline phase."

ls$BMEM_eq <- "\\[Y_t=\\beta_0+\\beta_1T_t+\\beta_2D_t+\\beta_3[T_t-(n_1+1)]D_t+\\epsilon_t\\]"

ls$BMEM2 <- "We see slightly more divergence between these cases using the BMEM approach compared to GLMM. The BMEM size suggests that the red case gained about 13.3 items in response to treatment while the orange case gained about 19.5. However, it's again not clear whether this approach adequately accounts for the baseline trend."


#################### summary ################

sum1 <- "These 6 effect sizes each have their own strengths and weaknesses. In the plot below, scatterplots visualize the relationship between effect sizes (lower triangle) and coefficients of determination describe the amount of shared variance between measures (upper triangle)."

sum2 <- "These relationships likely change depending on the treatment design. In this case, these measures were calculated using a large simulated dataset of 500 participants under an AB design with 5 baseline and 10 treatment sessions. In this dataset, we created a distribution of participant ability estimates and item difficulty estimates based on Fergadiotis et al. (2015). Items were assigned to participants such that baseline performance would average 30% correct regardless of participant ability. Then we simulated item-level treatment response where the degree of treatment resposne was randomly assigned to participants."

sum3 <- "(1) The relationship between SMD and other measures is characterized by increasing dissimilarity as effect sizes increase. This is likely due to the influence of using baseline variance in the denominator of SMD. In this sample baseline variance accounts of 34% of the variance in SMD scores."

sum4 <- "(2) The difference between NAP/Tau-U and other effect size measures is clear, as they do not capture differences in treatment effects when there is no overlap. However, they are high similar to each other."

sum5 <- "(3) PMG approximates the BMEM effect size which is expected since the BMEM effect size reflects absolute change between baseline and treatment when baseline performance is neutralized through stimuli selection."

sum6 <- "(4) Differences between the GLMM and BMEM measures may be explained by the fact that the GLMM effect size implemented does not take into account baseline trends. Furthermore, the GLMM effect size may not fully account for substantial level changes (improvements immediately following baseline)."
#################### methods ####################

methods1 <- ""
  
methods2 <- ""

check = 1
