library(bslib)
library(shinyjs)
library(bsicons)

page_fluid(
  
  tags$head(includeHTML("google-analytics.html")),
  
  title = "Partially clustered sample size calculator",
  navbarPage("Sample size calculator for partially clustered trials"),
  navset_pill(

    nav_panel("Information",
              br(),
              "This calculator can be used to calculate the target sample size for two-arm, parallel randomised
                      trials that include partial clustering when the outcome of interest is continuous or binary.
                      The sample size is calculated assuming the analysis will be performed using generalised estimating equations (GEEs)
                      with an independence or exchangeable working correlation structure and that clustered observations will be randomised
                      to the same treatment arm (cluster randomisation) or independently of each other (individual randomisation).",
              br(),
              br(),
              br(),
              "For a tutorial on how to use this app, refer to:",
              br(),
              "Lange KM, Sullivan TR, Kasza J, Yelland LN.",a("An online sample size calculator for partially clustered trials",href="https://pubmed.ncbi.nlm.nih.gov/41618613/"),
              br(),
              br(),
              "For full details, including the assumptions and limitations of the calculations, refer to:",
              br(),
              "Lange KM, Kasza J, Sullivan TR, Yelland LN.",a("Sample size calculations for partially clustered trials",href="https://pubmed.ncbi.nlm.nih.gov/40662648/"),
              br(),
              br(),
              "Created by: Kylie Lange",
              br(),
              "Last updated: 2 Feb, 2026",
              br(),
              br(),
              "Please email any questions or comments to:",
              br(),
              "Kylie Lange (",a("kylie.lange@adelaide.edu.au", href="mailto:kylie.lange@adelaide.edu.au")," ).",
              br(),
              br(),
              "If the calculator is not working on the Shinyapps website, R code for local implementation of the calculator is available in "
              ,a("this GitHub repository.",href="https://github.com/klange01/partial-clustering-ssizecalc")),
    
    nav_panel("Calculator",
              page_fillable(
                useShinyjs(),
                layout_columns(
                  
                  card(
                    card_header("User Inputs"),
                    helpText("If output is NA, check that all inputs are feasible values."),
                    
                    radioButtons(inputId = "outcome_type", label = "Type of Outcome", choices = c("Continuous","Binary"), inline = TRUE),
                    helpText("Select outcome of interest."),
                    
                    numericInput(inputId = "Indep_N_per_group", label = "Number of Observational Units per Trial Arm Assuming Independence",
                                 value = 100, min = 1, max = Inf),
#                    uiOutput("Indep_N_per_group"),
                    helpText(popover(span("Enter the number of observational units required per arm for your trial obtained using any
                                          standard sample size calculator that assumes the outcomes of all observational units are independent.
                                          If you have calculated the total sample size (N), the number of observational units per trial arm is N/2.",
                                          bs_icon("info-circle")), placement = "left",
                                     "The sample size assuming independence can be calculated using standard sample size tools. For example, the “pwr::pwr.t.test” command
                                     in R, or the “power twomeans” command in Stata, can be used to calculate the sample size for a two sample t-test for a continuous outcome.
                                     For a binary outcome, the “pwr::pwr.2p.test” command in R, or the “power twoproportions” command in Stata, can be used to calculate
                                     the sample size for a chi-square test.")),
                    
                    conditionalPanel(condition = "input.outcome_type=='Binary'",
                                     numericInput(inputId = "pc_I",label = "Intervention Arm Outcome Prevalence (%)", value = 30,
                                                  min = 0, max = 100, step = 1), br(),
                                     helpText("Enter the expected prevalence of the outcome in the intervention arm for your trial as a percentage.
                                              This should be the value that was used in the calculation of the sample size asumming independence entered above.")),

                    conditionalPanel(condition = "input.outcome_type=='Binary'",
                                     numericInput(inputId = "pc_C",label = "Control Arm Outcome Prevalence (%)", value = 40,
                                                  min = 0, max = 100, step = 1), br(),
                                     helpText("Enter the expected prevalence of the outcome in the control arm for your trial as a percentage.
                                              This should be the value that was used in the calculation of the sample size asumming independence entered above.")),

                    numericInput(inputId = "ICC", label = "Intracluster Correlation Coefficient (ICC)", value = 0.5,
                                 min=-1, max=1, step = 0.01),
                    helpText("Enter your best estimate of the correlation between outcomes of observational units from the same cluster for your trial."),
                    
                    radioButtons(inputId = "corr_struct", label = "GEE Working Correlation Structure", choices = list("Independence", "Exchangeable"), inline = TRUE),
                    helpText(popover(span("Specify whether data from your trial will be analysed assuming an independence or exchangeable working correlation structure.",
                                          bs_icon("info-circle")), placement = "left",
                                     "The independence working correlation structure accounts for clustering by applying a robust variance estimate for the treatment effect.
                                      The exchangeable working correlation structure estimates a constant correlation between the clustered observations and uses the 
                                     estimated correlation when accounting for the clustering. For more detail on these approaches and their different interpretations in
                                     clustered and partially clustered trials see articles by ", a("Kahan (2024)", href="https://pubmed.ncbi.nlm.nih.gov/38780480/"), ", ",
                                     a("Yelland (2015)", href="https://pubmed.ncbi.nlm.nih.gov/26332368/"), "and ", 
                                     a("Kahan (2022)", href="https://pubmed.ncbi.nlm.nih.gov/35422159/"), ". The default independence working correlation structure will provide
                                     a conservative sample size estimate if an alternative method of analysis is used.")),

#                    If want the extra help after the title:
#                    radioButtons(inputId = "corr_struct", label = popover(span("GEE Working Correlation Structure", bs_icon("info-circle")), placement = "right",
#                                                      "Extra help in here"),
#                    Original helpText:
#                    helpText("Specify whether data from your trial will be analysed assuming an independence or exchangeable working correlation structure."),
                    
                    radioButtons(inputId = "rand_method", label = "Method of Randomisation", choices = list("Cluster", "Individual"), inline = TRUE),
                    helpText(popover(span("Specify whether observational units from the same cluster will be randomised to the same trial arm (cluster) 
                                          or independently of each other (individual).",
                                          bs_icon("info-circle")), placement = "left",
                                     "When cluster randomisation is used, all observations within a cluster are randomised to the same treatment and so clusters are nested within
                                     treatment arm. When individual randomisation is used, observational units are randomised independently, such that members of the same 
                                     cluster may be randomised to the same or different treatments (and hence clusters and treatment arms may be crossed).")),
                    
                    conditionalPanel(condition = "input.outcome_type=='Binary'",
                                     radioButtons(inputId = "link_fn", label = "Analysis Model", choices = list("Logistic","Log Binomial"), inline = TRUE), br(),
                                     helpText("Specify whether data from your trial will be analysed using a logistic model (to estimate the odds ratio)
                                              or a log binomial model (to estimate the relative risk).")),
                  ),
                  
                  card(
                    card_header("Cluster Inputs"),
                    numericInput(inputId = "max_cluster_size", label = "Maximum Cluster Size", value = 3, min = 1, max = Inf),
#                    uiOutput("max_cluster_size"),
                    helpText("Enter the size of the largest cluster in the trial."),
                    
                    radioButtons(inputId = "p_type", label = "Percentages of Each Cluster Size",
                                 choices = c("% of clusters","% of observations"), inline = FALSE, width = validateCssUnit("100%")),
                    # Original helpText:
                    # helpText("Specify whether you will enter the percentage of clusters that you expect of each cluster size
                    #           OR the percentage of observational units that you expect will belong to each cluster size."),
                    helpText("The calculator requires specification of either the percentage of clusters you expect to have each possible cluster size, OR
                             the percentage of observational units that will belong to clusters of each size."),
                    helpText(popover(span("For each possible cluster size up to the maximum cluster size, enter the percentage of clusters/observations that are expected at that size.",
                                          bs_icon("info-circle")), placement = "right",
                                     "For example, if a trial in preterm infants (the observational units) is planned and researchers anticipate that 70% of infants will be singletons, 25% twins
                                     and 5% triplets, then the “% of observations” are entered as 70, 25 and 5, respectively. Alternatively, the percentage of mothers (the clusters)
                                     expected to have a singleton, twin or triplet pregnancy may be known instead, which would be entered as “% of clusters” with corresponding values 
                                     of 83, 15 and 2, respectively.")),
                      
                    layout_columns(
                      card(textOutput("cluster_title1"),
                           uiOutput("clusters"),
                           helpText("Percentages should sum to 100."),
                      ),
                      card(textOutput("cluster_title2"),
                           uiOutput("alt_clusters")),
                    ),
                  ),
                  
                  card(
                    card_header("Results"),
                    helpText("Target sample size for a two-arm partially clustered trial"),
                    span(textOutput("msg"), style = "color:red"),
                    tableOutput("results"),
                  )
                )
              )
    )
  )
)

