library(bslib)
library(shinyjs)

page_fluid(
  
  tags$head(includeHTML("google-analytics.html")),
  
  title = "Partially clustered sample size calculator",
  navbarPage("Sample size calculator for partially clustered trials"),
  navset_pill(

    nav_panel("Information",
              br(),
              "This calculator can be used to calculate the target sample size for two-arm, parallel group, randomised
                      trials that include partial clustering when the outcome of interest is continuous or binary.
                      The sample size is calculated assuming the analysis will be performed using generalised estimating equations (GEEs)
                      with an independence or exchangeable working correlation structure and that clustered observations will be randomised
                      to the same treatment group (cluster randomisation) or independently of each other (individual randomisation).",
              br(),
              br(),
              br(),
              "For a tutorial on how to use this app, refer to:",
              br(),
              "Lange KM, Sullivan TR, Kasza J, Yelland LN.",a("An online sample size calculator for partially clustered trials",href=""),
              br(),
              br(),
              "For full details, including the assumptions and limitations of the calculations, refer to:",
              br(),
              "Lange KM, Kasza J, Sullivan TR, Yelland LN.",a("Sample size calculations for partially clustered trials",href=""),
              br(),
              br(),
              "Created by: Kylie Lange",
              br(),
              "Last updated: 6th Dec, 2024",
              br(),
              br(),
              "Please email any questions or comments to:",
              br(),
              "Kylie Lange (",a("kylie.lange@adelaide.edu.au", href="mailto:kylie.lange@adelaide.edu.au")," ).",
              br(),
              br(),
              "If the calculator is not working on the Shinyapps website, R code for local implementation of the calculator is available on "
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
                    
                    numericInput(inputId = "Indep_N_per_group", label = "Number of Observational Units per Group Assuming Independence",
                                 value = 100, min = 1, max = Inf),
#                    uiOutput("Indep_N_per_group"),
                    helpText("Enter the number of observational units required per group for your trial obtained using any
                              standard sample size calculator that assumes the outcomes of all observational units are independent.
                              If you have calculated the total sample size (N), the number of observational units per group is N/2."),
                    
                    conditionalPanel(condition = "input.outcome_type=='Binary'",
                                     numericInput(inputId = "pc_I",label = "Intervention Group Outcome Prevalence (%)", value = 30,
                                                  min = 0, max = 100, step = 1), br(),
                                     helpText("Enter the expected prevalence of the outcome in the intervention group for your trial as a percentage.
                                              This should be the value that was used in the calculation of the sample size asumming independence entered above.")),

                    conditionalPanel(condition = "input.outcome_type=='Binary'",
                                     numericInput(inputId = "pc_C",label = "Control Group Outcome Prevalence (%)", value = 40,
                                                  min = 0, max = 100, step = 1), br(),
                                     helpText("Enter the expected prevalence of the outcome in the control group for your trial as a percentage.
                                              This should be the value that was used in the calculation of the sample size asumming independence entered above.")),

                    numericInput(inputId = "ICC", label = "Intracluster Correlation Coefficient (ICC)", value = 0.5,
                                 min=-1, max=1, step = 0.01),
                    helpText("Enter your best estimate of the correlation between outcomes of observational units from the same cluster for your trial."),
                    
                    radioButtons(inputId = "corr_struct", label = "GEE Working Correlation Structure", choices = list("Independence", "Exchangeable"), inline = TRUE),
                    helpText("Specify whether data from your trial will be analysed assuming an independence or exchangeable working correlation structure."),
                    
                    radioButtons(inputId = "rand_method", label = "Method of Randomisation", choices = list("Cluster", "Individual"), inline = TRUE),
                    helpText("Specify whether observational units from the same cluster will be randomised to the same treatment group (cluster) 
                              or independently of each other (individual)"),
                    
                    conditionalPanel(condition = "input.outcome_type=='Binary'",
                                     radioButtons(inputId = "link_fn", label = "Analysis Model", choices = list("Logistic","Log Binomial"), inline = TRUE), br(),
                                     helpText("Specify whether data from your trial will be analysed using a logistic model (to estimate the odds ratio)
                                              or a log binomial model (to estimate the relative risk).")),
                  ),
                  
                  card(
                    card_header("Cluster Inputs"),
                    numericInput(inputId = "max_cluster_size", label = "Maximum Cluster Size", value = 4, min = 1, max = Inf),
#                    uiOutput("max_cluster_size"),
                    helpText("Enter the size of the largest cluster in the trial."),
                    
                    radioButtons(inputId = "p_type", label = "Percentages of Each Cluster Size",
                                 choices = c("% of clusters","% of observations"), inline = FALSE, width = validateCssUnit("100%")),
                    helpText("Specify whether you will enter the percentage of clusters that you expect of each cluster size
                              OR the percentage of observational units that you expect will belong to each cluster size"),
                    
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
                    span(textOutput("msg"), style = "color:red"),
                    tableOutput("results"),
                  )
                )
              )
    )
  )
)

