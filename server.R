library(tidyverse)
library(shinyjs)

source("sample_size_functions.R")

# SERVER

function(input, output, session) {

  # If fractional N, round up. 
  observeEvent(input$Indep_N_per_group,{
    updateNumericInput(session = session, inputId = "Indep_N_per_group", value = abs(ceiling(input$Indep_N_per_group)))
  })
  
  # If fractional max cluster size, round up. 
  observeEvent(input$max_cluster_size,{
    updateNumericInput(session = session, inputId = "max_cluster_size", value = abs(ceiling(input$max_cluster_size)))
  })
  
  # create the required number of input fields based on max cluster size
  # default percentages are rounded to two decimal places
  # if values do not sum exactly to 100, recalc p_1 so the sum is exactly 100
  # split the resulting list_all into first and all-other elements, to handle the case when max cluster size = 1
  output$clusters = renderUI ({
    K = as.integer(input$max_cluster_size)
    p_default = round(100 / input$max_cluster_size, digits = 2)
    
    if ((p_default*K) != 100) {
      p_1 = round(100 - (p_default*(K-1)), digits = 2)
    } else {
      p_1 = p_default
    }
    input_1 = numericInput(inputId = "p_1", label = "Cluster size = 1", value = p_1, min = 0, max = 100, step = 0.01)
    list_all = list(input_1)
    if (input$max_cluster_size > 1){
      other_inputs = lapply(2:K, function(k) {
        numericInput(inputId = paste0("p_", k), label = paste0("Cluster size = ", k), value = p_default, min = 0, max = 100, step = 0.01)
      })
      list_all= list(list_all, other_inputs)
    }
    list_all
    })
  
  # create fields for the alternative set of cluster percentages
  output$alt_clusters = renderUI ({
    K = as.integer(input$max_cluster_size)
    propn_list = propn_list()
    # if user has input percentages of clusters (deltas) convert them to proportions of observations (gammas)
    if (input$p_type == "% of clusters") {
      alt_p = delta_to_gamma(propn_list)
    # if user has input percentages of observations (gammas) convert them to proportions of clusters (deltas)
    } else if (input$p_type == "% of observations") {
      alt_p = gamma_to_delta(propn_list)
    }
    # convert the derived proportions to percentages and round to two decimal places
    alt_p = lapply(alt_p, function(k) round(100 * k, digits = 2))
    # generate the numeric fields and disable them from user editing
    alt_inputs = mapply(function(k) {
      disabled(numericInput(inputId = paste0("altp_", k), label = paste0("Cluster size = ", k), value = alt_p[k], min = 0, max = 100, step = 0.01))
    }, k = 1:K, SIMPLIFY = FALSE)
    alt_inputs
  })

  # create list of input percentages from the individual entries, p_k
  perc_list  = reactive(map(1:input$max_cluster_size, function(k) {input[[paste0("p_", k)]]}))  # user entries (percentages)
  # convert the input percentages to proportions
  propn_list = reactive(map(1:input$max_cluster_size, function(k) {input[[paste0("p_", k)]]} / 100))  # user entries (proportions)

  # if user has input proportions of clusters (deltas) convert them to proportions of observations (gammas)
  # otherwise the entered proportions of observations are the gammas
  gamma_list = reactive({
    propn_list = propn_list()
    if (input$p_type == "% of clusters") {
      gamma_list = delta_to_gamma(propn_list)
    } else if (input$p_type == "% of observations") {
      gamma_list = propn_list
    }
    gamma_list
  })
  
  # create cluster column titles based on the user selected input
  output$cluster_title1 = reactive({if (input$p_type == "% of clusters") "% of Clusters" else "% of Observations"})
  output$cluster_title2 = reactive({if (input$p_type == "% of clusters") "% of Observations" else "% of Clusters"})
  
  # convert input prevalence percentages to proportions
  pi_I = reactive(input$pc_I / 100)
  pi_C = reactive(input$pc_C / 100)
  
  # calculate the design effect based on user inputs
  deff = reactive({
    
    gamma_list = gamma_list()
    pi_I       = pi_I()
    pi_C       = pi_C()
    
    deff = calc_deff(outcome = input$outcome_type, corr = input$corr_struct, rand = input$rand_method,
                     ICC = input$ICC, link = input$link_fn, gamma = gamma_list, pi_I = pi_I, pi_C = pi_C)
    # subtract very small value. This fixes an issue with deffs that should be exactly 2dp having very small trailing values
    # that caused the sample size to be rounded up to one value higher than it should
    deff = deff - (1E-14)
    deff
  })
  
  # create output table
  results.data.table = reactive({

    perc_list  = perc_list()
    gamma_list = gamma_list()
    deff       = deff()

    # Check that inputs are reasonable values. If not, set deff to NA
    if((input$Indep_N_per_group < 1) |
       (input$ICC < -1 | input$ICC > 1) |
       (input$pc_I < 0 | input$pc_I > 100) |
       (input$pc_C < 0 | input$pc_C > 100) |
       (input$max_cluster_size <= 0) |
       (sum(unlist(perc_list)) != 100)){
      deff = NA
    }

    # calculate N and number of clusters (M)
    N_per_group   = ceiling(deff*input$Indep_N_per_group)
    N_total       = 2 * ceiling(deff*input$Indep_N_per_group)
    gamma_div_k   = gamma_list %>% imap(~ .x/.y)
    M_per_group   = ceiling(N_per_group * sum(unlist(gamma_div_k)))
    M_total_clus  = 2 * ceiling(N_per_group * sum(unlist(gamma_div_k))) # calc num of clusters per arm and multiply by 2
    M_total_ind   = ceiling(N_total * sum(unlist(gamma_div_k))) # calc num of clusters based on total number of obs (as clusters do not assign to arms)
    
    # data frame of results, including check for if deff is less than or equal to 0
    if (is.na(deff) | (deff <= 0)) {
      if (input$rand_method == "Cluster") {
        # Row names for output
        row.names = c("Design Effect", "Number Observations per Trial Arm", "Number Clusters per Trial Arm", "Number Observations Total", "Number Clusters Total")
        data.frame("Result" = row.names, "Value" = c(NA, NA, NA, NA, NA))
      } else if (input$rand_method == "Individual") {
        # Row names for output
        row.names = c("Design Effect", "Number Observations per Trial Arm", "Number Observations Total", "Number Clusters Total")
        data.frame("Result" = row.names, "Value" = c(NA, NA, NA, NA))
      }
    } else if (deff > 0) {
      if (input$rand_method == "Cluster") {
        # Row names for output
        row.names = c("Design Effect", "Number Observations per Trial Arm", "Number Clusters per Trial Arm", "Number Observations Total", "Number Clusters Total")
        
        data.frame("Result" = row.names, "Value" = c(as.character(format(round(deff, digits = 4), nsmall = 4)),
                                                     as.character(format(N_per_group, nsmall = 0)),
                                                     as.character(format(M_per_group, nsmall = 0)),
                                                     as.character(format(N_total, nsmall = 0)),
                                                     as.character(format(M_total_clus, nsmall = 0))))
      } else if (input$rand_method == "Individual") {
        # Row names for output
        row.names = c("Design Effect", "Number Observations per Trial Arm", "Number Observations Total", "Number Clusters Total")
        
        data.frame("Result" = row.names, "Value" = c(as.character(format(round(deff, digits = 4), nsmall = 4)),
                                                     as.character(format(N_per_group, nsmall = 0)),
                                                     as.character(format(N_total, nsmall = 0)),
                                                     as.character(format(M_total_ind, nsmall = 0))))
      }
    }
  })

  results.error = reactive({
    # Check that inputs and derived deff are reasonable values. If not, generate error message
    perc_list = perc_list()
    deff      = deff()
    error_msg = ""

    if (input$Indep_N_per_group < 1){
      error_msg = "The sample size per group must be greater than 1."
    }
    if (input$ICC < -1 | input$ICC > 1){
      error_msg = "The ICC must be between -1 and 1."
    }
    if (input$pc_I < 0 | input$pc_I > 100){
      error_msg = "The intervention arm outcome prevalence must be between 0 and 100."
    }
    if (input$pc_C < 0 | input$pc_C > 100){
      error_msg = "The control arm outcome prevalence must be between 0 and 100."
    }
    if (sum(unlist(perc_list)) != 100){
      error_msg = "The cluster percentages must sum to 100."
    }
    if (all(unlist(lapply(perc_list, function(k) k >= 0 ))) == FALSE){
      error_msg = "Cluster percentages must be greater than or equal to 0."
    }
    if (input$max_cluster_size <= 0){
      error_msg = "The maximum cluster size must be greater than 0."
    }
    if (!is.na(deff) & (deff <= 0)){
      error_msg = "The design effect is less than or equal to 0. Try entering different inputs."
    }
    error_msg
  })
  output$msg = renderText({results.error()})
  
  # Output data table results without any data table options (page no., row number, etc..)
  output$results = renderTable({results.data.table()}, options = list(lengthChange= F, paging = F, searching = F, ordering= F, info=F))

}