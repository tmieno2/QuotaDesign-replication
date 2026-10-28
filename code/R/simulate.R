# !===========================================================
# ! Batch run simulations
# !===========================================================
#' Batch run simulations
#' 
#' This function runs simualtions for a set of use-specified parameters in parallel
#' 
#' @param par_list A data.frame containing the parameter sets to run simulations on. Each row represents a unique parameter set.
#' @param file_name_append An optional string to append to the output file names for differentiation.
#' @param reverse A boolean indicating whether to reverse the order of parameter sets before running simulations. Default is FALSE.
#' @param ... Additional arguments to pass to the `sim_path()` function.
#' @return NULL. The function saves the simulation results to files
batch_run_simulate <- function(par_list, file_name_append = NA, reverse = FALSE, ...) {

  if (reverse == TRUE) {
    par_list <- rev(par_list)
  }

  parallel::mclapply(
    1:nrow(par_list),
    \(x) {

      print(paste0("Running simulation for parameter set ", x, " out of ", nrow(par_list), "..."))

      #--- get parameters and file name ---#
      pars <- get_parameters(x, par_list, file_name_append)

      #--- run simulation ---#
      sim_path(
        annual_quota = pars$annual_quota,
        alloc_year = pars$alloc_year,
        pen_ratio = pars$pen_ratio,
        max_co_ratio = pars$max_co_ratio,
        co_ratio = pars$co_ratio,
        endowment = pars$endowment,
        dp_result_file = pars$dp_result_file,
        sim_result_file = pars$sim_result_file,
        ...
      )
    },
    mc.cores = parallel::detectCores() - 4,
    mc.preschedule = FALSE
  )
}

# !===========================================================
# ! Combine the results
# !===========================================================
# This function combines the results from multiple simulation runs into a single data.table.
#' @param par_list A data.frame containing the parameter sets used in the simulations. Each row represents a unique parameter set.
#' @param file_name_append An optional string that was appended to the output file names during simulation.
#' @param num_cores The number of CPU cores to use for parallel processing. Default is the total number of available cores minus 4.
#' @return A combined data.table of all simulation results.
combine_results <- function(par_list, file_name_append = NA, num_cores = parallel::detectCores() - 4) {

  data_return <-
    parallel::mclapply(
      1:nrow(par_list),
      \(x) {
        file_name <- get_parameters(x, par_list, file_name_append)$sim_result_file
        readRDS(file_name)
      },
      mc.cores = num_cores,
      mc.preschedule = FALSE
    ) %>%
    rbindlist()

  return(data_return)
}

# !===========================================================
# ! Get parameters
# !===========================================================

#' Get parameters and file names for a specific row in the parameter list
#' 
#' This function extracts the parameters and constructs file names for a specific row in the parameter list.
#' 
#' @param x The row index in the parameter list.
#' @param par_list A data.frame containing the parameter sets. Each row represents a unique parameter set.
#' @param file_name_append An optional string to append to the output file names for differentiation.
#' @return A list containing the extracted parameters and constructed file names.
get_parameters <- function(x, par_list, file_name_append = NA) {
  #++++++++++++++++++++++++++++++++++++
  #+ Parameter setup
  #++++++++++++++++++++++++++++++++++++
  #--- annual allocation ---#
  annual_quota <- par_list[x, annual_quota]
  #--- number of years within one allocation period ---#
  alloc_year <- par_list[x, alloc_year]
  #--- penalty ratio ---#
  pen_ratio <- par_list[x, pen_ratio]
  #--- carry-over ratio ---#
  co_ratio <- par_list[x, co_ratio]
  #--- maximum carry over ratio ---#
  max_co_ratio <- par_list[x, max_co_ratio]
  #--- endowment ---#
  endowment <- par_list[x, endowment]
  #--- DP result file name ---#
  dp_result_file <- par_list[x, dp_result_file]

  sim_result_file <- paste0(
    "results/simulation/sim_results_aq_", annual_quota,
    "_ay_", alloc_year,
    "_max_co_ratio_", max_co_ratio,
    "_co_", co_ratio,
    "_pen_", pen_ratio,
    "_end_", endowment,
    ifelse(
      is.na(file_name_append),
      "",
      paste0("_", file_name_append)
    ),
    ".rds"
  )

  return(
    list(
      annual_quota = annual_quota,
      alloc_year = alloc_year,
      pen_ratio = pen_ratio,
      max_co_ratio = max_co_ratio,
      co_ratio = co_ratio,
      endowment = endowment,
      dp_result_file = dp_result_file,
      sim_result_file = sim_result_file
    )
  )
}

# !===========================================================
# ! Simulate
# !===========================================================

#' Simulate water quota usage over multiple periods
#' 
#' This function simulates water quota usage over multiple periods based on specified parameters and saves the results to a file.
#' 
#' @param annual_quota The annual water quota.
#' @param alloc_year The number of years within one allocation period.
#' @param pen_ratio The penalty ratio for exceeding the quota.
#' @param max_co_ratio The maximum carry-over allowed.
#' @param co_ratio The carry-over ratio for unused quota.
#' @param endowment The initial water quota endowment.
#' @param dp_result_file The file path to the dynamic programming result file.
#' @param sim_result_file The file path to save the simulation results.
#' @param force_run A boolean indicating whether to force the simulation to run even if the result file exists. Default is FALSE.
#' @param summarize A boolean indicating whether to summarize theme( results. Default is TRUE.
#' @param num_sim The number of simulation runs. Default is 500,000. Default is 500,000.
#' @return NULL. The function saves the simulation results to a file
sim_path <- function(annual_quota, alloc_year, pen_ratio, max_co_ratio, co_ratio, endowment, dp_result_file, sim_result_file, force_run = FALSE, summarize = TRUE, num_sim = 500000) {
  
  #++++++++++++++++++++++++++++++++++++
  #+ Set up
  #++++++++++++++++++++++++++++++++++++
  #' total quota per allocation period
  W_total <- alloc_year * annual_quota
  max_quota_carry_over <- W_total * max_co_ratio

  #' number of years
  prod_T <- ((50 %/% alloc_year) + ceiling(50 %% alloc_year)) * alloc_year

  if (file.exists(here::here(sim_result_file)) & force_run == FALSE) {
    print("Simulation already complete for this case. Skipping.")
    return(NULL)
  }

  #' Water quota grids
  # W_min <- -max_ir * pen_ratio * 2
  # W_max <- W_total + 30
  # W_ls <- c(seq(W_min, W_max, by = 0.2), -0.001) %>%
  # sort(decreasing = FALSE)

  #++++++++++++++++++++++++++++++++++++
  #+ Set up simulations
  #++++++++++++++++++++++++++++++++++++
  # === simulation parameters ===#
  set.seed(784343)

  W_start <- W_total + endowment
  weather_year_ls <- base_data[, year] %>% unique()

  # === starting data ===#
  starting_data_store <- list()
  starting_data_store[["1"]] <- data.table(
    sim_id = 1:num_sim,
    W_start = W_start,
    period = 1
  ) %>%
    setkey(sim_id, period)

  # === weather data ===#
  # randomly draw weather-year from the 35 weather-year combinations
  weather_data <-
    data.table(
      sim_id = rep(1:num_sim, prod_T),
      year = sample(weather_year_ls, (prod_T) * num_sim, replace = TRUE),
      period = rep(1:prod_T, each = num_sim)
    )

  # === optimal smt choice ===#
  # this maps remaining W to the optimal smt choice
  opt_choice_temp <- readRDS(here::here(dp_result_file))

  W_ls <-
    opt_choice_temp[[1]][, W] %>%
    # .[-length(.)] %>%
    sort(decreasing = FALSE)
  
  #++++++++++++++++++++++++++++++++++++
  #+ Simulation
  #++++++++++++++++++++++++++++++++++++
  #--- results storage ---#
  results_ls <- list()

  # k <- 1
  for (k in 1:prod_T) {
    print(paste0("t=", k))
    #--- all the weather combinations ---#
    weather_W_data <-
      copy(starting_data_store[[paste(k)]]) %>%
      #--- merge with weather data ---#
      weather_data[period == k][., on = c("period", "sim_id")]

    #--- find the closest W start value ---#
    W_vec_apx <-
      data.table(w = W_ls, val = W_ls) %>%
      setattr("sorted", "w") %>%
      .[J(weather_W_data[, W_start]), roll = "nearest"] %>%
      .[, val]

    final_data <-
      weather_W_data[, W := W_vec_apx] %>%
      #--- selecting from 1 ~ AY ---#
      # Years 1 ~ AY has the least influence from the last time period
      # These are the closes to steady state decision making if the time period is infinite
      opt_choice_temp[[ifelse(k %% alloc_year == 0, alloc_year, k %% alloc_year)]][., on = c("W")] %>%
      base_data[., on = c("smt", "year")] %>%
      #--- nth allocation period cylcle ---#
      .[, alloc_period := (k - 1) %/% alloc_year + 1] %>%
      #--- what year in the allocation period ---#
      .[, year_in_alloc := ifelse(k %% alloc_year == 0, alloc_year, k %% alloc_year)]

    if ((k %% alloc_year) == 0) {
      final_data %<>%
        .[, remainder := W - ir] %>%
        .[, carry_over := pmax(0, pmin(max_quota_carry_over, remainder * co_ratio))] %>%
        .[, quota_penalty := remainder * pen_ratio] %>%
        .[, W_next := W_total + ifelse(remainder >= 0, carry_over, quota_penalty)] %>%
        .[, remainder := NULL] %>%
        .[, carry_over := NULL] %>%
        .[, quota_penalty := NULL]
    } else {
      final_data %<>%
        .[, W_next := W - ir]
    }

    results_ls[[paste(k)]] <- final_data

    #--------------------------
    # Starting data for the next period
    #--------------------------
    starting_data_store <- list()

    starting_data_store[[paste(k + 1)]] <-
      copy(final_data) %>%
      .[, .(sim_id, W_next)] %>%
      setnames("W_next", "W_start") %>%
      .[, period := k + 1]

    rm(final_data)
    gc(verbose = getOption("verbose"), reset = FALSE)
  }

  combined_results <-
    rbindlist(results_ls) %>%
    .[, profit := corn_P * yield - ir_P * ir - 600] %>%
    # .[, utility := crra_u(0.6, profit)] %>%
    .[, w_broke := ifelse(W_start < 0, 1, 0)]

  if (summarize == TRUE) {
    sum_results <-
      combined_results %>%
      .[, .(
        yield = mean(yield),
        profit = mean(profit),
        profit_q_0 = min(profit),
        profit_q_25 = quantile(profit, prob = 0.25),
        profit_q_50 = quantile(profit, prob = 0.50),
        profit_q_75 = quantile(profit, prob = 0.75),
        profit_q_100 = max(profit),
        yield_sd = sd(yield),
        ir_q_0 = min(ir),
        ir_q_25 = quantile(ir, prob = 0.25),
        ir_q_50 = quantile(ir, prob = 0.50),
        ir_q_75 = quantile(ir, prob = 0.75),
        ir_q_100 = max(ir),
        ir = mean(ir),
        W_start = mean(W_start),
        W_broke = mean(w_broke),
        alloc_period = mean(alloc_period)
      ), by = .(period, year_in_alloc)] %>%
      .[, annual_quota := annual_quota] %>%
      .[, alloc_year := alloc_year] %>%
      .[, max_co_ratio := max_co_ratio] %>%
      .[, co_ratio := co_ratio] %>%
      .[, pen_ratio := pen_ratio] %>%
      .[, endowment := endowment]

    saveRDS(sum_results, file = sim_result_file)
  } else {
    original_results <-
      combined_results %>%
      .[, annual_quota := annual_quota] %>%
      .[, alloc_year := alloc_year] %>%
      .[, max_co_ratio := max_co_ratio] %>%
      .[, co_ratio := co_ratio] %>%
      .[, pen_ratio := pen_ratio] %>%
      .[, endowment := endowment]
    saveRDS(original_results, file = sim_result_file)
  }

  rm(results_ls, weather_W_data, weather_data, starting_data_store)
  gc()
}
