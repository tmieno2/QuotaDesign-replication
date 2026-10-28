#' Backward Induction
#' 
#' This function performs backward induction to solve a dynamic programming problem related to water quota allocation.
#' 
#' @param last_period_data A data.table containing the irrigation-profit data for the last period.
#' @param base_data A data.table containing the base profit data for each state and action.
#' @param annual_quota The annual water quota.
#' @param alloc_year The number of years within one allocation period.
#' @param pen_ratio The penalty ratio for exceeding the quota.
#' @param co_ratio The carry-over ratio for unused quota.
#' @param max_co_ratio The maximum carry-over amount allowed.
#' @param max_ir The maximum irrigation used
#' @param output_file_name The name of the file to save the results.
#' @return Saves the optimal policy and value function to the specified output file.
solve_BI <- function(last_period_data, base_data, annual_quota, alloc_year, pen_ratio, co_ratio, max_co_ratio, max_ir, output_file_name) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # i <- 1
  # #--- annual quota ---#
  # annual_quota <- aq_ay_pen_co_list[i, annual_quota]
  # #--- number of years within one allocation period ---#
  # alloc_year <- aq_ay_pen_co_list[i, alloc_year]
  # #--- total quota per allocation period ---#
  # W_total <- alloc_year * annual_quota
  # #--- penalty ratio ---#
  # pen_ratio <- aq_ay_pen_co_list[i, pen_ratio]
  # #--- carry-over ratio ---#
  # co_ratio <- aq_ay_pen_co_list[i, co_ratio]
  # #--- maximum carry over allowrd ---#
  # max_co_ratio <- aq_ay_pen_co_list[i, max_co_ratio]

  #++++++++++++++++++++++++++++++++++++
  #+ File check
  #++++++++++++++++++++++++++++++++++++
  #--- File names to check ---#
  result_file <- here::here(output_file_name)

  if (file.exists(result_file)) {
    print("Result file exist. Skipping to the next iteration.")
    return(NULL)
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Set up parameters
  #++++++++++++++++++++++++++++++++++++
  #--- number of years ---#
  prod_T <- ((50 %/% alloc_year) + ceiling(50 %% alloc_year)) * alloc_year

  #--- total allocation ---#
  W_total <- alloc_year * annual_quota

  #--- maximum quota allowed to be carried over ---#
  max_quota_carry_over <- W_total * max_co_ratio

  #--- Min and max of the water quota grids ---#
  W_min <- floor(min(-max_ir, -max_ir * pen_ratio + W_total))
  W_max <- W_total + max_quota_carry_over

  # !===========================================================
  # ! t = T
  # !===========================================================
  #++++++++++++++++++++++++++++++++++++
  #+ Utility achievable at t = T conditional on W
  #++++++++++++++++++++++++++++++++++++
  #' Find the best smt and its associated utility for each level of quota in W_ls

  #--- relevant portion of W_ls ---#
  Profit_W_relevant <- last_period_data[W >= W_min & W <= W_max, ]

  #--- W threshold ---#
  # if W_it is greater than this at the last period, then you can irrigate in the first year of the next allocation period
  breaks <- get_break_poitns(W_total, pen_ratio, W_min)

  #--- Combine ---#
  # It is numerically important that the last number is lower than the addition and subtraction from W_threshold
  W_ls <- c(Profit_W_relevant[, W], breaks) %>% sort()

  #++++++++++++++++++++++++++++++++++++
  #+ Value function for t=T [V_T(W)]
  #++++++++++++++++++++++++++++++++++++
  VF_T <- scam(e_pi ~ s(W, k = 20, bs = "mpi"), data = Profit_W_relevant)

  # !===========================================================
  # ! Backward Induction (t < T)
  # !===========================================================
  #++++++++++++++++++++++++++++++++++++
  #+ Define the function to find the best smt
  #++++++++++++++++++++++++++++++++++++
  #' Finds the optimal smt and its associated utility conditional on the level of beginning quota

  find_opt_smt <- function(t) {
    #--- find quota in the next period ---#
    if ((t %% alloc_year) == 0) {
      #--- if the first year of a new allocation period ---#
      temp_data <-
        #--- get all the combinations of smt-year-W_it ---#
        rbindlist(lapply(1:length(W_ls), function(i) cbind(base_data, W_it = W_ls[i]))) %>%
        .[, remainder := W_it - ir] %>%
        .[, carry_over := pmax(0, pmin(max_quota_carry_over, remainder * co_ratio))] %>%
        .[, quota_penalty := remainder * pen_ratio] %>%
        .[, W_next := W_total + ifelse(remainder >= 0, carry_over, quota_penalty)]
    } else {
      #--- if NOT the first year of a new allocation period ---#
      temp_data <-
        rbindlist(lapply(1:length(W_ls), function(i) cbind(base_data, W_it = W_ls[i]))) %>%
        .[, W_next := W_it - ir]
    }

    #--- Limit the lowest quota in the next period to be W_min ---#
    #' This will ensure that W_next does NOT goes outside the range of
    #' the gam estimations.
    temp_data[W_next < W_min, W_next := W_min]
    temp_data[W_next > W_max, W_next := W_max]

    #--- Cannot irrigate if W_it < 0 ---#
    # Note: keep only the rainfed production (which is smt==1)
    temp_data <- temp_data[!(W_it < 0 & smt != 1), ]

    #---------------------
    #- Utility from the next period on
    #---------------------
    #--- unique values of W_next ---#
    W_next_data <-
      temp_data[order(W_next), W_next] %>%
      unique() %>%
      data.table(W_next = .)

    # pred_data <- temp_pi_next$pred_data[[3]]
    # vf_scam <- temp_pi_next$vf_scam[[3]]
    # pred_data[, e_pi_next := predict(vf_scam, newdata = pred_data, se.fit = FALSE)]
    # temp_pi_next$W_min
    # temp_pi_next$W_max

    #---------------------
    #- Find V(W_next)
    #---------------------
    if ((t + 1) == prod_T) {
      temp_pi_next <-
        W_next_data %>%
        setnames("W_next", "W") %>%
        .[, e_pi_next := predict(VF_T, newdata = .)] %>%
        setnames("W", "W_next")
    } else {
      W_vec_apx <-
        data.table(w = W_ls, val = W_ls) %>%
        setattr("sorted", "w") %>%
        .[J(W_next_data[, W_next]), roll = "nearest"] %>%
        .[, val]

      temp_pi_next <-
        W_next_data[, W := W_vec_apx] %>%
        opt_choice_ls[[t + 1]][., on = "W"] %>%
        setnames("e_pi", "e_pi_next") %>%
        .[, smt := NULL]
    }

    #---------------------
    #- Find the best SMT for each value of W_it (starting quota)
    #---------------------
    temp_final <-
      #--- combine the current and future utility datasets  ---#
      temp_pi_next[temp_data, on = "W_next"] %>%
      #--- total profit (current + future) ---#
      .[, pi_sum := profit + sigma * e_pi_next] %>%
      #--- expected profit by SMT and W_it ---#
      .[, .(e_pi = mean(pi_sum)), by = .(smt, W_it)] %>%
      #--- Best SMT by W_it ---#
      .[, .SD[e_pi == max(e_pi), ], by = W_it] %>%
      setnames("W_it", "W")

    return(temp_final)
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Run BI
  #++++++++++++++++++++++++++++++++++++
  opt_choice_ls <- vector(mode = "list", length = prod_T - 1)

  for (t in (prod_T - 1):1) {
    print(t)
    opt_choice_ls[[t]] <- find_opt_smt(t)
  }

  saveRDS(
    opt_choice_ls,
    here::here(output_file_name)
  )

  rm(VF_T, opt_choice_ls)
  gc()

}


# !===========================================================
# ! Helper function
# !===========================================================
# Find maximum utility that can be achieved conditional on W_it
# Only at the final period, there is a financial penalty of $30 per inch of excess water. It is hard to limit certain smt due to uncertainty. This is innocuous as the influence of this does not last longer as you go backward into earlier periods

find_max_u <- function(W_it, data) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # W_it <- -130
  # data <- base_data

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  if (W_it < 0) { # cannot irrigate
    max_e_u <-
      copy(data) %>%
      .[smt == 1] %>%
      .[, excess_w := pmin(0, W_it - ir)] %>%
      .[, profit := profit + 100 * excess_w] %>%
      .[, u_temp := crra_u(0.6, profit)] %>%
      .[, mean(u_temp)]
  } else { # can irrigate
    max_e_u <-
      copy(data) %>%
      .[, excess_w := pmin(0, W_it - ir)] %>%
      .[, profit := profit + 100 * excess_w] %>%
      .[, u_temp := crra_u(0.6, profit)] %>%
      .[, .(e_u = mean(u_temp)), by = smt] %>%
      .[, max(e_u, na.rm = TRUE)]
  }

  temp_data <-
    data.table(
      e_u = max_e_u,
      W = W_it
    )

  return(temp_data)
}

#'  Constant relative risk aversion utility function
#' @param rho The coefficient of relative risk aversion.
#' @param x The profit level.
#' @return The utility value. 
crra_u <- function(rho, x) {
  u <- (x^(1 - rho) - 1) / (1 - rho)
  return(u)
}


# opt_choice <- opt_choice_temp[[48]]
# ggplot(opt_choice) +
#   geom_point(aes(y = e_u, x = W))

# opt_choice <- opt_choice_temp[[49]]
# ggplot(opt_choice) +
#   geom_point(aes(y = e_u, x = W))

#'  Get break points where the slope of V(W) changes
#' @param W_total Total water quota available in the allocation period.
#' @param pen_ratio The penalty ratio for exceeding the quota.
#' @param W_min The minimum water quota level considered.
#' @return A numeric vector of break points where the slope of V(W) changes.
get_break_poitns <- function(W_total, pen_ratio, W_min) {
  temp <-
    data.table(
      i = 1:50
    ) %>%
    .[, part := -W_total * pen_ratio^(i - 1)] %>%
    .[, denom := pen_ratio^i] %>%
    .[, breaks := cumsum(part) / denom] %>%
    .[, dif_breaks := c(1, diff(breaks))] %>%
    .[abs(dif_breaks) > 0.1, ] %>%
    .[breaks >= W_min, ] %>%
    .[, breaks_up := breaks + 0.0001 * (i + 1)] %>%
    .[, breaks_down := breaks - 0.0001 * (i + 1)]

  breaks <- c(temp$breaks_up, temp$breaks_down)

  return(breaks)
}

#'  Find maximum profit that can be achieved conditional on W_it
#' @param W_it The water quota level at the beginning of the period.
#' @param data A data.table containing the profit data for each state and action.
#' @return A data.table with the maximum expected profit and the corresponding water quota level. 
find_max_e_pi <- function(W_it, data) {
  if (W_it < 0) { # cannot irrigate
    max_e_pi <-
      copy(data) %>%
      .[smt == 1] %>%
      .[, excess_w := pmin(0, W_it - ir)] %>%
      .[, profit := profit + 100 * excess_w] %>%
      .[, mean(profit)]
  } else { # can irrigate
    max_e_pi <-
      copy(data) %>%
      .[, excess_w := pmin(0, W_it - ir)] %>%
      .[, profit := profit + 100 * excess_w] %>%
      .[, .(e_pi = mean(profit)), by = smt] %>%
      .[, max(e_pi, na.rm = TRUE)]
  }

  temp_data <-
    data.table(
      e_pi = max_e_pi,
      W = W_it
    )

  return(temp_data)
}
