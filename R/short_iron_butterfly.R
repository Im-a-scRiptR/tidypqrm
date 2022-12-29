

#' Generate shifly grid
#'
#' Internal: This function samples legs within a range to be tested on the shifly
#'
#' @import purrr
#' @import tibble
#' @import tidyr
#' @importFrom magrittr `%>%`
#' @param min_pctl minimum strike percentile
#' @param max_pctl maximum strike percentile
#' @param incrament the increment on a parameter sequences
#' @export
shifly_K_grid <-
  function(min_pctl = 0.05,max_pctl = 0.95,incrament = 0.0025) {
    purrr::set_names(
      c(
        sample(
          seq(min_pctl*100,50-incrament*100,incrament*100),
          1),
        50,
        sample(
          seq(50+incrament*100,max_pctl*100,incrament*100),
          1)
      ), c("low_leg", "mid_leg", "upp_leg")
    ) %>%
      tibble::enframe() %>%
      tidyr::pivot_wider(names_from = name,values_from = value)
  }

#' Create shifly grid
#'
#' This function amends the base grid created from
#' sim.oos.resamps.and.future.trade
#'
#' @import dplyr
#' @importFrom magrittr `%>%`
#' @param new_iter_tbl an object created from sim.oos.resamps.and.future.trade
#' @param min_pctl minimum strike percentile
#' @param max_pctl maximum strike percentile
#' @param incrament the increment on a parameter sequences
#' @param min_hold_days the minimum number of days to hold the trade
#' @param max_hold_days the maximum number of days to hold the trade
#' which should always be less than the horizon
#' @export
make_shifly_grid <-
  function(new_iter_tbl,
           min_pctl,
           max_pctl,
           incrament,
           min_hold_days,
           max_hold_days) {
    legs <-
      1:nrow(new_iter_tbl) %>%
      lapply(\(i) shifly_K_grid(min_pctl, max_pctl, incrament)) %>%
      dplyr::bind_rows()
    shifly_iter_tbl <-
      legs %>% dplyr::bind_cols(new_iter_tbl[, -(1:2)])
    shifly_iter_tbl$hold_days <-
      1:nrow(shifly_iter_tbl) %>%
      sapply(\(i) sample(seq(min_hold_days, max_hold_days, 1), 1))
    return(shifly_iter_tbl)
  }

#' Trimming for the strikes
#'
#' This function trims up the simulations into a percentiles table with the
#' closest strikes we can reasonably place a shifly on. In the future, REAL
#' historical option values will be used. This is just a cop-out helper function.
#'
#' @importFrom plyr `round_any`
#' @import tidyr
#' @import dplyr
#' @importFrom magrittr `%>%`
#' @param all_outcomes a list of percentiles and corresponding values for
#' each OOS fold tested created by sim.oos.resamps.and.future.trade
#' @param low_leg a single value for the long put in the strangle
#' @param mid_leg a single value for 2 contracts in the short straddle
#' @param upp_leg a single value for the long call in the strangle
#' @export
trim.all.outcomes <-
  function(all_outcomes,low_leg, mid_leg, upp_leg) {
    all_outcomes %>%
      dplyr::bind_rows() %>%
      tidyr::pivot_wider(names_from = id, values_from = value) %>%
      dplyr::select(index,
                    outcome,
                    tidyr::starts_with(paste0(low_leg, "th_pctle")),
                    tidyr::starts_with(paste0(mid_leg, "th_pctle")),
                    tidyr::starts_with(paste0(upp_leg, "th_pctle"))) %>%
      dplyr::rename_with( ~ paste0(c("low_leg","mid_leg","upp_leg")),
                          contains("pctle")) %>%
      dplyr::mutate(across(
        contains(c("low", "mid")),
        ~ dplyr::if_else(
          condition = nchar(plyr::round_any(.x, 1, floor)) >= 5,
          true      = plyr::round_any(.x, 100, floor),
          false     =
            dplyr::if_else(
              condition = nchar(plyr::round_any(.x, 1, floor)) >= 4,
              true      = plyr::round_any(.x, 50, floor),
              false     =
                dplyr::if_else(
                  condition = nchar(plyr::round_any(.x, 1, floor)) >= 3,
                  true      = plyr::round_any(.x, 5, floor),
                  false     =
                    dplyr::if_else(
                      condition = nchar(plyr::round_any(.x, 1, floor)) >= 2,
                      true      = plyr::round_any(.x, 1, floor),
                      false     = plyr::round_any(.x, 1, ceiling)
                    )
                )
            )
        )
      )) %>%
      dplyr::mutate(across(
        contains("upp"),
        ~ dplyr::if_else(
          condition = nchar(plyr::round_any(.x, 1, ceiling)) >= 5,
          true      = plyr::round_any(.x, 100, ceiling),
          false     =
            dplyr::if_else(
              condition = nchar(plyr::round_any(.x, 1, ceiling)) >= 4,
              true      = plyr::round_any(.x, 50, ceiling),
              false     =
                dplyr::if_else(
                  condition = nchar(plyr::round_any(.x, 1, ceiling)) >= 3,
                  true      = plyr::round_any(.x, 5, ceiling),
                  false     =
                    dplyr::if_else(
                      condition = nchar(plyr::round_any(.x, 1, ceiling)) >= 2,
                      true      = plyr::round_any(.x, 1, ceiling),
                      false     = plyr::round_any(.x, 1, ceiling)
                    )
                )
            )
        )
      ))
  }

#' Create option contract arguments
#'
#' This function creates a table of option argument. In the future, support for
#' a continuous dividend yield and risk free rate will be properly canonized.
#' In the future,
#'
#' @importFrom utils `tail`
#' @import dplyr
#' @import lubridate
#' @import rsample
#' @importFrom magrittr `%>%`
#' @param trimmed_outcomes an object created by trim.all.outcomes
#' @param oos_resamples the original and unaltered assessment set of resamples
#' @param price_data a data frame with adjusted close data and an index
#' @export
make.opt.args <-
  function(trimmed_outcomes,oos_resamples,price_data) {
    opt_args <-
      1:nrow(trimmed_outcomes) %>%
      lapply(\(jj) {
        opt_tbl <-
          dplyr::tibble(
            K       = sort(c(as.numeric(trimmed_outcomes[jj, 3:ncol(trimmed_outcomes)]),as.numeric(trimmed_outcomes[jj, 4]))),
            DTE     = nrow(rsample::assessment(oos_resamples$splits[[jj]])),
            VOL     = stats::sd(rsample::analysis(oos_resamples$splits[[jj]]) %>% dplyr::select(outcome) %>% dplyr::pull(outcome) %>% sample(365)) * sqrt(365),
            DIV_Y   = 0,
            RR      = 0,
            FLAG    = c("long_strangle_put", "short_straddle_put","short_straddle_call","long_strangle_call"),
            EXP_T   = lubridate::as_date(utils::tail(rsample::assessment(oos_resamples$splits[[jj]])$index, 1)),
            OPEN_T  = EXP_T - lubridate::days(DTE),
            S_0     = price_data[which.min(abs(price_data$index - OPEN_T)),] %>% dplyr::select(!!my_sym) %>% as.numeric()
          ) %>% suppressWarnings()
        return(opt_tbl)
      }) %>% dplyr::bind_rows()
    return(opt_args)
  }

#' Calculate Option Stuff - Internal
#'
#'
#' This function uses the RQuantlib library to price options
#'
#' @import dplyr
#' @importFrom stringr `str_detect`
#' @importFrom magrittr `%>%`
#' @importFrom RQuantLib `AmericanOption`
#' @param opt_args an object created from the internal calls of
#' get.and.price.rolling.options
#' @export
.calc.and.format.greeks <-
  function(opt_args) {
    opt_args <-
      opt_args %>%
      dplyr::mutate(flag =
                      dplyr::if_else(
                        stringr::str_detect(FLAG,"call"),
                        "call","put"))
    greeks <-
      1:nrow(opt_args) %>%
      lapply(\(ii) {
        out <-
          RQuantLib::AmericanOption(
            type           = opt_args$flag[ii],
            underlying     = opt_args$S_0[ii],
            strike         = opt_args$K[ii],
            dividendYield  = opt_args$DIV_Y[ii],
            riskFreeRate   = opt_args$RR[ii],
            maturity       = opt_args$DTE[ii] / 365,
            volatility     = opt_args$VOL[ii],
            timeSteps      = 150,
            gridPoints     = 149,
            engine         = "CrankNicolson"
          ) %>% .[1:3] %>% dplyr::as_tibble() %>%
          dplyr::rename_with( ~ paste0(c("FV", "DE", "GA")), everything())
        return(opt_args[ii, ] %>% dplyr::bind_cols(out))
      }) %>% dplyr::bind_rows()
    return(greeks)
  }

#' Calculate Option Stuff
#'
#' This function adds in more data that will be helpful in making more accurate
#' readings on option contract values and their greeks, hopefully...
#'
#' @import lubridate
#' @import timetk
#' @import TTR
#' @import dplyr
#' @importFrom magrittr `%>%`
#' @importFrom stats `na.omit`
#' @param opt_args an object created from make.opt.args
#' @param price_data a data frame with adjusted close data and an index
#' @param ret_data a data frame of adj close returns that contains my_sym as
#' a column
#' @export
get.and.price.rolling.options <-
  function(opt_args, price_data, ret_data) {
    d <-
      1:length(unique(opt_args$EXP_T)) %>%
      lapply(\(exp) {
        a <- opt_args %>% dplyr::filter(EXP_T == unique(EXP_T)[exp])
        b <- lubridate::as_date(a$OPEN_T[1]:a$EXP_T[1])
        p <-
          price_data %>%
          dplyr::select(1, !!my_sym) %>%
          timetk::pad_by_time(index, .fill_na_direction = 'updown') %>%
          suppressMessages() %>%
          dplyr::filter(index %in% b)
        vol <-
          ret_data %>%
          dplyr::select(1,!!my_sym) %>%
          timetk::tk_xts(silent = TRUE) %>%
          TTR::runSD(n = a$DTE[1]) %>% na.omit() %>%
          timetk::tk_tbl(silent = TRUE) %>%
          dplyr::mutate(across(where(is.numeric), ~ .x * sqrt(365))) %>%
          dplyr::filter(index %in% b) %>% dplyr::pull(2)
        d <-
          1:length(unique(a$FLAG)) %>%
          lapply(\(flag) {
            d <-
              dplyr::bind_rows(rep(list(a[flag,]), length(p[[2]]))) %>%
              dplyr::mutate(OPEN_T = b,
                            S_0 = p[[2]],
                            VOL =  vol)
          }) %>%
          dplyr::bind_rows() %>%
          dplyr::arrange(OPEN_T)
        return(d)
      }) %>%
      dplyr::bind_rows()
    return(d %>% .calc.and.format.greeks())
  }

#' Make Shifly Values
#'
#' This function generates the rolling shifly values
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr `%>%`
#' @param rolling_options an object created from get.and.price.rolling.options
#' @export
make.shifly.values <-
  function(rolling_options) {
    1:length(unique(rolling_options$EXP_T)) %>%
      lapply(\(exp) {
        strangle_vals <-
          rolling_options %>%
          dplyr::filter(EXP_T == unique(EXP_T)[exp]) %>%
          dplyr::select(OPEN_T, FLAG, flag, FV, K, S_0) %>%
          tidyr::pivot_wider(names_from = FLAG, values_from = flag:S_0) %>%
          dplyr::rename(index = OPEN_T) %>%
          dplyr::mutate(
            shifly_value =
              -(FV_long_strangle_put - FV_short_straddle_put -
                  FV_short_straddle_call + FV_long_strangle_call),
            EXP_T = unique(rolling_options$EXP_T)[exp]
          )
        return(strangle_vals)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-contains(c("flag_","S_0_short","S_0_long_strangle_put"))) %>%
      dplyr::rename_with(~ paste0("S_0"),contains("S_0"))
  }

#' Generate shifly returns
#'
#' This function attaches the rolling options returns for the shifly values.
#' We use a linear method instead of log returns because we are aggregating across
#' strategies and not time.
#'
#' @import dplyr
#' @importFrom magrittr `%>%`
#' @param out an object created from make.shifly.values
#' @export
attach.option.returns <-
  function(out) {
    1:length(unique(out$EXP_T)) %>%
      lapply(\(j) {
        df            <- out %>% dplyr::filter(EXP_T == unique(EXP_T)[j])
        df %>%
          dplyr::bind_cols(1:nrow(df) %>%
                             lapply(\(i) {
                               start_shifly_value     = df$shifly_value[1]
                               current_shifly_value   = df$shifly_value[i]
                               shifly_ret             =
                                 ((current_shifly_value / start_shifly_value) - 1)
                               return(tibble(shifly_ret))
                             }) %>% dplyr::bind_rows())
      }) %>%
      dplyr::bind_rows()
  }

#' Run the Shifly Strategy
#'
#' This strategy implements our constraints/parameters on a wealth generation
#' model centered around rules that govern a short iron butterfly. In the future,
#' taxes and fees will be accounted for. Random leg assignment will also be
#' accounted for.
#'
#' @import dplyr
#' @import tibble
#' @importFrom magrittr `%>%`
#' @param out an object created from attach.option.returns
#' @param initial_balance a single value of the starting account balance
#' @param capital_reserve a single value between 0-1
#' @param profit_take_pct a single value between 0-1
#' @param stop_loss a single value between 0-1
#' @param hold_days a single value for a number of days we choose to hold the trade before BTC
#' @export
run.shifly.strategy <-
  function(out,
           initial_balance,
           capital_reserve,
           profit_take_pct,
           stop_loss,
           hold_days) {
    # Run the balances
    new_bal <- c()
    acc_vals_tbls <- list()
    pct_to_bet <- (1 - capital_reserve)
    for (j in 1:length(unique(out$EXP_T))) {
      # message("Running account values for ",
      #         (out %>% dplyr::filter(EXP_T == unique(EXP_T)[j]))$index[1],
      #         " to ",unique(out$EXP_T)[j])
      df <- out %>% dplyr::filter(EXP_T == unique(EXP_T)[j])
      if (j == 1) {
        bal <- initial_balance * pct_to_bet
        res <- initial_balance * (1 - pct_to_bet)
      } else if (j != 1) {
        bal <- new_bal[[j - 1]][length(new_bal[[j - 1]])] * pct_to_bet
        res <-
          new_bal[[j - 1]][length(new_bal[[j - 1]])] * (1 - pct_to_bet)
      }
      K_vec <- c(df$K_long_strangle_call[1],df$K_long_strangle_put[1])
      widest <-
        K_vec %>%
        sapply(\(x) abs(x - df$K_short_straddle_call[1])) %>% which.max()
      max_risk <-
        (abs(K_vec[widest] - df$K_short_straddle_put[1]) - df$shifly_value[1]) * 100
      total_contracts <-
        plyr::round_any(bal / max_risk, 1, floor)
      max_profit <-
        net_credit <- total_contracts * df$shifly_value[1] * 100
      max_loss <- total_contracts * max_risk
      remaining_bal <- bal - max_loss
      a <-
        df %>%
        dplyr::select(1,shifly_value,shifly_ret) %>%
        tibble::add_column(contracts = total_contracts) %>%
        dplyr::mutate(prem_val = contracts * shifly_value)
      a <-
        a %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          acc_val =
            dplyr::case_when(
              prem_val <= (max_profit / 100) ~
                (res + remaining_bal + max_loss + ((max_profit /100) - prem_val)),
              prem_val >  (max_profit / 100) ~
                res + remaining_bal + max_loss - prem_val + (max_profit /100)
            )
        )
      a$trade_action <- "nothing"
      a$trade_action[hold_days:nrow(a)] <- "BTC"
      a <-
        a %>%
        dplyr::mutate(
          trade_action =
            dplyr::case_when(
              shifly_ret >= stop_loss ~ "BTC",
              shifly_ret <= -profit_take_pct ~ "BTC",
              TRUE ~ trade_action
            )
        ) %>%
        dplyr::ungroup()
      if ("BTC" %in% a$trade_action) {
        a[which(a$trade_action == "BTC"),]$acc_val <-
          a[which(a$trade_action == "BTC")[1],]$acc_val
      }
      acc_vals_tbls[[j]] <- a
      new_bal[j] <- tail(a$acc_val, 1)
    }
    return(acc_vals_tbls %>% dplyr::bind_rows())
  }

