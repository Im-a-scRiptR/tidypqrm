
#' Futurize the data
#'
#' This function take a data frame and extends it out to the investment horizon
#'
#' @import rsample
#' @importFrom magrittr `%>%`
#' @importFrom lubridate `as_date`
#' @param data a data frame which should be from my_datasets
#' @param length_out the investment horizon
#' @export
make.full.future.split <-
  function(data,length_out = 30) {
    future_table <-
      colnames(data)[2:ncol(data)] %>%
      purrr::map(~ tibble(index = lubridate::as_date(data$index[nrow(data)]:(data$index[nrow(data)] + length_out)),!!.x := NA)) %>%
      purrr::reduce(dplyr::inner_join, by = "index")
    future_table <- future_table[-which(future_table$index %in% data$index),]
    future_table[which(future_table$index < Sys.Date()),2:ncol(future_table)] <- 0
    full_table <- data %>% dplyr::bind_rows(future_table)
    indices <- list(list(analysis = which(!is.na(full_table$outcome)), assessment = which(is.na(full_table$outcome))))
    future_split <- lapply(indices, rsample::make_splits, data = full_table)[[1]]
    return(future_split)
  }

#' Convert to simulated resamples
#'
#' This function takes an reset object and converts all OOS sets to simulated
#' data by way of MASS::mvrnorm.
#'
#' @import rsample
#' @importFrom magrittr `%>%`
#' @import dplyr
#' @param rset_obj an rset object created by make.rolling.custom.resamples
#' @export
amend.and.simulate.resamples <-
  function(rset_obj) {
    for (ii in 1:length(rset_obj$splits)) {
      an <- rsample::analysis(rset_obj$splits[[ii]])
      as <- rsample::assessment(rset_obj$splits[[ii]])
      meanz <-
        an %>%
        dplyr::summarise(across(where(is.double),  ~ mean(.x))) %>%
        dplyr::select(-1) %>% as.numeric()
      sims <- sim.my.features(as, an, meanz)
      sims$outcome <- as$outcome
      rset_obj$splits[[ii]]$data[which(rset_obj$splits[[ii]]$data$index %in% sims$index),] <-
        sims
    }
    return(rset_obj)
  }

#' Simulate Features
#'
#' This function takes the components of a split object and simulates the OOS set
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats cov
#' @importFrom magrittr `%>%`
#' @import dplyr
#' @import tibble
#' @param as the analysis set to take a covariance matrix from
#' @param an the assessment set to replace data for
#' @param meanz the colmeans of the analysis set
#' @export
sim.my.features <-
  function(as, an, meanz) {
    sim <-
      MASS::mvrnorm(n = nrow(as),
                    mu = meanz,
                    Sigma = stats::cov(an[, -1])) %>%
      dplyr::as_tibble() %>% tibble::add_column(index = as$index, .before = 1)
    sim[which(rowMeans(as[, -1]) == 0), -1] <- 0
    return(sim)
  }

#' Fit and Simulate the data sets
#'
#' Internal -
#' WARNING: parallelization used. This function is responsible for running
#' predictions on simulated data. The more simulations, the higher the accuracy,
#' but accuracy may be marginal after a certain amount. The goal is to return the
#' percentiles that the model predicts.
#'
#' @import pbmcapply
#' @import parallelly
#' @import tidyr
#' @import ggplot2
#' @import gghighlight
#' @importFrom magrittr `%>%`
#' @import rsample
#' @import dplyr
#' @import tibble
#' @importFrom stats `predict`
#' @param fitt the best workflow embedded in the output of extract.best.oos.workflow
#' @param split a split object, preferably NOT the validation set
#' @param my_sym a single ticker
#' @param start_price the last price before the prediction occurs
#' @param num_sims number of runs/calls to MASS::mvrnorm
#' @param probs the terminal adj close predicted percentiles to summarise
#' @export
sim.and.fit <-
  function(fitt,
           split,
           my_sym,
           start_price,
           num_sims = 500,
           probs = c(.05,.30,.50,.70,.95)) {
    an <- rsample::analysis(split)
    as <- rsample::assessment(split)
    meanz <-
      an %>%
      dplyr::summarise(across(where(is.double),  ~ mean(.x))) %>%
      dplyr::select(-1) %>% as.numeric()
    sims <-
      1:num_sims %>%
      pbmcapply::pbmclapply(\(ii) {
        sim.my.features(as, an, meanz)
      },
      mc.cores = (parallelly::availableCores() - 2))
    tmppp <-
      1:length(sims) %>%
      pbmcapply::pbmclapply(\(ii) {
        fitt %>%
          predict(sims[[ii]]) %>%
          dplyr::rename_with( ~ paste0("sim_", ii), everything())
      }, mc.cores = (parallelly::availableCores() - 2))
    pred_meanz <- tmppp %>% sapply(\(x) x[[1]] %>% mean())
    best_guess <-
      which.min(abs(pred_meanz) - abs(mean(pred_meanz)))[1]
    plot_data_2 <-
      tibble(index = as$index) %>%
      dplyr::bind_cols(tmppp %>%
                         dplyr::bind_cols() %>%
                         dplyr::mutate(across(
                           !contains("index"),
                           ~ exp(cumsum(.x)) * start_price
                         ))) %>%
      dplyr::rename_with( ~ paste0("best_guess"), ends_with(paste0("sim_", best_guess))) %>%
      dplyr::relocate(best_guess, .after = 1)
    if (!is.na(as$outcome[1])) {
      plot_data_2 <-
        plot_data_2 %>%
        tibble::add_column(as %>% dplyr::select(2) %>% dplyr::mutate(across(
          where(is.numeric),
          ~ exp(cumsum(.x)) * start_price
        )),
        .before = 2) %>%
        tidyr::gather("id", "value", 2:ncol(.))
      percentiles_df <-
        plot_data_2 %>%
        dplyr::filter(id != "outcome") %>%
        dplyr::group_by(index) %>%
        dplyr::summarise(value_percentiles = stats::quantile(value, probs)) %>%
        dplyr::ungroup() %>% dplyr::group_by(index) %>%
        dplyr::mutate(percentile = paste0(probs * 100, "th_pctle")) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = percentile, values_from = value_percentiles) %>%
        tidyr::gather("id", "value", 2:ncol(.)) #%>%
      plot_data_2 <-
        plot_data_2 %>% dplyr::bind_rows(percentiles_df)
      highlight_data <-
        plot_data_2 %>% dplyr::filter(id %in% c("outcome", "best_guess"))
    } else {
      plot_data_2 <-
        plot_data_2 %>% tidyr::gather("id", "value", 2:ncol(.))
      percentiles_df <-
        plot_data_2 %>%
        dplyr::filter(id != "outcome") %>%
        dplyr::group_by(index) %>%
        dplyr::summarise(value_percentiles = stats::quantile(value, !!probs)) %>%
        dplyr::ungroup() %>% dplyr::group_by(index) %>%
        dplyr::mutate(percentile = paste0(probs * 100, "th_pctle")) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = percentile, values_from = value_percentiles) %>%
        tidyr::gather("id", "value", 2:ncol(.)) #%>%
      plot_data_2 <-
        plot_data_2 %>% dplyr::bind_rows(percentiles_df)
      highlight_data <-
        plot_data_2 %>% dplyr::filter(id == "best_guess")
    }
    return(
      list(
        plot_data_2,
        highlight_data,
        plot_data_2 %>%
          ggplot(aes(
            index, value, colour = id, group = id
          )) +
          geom_line(show.legend = FALSE) +
          gghighlight(id %in% c(
            "outcome", "best_guess", unique(percentiles_df$id)
          )) +
          ggtitle(paste0(
            my_sym, " prediction from ", as$index[1], " to ", as$index[nrow(as)]
          ))
      )
    )
  }

#' Fit and Simulate the data sets
#'
#' Internal -
#' WARNING: parallelization used. This function is responsible for running
#' predictions on simulated data. The more simulations, the higher the accuracy,
#' but accuracy may be marginal after a certain amount. The goal is to return the
#' percentiles that the model predicts.
#'
#' @import rsample
#' @import dplyr
#' @importFrom magrittr `%>%`
#' @importFrom quantmod `getQuote`
#' @param split a split object, preferably NOT the validation set
#' @param fitt the best workflow embedded in the output of extract.best.oos.workflow
#' @param my_sym a single ticker
#' @param price_data a data frame with adjusted close data and an index
#' @param num_sims number of runs/calls to MASS::mvrnorm
#' @param probs the terminal adj close predicted percentiles to summarise
#' @export
workflow.backtest <-
  function(split,fitt,my_sym,price_data,num_sims,probs) {
    start_price <-
      price_data %>%
      dplyr::select(1,!!my_sym) %>%
      dplyr::filter(index <= rsample::analysis(split)$index[nrow(rsample::analysis(split))]) %>%
      dplyr::arrange(desc(index)) %>%
      dplyr::slice(1) %>%
      dplyr::pull(!!my_sym)
    if (is.na(start_price)) {
      start_price <- quantmod::getQuote(my_sym)$Last
    }
    plot_data <-
      fitt %>%
      sim.and.fit(split, my_sym, start_price, num_sims, probs) %>%
      suppressMessages() %>% suppressWarnings()
    return(plot_data)
  }

#' Fit and Simulate the data sets
#'
#' Internal -
#' WARNING: parallelization used. This function is responsible for running
#' predictions on simulated data. The more simulations, the higher the accuracy,
#' but accuracy may be marginal after a certain amount. The goal is to return the
#' percentiles that the model predicts.
#'
#' @import dplyr
#' @importFrom magrittr `%>%`
#' @param resamples an rset object supplied by make.rolling.custom.resamples
#' @param fitt the best workflow embedded in the output of extract.best.oos.workflow
#' @param oos_split a split object, preferably NOT the validation set
#' @param my_sym a single ticker
#' @param price_data a data frame with adjusted close data and an index
#' @param num_sims number of runs/calls to MASS::mvrnorm
#' @param probs the terminal adj close predicted percentiles to summarise
#' @export
sim.and.backtest.workflow.resamples <-
  function(resamples,fitt,oos_split,my_sym,price_data,num_sims,probs) {
    insamp_outcomes <-
      resamples %>%
      amend.and.simulate.resamples() %>%
      dplyr::pull(splits) %>%
      lapply(function(x) {
        return(x %>% workflow.backtest(fitt, my_sym, price_data, num_sims, probs))})
    plots <- insamp_outcomes %>% lapply(\(x) x[[3]])
    insamp_outcomes <-
      insamp_outcomes %>%
      lapply(function(x) x[[1]]) %>%
      lapply(function(x) {
        x %>%
          dplyr::filter(stringr::str_detect(id, "best_guess|pctle|outcome")) %>%
          dplyr::group_by(id) %>% dplyr::filter(index == max(index))
      })
    oos_outcome <-
      oos_split %>%
      workflow.backtest(fitt, my_sym, price_data, num_sims, probs)
    plots <- c(plots,list(oos_outcome[[3]]))
    oos_outcome <-
      oos_outcome[[1]] %>%
      dplyr::filter(stringr::str_detect(id, "best_guess|pctle|outcome")) %>%
      dplyr::group_by(id) %>% dplyr::filter(index == max(index))
    all_outcomes <-
      insamp_outcomes %>% append(list(oos_outcome)) %>% lapply(dplyr::ungroup)
    return(list(all_outcomes,plots))
  }

#' Create optimization grid
#'
#' This function creates a hyperparameter grid that will be random searched
#'
#' @importFrom tidyr `crossing`
#' @param initial_balance starting account balance
#' @param profit_take_pct a sequence
#' @param stop_loss a sequence
#' @param capital_reserve a sequence
#' @param min_pctl minimum strike percentile
#' @param max_pctl maximum strike percentile
#' @param pct_below_mid threshold below middle percentile strike
#' @param incrament the incrament on a parameter sequences
#' @param total_param_sets total paramaters to random search
#' @export
make.hyperparameter.table <-
  function(initial_balance,profit_take_pct,stop_loss,capital_reserve,
           min_pctl,max_pctl,pct_below_mid,incrament,total_param_sets) {
    # Generating parameters to search over
    iter_tbl <-
      tidyr::crossing(
        tibble(
          short_put_pctl  = sample(seq(min_pctl, 0.50 - pct_below_mid, incrament)),
          short_call_pctl = sample(seq(.50 + pct_below_mid, max_pctl , incrament))
        ),
        initial_balance,
        profit_take_pct,
        stop_loss,
        capital_reserve
      )
    # Prevents errors and samples max rows if necessary
    if(total_param_sets > nrow(iter_tbl)) total_param_sets <- nrow(iter_tbl)
    # Not going to sample the entire thing...therefore, random search
    iters <- sample(1:nrow(iter_tbl),total_param_sets)
    new_iter_tbl <- iter_tbl[iters,]
    return(new_iter_tbl)
  }

#' Fit and Simulate
#'
#' This function serves as the simulator for the adjusted close price of the
#' target asset. Naturally, we can place bounds on the parameters, but watch out!
#' Different strategies cannot always take the same parameters!
#'
#' @importFrom magrittr `%>%`
#' @import rsample
#' @param split a split object
#' @param tuning_results a table created by extract.best.oos.workflow
#' @param horizon the investment horizon
#' @param price_data a data frame with adjusted close data and an index
#' @param my_sym a single ticker
#' @param num_sims number of runs/calls to MASS::mvrnorm
#' @param incrament the increment on a parameter sequences
#' @param min_pctl minimum strike percentile
#' @param max_pctl maximum strike percentile
#' @param pct_below_mid threshold below middle percentile strike
#' @param initial_balance starting account balance
#' @param profit_take_pct a sequence
#' @param stop_loss a sequence
#' @param capital_reserve a sequence
#' @param total_param_sets total paramaters to random search
#' @param max_asessment_slices max slices/folds in resamples to test
#' @export
sim.oos.resamps.and.future.trade <-
  function(split,tuning_results,horizon,price_data,my_sym,
           num_sims             = 1000,
           incrament            = 0.0025,
           min_pctl             = 0.05,
           max_pctl             = 0.95,
           pct_below_mid        = 0.10,
           initial_balance      = 100000,
           profit_take_pct      = seq(0.05, 1.00, 0.01),
           stop_loss            = seq(0.05, 1.00, 0.01),
           capital_reserve      = seq(0.00, 0.20, 0.01),
           total_param_sets     = 1000,
           max_asessment_slices = 10
  ) {
    fitt             <-  fit(tuning_results$.workflow[[1]], rsample::analysis(split))
    future_split     <-  split$data %>% make.full.future.split(horizon)
    new_iter_tbl     <-
      make.hyperparameter.table(
        initial_balance,
        profit_take_pct,
        stop_loss,
        capital_reserve,
        min_pctl,
        max_pctl,
        pct_below_mid,
        incrament,
        total_param_sets
      )
    oos_resamples  <-
      split %>%
      make.rolling.custom.resamples(
        horizon    = horizon,
        DOW_start  = weekdays(Sys.Date()),
        max_slices = max_asessment_slices,
        use_analysis = FALSE
      )
    all_outcomes    <-
      oos_resamples %>%
      sim.and.backtest.workflow.resamples(
        fitt,
        future_split,
        my_sym,
        price_data,
        num_sims,
        probs = seq(min_pctl, max_pctl, incrament)
      ) %>% .[[1]]
    future_prediction <- all_outcomes[[length(all_outcomes)]]
    all_outcomes      <- all_outcomes[-length(all_outcomes)]
    return(list(all_outcomes, future_prediction, new_iter_tbl,oos_resamples))
  }


