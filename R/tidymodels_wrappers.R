
#' Short function to split the data
#'
#' This function is a wrapper for initial_time_split from rsample
#'
#' @import rsample
#' @param my_datasets data sets created from yf.load.and.curate.financial.dataset
#' @param prop a fraction or a decimal with value between 0-1
#' @export
first_split <-
  function(my_datasets,prop) {
    data <- my_datasets[[1]]
    return(rsample::initial_time_split(data, prop))
  }

#' Builds custom workflowset
#'
#' This function builds a workflowset that will be plugged into the
#' workflow_map function from workflowsets
#'
#' @import parsnip
#' @import recipes
#' @import recipeselectors
#' @import glmnet
#' @import tune
#' @importFrom magrittr `%>%`
#' @param split a split created by running first.split once and then running it
#' again on the listed analysis set of the resulting split object.
#' @export
build.workflowset <-
  function(split) {
    base_model <-
      parsnip::linear_reg() %>%
      parsnip::set_mode("regression") %>%
      parsnip::set_engine("lm")
    # Take the below out when the issue is fixed about the warning message
    recipes::recipe(outcome ~ ., rsample::analysis(split)) %>%
      recipeselectors::step_select_vip(
        recipes::all_numeric_predictors(),
        outcome   = "outcome",
        model     = base_model,
        threshold = .90
      ) %>%
      recipes::step_mutate(index = row_number(index)) %>%
      recipes::prep() %>%
      recipes::bake(rsample::analysis(split)) %>%
      suppressWarnings() %>% suppressMessages() %>%
      invisible()
    # Take the above out when the issue is fixed about the warning message
    my_recipe <-
      recipes::recipe(outcome ~ ., rsample::analysis(split)) %>%
      recipeselectors::step_select_vip(
        recipes::all_numeric_predictors(),
        outcome   = "outcome",
        model     = base_model,
        threshold = tune::tune()
      ) %>%
      recipes::step_mutate(index = row_number(index))
    glmnet_model <-
      parsnip::linear_reg(penalty = tune::tune(),
                 mixture = tune::tune()) %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("regression")
    return(workflowsets::workflow_set(
      preproc = list(my_recipe = my_recipe),
      models = list(my_model = glmnet_model),
      cross = TRUE
    ))
  }

#' DOW helper function
#'
#' This function returns a boolean that helps the resampling process locate
#' a user specified DOW.
#'
#' @param date the date we want to test against the supplied DOW
#' @param DOW_start the DOW we want each OOS assessment set to start on
#' @export
is.DOW.to.STO <-
  function(date, DOW_start) {
    weekdays(date) == DOW_start
  }

#' Customized Resampling
#'
#' This function makes customized cumulative resamples with a motivation to
#' keep each OOS set within the resamples starting and ending on the same DOW
#'
#' @import rsample
#' @importFrom magrittr `%>%`
#' @param split a split object, preferably a validation split
#' @param horizon the investment horizon
#' @param DOW_start the DOW we want each OOS assessment set to start on
#' @param max_slices the number of folds/slices in our resamples
#' @param use_analysis if TRUE, uses the analysis set of the split, if FALSE,
#' uses the assessment set of the split
#' @export
make.rolling.custom.resamples <-
  function(split,horizon,DOW_start,max_slices,use_analysis = TRUE) {
    if(use_analysis == TRUE) {
      resamples <-
        rsample::rolling_origin(
          data       = rsample::analysis(split),
          initial    = 365,
          assess     = (horizon + 7),
          cumulative = TRUE,
          skip       = (horizon + 7),
        )
    } else {
      resamples <-
        rsample::rolling_origin(
          data       = rsample::assessment(split),
          initial    = 365,
          assess     = (horizon + 7),
          cumulative = TRUE,
          skip       = (horizon + 7),
        )
    }
    resamples <-
      resamples %>%
      dplyr::slice(if (nrow(.) > max_slices)
        sample(1:nrow(.), max_slices, FALSE)
        else
          1:nrow(.)) %>%
      list() %>%
      lapply(\(x) rsample::manual_rset(splits = x$splits, ids = x$id)) %>%
      .[[1]]
    for(ii in 1:nrow(resamples)) {
      df <- resamples$splits[[ii]]
      tt <- which(is.DOW.to.STO(df$data[df$out_id, ]$index,DOW_start))[1]
      tt_0 <- tt-1
      resamples$splits[[ii]]$out_id <- df$out_id[tt]:(df$out_id[tt]+horizon)
      if(tt_0 != 0) {
        resamples$splits[[ii]]$in_id <- 1:df$out_id[tt_0]
      }
    }
    return(resamples)
  }

#' Tune the workflows
#'
#' This function will tune our workflow over each set in the resamples via random
#' search
#'
#' @import yardstick
#' @import workflowsets
#' @import tune
#' @import doParallel
#' @importFrom magrittr `%>%`
#' @param workflowset a workflowset supplied by build.workflowset
#' @param resamples an rset object supplied by make.rolling.custom.resamples
#' @param num_cores a number of cores that workflow_map will use to run in parallel
#' @param total_param_sets the grid size of parameters to optimize over
#' @export
tune.workflowsets.random.search <-
  function(workflowset,
           resamples,
           num_cores,
           total_param_sets = 50) {
    doParallel::registerDoParallel(cores = num_cores)
    tuned_models <-
      workflowset %>%
      workflowsets::workflow_map(
        fn                   = "tune_grid",
        resamples            = resamples,
        grid                 = total_param_sets,
        metrics              = yardstick::metric_set(rmse, rsq),
        verbose              = TRUE,
        control              =
          tune::control_grid(
            save_pred        = FALSE,
            parallel_over    = "resamples",
            save_workflow    = TRUE
          )
      )
    doParallel::stopImplicitCluster()
    return(tuned_models)
  }

#' Extract the best workflow
#'
#' This function helps extract the best performing workflow from the output of
#' tune.workflowsets.random.search
#'
#' @import workflowsets
#' @import tune
#' @importFrom magrittr `%>%`
#' @param tuned_models a table created by tune.workflowsets.random.search
#' @param split a split object, preferably a validation split
#' @param metric a metric, class == character, that was supplied to the yardstick::metric_set in
#' tune.workflowsets.random.search
#' @export
extract_best.oos.workflow <-
  function(tuned_models,split,metric = "rmse") {
    sub_models <-
      tuned_models %>%
      workflowsets::rank_results(select_best = TRUE) %>%
      dplyr::filter(.metric == metric) %>%
      dplyr::select(model, .config, !!metric := mean, rank)
    best <-
      tuned_models$wflow_id %>%
      purrr::map( ~ tuned_models %>%
                    workflowsets::extract_workflow_set_result(.x) %>%
                    tune::select_best(metric = metric))
    tuning_results <-
      1:nrow(tuned_models) %>%
      lapply(function(ii) {
        tuned_models %>%
          workflowsets::extract_workflow(tuned_models$wflow_id[ii]) %>%
          tune::finalize_workflow(best[[ii]]) %>%
          tune::last_fit(split = split)
      })
    best_wf <-
      which.max(lapply(tuning_results,workflowsets::collect_metrics) %>%
                  sapply(\(x) x$.estimate[2]))

    return(tuning_results[[best_wf]])
  }

#' Plot best workflow linear regression
#'
#' Take the output of extract.best.oos.workflow and returns a classic
#' linear regression plot for observed vs. predicted
#'
#' @import tune
#' @import ggplot2
#' @importFrom magrittr `%>%`
#' @param tuning_results a table created by extract.best.oos.workflow
#' @export
plot_best.wf.linear.reg <-
  function(tuning_results) {
    tuning_results %>%
      tune::collect_predictions() %>%
      ggplot2::ggplot(aes(x = outcome, y = .pred)) +
      ggplot2::geom_abline(color = "gray50", lty = 2) +
      ggplot2::geom_point(alpha = 0.5) +
      tune::coord_obs_pred() +
      ggplot2::labs(x = "observed", y = "predicted")
  }


