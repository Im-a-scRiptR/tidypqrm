
#' Reading in tickers for features
#'
#' This function is used to read in a list of tickers. This can be provided by
#' running get_Desired_Tickers_from_Finviz and saving the second list item as
#' the file name within the current directory.
#'
#' @param file a file name within the directory containing a list of tickers
#' @param num_stocks number of stocks to add on as features
#' @export
make.feature.symbols.vec <-
  function(file, num_stocks) {
    readRDS(file)[1:num_stocks]
  }

#' Cleaning the ticker set
#'
#' This function cleans the ticker list provided by removing your target
#' ticker from the set if and only if it is present inside the list.
#'
#' @param feat_syms a list of tickers to be used a features
#' @param my_sym a single ticker
#' @export
clean.symbols <-
  function(feat_syms, my_sym) {
    if (my_sym %in% feat_syms) {
      feat_syms <- feat_syms[-which(feat_syms == my_sym)]
    }
    symbols <- c(my_sym, feat_syms)
    return(symbols)
  }

#' Download Yahoo Finance data
#'
#' This is a safe wrapper to get adjusted close prices from yahoo finance
#'
#' @import quantmod
#' @importFrom magrittr `%>%`
#' @param symbol single/vector of tickers to retrieve adjusted close
#' @param yearz a look-back in amount of years, can be double or integer
#' @export
safe.yf.adjusted.price.data.retrieval <-
  function(symbol, yearz) {
    p <-
      try(quantmod::Ad(try(quantmod::getSymbols(
        Symbols     = symbol,
        auto.assign = FALSE,
        warnings    = FALSE,
        verbose     = FALSE,
        from        = Sys.Date() - (365 * yearz)
      ))
      )) %>%
      suppressWarnings() %>% suppressMessages()
    Sys.sleep(1)
    return(p)
  }

#' Clean the download Yahoo Finance data
#'
#' This function downloads data, reductively joins, by "index", each stock's
#' adj close vector, then pads the adj close using an updown method.
#'
#' @import timetk
#' @import dplyr
#' @import purrr
#' @import pbapply
#' @importFrom magrittr `%>%`
#' @param symbols with the target ticker in first position,
#' supply this vector of tickers to retrieve adjusted close
#' @param yearz a look-back in amount of years, can be double or integer
#' @export
gather_and.pad.yf.adj.price.data <-
  function(symbols,yearz) {
    price_data <-
      symbols %>%
      pbapply::pblapply(\(symbol) {
        safe.yf.adjusted.price.data.retrieval(symbol, yearz)
      }) %>%
      purrr::keep(\(x) !inherits(x,"try-error")) %>%
      purrr::map(\(x) x %>% timetk::tk_tbl(silent = TRUE)) %>%
      purrr::keep(\(x) !inherits(x,"try-error")) %>%
      remove.young.stocks() %>%
      purrr::reduce(dplyr::inner_join, by = "index") %>%
      dplyr::rename_with(\(x) stringr::str_replace(x, ".Adjusted", "")) %>%
      dplyr::arrange(index) %>%
      timetk::pad_by_time(.fill_na_direction = 'updown') %>% suppressMessages()
    return(price_data)
  }

#' Remove stocks that aren't old enough
#'
#' This function removes any stocks that do not meet the minimum data
#' requirement.
#'
#' @import lubridate
#' @import purrr
#' @importFrom magrittr `%>%`
#' @param price_data a data frame with adjusted close data and an index
#' @export
remove.young.stocks <-
  function(price_data) {
    main <- price_data[[1]]$index[1]
    dates <-
      price_data %>%
      purrr::map(~ .x$index[1]) %>%
      purrr::as_vector() %>%
      lubridate::as_date()
    if(sum(which(dates != main)) > 0) {
      return(price_data[-which(dates != main)])
    } else {
      return(price_data)
    }
  }

#' Create a log returns data set
#'
#' This function creates the "approximate invariants" from the
#' price_data, aka "risk drivers, that we want to model. It also adjusts
#' for any latent dates up until the day before Sys.Date.
#'
#' @import TTR
#' @import timetk
#' @import dplyr
#' @importFrom magrittr `%>%`
#' @param price_data a data frame with adjusted close data and an index
#' @export
generate.log.returns.data <-
  function(price_data) {
    ret_data <-
      price_data %>%
      suppressMessages() %>%
      timetk::tk_xts(silent = TRUE) %>%
      TTR::ROC() %>% .[-1] %>%
      timetk::tk_tbl(silent = TRUE)
    # Fixes latent dates without any data, so we manually pad with 0's
    ret_data <-
      ret_data %>%
      dplyr::bind_rows(
        dplyr::tibble(index = seq(ret_data$index[nrow(ret_data)],(Sys.Date()-1),by = 1)[
          which(!(seq(ret_data$index[nrow(ret_data)],(Sys.Date()-1),by = 1) %in% ret_data$index))
        ]) %>% dplyr::bind_cols(
          ret_data[1,2:ncol(ret_data)] %>% dplyr::mutate(across(everything(), ~ 0))
        )
      )
    return(ret_data)
  }

#' Loads Data Sets
#'
#' This function loads the price and returns data sets.
#'
#' @importFrom magrittr `%>%`
#' @param my_sym a single ticker
#' @param num_features number of stocks to add on as features
#' @param yearz a look-back in amount of years, can be double or integer
#' @param ticker_file a file name within the directory containing a list of tickers
#' @export
generate.yf.price.and.returns.datasets <-
  function(my_sym, num_features, yearz, ticker_file) {
    feat_syms <-
      make.feature.symbols.vec(
        file = ticker_file,
        num_stocks = num_features)
    if (my_sym %in% feat_syms) {
      feat_syms <- feat_syms[-which(feat_syms == my_sym)]
    }
    symbols <- c(my_sym, feat_syms)
    price_data <-
      symbols %>% gather_and.pad.yf.adj.price.data(yearz)
    ret_data   <-
      price_data %>%
      generate.log.returns.data()
    return(list(price_data, ret_data))
  }

#' Final formatting for price and return data sets
#'
#' This function loads two data sets: in position 1, the price_data -
#' "risk drivers", and in position 2, the ret_data - "invariants". We also
#' format the ret_data to be more easily compatible with the custom ML wrappers.
#'
#' @import dplyr
#' @importFrom magrittr `%>%`
#' @param my_sym a single ticker
#' @param num_features number of stocks to add on as features
#' @param yearz a look-back in amount of years, can be double or integer
#' @param ticker_file a file name within the directory containing a list of tickers
#' @export
yf.load.and.curate.financial.dataset <-
  function(my_sym,num_features,yearz,ticker_file) {
    # Load Data
    sym_data   <-
      generate.yf.price.and.returns.datasets(my_sym,num_features,yearz,ticker_file)
    # Assign Data
    price_data <- sym_data[[1]]
    ret_data   <- sym_data[[2]]
    # Final Curation
    ret_data    <-
      ret_data %>%
      dplyr::rename(index = 1, outcome = 2) %>% suppressMessages()
    return(list(ret_data, price_data))
  }

