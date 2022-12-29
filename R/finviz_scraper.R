

#' Scrape Finviz
#'
#' This function scrapes and collects data from FinViz.
#'
#' @import rvest
#' @import pbapply
#' @importFrom magrittr `%>%`
#' @param url the url to a finviz.com screener page
#' @export
get_Desired_Tickers_from_Finviz <- function(url) {
  toPct          <- function(x) {
    round(as.numeric(gsub("%", "", x)), 4)
  }
  formatVOL      <- function(x) {
    as.numeric(gsub(",", "", x))
  }
  format.AVE_VOL <- function(x) {
    as.numeric(gsub("M", "", x))
  }
  formatKMB      <- function(x) {
    tmp <- gsub("K", "e3", x)
    tmp <- gsub("M", "e6", tmp)
    tmp <- gsub("B", "e9", tmp)
    as.numeric(tmp)
  }

  # SCRAPE FINVIZ TBALES FOR DATA

  data             <- rvest::read_html(url)
  PAGES            <-
    data %>%
    rvest::html_nodes(".screener-pages") %>%
    .[[length(data %>%
                rvest::html_nodes(".screener-pages"))]] %>%
    rvest::html_text2() %>%
    as.numeric()
  LAST             <- 20 * (PAGES - 1) + 1
  accPAGES         <-
    lapply(as.list(seq(1, LAST, 20)),
           function(x)
             paste0(url, "&r=", x, ""))
  getFinViz        <-  function(x)
  {
    Sys.sleep(sample(seq(0.70,1.00,0.01)*10,1))
    data           <- rvest::read_html(x)
    data %>%
      rvest::html_nodes("table.table-light") %>%
      rvest::html_table(header = T, fill = T)
  }
  finViz1          <- pbapply::pblapply(accPAGES, function(x) {
    tmp            <- try(getFinViz(x))
    if (!inherits(tmp, 'try-error'))
      tmp
  })
  finViz1          <- finViz1[lapply(finViz1, length) > 0]
  dat              <-
    do.call(rbind, lapply(finViz1, function(x)
      do.call(rbind, x)))
  dat$Date         <- rep(Sys.Date(), nrow(dat))
  dat$Price        <- as.numeric(dat$Price)
  dat$Change       <- toPct(dat$Change)
  dat$Volume       <- formatVOL(dat$Volume)
  tick_leng        <- length(dat$Ticker)
  Tickers          <- as.vector(dat$Ticker[1:tick_leng])
  message("Scrape Complete")
  return(list(dat, Tickers))
}

