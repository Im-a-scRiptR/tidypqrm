% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidymodels_wrappers.R
\name{tune.workflowsets.random.search}
\alias{tune.workflowsets.random.search}
\title{Tune the workflows}
\usage{
tune.workflowsets.random.search(
  workflowset,
  resamples,
  num_cores,
  total_param_sets = 50
)
}
\arguments{
\item{workflowset}{a workflowset supplied by build.workflowset}

\item{resamples}{an rset object supplied by make.rolling.custom.resamples}

\item{num_cores}{a number of cores that workflow_map will use to run in parallel}

\item{total_param_sets}{the grid size of parameters to optimize over}
}
\description{
This function will tune our workflow over each set in the resamples via random
search
}
