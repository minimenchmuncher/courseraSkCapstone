% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readPackageSettings.R
\name{readPackageSettings}
\alias{readPackageSettings}
\title{Read Package Settings}
\usage{
readPackageSettings(x = "settings.yaml", environ = "Production")
}
\arguments{
\item{x}{a yaml file containing the package settings}

\item{environ}{a string specifying what environment to use, either "Production" or "Testing"}
}
\value{
a list of the settings to use.
}
\description{
Read Package Settings
}

