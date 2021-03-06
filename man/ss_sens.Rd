% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sample_Size_for_EV.R
\name{ss_sens}
\alias{ss_sens}
\title{Minimum sample size for external validation of a clinical prediction model with a dichotomous outcome}
\usage{
ss_sens(alpha, se, d, prev)
}
\arguments{
\item{alpha}{Level of Significance}

\item{se}{Expected sensitivity}

\item{d}{Maximum margin of error}

\item{prev}{Population level prevalence}
}
\value{
A html file contains minimum sample size
}
\description{
Sample size required for a given sensitivity
}
\examples{

#Eg .To target 80\% sensitivity at 95\% confidence level,
# maximum margin of error 5\% for a precision of 95\% and
#assuming 30\% of the population with particular disease.
ss_sens( alpha=0.05, se= 0.8, d=0.05,prev=0.3)

#for different combinations of parameters
ss_sens( alpha=c(0.05), se=c(0.8 , 0.9), d=c(0.05 , 0.1 ),prev=0.3)
}
\references{
Arenas-Cavalli JT, Abarca I, Rojas-Contreras M, Bernuy F, Donoso R. Clinical validation of an artificial intelligence-based diabetic retinopathy screening tool for a national health system. Eye (Lond). 2021 Jan 11. doi: 10.1038/s41433-020-01366-0. Epub ahead of print. Erratum in: Eye (Lond). 2021 Jul 23;: PMID: 33432168.
}
\seealso{
ss_citl, ss_cal_slope , ss_R2val , ss_res_var , ss_spec
}
