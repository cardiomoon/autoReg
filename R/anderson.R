#' Remission survival times of 42 leukemia patients
#'
#' A dataset containing  survival time of 42 leukemia patients
#'
#' @format A data.frame with 42 rows and 5 variables
#' \describe{
#' \item{time}{survival time in weeks}
#' \item{status}{censoring status 1=failure 0=censored}
#' \item{sex}{sex 0=Female 1=Male}
#' \item{logWBC}{log white blood cell count}
#' \item{rx}{treatment status 1=control 0 =treatment}
#' }
#' @source David G. Kleinbaum and Mitchel Klein. Survival Analysis. A Self-Learning Text(3rd ed)(Springer,2012) ISBN: 978-1441966452
"anderson"

#' Remission survival times of 42 leukemia patients
#'
#' A dataset containing  survival time of 42 leukemia patients
#' This data is the same data with anderson, but sex and rx variable are factors not numeric
#'
#' @format A data.frame with 42 rows and 5 variables
#' \describe{
#' \item{time}{survival time in weeks}
#' \item{status}{censoring status 1=failure 0=censored}
#' \item{sex}{sex "Female" or "Male}
#' \item{logWBC}{log white blood cell count}
#' \item{rx}{treatment status "treatment" or "control"}
#' }
#' @source David G. Kleinbaum and Mitchel Klein. Survival Analysis. A Self-Learning Text(3rd ed)(Springer,2012) ISBN: 978-1441966452
"anderson1"

