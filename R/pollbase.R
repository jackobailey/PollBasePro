#' Raw PollBase data in long-format
#'
#' A dataset containing Mark Pack's raw pollbase figures for each of Britain's three main parties, tidied and converted to long-format for easy analysis.
#'
#' @format A data frame with 6,124 rows and 12 variables:
#' \describe{
#'   \item{id}{Unique poll identification number}
#'   \item{election}{Date of last general election}
#'   \item{govt}{Largest party in government after the last general election}
#'   \item{start}{First day of fieldwork}
#'   \item{end}{Last day of fieldwork}
#'   \item{pollster}{Polling company that conducted the poll}
#'   \item{n}{Sample size}
#'   \item{con}{Voting intention: Conservative}
#'   \item{lab}{Voting intention: Labour}
#'   \item{lib}{Voting intention: Liberal}
#'   \item{con_ldr}{Leader of the Conservative Party}
#'   \item{lab_ldr}{Leader of the Labour Party}
#'   \item{lib_ldr}{Leader of the Liberals (various forms)}
#' }
"pollbase"
