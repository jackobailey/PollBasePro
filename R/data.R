#' Raw PollBase data in long-format
#'
#' A dataset containing Mark Pack's raw pollbase figures for each of Britain's three main parties, tidied and converted to long-format for easy analysis.
#'
#' @format A data frame:
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


#' Daily Estimates of British Voting Intention, 1955 to Present
#'
#' A dataset containing daily estimates of aggregate voting intention in Britain for each of the country's three largest parties between 1955 and the present.
#'
#' @format A data frame:
#' \describe{
#'   \item{date}{Date}
#'   \item{election}{Date of last general election}
#'   \item{govt}{Largest party in government after the last general election}
#'   \item{con_est}{Posterior mean: Conservative voting intention}
#'   \item{con_err}{Posterior error: Conservative voting intention}
#'   \item{lab_est}{Posterior mean: Labour voting intention}
#'   \item{lab_err}{Posterior error: Labour voting intention}
#'   \item{lib_est}{Posterior mean: Liberal voting intention}
#'   \item{lib_err}{Posterior error: Liberal voting intention}
#'   \item{con_ldr}{Leader of the Conservative Party}
#'   \item{lab_ldr}{Leader of the Labour Party}
#'   \item{lib_ldr}{Leader of the Liberals (various forms)}
#'   \item{week}{Weekly subset indicator}
#'   \item{month}{Monthly subset indicator}
#'   \item{quarter}{Quarterly subset indicator}
#'   \item{year}{Yearly subset indicator}
#' }
"pollbasepro"


#'  List of "Red Wall" Constituencies
#'
#' A dataset containing all "red wall" constituencies targetted by the Conservatives at the 2019 election. Compiled from James Kanagasoorium's original list that spawned the term.
#'
#' @format A data frame:
#' \describe{
#'   \item{constituency_name}{Red wall constituency name}
#'   \item{constituency_code}{Red wall constituency code}
#' }
"red_wall"


#'  List of Labour, Conservative, and Liberal Party Leaders
#'
#' A dataset containing all leaders of the Conservative Party, the Labour Party, and the Liberals/Liberal Democrats.
#'
#' @format A data frame:
#' \describe{
#'   \item{leader}{Name of party leader}
#'   \item{start}{Date they became party leader}
#'   \item{end}{Date they stood down as party leader}
#'   \item{party}{The leader's party}
#' }
"party_leaders"


#'  List of British Election Dates Since Universal Suffrage
#'
#' A dataset containing the dates of all British General Elections since the advent of universal suffrage in 1928.
#'
#' @format A data frame:
#' \describe{
#'   \item{date}{Date of election}
#' }
"election_dates"


#'  List of British Prime Ministers
#'
#' A dataset containing the names of all British Prime Ministers.
#'
#' @format A data frame:
#' \describe{
#'   \item{prime_minister}{Name of Prime Minister}
#'   \item{pm_party}{Prime Minister's party}
#'   \item{start}{First day in term as Prime Minister}
#'   \item{end}{Last day in term as Prime Minister}
#' }
"prime_ministers"
