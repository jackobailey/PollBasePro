#' Barnett Formula
#'
#' The British government uses the Barnett formula to allocate money for public expenditure in Scotland, Wales, and Northern Ireland based on changes to funding in England (and something other nations too). This function takes the necessary inputs and returns the expected change in funding expected under the Barnett formula.
#'
#' @param eng_fund Amount of extra funding allocated to England.
#' @param pop_prop Population proportion compared to England.
#' @param comp_fct Comparability factor as a proportion. Value reflects how comparable the English departmental programme is to the devolved administration's programme.
#' @return A vector of numeric data
#' @examples
#' barnett(eng_fund = 1e+09, pop_prop = .1034, comp_fct = .997)
#' @export

barnett <- function(eng_fund, pop_prop, comp_fct){

  # Return an error if the user provided no English funding data

  if(is.null(eng_fund) == T){
    stop("You didn't say how much extra funding allocated to England! Use the 'eng_fund' argument (e.g. 'eng_fund = 1000000').")
  }


  # Return an error if the user didn't provide a population proportion

  if(is.null(pop_prop) == T){
    stop("You didn't provide a population proportion to use! Try the 'pop_prop' argument (e.g. pop_prop = 0.1034).")
  }


  # Return an error if the user didn't provide a comparability factor

  if(is.null(pop_prop) == T){
    stop("You didn't provide a comparability factor to use! Try the 'comp_fct' argument (e.g. comp_fct = 0.997).")
  }


  # Return an error if the user provides any non-numeric data

  if(is.numeric(eng_fund) == F | is.numeric(pop_prop) == F | is.numeric(comp_fct) == F ){
    stop("All data must be numeric for the function to work.")
  }


  # Run the formula

  barnett <- eng_fund * pop_prop * comp_fct


  # Return the answer to the user

  return(barnett)

}
