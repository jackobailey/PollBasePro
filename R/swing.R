#' Compute Two-Party Swing
#'
#' This convenience function takes two numeric vectors of vote share data and returns the estimated two-party swing.
#'
#' @param party_a A vector of length 2. The first value corresponds to the most recent election.
#' @param party_b A vector of length 2. The first value corresponds to the most recent election.
#' @param type Which swing calculation to use. Either "butler" or "steed". Defaults to "butler".
#' @examples
#' \dontrun{
#' swing(party_a = c(0.5, 0.4), party_b = c(0.5, 0.6), type = "butler")
#' }
#' @export

swing <- function(party_a = NULL, party_b = NULL, type = "butler"){

  # Check party_a =! NULL, party_b =! NULL, and both != NULL

  if(is.null(party_a) == T & is.null(party_b) == T){
    stop("Both party_a and party_b are NULL. Please input values. E.g. 'party_a = c(0.5, 0.4), party_b = c(0.5, 0.6)'")
  } else if(is.null(party_a) == T){
    stop("party_a is NULL. Please input values. E.g. 'party_a = c(0.5, 0.4)'")
  } else if(is.null(party_b) == T){
    stop("party_b is NULL. Please input values. E.g. 'party_a = c(0.5, 0.4)'")
  }


  # Check that party_a and party_b are of type vector

  if(is.vector(party_a) == F & is.vector(party_b) == F){
    stop("Both party_a and party_b are not numeric. Please input numeric values. E.g. 'party_a = c(0.5, 0.4), party_b = c(0.5, 0.6)'")
  } else if(is.vector(party_a) == F){
    stop("party_a is not numeric. Please input numeric values. E.g. 'party_a = c(0.5, 0.4)'")
  } else if(is.vector(party_b) == F){
    stop("party_b is not numeric. Please input numeric values. E.g. 'party_a = c(0.5, 0.4)'")
  }


  # Check that party_a and party_b are of length 2

  if(length(party_a) != 2 & length(party_b) != 2){
    stop("Both party_a and party_b are not of length 2. Please input numeric vectors of length 2. E.g. 'party_a = c(0.5, 0.4), party_b = c(0.5, 0.6)'")
  } else if(length(party_a) != 2){
    stop("party_a is not of length 2. Please input numeric vectors of length 2. E.g. 'party_a = c(0.5, 0.4)'")
  } else if(length(party_b) != 2){
    stop("party_b is not of length 2. Please input numeric vectors of length 2. E.g. 'party_a = c(0.5, 0.4)'")
  }


  # Check that type is "butler" or "steed"

  if(!tolower(type) %in% c("butler", "steed")){
    stop("Please choose a valid formula to use. Can be either 'butler' or 'steed'.")
  }


  # Compute swing

  if(type == "butler"){

    # Compute within-party change

    swing_a <- party_a[[1]] - party_a[[2]]
    swing_b <- party_b[[1]] - party_b[[2]]


    # Compute swing

    swing <- (swing_a - swing_b)/2


  } else if(type == "steed"){

    # Compute within-party change

    swing_a <- party_a[[1]] / (party_a[[1]] + party_b[[1]])
    swing_b <- party_a[[2]] / (party_a[[2]] + party_b[[2]])


    # Compute swing

    swing <- swing_a - swing_b

  }


  # Return swing to user

  return(swing)


}
