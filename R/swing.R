#' Compute Two-Party Swing
#'
#' This convenience function takes two numeric vectors of vote share data and returns the estimated two-party swing.
#'
#' @param a1 Party A vote share at time 1.
#' @param a2 Party A vote share at time 2.
#' @param b1 Party B vote share at time 1.
#' @param b2 Party B vote share at time 2.
#' @param type Which swing calculation to use. Either "butler" or "steed". Defaults to "butler".
#' @examples
#' \dontrun{
#' swing(a1 = .5, a2 = .4, b1 = .5, b2 = .6, type = "butler")
#' }
#' @export

swing <- function(a1, a2, b1, b2, type = "butler"){


  # Check that type is "butler" or "steed"

  if(!tolower(type) %in% c("butler", "steed")){
    stop("Please choose a valid formula to use. Can be either 'butler' or 'steed'.")
  }


  # Compute swing

  if(type == "butler"){

    # Compute swing

    swing <- ((a2 - a1) - (b2 - b1))/2


  } else if(type == "steed"){


    # Compute swing

    swing <- (a2 / (a2 + b2)) - (a1 / (a1 + b1))

  }


  # Return swing to user

  return(swing)


}
