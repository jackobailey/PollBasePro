#' Compute Lead of One Party Over Another
#'
#' We often want to know how much further one party is ahead of another in the polls. But this can be tricky when the polls contain error. This function takes a dataset with columns named "{{party}}_est" and "{{party}}_err", computes the lead of one party over another, and appends it plus the error in the estimate to the data.
#'
#' @param data A dataset containing variables called {{party}}_est and {{party}}_err
#' @param party1 The name of party
#' @param party2 The name of your date variable in your data.
#' @param name The name . Defaults to "lead".
#' @return A tibble of data.
#' @examples
#' compute_lead(data = pollbasepro, party1 = "con", party2 = "lab")
#' @export

compute_lead <- function(data = NULL, party1 = "con", party2 = "lab", name = "lead"){

  # Return an error if the user provided no data

  if(is.null(data) == T){
    stop("You didn't provide any data! Use the 'data' argument (e.g. 'data = pollbase').")
  }


  # Return an error if the user didn't provide the parties

  if(is.null(party1) == T){
    stop("You didn't provide a first party to use! Try the 'party1' argument (e.g. party1 = 'con').")
  } else if(is.null(party2) == T){
    stop("You didn't provide a second party to use! Try the 'party1' argument (e.g. party2 = 'lab').")
  }


  # Compute lead

  lead <-
    tibble(
      lead_est = data[[paste0(party1, "_est")]] - data[[paste0(party2, "_est")]],
      lead_err = sqrt(data[[paste0(party1, "_err")]]^2 + data[[paste0(party2, "_err")]]^2)
    )


  # Rename variables

  names(lead) <- paste0(name, c("_est", "_err"))


  # Append to data

  data <- tibble::tibble(cbind(data, lead))


  # Return data to user

  return(data)


}
