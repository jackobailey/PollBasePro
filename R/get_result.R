#' Get Election Result Based on Party and Election
#'
#' Given a vector of dates, this function provides a vector of the same length that corresponds to the date of the last election for each element in the vector. Applies only to the 1935 election onwards.
#'
#' @param election A date corresponding to a UK election.
#' @param party A string reflecting a party. Either "con", "lab", or "lib".
#' @export

get_result <- function(election = "1997-05-01", party = "lab"){

  # Get date

  result <- dplyr::case_when(
    # 1955
    election == "1955-05-26" & party == "con" ~ 49.3/100,
    election == "1955-05-26" & party == "lab" ~ 47.4/100,
    election == "1955-05-26" & party == "lib" ~ 2.8/100,
    # 1959
    election == "1959-10-08" & party == "con" ~ 48.8/100,
    election == "1959-10-08" & party == "lab" ~ 44.6/100,
    election == "1959-10-08" & party == "lib" ~ 6.0/100,
    # 1964
    election == "1964-10-15" & party == "con" ~ 42.9/100,
    election == "1964-10-15" & party == "lab" ~ 44.8/100,
    election == "1964-10-15" & party == "lib" ~ 11.4/100,
    # 1966
    election == "1966-03-31" & party == "con" ~ 41.4/100,
    election == "1966-03-31" & party == "lab" ~ 48.8/100,
    election == "1966-03-31" & party == "lib" ~ 8.6/100,
    # 1970
    election == "1970-06-18" & party == "con" ~ 46.2/100,
    election == "1970-06-18" & party == "lab" ~ 43.8/100,
    election == "1970-06-18" & party == "lib" ~ 7.6/100,
    # 1974 Feb
    election == "1974-02-28" & party == "con" ~ 38.6/100,
    election == "1974-02-28" & party == "lab" ~ 38.0/100,
    election == "1974-02-28" & party == "lib" ~ 19.8/100,
    # 1974 Oct
    election == "1974-10-10" & party == "con" ~ 36.6/100,
    election == "1974-10-10" & party == "lab" ~ 40.2/100,
    election == "1974-10-10" & party == "lib" ~ 18.8/100,
    # 1979
    election == "1979-05-03" & party == "con" ~ 44.9/100,
    election == "1979-05-03" & party == "lab" ~ 37.7/100,
    election == "1979-05-03" & party == "lib" ~ 14.1/100,
    # 1983
    election == "1983-06-09" & party == "con" ~ 43.5/100,
    election == "1983-06-09" & party == "lab" ~ 28.3/100,
    election == "1983-06-09" & party == "lib" ~ 26.0/100,
    # 1987
    election == "1987-06-11" & party == "con" ~ 43.3/100,
    election == "1987-06-11" & party == "lab" ~ 31.5/100,
    election == "1987-06-11" & party == "lib" ~ 23.1/100,
    # 1992
    election == "1992-04-09" & party == "con" ~ 42.8/100,
    election == "1992-04-09" & party == "lab" ~ 35.2/100,
    election == "1992-04-09" & party == "lib" ~ 18.3/100,
    # 1997
    election == "1997-05-01" & party == "con" ~ 31.0/100,
    election == "1997-05-01" & party == "lab" ~ 44.0/100,
    election == "1997-05-01" & party == "lib" ~ 17.0/100,
    # 2001
    election == "2001-06-07" & party == "con" ~ 32.7/100,
    election == "2001-06-07" & party == "lab" ~ 42.0/100,
    election == "2001-06-07" & party == "lib" ~ 18.8/100,
    # 2005
    election == "2005-05-05" & party == "con" ~ 33.2/100,
    election == "2005-05-05" & party == "lab" ~ 36.2/100,
    election == "2005-05-05" & party == "lib" ~ 22.6/100,
    # 2010
    election == "2010-05-06" & party == "con" ~ 36.9/100,
    election == "2010-05-06" & party == "lab" ~ 29.7/100,
    election == "2010-05-06" & party == "lib" ~ 23.6/100,
    # 2015
    election == "2015-05-07" & party == "con" ~ 37.7/100,
    election == "2015-05-07" & party == "lab" ~ 31.2/100,
    election == "2015-05-07" & party == "lib" ~ 8.1/100,
    # 2017
    election == "2017-06-08" & party == "con" ~ 43.4/100,
    election == "2017-06-08" & party == "lab" ~ 41.0/100,
    election == "2017-06-08" & party == "lib" ~ 7.6/100,
    # 2019
    election == "2019-12-12" & party == "con" ~ 44.7/100,
    election == "2019-12-12" & party == "lab" ~ 32.9/100,
    election == "2019-12-12" & party == "lib" ~ 11.8/100
  )


  # Return vector to user

  return(result)

}

