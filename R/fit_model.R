#' Fit Poll Pooling Model
#'
#' This function provides a simple way to fit a variant of Simon Jackman's "polling the polls" model.
#'
#' @param data Raw pollbase data.
#' @param init Initial election date.
#' @param final Final election date.
#' @param party A string reflecting a party. Either "con", "lab", or "lib".
#' @param alpha_init The party's vote share at the initial election.
#' @param alpha_final The party's vote share at the final election.
#' @param refresh How often to report model iterations. Defaults to 0.
#' @export

fit_model <- function(data, init, final, party, alpha_init, alpha_final, refresh = 0){

  # Get pipe from Magrittr

  `%>%` <- magrittr::`%>%`


  # Print party and election

  print(paste0("Fitting model: ", party, " ", init))


  # Subset data

  data <- data[data$election == init, ]


  # Run model

  if(init < "2019-12-12"){

    # Compile Stan model

    model <-
      cmdstanr::cmdstan_model(
        here::here("models", "model_2anchors.stan"),
        cpp_options = list(stan_threads = TRUE)
      )


    # Create data for Stan

    stan_data <-
      list(
        N = nrow(data),
        T = (lubridate::interval(init, final)/lubridate::days(1)) + 1,
        P = length(unique(data$pollster)),
        y = data[[party]],
        s = sqrt((data[[party]]*(1 - data[[party]]))/(ifelse(is.na(data$n) == T, data$n_est, data$n)/data$days)),
        index = data$index,
        pollster = as.numeric(factor(data$pollster)),
        alpha_init = alpha_init,
        alpha_final = alpha_final
      )


    # Sample from model

    fit <-
      model$sample(
        data = stan_data,
        seed = 666,
        chains = 4,
        parallel_chains = 4,
        threads_per_chain = 3,
        refresh = refresh,
        show_messages = FALSE,
        init =
          list(
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data)))
          ),
        adapt_delta = 0.99,
        max_treedepth = 15
      )

  } else if(init == "2019-12-12"){

    # Compile Stan model

    model <-
      cmdstanr::cmdstan_model(
        here::here("models", "model_1anchor.stan"),
        cpp_options = list(stan_threads = TRUE)
      )


    # Create data for Stan

    stan_data <-
      list(
        N = nrow(data),
        T = round((lubridate::interval(init, Sys.Date())/lubridate::days(1)) + 1, 0),
        P = length(unique(data$pollster)),
        y = data[[party]],
        s = sqrt((data[[party]]*(1 - data[[party]]))/(ifelse(is.na(data$n) == T, data$n_est, data$n)/data$days)),
        index = data$index,
        pollster = as.numeric(factor(data$pollster)),
        alpha_init = alpha_init
      )


    # Sample from model

    fit <-
      model$sample(
        data = stan_data,
        seed = 666,
        chains = 4,
        parallel_chains = 4,
        threads_per_chain = 3,
        refresh = 0,
        show_messages = FALSE,
        init =
          list(
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data))),
            list(alpha = rep(alpha_init, nrow(data)))
          ),
        adapt_delta = 0.99,
        max_treedepth = 15
      )

  }


  # Get model summary

  mod_sum <-
    fit$summary(variables = "alpha") %>%
    dplyr::filter(stringr::str_detect(variable, "alpha") == T) %>%
    dplyr::mutate(
      index =
        variable %>%
        stringr::str_remove("alpha\\[") %>%
        stringr::str_remove("\\]"),
      date = lubridate::as_date({{init}}) + (dplyr::row_number() - 1)
    ) %>%
    dplyr::select(
      date,
      est = mean,
      sd
    ) %>%
    dplyr::mutate(
      party = {{party}}
    )


  # Return model summary to user

  return(mod_sum)

}
