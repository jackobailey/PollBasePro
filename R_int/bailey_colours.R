#' Jack Bailey's Colour Palette.
#'
#' This is a convenience function that makes it easier for me to make plots that use colours I like.
#' @param ... The name of the colour you want.
#' @export
#'

bailey_colours <- function(...) {

  choice <- c(...)

  cols <- c(
    `red` = "#D10B0C",
    `red2` = "#D52122",
    `red3` = "#D93738",
    `red4` = "#DD4D4E",
    `red5` = "#E16364",
    `red6` = "#E5797A",
    `red7` = "#EA9090",
    `red8` = "#EEA6A6",
    `red9` = "#F2BCBC",
    `red10` = "#F6D2D2",
    `blue` = "#0D68C3",
    `blue2` = "#2375C8",
    `blue3` = "#3983CD",
    `blue4` = "#4F91D3",
    `blue5` = "#659ED8",
    `blue6` = "#7BACDE",
    `blue7` = "#91BAE3",
    `blue8` = "#A7C8E9",
    `blue9` = "#BDD5EE",
    `blue10` = "#D3E3F4",
    `grey` = "#5C5C5C",
    `grey2` = "#6A6A6A",
    `grey3` = "#797979",
    `grey4` = "#888888",
    `grey5` = "#979797",
    `grey6` = "#A6A6A6",
    `grey7` = "#B4B4B4",
    `grey8` = "#C3C3C3",
    `grey9` = "#D2D2D2",
    `grey10` = "#E1E1E1",
    `black` = "#000000",
    `white` = "#FFFFFF"
  )

  if (is.null(choice)) return(cols)

  `names<-`(cols[choice], NULL)
}
