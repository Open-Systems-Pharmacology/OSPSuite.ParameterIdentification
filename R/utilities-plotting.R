### Commented out until https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/issues/91 is fixed
#' #' @description
#' #' Plot the profiles of the objective function calculated by the calculateOFVProfiles() method.
#' #' @param profiles A list of tibbles, with each tibble containing two columns. The first column should contain
#' #' several values for the parameter, and the second column, called `ofv`, should contain the matching objective function value.
#' #' @return A list of ggplot2 objects, one for each parameter.
#' #' @export
#' plotOFVProfiles <- function(profiles) {
#'   if (missing(profiles)) {
#'     stop(messages$profilesNotSupplied)
#'   }
#'   plotList <- vector(mode = "list", length = length(profiles))
#'   for (idx in seq_along(profiles)) {
#'     parameterName <- names(profiles)[[idx]]
#'     plotList[[idx]] <- ggplot2::ggplot(
#'       data = profiles[[idx]],
#'       ggplot2::aes(x = .data[[parameterName]], y = .data[["ofv"]])
#'     ) +
#'       ggplot2::theme_bw() +
#'       ggplot2::geom_point(ggplot2::aes(col = 1 / ofv)) +
#'       ggplot2::labs(x = parameterName, y = "OFV") +
#'       ggplot2::scale_color_viridis_c() +
#'       ggplot2::guides(col = "none")
#'   }
#'   return(plotList)
#' }
