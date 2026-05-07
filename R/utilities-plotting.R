#' Plot Objective Function Value (OFV) Profiles
#'
#' @description
#' Visualizes the OFV profiles produced by
#' `ParameterIdentification$calculateOFVProfiles()`. Each profile is rendered
#' as a scatter plot of the OFV against the corresponding parameter value,
#' with point color encoding OFV (lower = brighter, reversed Viridis).
#'
#' @details
#' Around a (local) minimum the objective function is expected to be convex
#' and roughly parabolic. Inspecting the OFV profile around a tentative
#' optimum is therefore a quick visual check that the optimization has
#' converged to a sensible point in parameter space.
#'
#' Because parameter paths in OSP simulations can be long
#' (e.g. `Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec`),
#' the x-axis label uses only the last `|`-separated segment, while the full
#' path is shown as the plot title.
#'
#' @param profiles A named list of tibbles as returned by
#'   `ParameterIdentification$calculateOFVProfiles()`. Each tibble must
#'   contain exactly two columns: the parameter values (named after the
#'   parameter's path) and the matching objective function values (named
#'   `ofv`). The list names must match the parameter-value column names.
#'
#' @return A list of `ggplot` objects, one per parameter profile, in the
#'   same order as `profiles`.
#'
#' @seealso [ParameterIdentification] for `calculateOFVProfiles()`.
#'
#' @examples
#' \dontrun{
#' # piTask is a configured ParameterIdentification instance
#' ofvProfiles <- piTask$calculateOFVProfiles()
#' plots <- plotOFVProfiles(ofvProfiles)
#' plots[[1]]
#' }
#'
#' @export
plotOFVProfiles <- function(profiles) {
  if (missing(profiles)) {
    stop(messages$profilesNotSupplied())
  }
  plotList <- vector(mode = "list", length = length(profiles))
  for (idx in seq_along(profiles)) {
    parameterName <- names(profiles)[[idx]]
    axisLabel <- sub(".*\\|", "", parameterName)
    plotList[[idx]] <- ggplot2::ggplot(
      data = profiles[[idx]],
      ggplot2::aes(x = .data[[parameterName]], y = .data[["ofv"]])
    ) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(ggplot2::aes(col = .data[["ofv"]]), shape = 16, size = 2) +
      ggplot2::labs(title = parameterName, x = axisLabel, y = "OFV") +
      ggplot2::scale_color_viridis_c(direction = -1) +
      ggplot2::guides(col = "none")
  }
  return(plotList)
}
