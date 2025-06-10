.onAttach <- function(lib, pkg) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_inform(
    c(
      "!" = "{.pkg forestTIME.builder} is an {.emph experimental} package and currently a {.emph work-in-progress}. It is {.emph not} an official product of the US Forest Service."
    ),
    class = "packageStartupMessage"
  )
}
# .onAttach()
# suppressPackageStartupMessages(.onAttach())
