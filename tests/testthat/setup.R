# Disable showtext for snapshot tests.
#
if (requireNamespace("showtext", quietly = TRUE)) {
  showtext::showtext_auto(enable = FALSE)
}
