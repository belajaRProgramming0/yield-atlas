format_number <- function(x) {
  if (length(x) != 1L || !is.finite(x)) return("Unknown")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}
format_percent <- function(x) {
  if (length(x) != 1L || !is.finite(x)) return("Unknown")
  sprintf("%.1f%%", 100 * x)
}
format_compact <- function(x) {
  if (length(x) != 1L || !is.finite(x)) return("Unknown")
  if (abs(x) >= 1e6) return(sprintf("%.2fM", x / 1e6))
  if (abs(x) >= 1e3) return(sprintf("%.1fK", x / 1e3))
  format_number(x)
}
valid_volume <- function(x) sum(x[is.finite(x) & x > 0])
calculate_scenario <- function(amounts, funded_percent, fee_percent) {
  stopifnot(length(funded_percent) == 1L, is.finite(funded_percent),
            funded_percent >= 0, funded_percent <= 100,
            length(fee_percent) == 1L, is.finite(fee_percent), fee_percent >= 0, fee_percent <= 5)
  referred <- valid_volume(amounts)
  funded <- referred * funded_percent / 100
  list(referred = referred, funded = funded, fee = funded * fee_percent / 100)
}
