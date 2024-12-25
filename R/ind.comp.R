#' takes r from individual item with outcome to estimate the r between composite of those items and outcome
#' estimation is based on: 
#' use with r_composite, y = ( Sum from i=1 to n of r_i,y) / sqrt( n + 2 × ( Sum of r_i,j over all i<j ) )
#'
#' @param item_outcome a vector of correlations between each item and the outcome
#' @param inter_item a symmetric correlation matrix of inter-item correlations
#' @examples 
#'
#' # example usage
#' # correlations of individual items with the outcome
#' item_outcome <- c(0.5, 0.6, 0.4)
#' 
#' # symmetric inter-item correlation matrix
#' inter_item <- matrix(
#'   c(1.0, 0.3, 0.2,
#'     0.3, 1.0, 0.4,
#'     0.2, 0.4, 1.0),
#'   nrow = 3, byrow = TRUE
#' )
#'
#' # estimate composite correlation
#' ind.comp(item_outcome, inter_item)
#'
#' @importFrom magrittr "%>%"
#' @export
ind.comp <- function(item_outcome, inter_item) {

  n <- length(item_outcome)
  
  if (nrow(inter_item) != n || ncol(inter_item) != n) {
    stop("Dimensions of 'inter_item' must match the length of 'item_outcome'. please revise.")
  }
  
  numerator <- sum(item_outcome)
  
  sum_inter <- sum(inter_item[lower.tri(inter_item)])
  denominator <- sqrt(n + 2 * sum_inter)
  
  composite_r <- numerator / denominator
  return(composite_r)
  
}
