#' Generates a list of codes corresponding to the relevant search category applying to all terms
#'
#' @param category search category of interest, must be common to all search terms. Search terms cannot be compared using different search categories
#'
#' @return A tibble of categories and associated codes
#' @export
#'
#' @examples

categories_list <- function(category) {
	cat_list <- gtrendsR::categories %>%
		as_tibble() %>%
		filter(grepl(tolower(category), tolower(name), .))

	if(nrow(cat_list) == 1) {
		return(cat_list)
	} else {
		if(nrow(cat_list) == 0) {
			stop(paste0("Error: ", category, " did not correspond to any Google Trends categories. Please try a different term."))
		}

		if(nrow(cat_list) >= 1) {
			print(cat_list)

			new_info <- readline(paste0(category, " received multiple matches, please specify a second term to further narrow the list of categories: "))

			cat_list %>%
				filter(grepl(tolower(as.character(new_info)), tolower(name), .))
		}
	}

	return(cat_list)
}