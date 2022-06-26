#' Finds the benchmark term that other terms in the selected list will be calibrated against for consistency.
#'
#' @param terms Google Trends search terms of interest.
#' @param regions Relevant regions to compare for search interest. Please use "region, country" syntax in the list of terms if looking at sub-national geographical areas.
#' @param source Relevant search engine. Choose from c("web", "news", "images", "froogle", "youtube")
#' @param category Search category to be applied. Search category applies to all search terms.
#'
#' @return Country/Term pair
#' @export
#'
#' @examples

benchmark <- function (terms, regions, source, category) {
	selected_regions <- region_list(regions)

	terms <- terms %>%
		as_tibble()

	names(terms) <- category
	
	df_combined <- selected_regions %>%
		full_join(terms, by = character())
	num <- 1
	benchmark <- matrix(nrow = length(regions), ncol = 2)
	
	for (i in unique(df_combined$region)) {
		
	}
}
