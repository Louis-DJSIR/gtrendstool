#' Explore and filter the list of available search categories in google trends
#' 
#' @return A datatable object which opens an html file in the browser
#' @export
#'
#' @examples

region_search <- function (...) {
	df_categories <- gtrendsR::categories %>%
		dplyr::as_tibble() %>%
		dplyr::mutate(id = as.numeric(id)) %>%
		dplyr::arrange(id) %>%
		unique()

	df_categories %>%
		DT::datatable(options = list(paging = TRUE,
										 pageLength = 10,
										 scrollX = FALSE,
										 scrollY = TRUE,
										 autoWidth = TRUE,
										 server = FALSE,
										 dom = "Bfrtip",
										 buttons = c('csv', 'excel')
										 ),
				      extensions = "Buttons",
				      selection = "Single",
				      filter = "bottom")
}