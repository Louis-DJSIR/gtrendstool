#' Explore and filter the list of available geographical regions in google trends, this includes both national and sub-national geographical areas
#' 
#' @return A datatable object which opens an html file in the browser
#' @export
#'
#' @examples

region_search <- function (...) {
	df_countries <- gtrendsR::countries %>%
		dplyr::as_tibble() %>%
		dplyr::filter(is.na(sub_code)) %>%
		dplyr::transmute(country_code = country_code
				  , country_name = name) %>%
		dplyr::right_join(gtrendsR::countries, by = "country_code") %>%
		dplyr::transmute(country_name = country_name
						 , region_name = name
						 , code = ifelse(is.na(sub_code), country_code, sub_code)) %>%
		dplyr::mutate(subnational = dplyr::case_when(nchar(code) == 2 ~ FALSE
											  , nchar(code) > 2 ~ TRUE)) %>%
		dplyr::group_by(code) %>%
		dplyr::filter(dplyr::row_number() == 1) %>%
		dplyr::ungroup() %>%
		dplyr::arrange(subnational)

	df_countries %>%
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