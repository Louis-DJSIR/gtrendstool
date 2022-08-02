#' Generates a list of codes corresponding to the relevant search category applying to all terms
#'
#' @param category search category of interest, must be common to all search terms. Search terms cannot be compared using different search categories. To search without filtering for categories, please select "All categories"
#'
#' @return A tibble of categories and associated codes
#' @export
#'
#' @examples

categories_list <- function(category) {
	cat_list <- gtrendsR::categories %>%
		dplyr::as_tibble() %>%
		dplyr::filter(grepl(tolower(category), tolower(name), .))

	message("To search without category filters please search for and select: All categories")

	if(nrow(cat_list) == 1) {
		return(cat_list)
	} else {
		if(nrow(cat_list) == 0) {
			stop(paste0("Error: '", category, "' did not correspond to any Google Trends categories. Please try a different term."))
		}

		if(nrow(cat_list) > 1) {
			print(cat_list)

			new_info <- readline(paste0("'", category, "' received multiple category matches, please specify a second term to further narrow the list of categories: "))

			cat_list2 <- cat_list %>%
				dplyr::filter(grepl(tolower(as.character(new_info)), tolower(name), .))

			if(nrow(cat_list2) == 0) {
				cat_list <- cat_list
				stop(paste0("'", new_info, "' did not receive a match in the list of categories, please try again."))
			}

			if(nrow(cat_list2) == 1) {
				cat_list <- cat_list2
			}

			if(nrow(cat_list2) > 1) {
				print(cat_list2)
				line_no <- readline(paste0("'", new_info, "' received multiple matches, please specify which row number you would like to select: "))
				cat_list <- cat_list2 %>%
					dplyr::slice(as.numeric(line_no))
			}
		}
	}

	if(nrow(cat_list) > 1) {
		warning("Multiple categories have been selected, before using this output further, please filter to the specific category of interest.")
	}
	message(paste0("'", head(cat_list, 1)$name , "' chosen, thank you."))

	assign("selected_category", cat_list, envir=globalenv())
	selected_category <<- cat_list
	return(cat_list)
}