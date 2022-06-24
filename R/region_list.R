#' Generates a list of codes corresponding to the relevant search regions
#'
#' @param regions Google Trends regions such as countries and/or sub-divisions for states/provinces/territories. Please use syntax "region, country" if you require sub-national geographies. For example regions = c("Australia", "Victoria, Australia", "India") is a suitable input into this function
#'
#' @return A tibble of regions and associated codes
#' @export
#'
#' @examples

region_list <- function(regions) {

	list_items <- vector(mode = "list", length = length(regions))

	for (i in 1:length(regions)) {
		if (grepl(",", regions[i])) {
			region <- gsub(" ", "", strsplit(regions[i], ",")[[1]][1])
			country <- gsub(" ", "", strsplit(regions[i], ",")[[1]][2])
			code <- gtrendsR::countries %>%
				dplyr::as_tibble() %>%
				dplyr::filter(name %in% toupper(country)) %>%
				dplyr::select(country_code) 

			list_items[[i]] <- gtrendsR::countries %>%
				dplyr::as_tibble() %>%
				dplyr::filter(name %in% toupper(region)) %>%
				dplyr::filter(country_code %in% code) %>%
				dplyr::rename(region = name) 

			if(nrow(list_items[[i]]) != 1) {
				stop(paste0("Error: ", regions[i], " does not correspond to a unique google trends region, if ", regions[i], " is a sub-national geography, please re-specify the region as ", paste0(regions[i], ", country")))
			}
		} else {
			list_items[[i]] <- gtrendsR::countries %>%
				dplyr::as_tibble() %>%
				dplyr::filter(name %in% toupper(regions[i])) %>% 
				dplyr::rename(region = name)
			if(nrow(list_items[[i]]) != 1) {
				stop(paste0("Error: ", regions[i], " does not correspond to a unique google trends region, if ", regions[i], " is a sub-national geography, please re-specify the region as ", paste0(regions[i], ", country")))
			}
		}
	}

	iso_list <- do.call(dplyr::bind_rows, list_items) %>%
		dplyr::mutate(sub_code = ifelse(is.na(sub_code), country_code, sub_code)) %>%
		dplyr::select(-country_code)

	return(iso_list)
}