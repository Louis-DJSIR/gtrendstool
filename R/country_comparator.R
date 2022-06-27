#' Download and aggregate multiple Google Trends data queries
#'
#' @param terms Google Trends search terms of interest.
#' @param regions Relevant regions to compare for search interest. Please use "region, country" syntax in the list of terms if looking at sub-national geographical areas.
#' @param source Relevant search engine. Choose from c("web", "news", "images", "froogle", "youtube")
#' @param category Search category to be applied. Search category applies to all search terms.
#' @param timeframe Window of time that google trends is searching for hits.
#' @param write If TRUE, the combined downloaded google trends data will be saved as a .csv to the desired destination
#' @param path location for saving combined google trends data .csv file
#'
#' @return A tibble
#' @export
#'
#' @examples

country_comparator <- function(local = FALSE, path = tempdir(), terms, regions, source, category) {

	if(local) {
		df_dl <- read_csv(file.path(path, list.files(path)[grepl("combined_google_trends_", list.files(path))]))
	} else {
		df_dl <- gtrendsdl(terms = terms, regions = regions, source = source, category = category)
	}
	
	df_peaks <- df_dl %>%
		dplyr::filter(hits == "100") 

	# df_keywords <- df_peaks %>%
	# 	dplyr::select(keyword) %>%
	# 	unique()

	# df_regions <- df_peaks %>%
	# 	dplyr::select(region) %>%
	# 	unique()

	# query <- vector(mode = "list", length = (nrow(df_peaks)-1))

	# pb <- progress::progress_bar$new(
	# 	format = "Generating queries [:bar] :percent eta: :eta"
	# 	, total = (nrow(df_peaks)-1)
	# 	, clear = FALSE
	# 	, width = 60
	# 	)

	i <- 1

	query <- dplyr::bind_rows(df_peaks[i,], df_peaks[i+1,])

	for (i in 1:(nrow(df_peaks)-1)) {

		keyword <- as.vector(c(query$keyword[1], query$keyword[2]))
		geo = as.vector(c(query$geo[1], query$geo[2]))
		time = as.vector(c(query$time[1]))
		gprop = as.vector(c(query$gprop[1]))
		category = as.vector(c(query$category_id[1]))

		gtrends_one <- gtrendsR::gtrends(keyword = keyword
										 , geo = geo
										 , time = time
										 , gprop = gprop
										 , category = category)

		gtrends_leader <- gtrends_one$interest_over_time %>%
				dplyr::as_tibble() %>%
				dplyr::filter(hits == "100") %>%
				dplyr::group_by(keyword, geo) %>%
				dplyr::summarise(n = dplyr::n()) %>%
				dplyr::ungroup() %>%
				dplyr::arrange(n) %>%
				head(1) %>%
				dplyr::select(keyword, geo)

		leading <- df_peaks %>%
			dplyr::filter(keyword == gtrends_leader$keyword
				   , geo == gtrends_leader$geo)

		if (i+2 <= nrow(df_peaks)) {
			query <- dplyr::bind_rows(leading, df_peaks[i+2,])
			message(paste0("'", leading$keyword, "' in ", leading$region, " is the leading search term."))
		} else {
			message(paste0("'", leading$keyword, "' in ", leading$region, " is the most popular search term."))
		}
	}

	for (i in 1:(nrow(df_peaks)-1)) {
		query[[i]] <- dplyr::bind_rows(df_peaks[i,], df_peaks[i+1,])
		pb$tick()
		}

}