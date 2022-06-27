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

	leader <- df_peaks %>%
		head(1)

	remainder <- df_peaks %>%
		tail(-1)

	pb <- progress::progress_bar$new(
		format = "Comparing [:bar] :percent eta: :eta,  update: "
		, total = nrow(remainder)+1
		, clear = FALSE
		, width = 60
		)

	pb$tick()

	for (i in 1:nrow(remainder)) {
		query_temp <- dplyr::bind_rows(leader, remainder[i,]) %>%
			dplyr::select(keyword, geo) %>%
			as.matrix()

		gtrends_temp <- gtrendsR::gtrends(keyword = c(query_temp[1,1], query_temp[2,1])
										  , geo = c(query_temp[1,2], query_temp[2,2])
										  , time = df_peaks$time[1]
										  , gprop = df_peaks$gprop[1]
										  , category = df_peaks$category_id[1])

		winner <- suppressMessages(gtrends_temp$interest_over_time %>%
			dplyr::as_tibble() %>%
			dplyr::filter(as.character(hits) == "100") %>%
			dplyr::group_by(keyword, geo) %>%
			dplyr::summarise(n = dplyr::n()) %>%
			dplyr::ungroup() %>%
			dplyr::arrange(desc(n)) %>%
			head(1))

		loser <- gtrends_temp$interest_over_time %>%
			dplyr::as_tibble() %>%
			dplyr::filter(!(geo == winner$geo & keyword == winner$keyword)) %>%
			head(1)
			
		leader$keyword <- winner$keyword
		leader$geo <- winner$geo

		pb$tick()

		if (i == nrow(remainder)) {
				message(paste0("'", winner$keyword, "' in ", winner$geo, " is the most popular search term."))
			} else {
				message(paste0("'", winner$keyword, "' in ", winner$geo, " is more popular than '", loser$keyword, "' in ", loser$geo, "."))
			}
	}
	df_more <- df_peaks %>%
		dplyr::filter(geo == winner$geo & keyword == winner$keyword)

	df_less <- df_peaks %>%
		dplyr::filter(!(geo == winner$geo & keyword == winner$keyword))

	pb <- progress::progress_bar$new(
		format = "Downloading [:bar] :percent eta: :eta,  update: "
		, total = nrow(df_less)+1
		, clear = FALSE
		, width = 60
		)

	ratio <- vector(mode = "list", length = nrow(df_less))

	pb$tick()

	for (i in 1:nrow(df_less)) {
		query_temp <- dplyr::bind_rows(df_more, df_less[i,]) %>%
			dplyr::select(keyword, geo) %>%
			as.matrix()

		gtrends_temp <- gtrendsR::gtrends(keyword = c(query_temp[1,1], query_temp[2,1])
										  , geo = c(query_temp[1,2], query_temp[2,2])
										  , time = df_peaks$time[1]
										  , gprop = df_peaks$gprop[1]
										  , category = df_peaks$category_id[1])

		ratio[[i]] <- gtrends_temp$interest_over_time %>%
			dplyr::as_tibble() %>%
			dplyr::group_by(keyword, geo) %>%
			dplyr::mutate(hits = ifelse(hits == "<0", "0.5", hits)) %>%
			dplyr::filter(as.numeric(hits) == max(as.numeric(hits))) %>%
			dplyr::arrange(desc(hits)) %>%
			dplyr::group_by(keyword, geo) %>%
			dplyr::slice(1) %>%
			dplyr::ungroup() %>%
			dplyr::mutate(pair = paste0(keyword, "-", geo)) %>%
			dplyr::mutate(pair = paste0(ratio_calc$pair[1], "/", ratio_calc$pair[2]),
						  ratio = paste0(ratio_calc$hits[1], "/", ratio_calc$hits[2])) %>%
			head(1) %>%
			dplyr::select(pair, ratio)

		# loser <- gtrends_temp$interest_over_time %>%
		# 	dplyr::as_tibble() %>%
		# 	dplyr::filter(!(geo == winner$geo & keyword == winner$keyword)) %>%
		# 	head(1)
			
		# df_more$keyword <- winner$keyword
		# df_more$geo <- winner$geo

		pb$tick()

		# if (i == nrow(df_less)) {
		# 		message(paste0("'", winner$keyword, "' in ", winner$geo, " is the most popular search term."))
		# 	} else {
		# 		message(paste0("'", winner$keyword, "' in ", winner$geo, " is more popular than '", loser$keyword, "' in ", loser$geo, "."))
		# 	}
	}

	ratio_matrix <- do.call(dplyr::bind_rows, ratio)

	return(ratio_matrix)
}

# DIAGNOSTICS PLEASE DELETE AFTER THIS LINE

# terms <- c("tennis", "rugby", "football", "cricket", "formula 1")
# regions <- c("Australia", "New South Wales, Australia", "Victoria, Australia", "New Zealand")
# source <- "youtube"
# category <- "sports"

# country_comparator(
# 	terms = c("tennis", "rugby", "football", "cricket", "formula 1")
# 	, regions = c("Australia", "New South Wales, Australia", "Victoria, Australia", "New Zealand")
# 	, source = "youtube"
# 	, category = "sports"
# 	)