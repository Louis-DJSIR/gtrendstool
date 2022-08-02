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

gtrendsdl <- function(terms, regions, source, category, write = FALSE, path = tempdir()) {
	selected_regions <- region_list(regions)

	if(!("selected_category" %in% ls())) {
		selected_category <<- categories_list(category)
	} else {
		selected_category <<- selected_category
	}

	if(!("timeframe" %in% ls())) {
		timeframe <<- time_selector()
	} else {
		timeframe <<- timeframe
	}

	df_regions <- benchmark(terms, regions, source, category) %>%
		dplyr::left_join(selected_regions, by = "region")

	terms <- terms %>%
		dplyr::as_tibble()

	names(terms) <- "terms"

	df_combined <- df_regions %>%
		unique() %>%
		dplyr::full_join(terms %>% dplyr::as_tibble() %>% unique(), by = character())

	df_compare <- df_combined %>%
		dplyr::filter(benchmark != terms)

	df_benchmark <- df_combined %>%
		dplyr::filter(benchmark == terms)

	query <- vector(mode = "list", length = nrow(df_compare))

	k = 1

	pb <- progress::progress_bar$new(
		format = "Generating queries [:bar] :percent eta: :eta"
		, total = nrow(df_compare)
		, clear = FALSE
		, width = 60
		)

	for (i in df_benchmark$region) {
		bench <- df_benchmark %>% 
			dplyr::filter(region == i)

		comp <- df_compare %>%
			dplyr::filter(region == i)

		for (j in comp$terms) {
			query[[k]] <- dplyr::bind_rows(bench, comp %>% dplyr::filter(terms == j))
			k <- k + 1
		pb$tick()
		}
	}

	data_list <- vector(mode = "list", length = length(query))

	pb <- progress::progress_bar$new(
		format = "Downloading [:bar] :percent eta: :eta,  update: "
		, total = length(query)+1
		, clear = FALSE
		, width = 60
		)

	pb$tick()

	for (i in 1:length(query)) {
		keyword <- query[[i]]$terms %>%
			as.vector()

		geo <- query[[i]]$sub_code %>%
			as.vector()

		category_id <- query[[i]]$category_id %>%
				as.vector() %>%
				as.character() %>%
				unique()


		gtrends_raw <- gtrendsR::gtrends(keyword = keyword, geo = geo, time = timeframe, gprop = source, category = category_id)

		data_list[[i]] <- gtrends_raw$interest_over_time %>%
			dplyr::as_tibble() %>%
			dplyr::left_join(df_benchmark %>% dplyr::select(region, sub_code), by = c("geo" = "sub_code")) %>%
			dplyr::mutate(category = as.character(category)) %>%
			dplyr::rename(category_id = category) %>%
			dplyr::left_join(df_benchmark %>% dplyr::select(category, category_id) %>% unique(), by = "category_id")
		
		pb$tick()

		message(paste0(keyword[1], " in ", geo[1], " and ", keyword[2], " in ", geo[2], " completed [", i, "/", length(query), "]"))
	}

	df_bound <- do.call(rbind, data_list) %>%
		unique()

	if(write) {
		df_bound %>%
			write.csv(file = file.path(path, paste0("combined_google_trends_", source,".csv")))

		message(paste0("file ", paste0("combined_google_trends_", source,".csv"), " written in ", path))
	}

	return(df_bound)
}


# DIAGNOSTICS PLEASE DELETE AFTER THIS LINE

# terms <- c("tennis", "rugby", "football", "cricket", "formula 1")
# regions <- c("Australia", "New South Wales, Australia", "Victoria, Australia", "New Zealand")
# source <- "youtube"
# category <- "sports"

# gtrendsdl(
# 	terms = c("tennis", "rugby", "football", "cricket", "formula 1")
# 	, regions = c("Australia", "New South Wales, Australia", "Victoria, Australia", "New Zealand")
# 	, source = "youtube"
# 	, category = "sports"
# 	)