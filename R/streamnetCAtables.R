streamnetCAtables = function(api_key = Sys.getenv("STREAMNET_API_KEY", unset = NA_character_),
                             base_url = "https://api.streamnet.org") {
  streamnetFetchAllPages(
    path = "api/v1/ca/tables",
    api_key = api_key,
    base_url = base_url,
    all_pages = FALSE
  )
}