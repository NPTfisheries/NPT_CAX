streamnetFetchAllPages <- function(path,
                                   query = list(),
                                   api_key = Sys.getenv("STREAMNET_API_KEY", unset = NA_character_),
                                   per_page = 1000,
                                   page = 1,
                                   all_pages = TRUE,
                                   base_url = "https://api.streamnet.org") {
  
  if (!nzchar(api_key %||% "")) {
    stop("Missing StreamNet API key. Set STREAMNET_API_KEY or pass api_key=.", call. = FALSE)
  }
  
  fetch_page = function(p) {
    q = modifyList(query, list(
      api_key  = api_key,   # if StreamNet uses a different param name, change here
      page     = p,
      per_page = per_page
    ))
    
    out = streamnetGET(path = path, query = q, base_url = base_url, accept = "json")
    
    # i may need to adapt this depending on actual response structure. common patterns: out$records, out$data, or out (already a list of records).
    recs = out$records %||% out$data %||% out
    tibble::as_tibble(recs)
  }
  
  if (!isTRUE(all_pages)) return(fetch_page(page))
  
  p   = as.integer(page)
  acc = list()
  
  repeat {
    tbl = fetch_page(p)
    if (nrow(tbl) == 0L) break
    acc[[length(acc) + 1L]] = tbl
    if (nrow(tbl) < per_page) break
    p = p + 1L
  }
  
  dplyr::bind_rows(acc)
}
