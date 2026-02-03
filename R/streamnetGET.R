streamnetGET = function(path,
                         query = list(),
                         base_url = "https://api.streamnet.org",
                         accept = c("json", "xml"),
                         ...) {
  
  accept = match.arg(accept)
  
  stopifnot(is.character(path), length(path) == 1L)
  
  # normalize slashes
  path     = sub("^/+", "", path)
  base_url = sub("/+$", "", base_url)
  
  url = paste0(base_url, "/", path)
  
  ua = httr::user_agent("NPT_CoordinatedAssessments (R package); contact mikea@nezperce.org for issues")
  
  # accept header: default format used by StreamNet API is JSON, but XML is an option
  accept_hdr = if (accept == "xml") httr::accept("application/xml") else httr::accept("application/json")
  
  resp = httr::RETRY(
    "GET", url, ua, accept_hdr,
    query = query,
    times = 3, pause_base = 1,
    terminate_on = c(400, 401, 403, 404)
  )
  httr::stop_for_status(resp)
  
  # parse response
  if (accept == "xml") {
    xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))
  } else {
    # JSON
    httr::content(resp, as = "parsed", type = "application/json")
  }
  
} # end function