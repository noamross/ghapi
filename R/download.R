# TODO (maybe)
# - Vectorize on URLs
# - Allow for downloading whole directory contents if path is a directory
#     -  Make a recursive = TRUE argument for this case
# - Error messages/input checking

#' Gets a file from a github repo, using the Data API blob endpoint
#'
#' This avoids the 1MB limit of the content API and uses [gh::gh] to deal with
#' authorization and such.  See https://developer.github.com/v3/git/blobs/
#' @param url the URL of the file to download via API, of the form
#'   `github.com/:owner/:repo/blob/:path
#' @param ref the reference of a commit: a branch name, tag, or commit SHA
#' @param owner,repo,path,ref alternate way to specify the file.  These will
#'   override values in `url`
#' @param to_disk,destfile write file to disk (default=TRUE)?  If so, use the
#'   name in `destfile`, or the original filename by default
#' @param .token,.api_url,.method,.limit,.send_headers arguments passed on to
#'   [gh::gh]
#' @importFrom gh gh
#' @importFrom stringi stri_match_all_regex
#' @importFrom base64enc  base64decode
#' @export
#' @return Either the local path of the downloaded file (default), or a raw
#'   vector
gh_file <- function(url = NULL, ref=NULL,
                    owner = NULL, repo = NULL, path = NULL,
                    to_disk=TRUE, destfile=NULL,
                    .token = NULL, .api_url= NULL, .method="GET",
                    .limit = NULL, .send_headers = NULL) {
  if (!is.null(url)) {
    matches <- stri_match_all_regex(
      url,
      "(github\\.com/)?([^\\/]+)/([^\\/]+)/[^\\/]+/([^\\/]+)/([^\\?]+)"
    )
    owner <- owner %||% matches[[1]][3]
    repo <- repo %||% matches[[1]][4]
    ref <- ref %||% matches[[1]][5]
    path <- path %||% matches[[1]][6]
    pathdir <- dirname(path)
    pathfile <- basename(path)
  }

  dir_contents <- gh(
    "/repos/:owner/:repo/contents/:path",
    owner = owner, repo = repo, path = pathdir, ref = ref,
    .token = NULL, .api_url = NULL, .method = "GET",
    .limit = NULL, .send_headers = NULL
  )
  file_sha <- keep(dir_contents, ~ .$path == path)[[1]]$sha
  blob <- gh(
    "/repos/:owner/:repo/git/blobs/:sha",
    owner = owner, repo = repo, sha = file_sha,
    .token = NULL, .api_url = NULL, .method = "GET",
    .limit = NULL, .send_headers = NULL
  )
  raw <- base64decode(blob[["content"]])
  if (to_disk) {
    destfile <- destfile %||% pathfile
    writeBin(raw, con = destfile)
    return(destfile)
  } else {
    return(raw)
  }
}

# lifted from purrr, here to reduce dependencies
`%||%` <- function (x, y)
{
  if (is.null(x)) {
    y
  }
  else {
    x
  }
}
