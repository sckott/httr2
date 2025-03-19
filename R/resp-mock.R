mock_resp_env <- new.env()

#' mocking a connection object w/o digging into C code?
#'
#' @export
#' @keywords internal
#' @param resp A streaming [response] created by [req_perform_connection()].
#' @param mocks A list with objects of class `raw`
#' @return httr2 response
#' @examples
#' # resp_stream_raw
#' raw_mock <- function(req) {
#'   list(
#'     charToRaw("hello world"),
#'     charToRaw("hello mars"),
#'     charToRaw("hello jupiter")
#'   )
#' }
#'
#' # with context manager
#' with_mocked_responses(raw_mock, {
#'   resp <- request(example_url()) |>
#'     req_url_path("/stream-bytes/32768") |>
#'     req_perform_connection()
#'   print(resp_stream_raw(resp))
#'   print(resp_stream_raw(resp))
#'   print(resp_stream_raw(resp))
#'   print(resp_stream_raw(resp))
#'   close(resp)
#' })
#'
#' # local version
#' local_mocked_responses(raw_mock)
#' resp <- request(example_url()) |>
#'   req_url_path("/stream-bytes/32768") |>
#'   req_perform_connection()
#' resp_stream_raw(resp)
#' resp_stream_raw(resp)
#' resp_stream_raw(resp)
#' resp_stream_raw(resp)
#' close(resp)
#'
#' # resp_stream_lines
#' lines_mock <- function(req) {
#'   list(
#'     '{"a": 1}',
#'     '{"b": 2}',
#'     '{"c": 3}'
#'   )
#' }
#'
#' # with context manager
#' with_mocked_responses(lines_mock, {
#'   resp <- request(example_url()) |>
#'     req_template("GET /stream/:n", n = 3) |>
#'     req_perform_connection()
#'   print(resp_stream_lines(resp))
#'   print(resp_stream_lines(resp))
#'   print(resp_stream_lines(resp))
#'   print(resp_stream_lines(resp))
#'   close(resp)
#' })
#'
#' # local version
#' local_mocked_responses(lines_mock)
#' resp <- request(example_url()) |>
#'   req_template("GET /stream/:n", n = 3) |>
#'   req_perform_connection()
#' resp_stream_lines(resp)
#' resp_stream_lines(resp)
#' resp_stream_lines(resp)
#' resp_stream_lines(resp)
#' close(resp)
mock_resp_raw <- function(resp, mocks) {
  mocks_hash <- hash(mocks)
  if (is_null(env_get(mock_resp_env, mocks_hash, default = NULL))) {
    mock_resp_env[[mocks_hash]] <- mocks
  }
  tmp <- env_get(mock_resp_env, mocks_hash)
  if (is_empty(tmp)) {
    # remove element from list when empty
    env_unbind(mock_resp_env, mocks_hash)
    # return empty bytes
    return(raw())
  }
  resp <- mock_resp_env[[mocks_hash]][[1]]
  mock_resp_env[[mocks_hash]][[1]] <- NULL
  resp
}

#' @export
#' @rdname mock_resp_raw
mock_resp_lines <- function(resp, mocks) {
  mocks_hash <- hash(mocks)
  if (is_null(env_get(mock_resp_env, mocks_hash, default = NULL))) {
    mock_resp_env[[mocks_hash]] <- mocks
  }
  tmp <- env_get(mock_resp_env, mocks_hash)
  if (is_empty(tmp)) {
    # remove element from list when empty
    env_unbind(mock_resp_env, mocks_hash)
    # return empty bytes
    return(character())
  }
  resp <- mock_resp_env[[mocks_hash]][[1]]
  mock_resp_env[[mocks_hash]][[1]] <- NULL
  resp
}
