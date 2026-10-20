#' Fetch active users for shinyauthr
get_users_from_db <- function(con) {
  DBI::dbGetQuery(
    con,
    "
    SELECT
        username AS user,
        password_hash AS password,
        name
    FROM public.users
    WHERE is_active
    "
  )
}

#' Store a remember-me session token
add_sessionid_to_db <- function(con, user, sessionid) {
  DBI::dbExecute(
    con,
    "
    INSERT INTO public.remember_me_tokens
        (username, sessionid)
    VALUES
        ($1, $2)
    ",
    params = list(user, sessionid)
  )
}

#' Fetch unexpired remember-me session tokens (7 days)
get_sessionids_from_db <- function(con) {
  DBI::dbGetQuery(
    con,
    "
    SELECT
        username AS user,
        sessionid
    FROM public.remember_me_tokens
    WHERE created_at > now() - interval '7 days'
    "
  )
}

#' Revoke a remember-me session token
delete_sessionid_from_db <- function(con, sessionid) {
  DBI::dbExecute(
    con,
    "DELETE FROM public.remember_me_tokens WHERE sessionid = $1",
    params = list(sessionid)
  )
}
