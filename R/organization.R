#' Retrieve Repositories in an Organization
#'
#' Obtains repositories that belong to an organization according to
#' different levels.
#'
#' @param org  Name of the organization
#' @param type The kind of repository to list. Can be one of:
#'             `"all"`, `"public"`, `"private"`, `"forks"`, `"sources"`, `"member"`.
#'             Default: `"all"`
#' @references
#' <https://developer.github.com/v3/repos/#list-organization-repositories>
#' @export
#' @examples
#' get_org_repos("stat385uiuc", "all")
get_org_repos = function(org,
                         type = c("all", "public", "private", "forks",
                                  "sources", "member")) {
  # Require type
  type = match.arg(type)

  get_gh("GET /orgs/:org/repos", org = org, type = type)
}


#' Retrieve Membership list in an Organization
#'
#' Retrieves the usernames of people in an Organization
#'
#' @param org    Name of the organization
#' @param filter Filter members returned in the list. Can be one of: `"2fa_disabled"`
#'                or `"all"`. Default: `"all"`
#' @param role   Filter members returned by their role. Can be one of: `"all"`,
#'               `"admin"`, or `"member"`. Default: `"all"`
#'
#' @section Filter:
#'
#' - `"2fa_disabled"`
#'     - All members the authenticated user can see.
#' - `"all"`
#'     - Members without two-factor authentication enabled. Available for organization owners.
#'
#' @section Members List:
#'
#' - `"all"`
#'     - All members of the organization, regardless of role.
#' - `"admin"`
#'     - Organization owners.
#' - `"member"`
#'     - Non-owner organization members.
#'
#' @references
#' <https://developer.github.com/v3/orgs/members/#members-list>
#' @export
get_org_members_list = function(org,
                                filter = c("all", "2fa_disabled"),
                                role = c("all", "admin", "member")) {
  # Require type
  filter = match.arg(filter)

  # Require type
  role = match.arg(role)

  get_gh(
    "GET /orgs/:org/members",
    org = org,
    filter = filter,
    role = role
  )
}

#' Invite, Remove, or Modify User Membership in an Organization
#'
#' Sends an invitation, removes, or modifies a user role within an organization.
#'
#' @param org      Name of the organization.
#' @param username Name of the user on GitHub.
#' @param role     The role to give the user in the organization. Can be one of:
#'                  `"admin"` or `"member"`. Default: `"member"`
#'
#' @section Role:
#'
#' - `"admin"`
#'     - The user will become an owner of the organization.
#' - `"member"`
#'     - The user will become a non-owner member of the organization.
#'
#' @details
#' To prevent abuse, the authenticated user is limited to 50 organization
#' invitations per 24 hour period. If the organization is more than one month
#' old or on a paid plan, the limit is 500 invitations per 24 hour period.
#' @references
#' <https://developer.github.com/v3/orgs/members/#add-or-update-organization-membership>
#' @rdname org_member
#' @export
add_org_member = function(org, username, role = c("member", "admin")) {
  # Require type
  role = match.arg(role)

  put_gh(
    "PUT /orgs/:org/memberships/:username",
    org = org,
    username = username,
    role = role
  )
}

#' @references
#' <https://developer.github.com/v3/orgs/members/#remove-a-member>
#' @rdname org_member
#' @export
remove_org_member = function(org, username) {
  delete_gh("DELETE /orgs/:org/memberships/:username",
            org = org,
            username = username)
}

#' @rdname org_member
#' @export
update_org_member_role = add_org_member

#' @references
#' <>
#' @rdname org_member
#' @export
check_org_public_membership = function(org, username) {

  # By default, this returns an empty string "" if it succeeds...
  # A 404 error if it doesn't.
  res = tryCatch({
    get_gh("GET /orgs/:org/public_members/:username",
         org = org, username = username)
    },
    error = function(e) {
      FALSE
    })


  if(is.logical(res)) {
    return(res)
  }

  TRUE
}
