% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generated_client.R
\name{feature_flags_put_users}
\alias{feature_flags_put_users}
\title{Activate a feature for a user}
\usage{
feature_flags_put_users(flag_name, user_id)
}
\arguments{
\item{flag_name}{string required. The feature flag name.}

\item{user_id}{integer required. The user ID.}
}
\value{
A list containing the following elements:
\item{name}{string, The name of the feature flag.}
\item{userCount}{integer, The number of users with this feature flag enabled.}
\item{description}{string, }
\item{team}{string, }
\item{jira}{string, }
\item{added}{string, }
\item{groupCount}{integer, }
\item{organizationCount}{integer, }
\item{percentage}{integer, The target percentage of users who should have this feature flag enabled.}
\item{activeForMe}{boolean, Whether the feature flag is active for the current user.}
}
\description{
Activate a feature for a user
}
