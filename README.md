
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

# analytics

The goal of `analytics` is to provide an easy way to access and analyze
“dark” data generated before, during, and after the Data Science for
Open Wash Data Course.

## Installation

To install the package, you can use the following code:

``` r
# install.packages("devtools")
devtools::install_github("openwashdata/analytics")
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/q2/thf5k95955q8kn6twjrwljbr00jms0/T/RtmpFxjxao/remotesfda540e67cf3/openwashdata-analytics-d6c7304d3c4ef0168740664ab0f63cf2f1f787ea/DESCRIPTION’ ... OK
#> * preparing ‘analytics’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘analytics_0.1.0.tar.gz’
#> Warning: invalid uid value replaced by that for user 'nobody'
```

## What is this package for?

The Data Science for Open WASH Data Course conducted by Global Health
Engineering (GHE) at ETH Zürich generates data on participants’
engagement with the course, previous experiences with programming and
take-aways from the course. This package makes it easier to access data
stored in a variety of formats and provide a consolidate storage for it.
In the future, this data will be used to provide an overview of the
impact of the course.

### Who should use this package?

In short, anyone can use this package. However, access to raw data is
restricted to members of GHE and those invited by members of GHE. A data
package with data from the first iteration of the course will be
available soon. This data has been anonymized and can be used by anyone.

## Where is the data retrieved from?

1.  Google Drive: The GHE shared google drive collects data from survey
    responses. This data is stored in Google Sheets.

2.  Posit Cloud: Data about usage of a shared Posit Cloud space is
    retrieved from Posit cloud. The shared space was accessible to all
    course participants.

3.  Plausible: The website for the course is hosted on Plausible. Data
    about visitors, locations of visits and other trends in user
    interactions with the website are retrieved from Plausible.

4.  Github: Data from the course is also present on Github. This is
    generally course participants’ locations. This is anonymized.

## Permissions and API Keys

Certain permissions and API Keys are needed to access the data.

1.  Google Drive:

- A gmail account (username and password) with access to the relevant
  sheets OR
- A google service account with at least `read` access to the Google
  Sheet you need to read.
- If logging in for the first time, you will be prompted to authenticate
  your account and provide access permissions to the `googledrive`
  library

2.  Posit Cloud:

- A posit cloud account
- Access to the data science for open wash data course space on Posit
  Cloud
- [An API key with access to read data from the course
  space](https://docs.posit.co/connect/user/api-keys/)
- **Note**: This code utilises the rscloud library which handles
  authentication silently. There is no need to pass API keys to the
  function but one needs to be generated. If required, please use
  `rscloud_whoami()` to re-authenticate

3.  Plausible:

- [API key with access to read analytics data from
  Plausible](https://plausible.io/docs/stats-api#get-apiv1statsbreakdown)

4.  Github:

- A github account with access to the repository where the data is
  stored
- [A personal access token with repo scope to access the
  data](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token)

## Shared Database

All data pulled by this library is also available in a postgres
database. Access to this database is limited. If you have the username
and password for this data, you can access it directly using your
credentials.

On the other hand, you can also write to the database. This function
also cleans and archives old data. Write and Delete permission to the
database is required for this.

## Example

For detailed usage of and documentation of each function, please refer
to the vignettes.

``` r
library(analytics)
library(httr)
library(jsonlite)
library(googledrive)
library(googlesheets4)
get_plausible_data(site_url="ds4owd-001.github.io/website", pagewise=TRUE, token="SECRET_PLAUSIBLE_TOKEN")
get_survey_data(sheet_url="docs.google.com/spreadsheets", email="yourname@ethz.ch", n_course_modules=10)
get_pscloud_data(space_id="12345")
```

## Requirements

R libraries utilized in the code are: - `RPostgres` - `googledrive` -
`googlesheets4` - `rscloud` - `httr` - `jsonlite` - `lubridate` -
`tidyverse`

Other standard libraries should already be installed in your default R
environment.
