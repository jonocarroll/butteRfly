[![Linux/Mac Travis Build Status](https://img.shields.io/travis/jonocarroll/butteRfly/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/jonocarroll/butteRfly)
[![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jonocarroll/butteRfly/master.svg?label=Windows)](https://ci.appveyor.com/project/jonocarroll/butteRfly)
[![codecov](https://codecov.io/gh/jonocarroll/butteRfly/branch/master/graph/badge.svg)](https://codecov.io/gh/jonocarroll/butteRfly)
[![GitHub forks](https://img.shields.io/github/forks/jonocarroll/butteRfly.svg)](https://github.com/jonocarroll/butteRfly/network)
[![GitHub stars](https://img.shields.io/github/stars/jonocarroll/butteRfly.svg)](https://github.com/jonocarroll/butteRfly/stargazers)
[![Twitter](https://img.shields.io/twitter/url/https/github.com/jonocarroll/butteRfly.svg?style=social)](https://twitter.com/intent/tweet?text=Wow:&url=%5Bobject%20Object%5D)

# buttRfly

# *Are you a social butteRfly?*

Do you wish you could cross-correlate your activity on various 
social networks in a simple comparison? Look no further!

Build a social network dashboard in R using this package.

## Installation

You can install githubtools from github with:

```R
# install.packages("devtools")
devtools::install_github("jonocarroll/butteRfly")
```

## Features

#### Generate a collation of your (or anyone's) social network activity.

By providing the relevant user ID for each social network

```R
my_socials <- collate_socials(user = c("jonocarroll", "carroll_jono", 4168169), 
                              socials = c("GitHub", "Twitter", "StackOverflow"))
```
a collated series of GitHub-style tile charts is presented with the color-density representing level of activity on that social network (darker is more), tailored to the color scheme of that network (green for GitHub, blue for Twitter, orange for StackOverflow).

Additionally, number of contributions (commits/Tweets/accepted answers) can be overlayed.

![](https://github.com/jonocarroll/butteRfly/raw/master/man/figures/demo.png)

## Dependencies

Currently relies on [jonocarroll/githubtools](http://github.com/jonocarroll/githubtools) which contains the *theme_github* and *scale_fill_social* *ggplot2* functions. 

## Future plans:
  + shiny dashboard
  + Facebook (?)
  + Other socials?