# Marathon

Marathon is an R package to programmatically read concepts from a reference or pre-mapped list of codes into named concept sets for use in ATLAS

# Features

-   Takes lists of codes and vocabulary identifiers and maps them to OMOP standard via the vocabulary lookup

-   Can produce either spreadsheet or ATLAS concept set outputs

-   Can provide a facile route to cohort and concept set population of new ATLAS environments.

-   Without epidemiological expertise, peer reviewed reference libraries can be imported into ATLAS.

-   [COMING SOON] Automatic input list cleaning, integration with sql, json and CapR cohort definitions.

# Technology

Marathon is an R Package

# System Requirements

Requires R (Version 4.0.0 or higher) Installation of HADES is recommended and many packages are required. Java is required.

# Installation

1.  See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2.  In R, use the following commands to download and install

`install.packages("remotes")`

`remotes::install_github("DrTTrousers/Marathon")`

# User Documentation

*Coming soon*

# Troubleshooting / Support

Alpha Build! Investigate column names first and then check if vocabularies need sanitsing!

# Feedback

Please open an issue with any suggestions, or a Pull request if you want to join in!
