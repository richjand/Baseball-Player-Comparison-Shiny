# Baseball-Player-Comparison-Shiny

This repo contains code for an interactive Shiny app designed to allow users to use a set of statistics to identify similar baseball players.

Users specify a player, a date range, an age range, and a number of baseball statistics that for which they would like to draw a comparison. The app uses the Mahalanobis distance from the specified player to return a table of the most similar players.

The repo relies heavily on functions from dplyr and stringr as well as data from the Lahman package.
