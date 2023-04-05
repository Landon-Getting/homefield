season = 2021
week = 6

for(week_number in 1:week){

  if(week_number == 1){
    game_info <- cfbfastR::cfbd_game_info(year = season, week = week_number)

  } else{
    game_info <- rbind(game_info, cfbfastR::cfbd_game_info(year = season,
                                                           week = week_number))
  }
}

game_info_select <- game_info |>
  dplyr::mutate(
    winner = dplyr::if_else(.data$home_points > .data$away_points,
                            .data$home_id,
                            .data$away_id),
    loser = dplyr::if_else(.data$home_points < .data$away_points,
                           .data$home_id,
                           .data$away_id)
  ) |>
  dplyr::select(winner,
                loser)

if (division == "FBS") {
  land_owners <- c("FBS")
} else if (division == "FCS") {
  land_owners <- c("FBS", "FCS")
} else if (division == "ii") {
  land_owners <- c("FBS", "FCS", "ii")
} else if (division == "iii") {
  land_owners <- c("FBS", "FCS", "ii", "iii")
}

teams <- cfbfastR::cfbd_team_info(only_fbs = FALSE, year = season)|>
  dplyr::filter(classification %in% land_owners)|>
  dplyr::select(team_id)

for (i in 1:nrow(game_info_select)) {
  # Get the winner and loser team IDs
  winner_id <- game_info_select$winner[i]
  loser_id <- game_info_select$loser[i]

  # Find the row in teams with the loser team ID and replace it with the winner team ID
  teams$team_id[teams$team_id == loser_id] <- winner_id
}

all_teams <- cfbfastR::cfbd_team_info(only_fbs = FALSE, year = season)

alt_color_list = c("Akron",
                   "Baylor",
                   "Charlotte",
                   "Houston",
                   "Iowa",
                   "Kent State",
                   "LSU",
                   "Maryland",
                   "Minnesota",
                   "Montana",
                   "NC State",
                   "North Texas",
                   "Northwestern",
                   "SMU",
                   "Temple",
                   "Tennessee",
                   "Tulsa",
                   "UC Davis",
                   "UCLA",
                   "UMass",
                   "USC",
                   "Utah State",
                   "Wisconsin",
                   "Ohio State",
                   "Purdue")

alt_logo_list = c(
  "Air Force",
  "Alabama",
  "Boston College",
  "BYU",
  "California",
  "Cincinnati",
  "Clemson",
  "Duke",
  "Indiana",
  "James Madison",
  "Kansas State",
  "Kentucky",
  "Michigan",
  "Michigan State",
  "Nevada",
  'Oklahoma',
  "Oregon",
  "Pittsburgh",
  "Rice",
  "San Diego State",
  "TCU",
  "Texas",
  "Texas A&M",
  "Wake Forest",
  "Washington State")

# getting best colors
all_teams <- all_teams |>
  dplyr::mutate(best_color = dplyr::case_when(
    school %in% alt_color_list ~ alt_color,
    !school %in% alt_color_list ~ color
  ))

# getting best logos
all_teams <- all_teams |>
  dplyr::mutate(best_logo = dplyr::case_when(
    school %in% alt_logo_list ~ logo_2,
    !school %in% alt_logo_list ~ logo
  ))

all_teams <- all_teams |>
  dplyr::arrange(school) |>
  dplyr::select(team_id,
                school,
                latitude,
                longitude,
                best_color,
                best_logo) |>
  dplyr::rename(identifier = school,
                lat = latitude,
                lng = longitude,
                color = best_color,
                image = best_logo)

teams <- dplyr::left_join(teams,
                           all_teams,
                           by = "team_id") |>
           dplyr::select(identifier,
                         lat,
                         lng,
                         color,
                         image)

return(teams)



cfbfastR::cfbd_team_info(only_fbs = FALSE, year = season) |> View()





