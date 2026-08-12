#### Libraries ####

library(tidyverse)
library(fuzzyjoin)
library(janitor)
library(hoopR)
library(rvest)
library(httr)

#### Data Setup ####

player_stats <- load_mbb_player_box(seasons = 2011:2026) # 2011 is the earliest Sports Ref started tracking all adv stats
View(player_stats)

current_cusa_teams <- c("Delaware Blue Hens", "Florida International Panthers", "Jacksonville State Gamecocks",
                        "Kennesaw State Owls", "Liberty Flames", "Louisiana Tech Bulldogs",
                        "Middle Tennessee Blue Raiders", "Missouri State Bears", "New Mexico State Aggies",
                        "Sam Houston Bearkats", "UTEP Miners", "Western Kentucky Hilltoppers")

all_cusa_teams <- c("Delaware Blue Hens", "Florida International Panthers", "Jacksonville State Gamecocks",
                    "Kennesaw State Owls", "Liberty Flames", "Louisiana Tech Bulldogs",
                    "Middle Tennessee Blue Raiders", "Missouri State Bears", "New Mexico State Aggies",
                    "Sam Houston Bearkats", "UTEP Miners", "Western Kentucky Hilltoppers",
                    "Charlotte 49ers", "Cincinnati Bearcats", "DePaul Blue Demons", 
                    "East Carolina Pirates", "Florida Atlantic Owls", "Houston Cougars", 
                    "Louisville Cardinals", "Marquette Golden Eagles", "Marshall Thundering Herd", 
                    "Memphis Tigers", "Rice Owls", "Saint Louis Billikens", 
                    "SMU Mustangs", "Southern Miss Golden Eagles", "TCU Horned Frogs", 
                    "Tulane Green Wave", "Tulsa Golden Hurricane", "UAB Blazers", 
                    "UCF Knights", "Charlotte 49ers")

cusa_stats <- player_stats |>
  filter(team_display_name %in% all_cusa_teams)
View(cusa_stats)
names(cusa_stats)

#### Opponent Relative Strength (ORS) ####

power_teams <- c(
  "North Carolina Tar Heels", "Miami Hurricanes", "Syracuse Orange", "Arizona Wildcats", 
  "Missouri Tigers", "Clemson Tigers", "Notre Dame Fighting Irish", "Louisville Cardinals", 
  "Marquette Golden Eagles", "West Virginia Mountaineers", "Houston Cougars", 
  "Providence Friars", "Seton Hall Pirates", "Georgetown Hoyas", "Rutgers Scarlet Knights", 
  "Cincinnati Bearcats", "Pittsburgh Panthers", "St. John's Red Storm", "Utah Utes", 
  "Villanova Wildcats", "DePaul Blue Demons", "TCU Horned Frogs", "Kentucky Wildcats", 
  "Vanderbilt Commodores", "LSU Tigers", "South Carolina Gamecocks", "Iowa Hawkeyes", 
  "Texas Tech Red Raiders", "Oklahoma Sooners", "Oregon Ducks", "California Golden Bears", 
  "Oklahoma State Cowboys", "Wisconsin Badgers", "Nebraska Cornhuskers", "Kansas Jayhawks", 
  "Georgia Bulldogs", "Florida Gators", "Mississippi State Bulldogs", "USC Trojans", 
  "Stanford Cardinal", "Michigan Wolverines", "Texas Longhorns", "Oregon State Beavers", 
  "Iowa State Cyclones", "Auburn Tigers", "Minnesota Golden Gophers", "Florida State Seminoles", 
  "NC State Wolfpack", "Ohio State Buckeyes", "Michigan State Spartans", "Indiana Hoosiers", 
  "Kansas State Wildcats", "Texas A&M Aggies", "Maryland Terrapins", "Washington Huskies", 
  "Boston College Eagles", "Virginia Cavaliers", "Northwestern Wildcats", "UCLA Bruins", 
  "Virginia Tech Hokies", "Baylor Bears", "Alabama Crimson Tide", "Colorado Buffaloes", 
  "Wake Forest Demon Deacons", "Penn State Nittany Lions", "Illinois Fighting Illini", 
  "Purdue Boilermakers", "Creighton Bluejays", "UConn Huskies", "Xavier Musketeers", 
  "Butler Bulldogs", "SMU Mustangs", "UCF Knights", "BYU Cougars"
)

g5_mid_teams <- c(
  "Santa Clara Broncos", "Northern Iowa Panthers", "Rhode Island Rams", "New Mexico Lobos", 
  "UTEP Miners", "Boise State Broncos", "East Carolina Pirates", "Tulsa Golden Hurricane", 
  "Rice Owls", "Marshall Thundering Herd", "UTSA Roadrunners", "Nevada Wolf Pack", 
  "Southern Miss Golden Eagles", "UAB Blazers", "South Florida Bulls", "Tulane Green Wave", 
  "North Texas Mean Green", "Middle Tennessee Blue Raiders", "Florida International Panthers", 
  "Utah State Aggies", "Old Dominion Monarchs", "Southern Illinois Salukis", "Air Force Falcons", 
  "Dayton Flyers", "Hawai'i Rainbow Warriors", "San José State Spartans", 
  "Western Kentucky Hilltoppers", "Wichita State Shockers", "George Washington Colonials", 
  "George Washington Revolutionaries", "Saint Louis Billikens", "Charlotte 49ers", 
  "Florida Atlantic Owls", "Valparaiso Beacons", "Eastern Michigan Eagles", 
  "Georgia State Panthers", "Colorado State Rams", "Drake Bulldogs", "Louisiana Tech Bulldogs", 
  "Bradley Braves", "UNLV Rebels", "VCU Rams", "Fresno State Bulldogs", "San Diego State Aztecs", 
  "Gonzaga Bulldogs", "Evansville Purple Aces", "UMass Minutemen", "Massachusetts Minutemen", 
  "Temple Owls", "George Mason Patriots", "Fordham Rams", "Saint Joseph's Hawks", 
  "St. Bonaventure Bonnies", "Duquesne Dukes", "Richmond Spiders", "Belmont Bruins", 
  "Murray State Racers", "Illinois State Redbirds", "Missouri State Bears", 
  "Indiana State Sycamores", "Akron Zips", "Kent State Golden Flashes", "Toledo Rockets", 
  "Ohio Bobcats", "Bowling Green Falcons", "Buffalo Bulls", "Central Michigan Chippewas", 
  "Western Michigan Broncos", "Ball State Cardinals", "Miami (OH) Redhawks", "Miami (OH) RedHawks", 
  "Northern Illinois Huskies", "App State Mountaineers", "Appalachian State Mountaineers", 
  "Louisiana Ragin' Cajuns", "Texas State Bobcats", "South Alabama Jaguars", "Troy Trojans", 
  "Arkansas State Red Wolves", "Coastal Carolina Chanticleers", "Georgia Southern Eagles", 
  "UL Monroe Warhawks", "Memphis Tigers", "New Mexico State Aggies", "Sam Houston Bearkats", 
  "Liberty Flames", "Jacksonville State Gamecocks", "Kennesaw State Owls", "Delaware Blue Hens", 
  "Loyola Chicago Ramblers", "San Francisco Dons", "Saint Mary's Gaels", "Loyola Marymount Lions", 
  "San Diego Toreros", "Pepperdine Waves", "Pacific Tigers", "Portland Pilots", "Idaho Vandals"
)

## ORS Variable ##
cusa_stats <- cusa_stats |>
  mutate(
    ORS = case_when(
      opponent_team_display_name %in% power_teams ~ 10,
      opponent_team_display_name %in% g5_mid_teams ~ 5,
      TRUE ~ 1
    )
  )

## Sanity Check ##
cusa_stats |> 
  count(ORS)

## Season Totals ##
season_totals <- cusa_stats |>
  group_by(athlete_display_name, team_display_name, season) |>
  summarize(
    GP = n(),
    MP = sum(minutes, na.rm = TRUE),
    PTS = sum(points, na.rm = TRUE),
    FGM = sum(field_goals_made, na.rm = TRUE),
    FGA = sum(field_goals_attempted, na.rm = TRUE),
    FTM = sum(free_throws_made, na.rm = TRUE),
    FTA = sum(free_throws_attempted, na.rm = TRUE),
    TPM = sum(three_point_field_goals_made, na.rm = TRUE),
    TPA = sum(three_point_field_goals_attempted, na.rm = TRUE),
    TRB = sum(rebounds, na.rm = TRUE),
    ORB = sum(offensive_rebounds, na.rm = TRUE),
    AST = sum(assists, na.rm = TRUE),
    STL = sum(steals, na.rm = TRUE),
    BLK = sum(blocks, na.rm = TRUE),
    TOV = sum(turnovers, na.rm = TRUE),
    PF  = sum(fouls, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(DRB = TRB - ORB)
View(season_totals)

## Eligibility Filter ##
# Roughly inspired by NBA's award eligibility reqs             #
# 65 / 82 games (~79.3%), playing roughly 42.7% of the minutes #
eligible_players <- season_totals |>
  filter(GP >= 0.8 * 30,
         MP / GP >= 0.42 * 40)
View(eligible_players)

#### Advanced Stats ####

adv_stats <- eligible_players |>
  mutate(
    TS_pct = ((PTS / (2 * (FGA + 0.44 * FTA))) * 100)
  )

## Game Totals ##
# For PER and PIE #
game_totals <- cusa_stats |>
  group_by(game_id) |>
  summarize(
    game_PTS = sum(points, na.rm = TRUE),
    game_FGM = sum(field_goals_made, na.rm = TRUE),
    game_FGA = sum(field_goals_attempted, na.rm = TRUE),
    game_FTM = sum(free_throws_made, na.rm = TRUE),
    game_FTA = sum(free_throws_attempted, na.rm = TRUE),
    game_TRB = sum(rebounds, na.rm = TRUE),
    game_ORB = sum(offensive_rebounds, na.rm = TRUE),
    game_AST = sum(assists, na.rm = TRUE),
    game_STL = sum(steals, na.rm = TRUE),
    game_BLK = sum(blocks, na.rm = TRUE),
    game_TOV = sum(turnovers, na.rm = TRUE),
    game_PF  = sum(fouls, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  mutate(game_DRB = game_TRB - game_ORB)

## PIE Comp ##
szn_PIE <- cusa_stats |>
  mutate(DRB = rebounds - offensive_rebounds) |>
  left_join(game_totals, by = "game_id") |>
  mutate(
    num = (points + field_goals_made + free_throws_made - field_goals_attempted - 
             free_throws_attempted + DRB + (0.5 * offensive_rebounds) + assists + 
             steals + (0.5 * blocks) - fouls - turnovers),
    
    denom = (game_PTS + game_FGM + game_FTM - game_FGA - game_FTA + game_DRB + 
               (0.5 * game_ORB) + game_AST + game_STL + (0.5 * game_BLK) - game_PF - game_TOV),
    
    game_PIE = ifelse(denom == 0, 0, num / denom),
    
    adj_game_PIE = game_PIE * (1 + (ORS / 100)) # ORS adjustment... tier 10 = +10%, 5 = +5%, 1 = +1%
  ) |>
  group_by(athlete_display_name, team_display_name, season) |>
  summarize(avg_PIE = mean(adj_game_PIE, na.rm = TRUE), .groups = "drop")
View(szn_PIE)

## League Aggregates ##
league_agg <- cusa_stats |>
  summarize(
    lg_PTS = sum(points, na.rm = TRUE),
    lg_FGM = sum(field_goals_made, na.rm = TRUE),
    lg_FGA = sum(field_goals_attempted, na.rm = TRUE),
    lg_FTM = sum(free_throws_made, na.rm = TRUE),
    lg_FTA = sum(free_throws_attempted, na.rm = TRUE),
    lg_TRB = sum(rebounds, na.rm = TRUE),
    lg_ORB = sum(offensive_rebounds, na.rm = TRUE),
    lg_AST = sum(assists, na.rm = TRUE),
    lg_TOV = sum(turnovers, na.rm = TRUE),
    lg_PF  = sum(fouls, na.rm = TRUE)
  ) |>
  mutate(
    lg_DRB = lg_TRB - lg_ORB,
    factor = (2 / 3) - (0.5 * (lg_AST / lg_FGM)) / (2 * (lg_FGM / lg_FTM)),
    VOP = lg_PTS / (lg_FGA - lg_ORB + lg_TOV + 0.44 * lg_FTA),
    DRB_pct = lg_DRB / lg_TRB
  )

## Team Aggregates ##
team_agg <- cusa_stats |>
  group_by(team_display_name) |>
  summarize(
    tm_AST = sum(assists, na.rm = TRUE),
    tm_FGM = sum(field_goals_made, na.rm = TRUE),
    .groups = "drop"
  )

## PER Comp ##
per_calc <- eligible_players |>
  left_join(team_agg, by = "team_display_name") |>
  cross_join(league_agg) |>
  mutate(
    uPER = (1 / MP) * (TPM + (2 / 3) * AST + (2 - factor * (tm_AST / tm_FGM)) * FGM + 
                         (FTM * 0.5 * (1 + (1 - (tm_AST / tm_FGM)) + (2 / 3) * (tm_AST / tm_FGM))) - 
                         VOP * TOV - VOP * DRB_pct * (FGA - FGM) - 
                         VOP * 0.44 * (0.44 + (0.56 * DRB_pct)) * (FTA - FTM) + 
                         VOP * (1 - DRB_pct) * (TRB - ORB) + VOP * DRB_pct * ORB + 
                         VOP * STL + VOP * DRB_pct * BLK - 
                         PF * ((lg_FTM / lg_PF) - 0.44 * (lg_FTA / lg_PF) * VOP))
  )

lg_PER_avg <- weighted.mean(per_calc$uPER, per_calc$MP, na.rm = TRUE)

szn_PER <- per_calc |>
  mutate(PER = uPER * (15 / lg_PER_avg)) |>
  select(athlete_display_name, team_display_name, season, PER)
# uPER is unadjusted player efficiency rating        #
# PER is adjusted to assume the league average is 15 #

## Conference Stats Dataset ##
all_stats <- adv_stats |>
  left_join(szn_PIE, by = c("athlete_display_name", "team_display_name", "season")) |>
  left_join(szn_PER, by = c("athlete_display_name", "team_display_name", "season")) |>
  arrange(desc(PER))
View(all_stats)

#### Sports Reference Scraping ####

## Sports Ref Names ##
cusa_ref_slugs <- c(
  "rice", "louisiana-tech", "florida-atlantic", "uab", "western-kentucky", 
  "central-florida", "missouri-state", "florida-international", "marshall", 
  "new-mexico-state", "middle-tennessee", "texas-el-paso", "texas-christian", 
  "memphis", "marquette", "sam-houston-state", "liberty", "tulsa", 
  "cincinnati", "houston", "jacksonville-state", "charlotte", "depaul", 
  "southern-methodist", "louisville", "east-carolina", "tulane", 
  "southern-mississippi", "saint-louis", "delaware", "kennesaw-state"
)

target_seasons <- c(2011:2026)

## Team ~ Season ##
scraping_grid <- crossing(
  team_slug = cusa_ref_slugs,
  season = target_seasons
)
View(scraping_grid)

## Scraper ##
scraper <- function(team_slug, year) {
  
  url <- paste0("https://www.sports-reference.com/cbb/schools/", team_slug, "/men/", year, ".html")
  message("Currently scraping: ", team_slug, " (", year, ")")
  
  Sys.sleep(3)
  
  response <- GET(url)
  
  if (status_code(response) == 429) {
    message("Error 429: Rate Limit Hit.")
    return(data.frame())
  } else if (status_code(response) == 404) {
    message("Error 404: Page Not Found.")
    return(data.frame())
  } else if (status_code(response) != 200) {
    message("Error: ", status_code(response))
    return(data.frame())
  }
  
  page <- read_html(content(response, "text", encoding = "UTF-8"))
  
  advanced_table <- tryCatch(
    {
      tbl <- page |> 
        html_element("#players_advanced")
      
      if (inherits(tbl, "xml_missing")) {
        comments <- page |>
          html_elements(xpath = "//comment()") |>
          html_text()
        
        relevant_comments <- comments[grepl("<table", comments)]
        
        tbl <- paste(relevant_comments, collapse = "") |>
          read_html() |>
          html_element("#players_advanced")
      }
      
      tbl |>
        html_table() |>
        clean_names() |>
        filter(!is.na(player), player != "Player") |>
        mutate(team_slug = team_slug, season = year)
    },
    error = function(e) {
      message("Advance Table Not Found: ", conditionMessage(e))
      return(data.frame())
    }
  )
  
  return(advanced_table)
}

sports_ref_data <- map2_dfr(
  scraping_grid$team_slug, 
  scraping_grid$season, 
  ~scraper(.x, .y)
)

View(sports_ref_data)
str(sports_ref_data)
write_csv(sports_ref_data, "/Users/graham/desktop/sports_ref_data.csv")

#### Data Merging ####

## Cross Referencing ##

team_cross <- tibble(
  team_display_name = c(
    "Rice Owls", "Louisiana Tech Bulldogs", "Florida Atlantic Owls", "UAB Blazers",
    "Western Kentucky Hilltoppers", "UCF Knights", "Missouri State Bears",
    "Florida International Panthers", "Marshall Thundering Herd", "New Mexico State Aggies",
    "Middle Tennessee Blue Raiders", "UTEP Miners", "TCU Horned Frogs", "Memphis Tigers",
    "Marquette Golden Eagles", "Sam Houston Bearkats", "Liberty Flames", "Tulsa Golden Hurricane",
    "Cincinnati Bearcats", "Houston Cougars", "Jacksonville State Gamecocks", "Charlotte 49ers",
    "DePaul Blue Demons", "SMU Mustangs", "Louisville Cardinals", "East Carolina Pirates",
    "Tulane Green Wave", "Southern Miss Golden Eagles", "Saint Louis Billikens",
    "Delaware Blue Hens", "Kennesaw State Owls"
  ),
  team_slug = c(
    "rice", "louisiana-tech", "florida-atlantic", "uab", "western-kentucky",
    "central-florida", "missouri-state", "florida-international", "marshall", "new-mexico-state",
    "middle-tennessee", "texas-el-paso", "texas-christian", "memphis", "marquette",
    "sam-houston-state", "liberty", "tulsa", "cincinnati", "houston", "jacksonville-state",
    "charlotte", "depaul", "southern-methodist", "louisville", "east-carolina",
    "tulane", "southern-mississippi", "saint-louis", "delaware", "kennesaw-state"
  )
)

## Clean hoopR Names ##
hoopR_prep <- all_stats |>
  left_join(team_cross, by = "team_display_name") |>
  mutate(
    join_name = str_to_lower(athlete_display_name),
    join_name = str_remove_all(join_name, "[\\.\\'\\-\\,-]"),
    join_name = str_remove_all(join_name, "\\b(jr|sr|iii|ii|iv|v)\\b"),
    join_name = str_squish(join_name)
  )

## Clean Sports Ref Names ##
sports_ref_prep <- sports_ref_data |>
  mutate(
    join_name = str_to_lower(player),
    join_name = str_remove_all(join_name, "[\\.\\'\\-\\,-]"),
    join_name = str_remove_all(join_name, "\\b(jr|sr|iii|ii|iv|v)\\b"),
    join_name = str_squish(join_name)
  ) |>
  select(team_slug, season, join_name, ws, ws_40, bpm, obpm, dbpm, ts_percent,
         p_prod, f_tr, x3p_ar, usg_percent)

#### Final Dataset ####

## Missing Names from Unexact Matches ##
unmatched_names <- hoopR_prep |>
  anti_join(sports_ref_prep, by = c("team_slug", "season", "join_name")) |>
  select(join_name, team_slug, season) |>
  distinct()

## Fuzzyjoin ##
name_dict <- unmatched_names |>
  stringdist_inner_join(
    sports_ref_prep,
    by = "join_name",
    max_dist = 4,
    method = "lv",
    distance_col = "dist"
  ) |>
  filter(team_slug.x == team_slug.y, season.x == season.y) |>
  group_by(join_name.x, team_slug.x, season.x) |>
  slice_min(order_by = dist, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(
    team_slug = team_slug.x,
    season = season.x,
    hoopR_name = join_name.x,
    sports_ref_name = join_name.y
  )

## Final Table Merge ##
final_table <- hoopR_prep |>
  left_join(name_dict, by = c("team_slug", "season", "join_name" = "hoopR_name")) |>
  mutate(final_join_name = coalesce(sports_ref_name, join_name)) |>
  left_join(sports_ref_prep, by = c("team_slug", "season", "final_join_name" = "join_name")) |>
  select(-join_name, -sports_ref_name, -final_join_name, -team_slug)

View(final_table)
nrow(final_table)
write_csv(final_table, "/Users/graham/desktop/final_ncaa_dataset.csv")

## Distribution of Players per Season ##
season_counts <- final_table |> 
  count(season, name = "total_players") |>
  mutate(
    Percent = round((total_players / sum(total_players)) * 100, 2)
  )
View(season_counts)
