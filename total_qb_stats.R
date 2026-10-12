library(nflreadr)   # load_player_stats(), from nflverse
library(dplyr)      # filter, mutate, summarise, etc.
library(readr)      # write_csv
library(reactable)
library(htmlwidgets)
library(htmltools)

season       <- 2026
min_attempts <- 1      # minimum pass attempts in a week to count as a "QB game"
out_dir      <- "."    # change to wherever you want the CSVs saved

# -----------------------------------------------------------------------------
# 1. LOAD
# summary_level = "week" gives one row per player per game. ("reg" or "post"
# would give season totals, but we want weekly rows so we can aggregate however
# we like later.)
# -----------------------------------------------------------------------------
raw <- load_player_stats(seasons = season, summary_level = "week")

# -----------------------------------------------------------------------------
# 2. HANDLE COLUMN NAMES
# nflverse renamed some columns in its 2025 overhaul. This helper returns the
# first name in `options` that actually exists in the data, so the script works
# with either version. If none exist it stops and tells you which one is missing.
# -----------------------------------------------------------------------------
pick_col <- function(data, options) {
  hit <- options[options %in% names(data)]
  if (length(hit) == 0) {
    stop("None of these columns found: ", paste(options, collapse = ", "),
         "\nRun names(raw) to see what's available.")
  }
  hit[1]
}

col_int       <- pick_col(raw, c("passing_interceptions", "interceptions"))
col_sacks     <- pick_col(raw, c("sacks_suffered", "sacks"))
col_sack_yds  <- pick_col(raw, c("sack_yards_lost", "sack_yards"))
col_pass_att  <- pick_col(raw, c("attempts", "passing_attempts"))
col_comp      <- pick_col(raw, c("completions", "passing_completions"))

# Fumbles lost are split by play type; sum whichever pieces exist.
fumble_cols <- intersect(
  c("sack_fumbles_lost", "rushing_fumbles_lost", "receiving_fumbles_lost"),
  names(raw)
)

# -----------------------------------------------------------------------------
# 3. FILTER + STANDARDIZE (weekly table)
# all_of() / .data[[ ]] let us refer to columns by the names stored in the
# variables above instead of typing them literally.
# -----------------------------------------------------------------------------
qb_weekly <- raw %>%
  filter(position == "QB") %>%
  transmute(
    player_id,
    player        = player_display_name,
    team,
    season,
    week,
    season_type,
    completions   = .data[[col_comp]],
    pass_att      = .data[[col_pass_att]],
    pass_yds      = passing_yards,
    pass_tds      = passing_tds,
    pass_int      = .data[[col_int]],
    sacks         = .data[[col_sacks]],
    sack_yds_lost = abs(.data[[col_sack_yds]]),  # force positive so we subtract it later
    rush_att      = carries,
    rush_yds      = rushing_yards,
    rush_tds      = rushing_tds,
    fumbles_lost  = rowSums(across(all_of(fumble_cols)), na.rm = TRUE)
  ) %>%
  filter(pass_att >= min_attempts) %>%
  mutate(across(where(is.numeric), ~ coalesce(.x, 0)))   # NA -> 0

# -----------------------------------------------------------------------------
# 4. AGGREGATE (season table)
# group_by() splits the data into one group per QB; summarise() collapses each
# group to one row. across(...) applies sum() to every stat column at once.
# -----------------------------------------------------------------------------
stat_cols <- c("completions", "pass_att", "pass_yds", "pass_tds", "pass_int",
               "sacks", "sack_yds_lost", "rush_att", "rush_yds", "rush_tds",
               "fumbles_lost")

qb_season <- qb_weekly %>%
  filter(season_type == "REG") %>%
  group_by(player_id, player) %>%
  summarise(
    team  = last(team),          # most recent team (handles midseason trades)
    games = n(),
    across(all_of(stat_cols), sum),
    .groups = "drop"
  ) %>%
  # ---------------------------------------------------------------------------
# 5. COMBINED STATS
# Edit or add to these freely; everything is built from the 11 base stats.
# ---------------------------------------------------------------------------
mutate(
  comp_pct      = completions / pass_att,
  attempts      = pass_att + sacks + rush_att,     # total plays
  total_net_yds = pass_yds - sack_yds_lost + rush_yds,
  total_tds     = pass_tds + rush_tds,
  turnovers     = pass_int + fumbles_lost,
  tany_a        = (pass_yds + rush_yds + 20 * total_tds - 45 * turnovers -
                     sack_yds_lost) / attempts
) %>%
  # Keep only the final columns, in display order
  select(player, team, games, attempts, comp_pct, pass_yds, rush_yds,
         sack_yds_lost, total_net_yds, pass_tds, rush_tds, total_tds,
         pass_int, fumbles_lost, turnovers, tany_a) %>%
  arrange(desc(tany_a))

# -----------------------------------------------------------------------------
# 6. SAVE
# -----------------------------------------------------------------------------
last_week <- max(qb_weekly$week)
write_csv(qb_weekly, file.path(out_dir, paste0("qb_weekly_", season, ".csv")))
write_csv(qb_season, file.path(out_dir, paste0("qb_season_", season, ".csv")))

message("Saved through week ", last_week, ": ",
        nrow(qb_season), " QBs, ", nrow(qb_weekly), " QB-games.")

# Quick look at the leaders
print(head(qb_season, 15))

# -----------------------------------------------------------------------------
# 7. HTML LEADERBOARD
# install.packages(c("reactable", "htmlwidgets"))   # run once
# -----------------------------------------------------------------------------

# Qualifying threshold so a QB with 3 snaps doesn't top the efficiency stats.
# 15 plays per week elapsed; adjust to taste.
min_plays <- 15 * last_week

# qb_season already has exactly the display columns, so just filter
leaderboard <- qb_season %>%
  filter(attempts >= min_plays)

# Shades TANY/A cells from red (low) to green (high)
tany_style <- function(value) {
  rng <- range(leaderboard$tany_a, na.rm = TRUE)
  pct <- (value - rng[1]) / (rng[2] - rng[1])
  rgb_val <- colorRamp(c("#f4c7c3", "#ffffff", "#b7e1cd"))(pct)
  list(background = rgb(rgb_val, maxColorValue = 255), fontWeight = "bold")
}

table <- reactable(
  leaderboard,
  searchable      = TRUE,
  striped         = TRUE,
  highlight       = TRUE,
  defaultPageSize = 25,
  defaultSorted   = list(tany_a = "desc"),
  columns = list(
    player        = colDef(name = "Player", minWidth = 160),
    team          = colDef(name = "Team", align = "center", width = 70),
    games         = colDef(name = "G", width = 45),
    attempts      = colDef(name = "Atts", width = 60),
    comp_pct      = colDef(name = "Comp%", format = colFormat(percent = TRUE, digits = 1), width = 100),
    pass_yds      = colDef(name = "Pass Yds", format = colFormat(separators = TRUE)),
    rush_yds      = colDef(name = "Rush Yds", format = colFormat(separators = TRUE)),
    sack_yds_lost = colDef(name = "Sack Yds"),
    total_net_yds = colDef(name = "TNY", format = colFormat(separators = TRUE), width = 75),
    pass_tds      = colDef(name = "Pass TDs"),
    rush_tds      = colDef(name = "Rush TDs"),
    total_tds     = colDef(name = "Total TDs"),
    pass_int      = colDef(name = "INTs", minWidth = 70),
    fumbles_lost  = colDef(name = "FL", minWidth = 60),
    turnovers     = colDef(name = "Total TOs"),
    tany_a        = colDef(name = "TANY/A", format = colFormat(digits = 2),
                           style = tany_style)
  )
)

# Title and note above the table
page <- prependContent(
  table,
  h2(paste(season, "QB Total Production Leaderboard")),
  p(paste0("Regular season through Week ", last_week,
           ". Minimum ", min_plays, " attempts. Data: nflverse."))
)

saveWidget(page, file.path(out_dir, "index.html"),
           selfcontained = TRUE, title = "QB Leaderboard")

message("Leaderboard saved to index.html")