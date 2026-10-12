library(nflverse)
library(tidyverse)

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
    player     = player_display_name,
    team       = coalesce(team),   # one of these will be NA-free
    season,
    week,
    season_type,
    completions   = .data[[col_comp]],
    pass_att      = .data[[col_pass_att]],
    pass_yds      = passing_yards,
    pass_td       = passing_tds,
    pass_int      = .data[[col_int]],
    sacks         = .data[[col_sacks]],
    sack_yds_lost = abs(.data[[col_sack_yds]]),  # force positive so we subtract it later
    rush_att      = carries,
    rush_yds      = rushing_yards,
    rush_td       = rushing_tds,
    fumbles_lost  = rowSums(across(all_of(fumble_cols)), na.rm = TRUE)
  ) %>%
  filter(pass_att >= min_attempts) %>%
  mutate(across(where(is.numeric), ~ coalesce(.x, 0)))   # NA -> 0

# -----------------------------------------------------------------------------
# 4. AGGREGATE (season table)
# group_by() splits the data into one group per QB; summarise() collapses each
# group to one row. across(...) applies sum() to every stat column at once.
# -----------------------------------------------------------------------------
stat_cols <- c("completions", "pass_att", "pass_yds", "pass_td", "pass_int",
               "sacks", "sack_yds_lost", "rush_att", "rush_yds", "rush_td",
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
  dropbacks       = pass_att + sacks,
  comp_pct        = completions / pass_att,
  total_plays     = pass_att + sacks + rush_att,
  total_yds       = pass_yds - sack_yds_lost + rush_yds,
  total_td        = pass_td + rush_td,
  turnovers       = pass_int + fumbles_lost,
  yds_per_play    = total_yds / total_plays,
  # ANY/A, then a "total" version that folds in rushing and fumbles
  any_a           = (pass_yds + 20 * pass_td - 45 * pass_int - sack_yds_lost) /
    dropbacks,
  tany_a     = (pass_yds + rush_yds + 20 * total_td - 45 * turnovers -
                       sack_yds_lost) / total_plays,
  rush_share_yds  = rush_yds / total_yds
) %>%
  arrange(desc(total_yds))

# -----------------------------------------------------------------------------
# 6. SAVE
# -----------------------------------------------------------------------------
last_week <- max(qb_weekly$week)
write_csv(qb_weekly, file.path(out_dir, paste0("qb_weekly_", season, ".csv")))
write_csv(qb_season, file.path(out_dir, paste0("qb_season_", season, ".csv")))

message("Saved through week ", last_week, ": ",
        nrow(qb_season), " QBs, ", nrow(qb_weekly), " QB-games.")

# Quick look at the leaders
print(head(select(qb_season, player, team, games, total_yds, total_td,
                  turnovers, tany_a, comp_pct), 15))

# -----------------------------------------------------------------------------
# 7. HTML LEADERBOARD
# install.packages(c("reactable", "htmlwidgets"))   # run once
# -----------------------------------------------------------------------------
library(reactable)
library(htmlwidgets)
library(htmltools)

# Qualifying threshold so a QB with 3 snaps doesn't top the efficiency stats.
# 15 plays per week elapsed; adjust to taste.
min_plays <- 15 * last_week

leaderboard <- qb_season %>%
  filter(total_plays >= min_plays) %>%
  select(player, team, games, total_plays, comp_pct, pass_yds, rush_yds,
         total_yds, total_td, turnovers, yds_per_play, any_a, tany_a, comp_pct)

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
    player       = colDef(name = "Player", minWidth = 160),
    team         = colDef(name = "Team", align = "center"),
    games        = colDef(name = "G"),
    total_plays  = colDef(name = "Plays"),
    comp_pct     = colDef(name = "Comp %", format = colFormat(percent = TRUE, digits = 1)),
    pass_yds     = colDef(name = "Pass Yds", format = colFormat(separators = TRUE)),
    rush_yds     = colDef(name = "Rush Yds", format = colFormat(separators = TRUE)),
    total_yds    = colDef(name = "Total Yds", format = colFormat(separators = TRUE)),
    total_td     = colDef(name = "Total TD"),
    turnovers    = colDef(name = "TO"),
    comp_pct = colDef(name = "Comp %", format = colFormat(percent = TRUE, digits = 1)),
    yds_per_play = colDef(name = "Yds/Play", format = colFormat(digits = 2)),
    any_a        = colDef(name = "ANY/A", format = colFormat(digits = 2)),
    tany_a       = colDef(name = "TANY/A", format = colFormat(digits = 2),
                          style = tany_style)
  )
)

# Title and note above the table
page <- prependContent(
  table,
  h2(paste(season, "QB Total Production Leaderboard")),
  p(paste0("Regular season through Week ", last_week,
           ". Minimum ", min_plays, " total plays. Data: nflverse."))
)

saveWidget(page, file.path(out_dir, "index.html"),
           selfcontained = TRUE, title = "QB Leaderboard")

message("Leaderboard saved to index.html")