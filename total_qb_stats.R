library(nflreadr)     # load_player_stats(), from nflverse
library(dplyr)        # filter, mutate, summarise, etc.
library(readr)        # write_csv
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
col_cpoe      <- pick_col(raw, c("passing_cpoe", "cpoe"))

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
    fumbles_lost  = rowSums(across(all_of(fumble_cols)), na.rm = TRUE),
    cpoe          = .data[[col_cpoe]],           # percentage points, e.g. 3.2 = +3.2%
    pass_epa      = passing_epa,                 # includes sacks
    rush_epa      = rushing_epa                  # includes scrambles
  ) %>%
  filter(pass_att >= min_attempts) %>%
  # NA -> 0 for counting stats. CPOE is skipped: a missing CPOE is "unknown",
  # not "exactly average", so it stays NA and is ignored in the weighted mean.
  mutate(across(where(is.numeric) & !any_of("cpoe"), ~ coalesce(.x, 0)))

# -----------------------------------------------------------------------------
# 4. AGGREGATE (season table)
# group_by() splits the data into one group per QB; summarise() collapses each
# group to one row. across(...) applies sum() to every stat column at once.
# -----------------------------------------------------------------------------
stat_cols <- c("completions", "pass_att", "pass_yds", "pass_tds", "pass_int",
               "sacks", "sack_yds_lost", "rush_att", "rush_yds", "rush_tds",
               "fumbles_lost", "pass_epa", "rush_epa")

qb_season <- qb_weekly %>%
  filter(season_type == "REG") %>%
  group_by(player_id, player) %>%
  summarise(
    team  = last(team),          # most recent team (handles midseason trades)
    games = n(),
    # CPOE is a rate, so weight each week by its pass attempts. This line MUST
    # come before across(): after across() runs, pass_att is the season total.
    cpoe  = weighted.mean(cpoe, pass_att, na.rm = TRUE),
    across(all_of(stat_cols), sum),
    .groups = "drop"
  ) %>%
  # ---------------------------------------------------------------------------
# 5. COMBINED STATS
# Edit or add to these freely; everything is built from the base stats.
# ---------------------------------------------------------------------------
mutate(
  comp_pct      = completions / pass_att,
  attempts      = pass_att + sacks + rush_att,     # total plays
  total_net_yds = pass_yds - sack_yds_lost + rush_yds,
  total_tds     = pass_tds + rush_tds,
  turnovers     = pass_int + fumbles_lost,
  total_epa     = pass_epa + rush_epa,
  epa_per_play  = total_epa / attempts,
  net_td_pct    = (total_tds - turnovers) / attempts * 100,   # net TDs per 100 plays
  tany_a        = (pass_yds + rush_yds + 20 * total_tds - 45 * turnovers -
                     sack_yds_lost) / attempts
) %>%
  # Keep only the final columns, in display order
  select(player, team, games, attempts, comp_pct, cpoe, pass_yds, rush_yds,
         sack_yds_lost, total_net_yds, pass_tds, rush_tds, total_tds,
         pass_int, fumbles_lost, turnovers, net_td_pct, epa_per_play, tany_a) %>%
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
# -----------------------------------------------------------------------------

# Qualifying threshold so a QB with 3 snaps doesn't top the efficiency stats.
# 15 plays per week elapsed; adjust to taste.
min_plays <- 15 * last_week

# qb_season already has exactly the display columns, so just filter
leaderboard <- qb_season %>%
  filter(attempts >= min_plays)

# Red (low) -> white -> green (high) shading for any column.
# shade("tany_a") returns a styling function built from that column's range,
# so the same code colors both TANY/A and EPA/Play.
shade <- function(col) {
  rng <- range(leaderboard[[col]], na.rm = TRUE)
  function(value) {
    pct <- (value - rng[1]) / (rng[2] - rng[1])
    rgb_val <- colorRamp(c("#f4c7c3", "#ffffff", "#b7e1cd"))(pct)
    list(background = rgb(rgb_val, maxColorValue = 255), fontWeight = "bold")
  }
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
    games         = colDef(name = "G", width = 40),
    attempts      = colDef(name = "Atts", width = 59),
    comp_pct      = colDef(name = "Comp%", format = colFormat(percent = TRUE, digits = 1), width = 99),
    cpoe          = colDef(name = "CPOE", format = colFormat(digits = 1), width = 74),
    pass_yds      = colDef(name = "PYds", format = colFormat(separators = TRUE), width = 69),
    rush_yds      = colDef(name = "RUYds", format = colFormat(separators = TRUE), width = 79),
    sack_yds_lost = colDef(name = "SKYds", format = colFormat(separators = TRUE), width = 79),
    total_net_yds = colDef(name = "TNY", format = colFormat(separators = TRUE), width = 64),
    pass_tds      = colDef(name = "PTD", format = colFormat(separators = TRUE), width = 64),
    rush_tds      = colDef(name = "RUTD", format = colFormat(separators = TRUE), width = 75),
    total_tds     = colDef(name = "TD", format = colFormat(separators = TRUE), width = 54),
    pass_int      = colDef(name = "INT", minWidth = 59),
    fumbles_lost  = colDef(name = "FL", minWidth = 50),
    turnovers     = colDef(name = "TO", minWidth = 54),
    net_td_pct    = colDef(name = "Net TD%", format = colFormat(digits = 2), width = 99),
    epa_per_play  = colDef(name = "EPA/Play", format = colFormat(digits = 2), width = 99),
    tany_a        = colDef(name = "TANY/A", format = colFormat(digits = 2), width = 90,
                           style = shade("tany_a"))
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
