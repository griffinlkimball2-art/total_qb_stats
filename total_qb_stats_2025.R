library(nflreadr)     # load_player_stats(), from nflverse
library(dplyr)        # filter, mutate, summarise, etc.
library(readr)        # write_csv
library(reactable)
library(htmlwidgets)
library(htmltools)

season       <- 2025
min_attempts <- 1      # minimum pass attempts in a week to count as a "QB game"
out_dir      <- "."    # change to wherever you want the CSVs saved

# -----------------------------------------------------------------------------
# 1. LOAD
# summary_level = "week" gives one row per player per game. ("reg" or "post"
# would give season totals, but we want weekly rows so we can aggregate however
# we like later.)
# -----------------------------------------------------------------------------
raw <- load_player_stats(seasons = season, summary_level = "week")

# Play-by-play: one row per play. Used for exact season CPOE and to strip
# spikes and kneels out of the box-score totals.
pbp <- load_pbp(season)

# cpoe is NA on plays with no completion probability (spikes, some throwaways),
# so averaging the non-NA values per passer matches rbsdm.com.
cpoe_exact <- pbp %>%
  filter(season_type == "REG", !is.na(cpoe)) %>%
  group_by(player_id = passer_player_id) %>%
  summarise(cpoe = mean(cpoe), .groups = "drop")

# Spikes count as pass attempts (incompletions) in the box score.
# Counted per QB per game so they're only removed from games we keep.
spikes_wk <- pbp %>%
  filter(qb_spike == 1, play_type != "no_play", !is.na(passer_player_id)) %>%
  count(player_id = passer_player_id, season_type, week, name = "spikes")

# Kneels count as carries in the box score, and their yards (about -1 each)
# and EPA (often very negative) are baked into rushing yards and rushing EPA,
# so all three get removed.
kneels_wk <- pbp %>%
  filter(qb_kneel == 1, play_type != "no_play", !is.na(rusher_player_id)) %>%
  group_by(player_id = rusher_player_id, season_type, week) %>%
  summarise(
    kneels    = n(),
    kneel_yds = sum(yards_gained, na.rm = TRUE),
    kneel_epa = sum(epa, na.rm = TRUE),
    .groups = "drop"
  )

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
  # Attach each game's spikes and kneels (NA when a QB had none)
  left_join(spikes_wk, by = c("player_id", "season_type", "week")) %>%
  left_join(kneels_wk, by = c("player_id", "season_type", "week")) %>%
  # NA -> 0 for counting stats. CPOE is skipped: a missing CPOE is "unknown",
  # not "exactly average", so it stays NA in the weekly CSV.
  mutate(across(where(is.numeric) & !any_of("cpoe"), ~ coalesce(.x, 0))) %>%
  # Remove spikes and kneels: they aren't real attempts to gain yards, and
  # kneels penalize QBs whose teams are often in victory formation.
  # The spikes/kneels columns stay in the weekly CSV for reference.
  mutate(
    pass_att = pass_att - spikes,       # spikes are 0-for-1, 0 yards
    rush_att = rush_att - kneels,
    rush_yds = rush_yds - kneel_yds,    # kneel yards are negative, so this adds them back
    rush_epa = rush_epa - kneel_epa
  ) %>%
  filter(pass_att >= min_attempts)

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
  # Attach exact season CPOE from play-by-play (NA if a QB has no valid passes)
  left_join(cpoe_exact, by = "player_id") %>%
  # Keep only the final columns, in display order
  select(player, team, games, attempts, comp_pct, cpoe, pass_yds, rush_yds,
         sack_yds_lost, total_net_yds, pass_tds, rush_tds, total_tds,
         pass_int, fumbles_lost, turnovers, net_td_pct, epa_per_play, tany_a) %>%
  arrange(desc(tany_a))

# -----------------------------------------------------------------------------
# 6. SAVE
# -----------------------------------------------------------------------------
# Regular-season weeks only, so playoff weeks (19-22) never inflate the threshold
last_week <- max(qb_weekly$week[qb_weekly$season_type == "REG"])
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

# Red (low) -> clear (middle) -> green (high) shading for any column.
# Semi-transparent colors, so the shading reads on both light and dark backgrounds.
# shade("tany_a") returns a styling function built from that column's range.
shade <- function(col) {
  rng <- range(leaderboard[[col]], na.rm = TRUE)
  function(value) {
    if (is.na(value)) return(NULL)
    pct   <- (value - rng[1]) / (rng[2] - rng[1])      # 0 = worst, 1 = best
    alpha <- abs(pct - 0.5) * 2 * 0.55                 # stronger toward the extremes
    rgb_val <- if (pct >= 0.5) "63, 185, 80" else "248, 81, 73"
    list(background = sprintf("rgba(%s, %.2f)", rgb_val, alpha), fontWeight = "bold")
  }
}

# Page colors: the same palette as the landing page (index.html). The values are
# CSS variables, so the media query swaps everything to dark when the visitor's
# device is in dark mode.
page_css <- r"---(
:root {
  --bg: #ffffff; --text: #1f2328; --muted: #656d76;
  --card: #f6f8fa; --border: #d0d7de; --accent: #1a7f37;
}
@media (prefers-color-scheme: dark) {
  :root {
    --bg: #0d1117; --text: #e6edf3; --muted: #8d96a0;
    --card: #161b22; --border: #30363d; --accent: #3fb950;
  }
}
html, body { background: var(--bg); color: var(--text); }
body {
  margin: 0; padding: 24px 16px;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
}
h2 { margin: 16px 0 4px; }
p.note { color: var(--muted); margin: 0 0 16px; }
a.back {
  display: inline-block; padding: 6px 14px; font-size: 0.9rem;
  background: var(--card); color: var(--text); text-decoration: none;
  border: 1px solid var(--border); border-radius: 6px;
}
a.back:hover { border-color: var(--accent); }
)---"

table <- reactable(
  leaderboard,
  searchable      = TRUE,
  striped         = TRUE,
  highlight       = TRUE,
  defaultPageSize = 25,
  defaultSorted   = list(tany_a = "desc"),
  # Table colors point at the CSS variables above, so they follow light/dark mode
  theme = reactableTheme(
    color           = "var(--text)",
    backgroundColor = "var(--bg)",
    borderColor     = "var(--border)",
    stripedColor    = "var(--card)",
    highlightColor  = "var(--border)",
    inputStyle      = list(backgroundColor = "var(--card)", color = "var(--text)",
                           borderColor = "var(--border)"),
    selectStyle     = list(backgroundColor = "var(--card)", color = "var(--text)"),
    pageButtonHoverStyle  = list(backgroundColor = "var(--card)"),
    pageButtonActiveStyle = list(backgroundColor = "var(--border)")
  ),
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
  tags$style(HTML(page_css)),
  tags$a(href = "index.html", class = "back", "\u2190 All seasons"),
  h2(paste(season, "QB Total Production Leaderboard")),
  p(class = "note", paste0("Regular season through Week ", last_week,
                           ". Minimum ", min_plays, " attempts. Excludes spikes and kneels. Data: nflverse."))
)

# background = the page color; saveWidget otherwise forces a white background
saveWidget(page, file.path(out_dir, paste0("index_", season, ".html")),
           selfcontained = TRUE, title = paste(season, "QB Leaderboard"),
           background = "var(--bg)")

message("Leaderboard saved to index_", season, ".html")
