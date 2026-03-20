library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(DT)
library(htmltools)


# ---- App settings ----


default_csv <- "madness2026_compatible.csv"   # put this file in the same folder as app.R
regions_order <- c("West", "East", "South", "Midwest")
logos_dir <- "www/logos"            # optional: add team logo PNGs here

round_levels <- c(
  "No constraint",
  "Round of 64",
  "Round of 32",
  "Sweet 16",
  "Elite 8",
  "Final Four",
  "Runner-up",
  "Champion"
)

round_map <- c(
  "Round of 64" = 1,
  "Round of 32" = 2,
  "Sweet 16" = 3,
  "Elite 8" = 4,
  "Final Four" = 5,
  "Runner-up" = 6,
  "Champion" = 7
)

# ---- Helpers ----
conditionalize_probs <- function(dat) {
  needed <- c("team_name", "team_region", "team_seed", "rd1_win", "rd2_win", "rd3_win", "rd4_win", "rd5_win", "rd6_win", "rd7_win")
  stopifnot(all(needed %in% names(dat)))
  
  dat %>%
    mutate(
      rd2_win = rd2_win / rd1_win,
      rd3_win = rd3_win / rd2_win,
      rd4_win = rd4_win / rd3_win,
      rd5_win = rd5_win / rd4_win,
      rd6_win = rd6_win / rd5_win,
      rd7_win = rd7_win / rd6_win
    )
}

resolve_play_in <- function(mat_region) {
  seed_chars <- as.character(mat_region$team_seed)
  playin_seeds <- unique(gsub("[ab]$", "", seed_chars[grepl("[ab]$", seed_chars)]))
  playin_winners <- character(0)
  
  if (length(playin_seeds) == 0) {
    mat_region$team_seed <- as.numeric(mat_region$team_seed)
    mat_region <- mat_region[order(mat_region$team_seed), ]
    rownames(mat_region) <- NULL
    return(list(mat = mat_region, playin_winners = playin_winners))
  }
  
  for (seed in playin_seeds) {
    idx <- which(as.character(mat_region$team_seed) %in% paste0(seed, c("a", "b")))
    winner_idx <- sample(idx, size = 1, prob = mat_region[idx, "rd1_win"])
    playin_winners <- c(playin_winners, mat_region[winner_idx, "team_name"])
    
    mat_region <- rbind(mat_region[-idx, ], mat_region[winner_idx, ])
    mat_region[nrow(mat_region), "team_seed"] <- seed
  }
  
  mat_region$team_seed <- as.numeric(mat_region$team_seed)
  mat_region <- mat_region[order(mat_region$team_seed), ]
  rownames(mat_region) <- NULL
  
  list(mat = mat_region, playin_winners = playin_winners)
}

team_round_label <- function(round_num) {
  c(
    "Round of 64",
    "Round of 32",
    "Sweet 16",
    "Elite 8",
    "Final Four",
    "Runner-up",
    "Champion"
  )[round_num]
}

sample_game_winner <- function(team1, team2, rd_col) {
  probs <- c(team1[[rd_col]], team2[[rd_col]])
  probs <- probs / sum(probs)
  idx <- sample(c(1, 2), size = 1, prob = probs)
  if (idx == 1) team1 else team2
}

seed_team_label <- function(df) {
  paste0(df$team_seed, " ", df$team_name)
}

safe_logo_name <- function(team_name) {
  x <- tolower(team_name)
  x <- gsub("&", "and", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  paste0(x, ".png")
}

logo_tag <- function(team_name) {
  logo_file <- file.path(logos_dir, safe_logo_name(team_name))
  if (file.exists(logo_file)) {
    tags$img(src = file.path("logos", safe_logo_name(team_name)), class = "team-logo")
  } else {
    NULL
  }
}

check_constraint_conflicts <- function(dat, constraints_tbl) {
  active <- constraints_tbl %>%
    filter(constraint != "No constraint") %>%
    left_join(
      dat %>% distinct(team_name, team_region),
      by = "team_name"
    ) %>%
    mutate(required_round = unname(round_map[constraint]))
  
  if (nrow(active) == 0) {
    return(character(0))
  }
  
  msgs <- character(0)
  
  # Only one champion
  n_champs <- sum(active$constraint == "Champion")
  if (n_champs > 1) {
    msgs <- c(msgs, "Only one team can be selected as Champion.")
  }
  
  # At most two finalists
  n_runner_up_or_better <- sum(active$required_round >= 6)
  if (n_runner_up_or_better > 2) {
    msgs <- c(msgs, "At most two teams can be locked to Runner-up or better.")
  }
  
  # At most four Final Four teams
  n_final_four_or_better <- sum(active$required_round >= 5)
  if (n_final_four_or_better > 4) {
    msgs <- c(msgs, "At most four teams can be locked to Final Four or better.")
  }
  
  # Region-based checks
  region_counts_ff <- active %>%
    filter(required_round >= 5) %>%
    count(team_region, name = "n")
  
  if (nrow(region_counts_ff) > 0 && any(region_counts_ff$n > 1)) {
    bad_regions <- region_counts_ff$team_region[region_counts_ff$n > 1]
    msgs <- c(
      msgs,
      paste0(
        "Only one team per region can reach the Final Four. Problem region(s): ",
        paste(bad_regions, collapse = ", "),
        "."
      )
    )
  }
  
  region_counts_e8 <- active %>%
    filter(required_round >= 4) %>%
    count(team_region, name = "n")
  
  if (nrow(region_counts_e8) > 0 && any(region_counts_e8$n > 2)) {
    bad_regions <- region_counts_e8$team_region[region_counts_e8$n > 2]
    msgs <- c(
      msgs,
      paste0(
        "At most two teams per region can reach the Elite 8 or better. Problem region(s): ",
        paste(bad_regions, collapse = ", "),
        "."
      )
    )
  }
  
  region_counts_s16 <- active %>%
    filter(required_round >= 3) %>%
    count(team_region, name = "n")
  
  if (nrow(region_counts_s16) > 0 && any(region_counts_s16$n > 4)) {
    bad_regions <- region_counts_s16$team_region[region_counts_s16$n > 4]
    msgs <- c(
      msgs,
      paste0(
        "At most four teams per region can reach the Sweet 16 or better. Problem region(s): ",
        paste(bad_regions, collapse = ", "),
        "."
      )
    )
  }
  
  unique(msgs)
}

# ---- Tournament simulation ----
simulate_region <- function(region, dat) {
  mat_region <- dat[dat$team_region == region, ]
  playin <- resolve_play_in(mat_region)
  mat16 <- playin$mat
  
  r64_pairs <- list(c(1, 16), c(8, 9), c(5, 12), c(4, 13), c(6, 11), c(3, 14), c(7, 10), c(2, 15))
  
  r64_games <- lapply(r64_pairs, function(p) {
    t1 <- mat16[p[1], , drop = FALSE]
    t2 <- mat16[p[2], , drop = FALSE]
    w <- sample_game_winner(t1, t2, "rd2_win")
    list(team1 = t1, team2 = t2, winner = w)
  })
  
  r32_games <- list(
    {
      w <- sample_game_winner(r64_games[[1]]$winner, r64_games[[2]]$winner, "rd3_win")
      list(team1 = r64_games[[1]]$winner, team2 = r64_games[[2]]$winner, winner = w)
    },
    {
      w <- sample_game_winner(r64_games[[3]]$winner, r64_games[[4]]$winner, "rd3_win")
      list(team1 = r64_games[[3]]$winner, team2 = r64_games[[4]]$winner, winner = w)
    },
    {
      w <- sample_game_winner(r64_games[[5]]$winner, r64_games[[6]]$winner, "rd3_win")
      list(team1 = r64_games[[5]]$winner, team2 = r64_games[[6]]$winner, winner = w)
    },
    {
      w <- sample_game_winner(r64_games[[7]]$winner, r64_games[[8]]$winner, "rd3_win")
      list(team1 = r64_games[[7]]$winner, team2 = r64_games[[8]]$winner, winner = w)
    }
  )
  
  s16_games <- list(
    {
      w <- sample_game_winner(r32_games[[1]]$winner, r32_games[[2]]$winner, "rd4_win")
      list(team1 = r32_games[[1]]$winner, team2 = r32_games[[2]]$winner, winner = w)
    },
    {
      w <- sample_game_winner(r32_games[[3]]$winner, r32_games[[4]]$winner, "rd4_win")
      list(team1 = r32_games[[3]]$winner, team2 = r32_games[[4]]$winner, winner = w)
    }
  )
  
  e8_game <- {
    w <- sample_game_winner(s16_games[[1]]$winner, s16_games[[2]]$winner, "rd5_win")
    list(team1 = s16_games[[1]]$winner, team2 = s16_games[[2]]$winner, winner = w)
  }
  
  list(
    first_four = playin$playin_winners,
    mat16 = mat16,
    r64_games = r64_games,
    r32_games = r32_games,
    s16_games = s16_games,
    e8_game = e8_game,
    champion = e8_game$winner
  )
}

simulate_tournament <- function(dat) {
  regions <- unique(dat$team_region)
  if (!all(regions_order %in% regions)) {
    stop("The region names in the data do not match regions_order. Update regions_order at the top of app.R.")
  }
  
  region_results <- setNames(lapply(regions_order, simulate_region, dat = dat), regions_order)
  region_champs <- lapply(region_results, function(x) x$champion)
  
  ff1 <- {
    w <- sample_game_winner(region_champs[["East"]], region_champs[["South"]], "rd6_win")
    list(team1 = region_champs[["East"]], team2 = region_champs[["South"]], winner = w)
  }
  
  ff2 <- {
    w <- sample_game_winner(region_champs[["West"]], region_champs[["Midwest"]], "rd6_win")
    list(team1 = region_champs[["West"]], team2 = region_champs[["Midwest"]], winner = w)
  }
  
  title_game <- {
    w <- sample_game_winner(ff1$winner, ff2$winner, "rd7_win")
    list(team1 = ff1$winner, team2 = ff2$winner, winner = w)
  }
  
  all_teams <- dat %>% distinct(team_name) %>% mutate(result_round = 1L)
  
  advance_to <- function(df, round_num) {
    tibble(team_name = df$team_name, result_round = round_num)
  }
  
  for (reg in names(region_results)) {
    rr <- region_results[[reg]]
    all_teams <- all_teams %>%
      rows_update(bind_rows(lapply(rr$r64_games, function(g) advance_to(g$winner, 2))), by = "team_name") %>%
      rows_update(bind_rows(lapply(rr$r32_games, function(g) advance_to(g$winner, 3))), by = "team_name") %>%
      rows_update(bind_rows(lapply(rr$s16_games, function(g) advance_to(g$winner, 4))), by = "team_name") %>%
      rows_update(advance_to(rr$e8_game$winner, 5), by = "team_name")
  }
  
  all_teams <- all_teams %>%
    rows_update(bind_rows(advance_to(ff1$winner, 6), advance_to(ff2$winner, 6)), by = "team_name") %>%
    rows_update(advance_to(title_game$winner, 7), by = "team_name") %>%
    mutate(result_label = vapply(result_round, team_round_label, character(1)))
  
  list(
    team_results = all_teams,
    regions = region_results,
    ff1 = ff1,
    ff2 = ff2,
    title_game = title_game,
    champion = title_game$winner$team_name
  )
}




  estimate_constraint_probability <- function(dat, constraints_tbl) {
  active <- constraints_tbl %>%
    filter(constraint != "No constraint")
  
  if (nrow(active) == 0) {
    return(list(
      approx_prob = 1,
      percent_text = "100.000%",
      expected_sims = 1,
      label = "No constraints"
    ))
  }
  
  active <- active %>%
    mutate(required_round = unname(round_map[constraint])) %>%
    rowwise() %>%
    mutate(
      team_prob = {
        row_dat <- dat[dat$team_name == team_name, , drop = FALSE]
        
        if (nrow(row_dat) == 0) {
          NA_real_
        } else {
          prob_cols <- paste0("rd", 1:required_round, "_win")
          
          if (!all(prob_cols %in% names(row_dat))) {
            NA_real_
          } else {
            prod(as.numeric(row_dat[1, prob_cols]))
          }
        }
      }
    ) %>%
    ungroup()
  
  if (any(is.na(active$team_prob))) {
    return(list(
      approx_prob = NA_real_,
      percent_text = "Unavailable",
      expected_sims = NA_real_,
      label = "Could not estimate"
    ))
  }
  
  approx_prob <- prod(active$team_prob)
  expected_sims <- if (approx_prob > 0) ceiling(1 / approx_prob) else Inf
  
  percent_text <- if (approx_prob >= 0.001) {
    paste0(sprintf("%.3f", 100 * approx_prob), "%")
  } else {
    paste0(signif(100 * approx_prob, 3), "%")
  }
  
  label <- dplyr::case_when(
    approx_prob > 0.1 ~ "Very feasible",
    approx_prob > 0.01 ~ "Feasible",
    approx_prob > 0.001 ~ "Unlikely",
    approx_prob > 0.0001 ~ "Very unlikely",
    TRUE ~ "Extreme long shot"
  )
  
  list(
    approx_prob = approx_prob,
    percent_text = percent_text,
    expected_sims = expected_sims,
    label = label
  )
}




constraints_satisfied <- function(sim_result, constraints_tbl) {
  active <- constraints_tbl %>% filter(constraint != "No constraint")
  if (nrow(active) == 0) return(TRUE)
  
  check_tbl <- sim_result$team_results %>%
    select(team_name, result_round) %>%
    left_join(active %>% mutate(required_round = unname(round_map[constraint])), by = "team_name")
  
  all(check_tbl$result_round >= check_tbl$required_round, na.rm = TRUE)
}


generate_valid_brackets <- function(dat, constraints_tbl, n_keep = 20, max_tries = 50000, progress = NULL) {
  accepted <- vector("list", length = n_keep)
  kept <- 0
  tries <- 0
  
  while (kept < n_keep && tries < max_tries) {
    tries <- tries + 1
    sim <- simulate_tournament(dat)
    
    if (constraints_satisfied(sim, constraints_tbl)) {
      kept <- kept + 1
      accepted[[kept]] <- sim
    }
    
    if (!is.null(progress) && (tries %% 50 == 0 || kept == n_keep || tries == max_tries)) {
      progress$set(
        value = min(tries / max_tries, 1),
        message = "Running simulations...",
        detail = paste0(
          "Attempts: ", format(tries, big.mark = ","),
          " | Valid brackets: ", format(kept, big.mark = ","),
          " / ", format(n_keep, big.mark = ",")
        )
      )
    }
  }
  
  accepted <- accepted[seq_len(kept)]
  list(brackets = accepted, kept = kept, tries = tries)
}

summarize_brackets <- function(bracket_list) {
  if (length(bracket_list) == 0) {
    return(list(champions = tibble(), team_advancement = tibble()))
  }
  
  champions <- map_chr(bracket_list, "champion") %>%
    tibble(team_name = .) %>%
    count(team_name, sort = TRUE) %>%
    mutate(prop = n / sum(n))
  
  team_adv <- map_dfr(bracket_list, ~ .x$team_results, .id = "sim") %>%
    count(team_name, result_label, sort = TRUE) %>%
    group_by(team_name) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  list(champions = champions, team_advancement = team_adv)
}

summarize_upsets <- function(bracket_list) {
  if (length(bracket_list) == 0) {
    return(tibble())
  }
  
  all_games <- map_dfr(seq_along(bracket_list), function(i) {
    sim <- bracket_list[[i]]
    
    region_games <- map_dfr(names(sim$regions), function(reg) {
      rr <- sim$regions[[reg]]
      
      bind_rows(
        map_dfr(rr$r64_games, ~ tibble(
          sim_id = i,
          region = reg,
          round = "Round of 64",
          seed1 = as.numeric(.x$team1$team_seed),
          seed2 = as.numeric(.x$team2$team_seed),
          team1 = .x$team1$team_name,
          team2 = .x$team2$team_name,
          winner = .x$winner$team_name
        )),
        map_dfr(rr$r32_games, ~ tibble(
          sim_id = i,
          region = reg,
          round = "Round of 32",
          seed1 = as.numeric(.x$team1$team_seed),
          seed2 = as.numeric(.x$team2$team_seed),
          team1 = .x$team1$team_name,
          team2 = .x$team2$team_name,
          winner = .x$winner$team_name
        )),
        map_dfr(rr$s16_games, ~ tibble(
          sim_id = i,
          region = reg,
          round = "Sweet 16",
          seed1 = as.numeric(.x$team1$team_seed),
          seed2 = as.numeric(.x$team2$team_seed),
          team1 = .x$team1$team_name,
          team2 = .x$team2$team_name,
          winner = .x$winner$team_name
        )),
        tibble(
          sim_id = i,
          region = reg,
          round = "Elite 8",
          seed1 = as.numeric(rr$e8_game$team1$team_seed),
          seed2 = as.numeric(rr$e8_game$team2$team_seed),
          team1 = rr$e8_game$team1$team_name,
          team2 = rr$e8_game$team2$team_name,
          winner = rr$e8_game$winner$team_name
        )
      )
    })
    
    ff_games <- bind_rows(
      tibble(
        sim_id = i,
        region = "Final Four",
        round = "Final Four",
        seed1 = as.numeric(sim$ff1$team1$team_seed),
        seed2 = as.numeric(sim$ff1$team2$team_seed),
        team1 = sim$ff1$team1$team_name,
        team2 = sim$ff1$team2$team_name,
        winner = sim$ff1$winner$team_name
      ),
      tibble(
        sim_id = i,
        region = "Final Four",
        round = "Final Four",
        seed1 = as.numeric(sim$ff2$team1$team_seed),
        seed2 = as.numeric(sim$ff2$team2$team_seed),
        team1 = sim$ff2$team1$team_name,
        team2 = sim$ff2$team2$team_name,
        winner = sim$ff2$winner$team_name
      ),
      tibble(
        sim_id = i,
        region = "Championship",
        round = "Championship",
        seed1 = as.numeric(sim$title_game$team1$team_seed),
        seed2 = as.numeric(sim$title_game$team2$team_seed),
        team1 = sim$title_game$team1$team_name,
        team2 = sim$title_game$team2$team_name,
        winner = sim$title_game$winner$team_name
      )
    )
    
    bind_rows(region_games, ff_games)
  })
  
  all_games %>%
    mutate(
      better_seed = pmin(seed1, seed2),
      worse_seed = pmax(seed1, seed2),
      upset = case_when(
        winner == team1 & seed1 > seed2 ~ TRUE,
        winner == team2 & seed2 > seed1 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(seed1 != seed2) %>%
    count(round, better_seed, worse_seed, upset, name = "n") %>%
    tidyr::pivot_wider(names_from = upset, values_from = n, values_fill = 0) %>%
    rename(non_upsets = `FALSE`, upsets = `TRUE`) %>%
    mutate(
      total = upsets + non_upsets,
      upset_rate = upsets / total,
      matchup = paste0(worse_seed, " over ", better_seed)
    ) %>%
    arrange(round, desc(upset_rate))
}

# ---- Bracket UI ----
team_row <- function(df, winner = FALSE, game_round = NULL) {
  prob_col <- if (!is.null(game_round)) paste0("rd", game_round, "_win") else NULL
  prob_text <- NULL
  
  if (!is.null(prob_col) && prob_col %in% names(df)) {
    prob_val <- suppressWarnings(as.numeric(df[[prob_col]]))
    if (!is.na(prob_val)) {
      prob_text <- paste0(sprintf("%.1f", 100 * prob_val), "%")
    }
  }
  
  div(
    class = paste("team-row", if (winner) "team-win" else ""),
    span(class = "seed-pill", as.character(df$team_seed)),
    logo_tag(df$team_name),
    span(class = "team-name", df$team_name),
    if (!is.null(prob_text)) span(class = "team-prob", prob_text)
  )
}

render_game <- function(game, game_round = NULL) {
  div(
    class = "bracket-game",
    team_row(game$team1, identical(game$winner$team_name, game$team1$team_name), game_round = game_round),
    team_row(game$team2, identical(game$winner$team_name, game$team2$team_name), game_round = game_round)
  )
}

render_region_column <- function(region_obj, region_name) {
  r64 <- div(
    class = "round-col",
    div(class = "region-title", region_name),
    div(class = "round-title", "Round of 64"),
    lapply(region_obj$r64_games, function(g) div(class = "game-stack connector-right", render_game(g, game_round = 2)))
  )
  
  r32 <- div(
    class = "round-col",
    div(class = "round-title", "Round of 32"),
    lapply(region_obj$r32_games, function(g) div(class = "game-stack game-gap-r32 connector-left connector-right", render_game(g, game_round = 3)))
  )
  
  s16 <- div(
    class = "round-col",
    div(class = "round-title", "Sweet 16"),
    lapply(region_obj$s16_games, function(g) div(class = "game-stack game-gap-s16 connector-left connector-right", render_game(g, game_round = 4)))
  )
  
  e8 <- div(
    class = "round-col final-col",
    div(class = "round-title", "Elite 8"),
    div(class = "game-stack game-gap-e8 connector-left", render_game(region_obj$e8_game, game_round = 5))
  )
  
  div(class = "bracket-side", r64, r32, s16, e8)
}

render_final_four <- function(sim) {
  
  div(
    class = "center-stage",
    
    h3("Final Four"),
    
    div(
      class = "ff-row",
      
      div(
        class="ff-game",
        div(class="center-label","East vs South"),
        render_game(sim$ff1, game_round = 6)
      ),
      
      div(
        class="ff-game",
        div(class="center-label","West vs Midwest"),
        render_game(sim$ff2, game_round = 6)
      )
    ),
    
    div(
      class="championship-row",
      div(class="center-label","Championship"),
      render_game(sim$title_game, game_round = 7)
    ),
    
    div(
      class="champ-banner",
      div(class="champ-inner",
          logo_tag(sim$champion),
          span(class="champ-text", sim$champion)
      )
    )
  )
}

# ---- Data loader ----
load_default_data <- function() {
  if (!file.exists(default_csv)) {
    stop(paste0("Could not find ", default_csv, " in the app folder."))
  }
  read.csv(default_csv, stringsAsFactors = FALSE) %>% conditionalize_probs()
}

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
body { background:#0a0a0a; color:#e5e7eb; }

.container-fluid { max-width:1700px; }

.title-panel, .navbar {
  margin-bottom:12px;
}

.well {
  background:#0a0a0a;
  border:1px solid #334155;
  border-radius:14px;
  box-shadow:0 6px 18px rgba(0,0,0,0.4);
}

.btn-default, .btn-primary {
  border-radius:10px;
  font-weight:600;
}

.bracket-wrap {
  overflow-x:auto;
  padding:10px 0 18px 0;
}

.bracket-board {
  display:flex;
  gap:18px;
  align-items:flex-start;
  min-width:1500px;
}

.bracket-side {
  display:flex;
  gap:26px;
  align-items:flex-start;
}

.round-col { width:220px; }
.final-col { width:260px; }

.region-title {
  font-size: 20px;
  font-weight: 900;
  color: #ffffff;
  margin: 0 0 12px 4px;
  letter-spacing: 0.02em;
}

.round-title {
  font-size:12px;
  text-transform:uppercase;
  letter-spacing:0.12em;
  color:#cbd5e1;
  margin:0 0 8px 4px;
  font-weight:800;
}

.game-stack {
  position:relative;
  margin-bottom:14px;
}

.game-gap-r32 { margin-top:92px; }
.game-gap-s16 { margin-top:206px; }
.game-gap-e8 { margin-top:319px; }

.game-stack.connector-right::after {
  content:'';
  position:absolute;
  top:50%;
  right:-16px;
  width:16px;
  border-top:2px solid #475569;
}

.game-stack.connector-left::before {
  content:'';
  position:absolute;
  top:50%;
  left:-16px;
  width:16px;
  border-top:2px solid #475569;
}

.bracket-game {
  background:#111827;
  border:1px solid #475569;
  border-radius:12px;
  overflow:hidden;
  box-shadow:0 6px 14px rgba(0,0,0,0.5);
}

.team-row {
  display:flex;
  align-items:center;
  gap:8px;
  padding:8px 10px;
  font-size:13px;
  border-bottom:1px solid #334155;
  color:#e5e7eb;
}

.team-row:last-child {
  border-bottom:none;
}

.team-win {
  background: linear-gradient(90deg, #1d4ed8, #2563eb);
  font-weight: 800;
  color: white;
}

.seed-pill {
  display:inline-block;
  min-width:24px;
  text-align:center;
  margin-right:2px;
  padding:2px 6px;
  border-radius:999px;
  background:#334155;
  color:#cbd5f5;
  font-size:11px;
  font-weight:700;
}

.team-logo {
  width:18px;
  height:18px;
  object-fit:contain;
  flex:0 0 18px;
}

.team-name {
  flex:1 1 auto;
}

.team-prob {
  font-size:11px;
  color:#e2e8f0;
  font-weight:800;
  margin-left:8px;
}

.center-stage {
  width:500px;
  background:#111827;
  border:1px solid #475569;
  border-radius:18px;
  box-shadow:0 12px 30px rgba(0,0,0,0.55);
  padding:18px;
}

.center-stage h3 {
  margin-top:0;
  font-size:18px;
  font-weight:800;
  color:white;
}

.center-label {
  font-size:11px;
  text-transform:uppercase;
  letter-spacing:0.14em;
  color:#cbd5e1;
  margin:18px 0 10px;
  font-weight:800;
}

.champ-banner {
  margin-top:20px;
  background:linear-gradient(135deg,#b8860b,#ffd700,#fff3b0);
  color:black;
  border-radius:16px;
  padding:18px 16px;
  font-weight:900;
  font-size:28px;
  text-align:center;
  box-shadow:0 10px 24px rgba(0,0,0,0.55);
  border:1px solid rgba(255,255,255,0.25);
}

.ff-row {
  display:flex;
  justify-content:space-between;
  gap:10px;
  margin-top:10px;
}

.ff-game {
  flex:1;
  min-width:0;
}

.championship-row {
  margin-top:18px;
}

.championship-row .bracket-game {
  transform:scale(1.1);
}

/* Column titles */
.dataTables_wrapper thead th {
  background:#0f172a !important;
  color:#facc15 !important;   /* change to white if preferred */
  font-weight:700;
  border-bottom:1px solid #334155;
}

/* Row numbers on left side */
table.dataTable tbody th {
  color:#e5e7eb !important;
  background:#1e293b !important;
}

/* Table body cells */
table.dataTable tbody td {
  color:#e5e7eb !important;
}

/* Pagination + footer text */
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate {
  color:#e5e7eb !important;
}

/* Search box label */
.dataTables_wrapper .dataTables_filter label {
  color:#e5e7eb !important;
}

.shiny-input-container {
  margin-bottom:14px;
}
"))
  ),
  titlePanel("March Madness Constraint Simulator"),
  tags$h4(
    style="color:#94a3b8;margin-bottom:20px;",
    "Monte Carlo bracket simulation with constraint solving"
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_sims", "Valid brackets to keep", value = 20, min = 1, max = 100, step = 1),
      numericInput("max_tries", "Max simulation attempts", value = 5000, min = 100, step = 500),
      uiOutput("constraint_ui"),
      uiOutput("constraint_meter"),
      uiOutput("constraint_conflicts"),
      br(),
      actionButton("run_sim", "Run simulations")
    ),
    mainPanel(
      uiOutput("live_status"),
      verbatimTextOutput("status"),
      tabsetPanel(
        tabPanel("Bracket", uiOutput("bracket_view")),
        tabPanel("Champion odds", DTOutput("champ_table")),
        tabPanel("Upset odds", DTOutput("upset_table")),
        tabPanel("Advancement odds", DTOutput("adv_table"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  raw_data <- reactiveVal(NULL)
  sim_data <- reactiveVal(NULL)
  live_status <- reactiveVal("Idle")
  
  observe({
    raw_data(load_default_data())
  })
  
  output$constraint_ui <- renderUI({
    req(raw_data())
    teams <- raw_data() %>%
      distinct(team_name, team_region, team_seed) %>%
      mutate(label = paste0(team_name, " (", team_seed, ", ", team_region, ")")) %>%
      arrange(team_name)
    
    tagList(
      helpText("Choose any teams you want to lock in, then set the minimum round they must reach."),
      selectizeInput(
        "teams_to_show",
        "Teams to constrain",
        choices = setNames(teams$team_name, teams$label),
        multiple = TRUE,
        options = list(placeholder = "Select one or more teams")
      ),
      uiOutput("team_round_controls")
    )
  })
  
  output$team_round_controls <- renderUI({
    req(input$teams_to_show)
    tagList(
      lapply(input$teams_to_show, function(team) {
        selectInput(
          inputId = paste0("round_", gsub("[^A-Za-z0-9]", "_", team)),
          label = team,
          choices = round_levels,
          selected = "No constraint"
        )
      })
    )
  })
  
  current_constraints <- reactive({
    req(raw_data())
    teams <- input$teams_to_show
    if (is.null(teams) || length(teams) == 0) {
      return(tibble(team_name = character(), constraint = character()))
    }
    
    tibble(
      team_name = teams,
      constraint = map_chr(teams, ~ input[[paste0("round_", gsub("[^A-Za-z0-9]", "_", .x))]])
    )
  })
  
  output$constraint_conflicts <- renderUI({
    req(raw_data())
    
    conflicts <- check_constraint_conflicts(raw_data(), current_constraints())
    
    if (length(conflicts) == 0) {
      return(NULL)
    }
    
    div(
      style = "margin-top:10px; padding:12px; border:1px solid #7f1d1d; border-radius:12px; background:#7f1d1d; color:#ffffff;",
      tags$div(style = "font-weight:700; margin-bottom:6px;", "Constraint conflicts"),
      tags$ul(
        style = "margin-bottom:0; padding-left:18px;",
        lapply(conflicts, tags$li)
      )
    )
  })
  
  output$live_status <- renderUI({
    div(
      style = "margin-bottom:10px; padding:10px 12px; background:#0a0a0a; border:1px solid #334155; border-radius:12px; font-weight:600; color:#e5e7eb;",
      live_status()
    )
  })
  
  observeEvent(input$run_sim, {
    req(raw_data())
    constraints_tbl <- current_constraints()
    
    conflicts <- check_constraint_conflicts(raw_data(), constraints_tbl)
    if (length(conflicts) > 0) {
      live_status("Blocked by conflicting constraints.")
      showNotification(
        "Your selected constraints conflict with the bracket structure. Fix them before running simulations.",
        type = "error",
        duration = 8
      )
      return(NULL)
    }
    
    live_status("Starting simulations...")
    
    progress <- shiny::Progress$new(session, min = 0, max = 1)
    on.exit(progress$close(), add = TRUE)
    
    result <- generate_valid_brackets(
      dat = raw_data(),
      constraints_tbl = constraints_tbl,
      n_keep = input$n_sims,
      max_tries = input$max_tries,
      progress = progress
    )
    
    sim_data(list(
      result = result,
      summary = summarize_brackets(result$brackets),
      upsets = summarize_upsets(result$brackets)
    ))
    
    if (result$kept == 0) {
      showNotification(
        "Your constraints may be statistically impossible. Try relaxing them or increasing max simulation attempts.",
        type = "error",
        duration = 8
      )
    }
    
    live_status(paste0(
      "Done. Attempts: ", format(result$tries, big.mark = ","),
      " | Valid brackets: ", format(result$kept, big.mark = ","),
      " / ", format(input$n_sims, big.mark = ",")
    ))
  })
  
  output$status <- renderText({
    req(sim_data())
    paste0(
      "Valid brackets kept: ", sim_data()$result$kept,
      " / ", input$n_sims,
      "\nSimulation attempts: ", sim_data()$result$tries,
      if (sim_data()$result$kept == 0) "\nNo valid brackets found. Constraints may be too strict." else ""
    )
  })
  
  output$bracket_view <- renderUI({
    req(sim_data())
    req(sim_data()$result$kept > 0)
    sim <- sim_data()$result$brackets[[1]]
    
    div(
      class = "bracket-wrap",
      div(
        class = "bracket-board",
        render_region_column(sim$regions$East, "East"),
        render_region_column(sim$regions$South, "South"),
        render_final_four(sim),
        render_region_column(sim$regions$West, "West"),
        render_region_column(sim$regions$Midwest, "Midwest")
      )
    )
  })
  
  output$champ_table <- renderDT({
    req(sim_data())
    
    datatable(
      sim_data()$summary$champions,
      options = list(
        pageLength = 100,
        dom = "tip",
        class = "stripe hover"
      )
    ) %>%
      formatStyle(
        columns = names(sim_data()$summary$champions),
        backgroundColor = "#0a0a0a",
        color = "#e5e7eb",
        border = "1px solid #334155"
      )
  })
  
  output$adv_table <- renderDT({
    req(sim_data())
    
    datatable(
      sim_data()$summary$team_advancement,
      options = list(
        pageLength = 100,
        dom = "tip",
        class = "stripe hover"
      )
    ) %>%
      formatStyle(
        columns = names(sim_data()$summary$team_advancement),
        backgroundColor = "#0a0a0a",
        color = "#e5e7eb",
        border = "1px solid #334155"
      )
  })
  
  output$upset_table <- renderDT({
    req(sim_data())
    
    upset_df <- sim_data()$upsets %>%
      mutate(
        upset_rate = round(100 * upset_rate, 1)
      ) %>%
      select(round, matchup, upsets, total, upset_rate)
    
    datatable(
      upset_df,
      colnames = c("Round", "Matchup", "Upsets", "Total Games", "Upset Rate (%)"),
      options = list(
        pageLength = 100,
        dom = "tip",
        class = "stripe hover"
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(upset_df),
        backgroundColor = "#0a0a0a",
        color = "#e5e7eb",
        border = "1px solid #334155"
      ) %>%
      formatStyle(
        "upset_rate",
        background = styleColorBar(c(0, 100), "#ef4444"),
        backgroundSize = "98% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
  
  output$constraint_meter <- renderUI({
    req(raw_data())
    
    est <- estimate_constraint_probability(raw_data(), current_constraints())
    
    bg_col <- dplyr::case_when(
      est$label == "Very feasible"   ~ "green",
      est$label == "Feasible"        ~ "greenyellow",
      est$label == "Unlikely"        ~ "yellow",
      est$label == "Very unlikely"   ~ "orange",
      est$label == "Extreme long shot" ~ "red",
      TRUE ~ "grey"
    )
    
    div(
      style = paste0(
        "margin-top:10px; padding:12px; border:1px solid #334155; ",
        "border-radius:12px; background:", bg_col, "; color:#0a0a0a;"
      ),
      tags$div(style = "font-weight:700; margin-bottom:6px;", "Constraint probability meter"),
      tags$div(paste("Approx. probability:", est$percent_text)),
      tags$div(paste("Expected sims needed:", if (is.infinite(est$expected_sims)) "Very large" else format(est$expected_sims, big.mark = ","))),
      tags$div(style = "margin-top:6px; font-weight:600;", paste("Assessment:", est$label))
    )
  })
}

shinyApp(ui, server)
