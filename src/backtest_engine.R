# Unified simulation dispatcher
bt_sim_func <- function(data, strat_cfg) {
  trade_mode <- match.arg(strat_cfg$trade_mode, c("long", "short", "long_short"))
  
  out <- switch(
    trade_mode,
    long       = run_long_sim(data, strat_cfg),
    short      = run_short_sim(data, strat_cfg),
    long_short = run_long_short_sim(data, strat_cfg)
  )
  
  out <- enhance_bt_output(out, strat_cfg)
  
  return(out)
}

# Output post-processing pipeline
enhance_bt_output <- function(tbl, strat_cfg) {
  with(tbl, {
    trd_entry <- (position_state != dplyr::lag(position_state, default = "none")) & position_state != "none"
    trd_exit  <- (position_state != dplyr::lead(position_state, default = "none")) & position_state != "none"
  }) -> signals
  
  tbl <- bind_cols(tbl, signals)
  
  tbl <- tbl %>%
    mutate(
      pos_size = shares,
      pos_val = shares * Open,
      drawdn = round(log(equity / slider::slide_index_dbl(equity, seq_along(equity), max, .before = Inf)), 4),
      pos_type = case_when(pos_size > 0 ~ "LONG", pos_size < 0 ~ "SHORT", TRUE ~ "FLAT"),
      trd_num = slider::slide_index_int(trd_entry, seq_along(trd_entry), sum, .before = Inf)
    ) %>%
    group_by(trd_num) %>%
    mutate(
      is_closed = max(trd_exit),
      trd_length = row_number(),
      trd_base = round(first(Open) * first(pos_size), 2),
      trd_drawdn = round(log(equity / slider::slide_index_dbl(equity, seq_along(equity), max, .before = Inf)), 4),
      trd_drawup = round(log(equity / trd_base), 4),
      trd_ur_drawdn = round(log(equity / slider::slide_index_dbl(eqty_h, seq_along(eqty_h), max, .before = Inf)), 4),
      trd_ur_drawup = round(log(eqty_h / trd_base), 4)
    ) %>%
    ungroup() %>%
    mutate(
      trd_num = if_else(pos_size == 0, NA_integer_, trd_num),
      trd_length = if_else(pos_size == 0, NA_integer_, trd_length),
      trd_drawdn = if_else(pos_size == 0, NA_real_, trd_drawdn),
      trd_drawup = if_else(pos_size == 0, NA_real_, trd_drawup),
      trd_ur_drawdn = if_else(pos_size == 0, NA_real_, trd_ur_drawdn),
      trd_ur_drawup = if_else(pos_size == 0, NA_real_, trd_ur_drawup),
      trd_base = if_else(pos_size == 0, NA_real_, trd_base),
      pct_chg = round(if_else(row_number() == 1, log(equity / strat_cfg$init_eqty), log(equity / lag(equity))), 4)
    )
  
  if (!is.null(strat_cfg$incl_buynhold) && strat_cfg$incl_buynhold) {
    tbl <- tbl %>%
      mutate(
        bnh_pos_size = rep(1, n()),
        bnh_eqty = strat_cfg$init_eqty * Open / first(Open),
        bnh_pct_chg = round(if_else(row_number() == 1, log(bnh_eqty / strat_cfg$init_eqty), log(bnh_eqty / lag(bnh_eqty))), 4),
        bnh_dd = round(log(bnh_eqty / slider::slide_index_dbl(bnh_eqty, seq_along(bnh_eqty), max, .before = Inf)), 4)
      )
  }
  
  return(tbl)
}

# Long-only simulation engine
run_long_sim <- function(data, strat_cfg) {
  n <- nrow(data)
  entry <- data$trd_entry_long
  exit  <- data$trd_exit_long
  price_vec <- data$Open
  div_vec   <- data$Dividend %||% rep(0, n)
  
  cash_vec <- numeric(n)
  shares_vec <- numeric(n)
  equity_vec <- numeric(n)
  eqty_low_vec <- numeric(n)
  eqty_high_vec <- numeric(n)
  position_state_vec <- rep("none", n)
  
  cash_vec[1] <- strat_cfg$init_eqty
  equity_vec[1] <- cash_vec[1]
  eqty_low_vec[1] <- equity_vec[1]
  eqty_high_vec[1] <- equity_vec[1]
  
  for (i in 2:n) {
    pos_prev <- position_state_vec[i - 1]
    cash_prev <- cash_vec[i - 1]
    shares_prev <- shares_vec[i - 1]
    
    if (pos_prev == "none" && entry[i]) {
      result <- process_long_trade_bar(i, cash_prev, shares_prev, pos_prev,
                                       price_vec, entry, exit, div_vec, strat_cfg)
    } else if (pos_prev == "long" && exit[i]) {
      result <- process_long_trade_bar(i, cash_prev, shares_prev, pos_prev,
                                       price_vec, entry, exit, div_vec, strat_cfg)
    } else {
      result <- list(cash = cash_prev, shares = shares_prev, position_state = pos_prev)
    }
    
    cash_vec[i] <- result$cash
    shares_vec[i] <- result$shares
    position_state_vec[i] <- result$position_state
    equity_vec[i] <- result$cash + result$shares * price_vec[i]
    eqty_low_vec[i] <- min(eqty_low_vec[i - 1], equity_vec[i])
    eqty_high_vec[i] <- max(eqty_high_vec[i - 1], equity_vec[i])
  }
  
  bind_cols(data, tibble(
    cash = cash_vec,
    shares = shares_vec,
    equity = equity_vec,
    eqty_l = eqty_low_vec,
    eqty_h = eqty_high_vec,
    position_state = position_state_vec
  ))
}

# Short-only simulation engine
run_short_sim <- function(data, strat_cfg) {
  n <- nrow(data)
  entry <- data$trd_entry_short
  exit  <- data$trd_exit_short
  price_vec <- data$Open
  div_vec   <- data$Dividend %||% rep(0, n)
  
  cash_vec <- numeric(n)
  shares_vec <- numeric(n)
  equity_vec <- numeric(n)
  eqty_low_vec <- numeric(n)
  eqty_high_vec <- numeric(n)
  position_state_vec <- rep("none", n)
  
  cash_vec[1] <- strat_cfg$init_eqty
  equity_vec[1] <- cash_vec[1]
  eqty_low_vec[1] <- equity_vec[1]
  eqty_high_vec[1] <- equity_vec[1]
  
  for (i in 2:n) {
    pos_prev <- position_state_vec[i - 1]
    cash_prev <- cash_vec[i - 1]
    shares_prev <- shares_vec[i - 1]
    
    if (pos_prev == "none" && entry[i]) {
      result <- process_short_trade_bar(i, cash_prev, shares_prev, pos_prev,
                                        price_vec, entry, exit, div_vec, strat_cfg)
    } else if (pos_prev == "short" && exit[i]) {
      result <- process_short_trade_bar(i, cash_prev, shares_prev, pos_prev,
                                        price_vec, entry, exit, div_vec, strat_cfg)
    } else {
      result <- list(cash = cash_prev, shares = shares_prev, position_state = pos_prev)
    }
    
    cash_vec[i] <- result$cash
    shares_vec[i] <- result$shares
    position_state_vec[i] <- result$position_state
    equity_vec[i] <- result$cash + result$shares * price_vec[i]
    eqty_low_vec[i] <- min(eqty_low_vec[i - 1], equity_vec[i])
    eqty_high_vec[i] <- max(eqty_high_vec[i - 1], equity_vec[i])
  }
  
  bind_cols(data, tibble(
    cash = cash_vec,
    shares = shares_vec,
    equity = equity_vec,
    eqty_l = eqty_low_vec,
    eqty_h = eqty_high_vec,
    position_state = position_state_vec
  ))
}

# Long-short simulation engine
run_long_short_sim <- function(data, strat_cfg) {
  n <- nrow(data)
  entry_long  <- data$trd_entry_long
  exit_long   <- data$trd_exit_long
  entry_short <- data$trd_entry_short
  exit_short  <- data$trd_exit_short
  price_vec <- data$Open
  div_vec   <- data$Dividend %||% rep(0, n)
  
  cash_vec <- numeric(n)
  shares_vec <- numeric(n)
  equity_vec <- numeric(n)
  eqty_low_vec <- numeric(n)
  eqty_high_vec <- numeric(n)
  position_state_vec <- rep("none", n)
  
  cash_vec[1] <- strat_cfg$init_eqty
  equity_vec[1] <- cash_vec[1]
  eqty_low_vec[1] <- equity_vec[1]
  eqty_high_vec[1] <- equity_vec[1]
  
  for (i in 2:n) {
    pos_prev <- position_state_vec[i - 1]
    cash_prev <- cash_vec[i - 1]
    shares_prev <- shares_vec[i - 1]
    
    flip_from_long  <- pos_prev == "long"  && exit_long[i] && entry_short[i]
    flip_from_short <- pos_prev == "short" && exit_short[i] && entry_long[i]
    
    if (pos_prev == "none" && entry_long[i]) {
      result <- process_long_trade_bar(i, cash_prev, shares_prev, pos_prev,
                                       price_vec, entry_long, exit_long, div_vec, strat_cfg)
    } else if (pos_prev == "none" && entry_short[i]) {
      result <- process_short_trade_bar(i, cash_prev, shares_prev, pos_prev,
                                        price_vec, entry_short, exit_short, div_vec, strat_cfg)
    } else if (pos_prev == "long" && (exit_long[i] || flip_from_long)) {
      result <- process_long_trade_bar(i, cash_prev, shares_prev, pos_prev,
                                       price_vec, entry_long, exit_long, div_vec, strat_cfg)
    } else if (pos_prev == "short" && (exit_short[i] || flip_from_short)) {
      result <- process_short_trade_bar(i, cash_prev, shares_prev, pos_prev,
                                        price_vec, entry_short, exit_short, div_vec, strat_cfg)
    } else {
      result <- list(cash = cash_prev, shares = shares_prev, position_state = pos_prev)
    }
    
    cash_vec[i] <- result$cash
    shares_vec[i] <- result$shares
    position_state_vec[i] <- result$position_state
    equity_vec[i] <- result$cash + result$shares * price_vec[i]
    eqty_low_vec[i] <- min(eqty_low_vec[i - 1], equity_vec[i])
    eqty_high_vec[i] <- max(eqty_high_vec[i - 1], equity_vec[i])
  }
  
  bind_cols(data, tibble(
    cash = cash_vec,
    shares = shares_vec,
    equity = equity_vec,
    eqty_l = eqty_low_vec,
    eqty_h = eqty_high_vec,
    position_state = position_state_vec
  ))
}
