library(tidyverse)

# UI Constants and Icons
UI_ICONS <- list(
  health = "❤️ ",
  ammo = "🔫",
  item = "📦",
  zombie = "🧟",
  heal = "💚",
  key = "🔑",
  success = "✓",
  error = "❌",
  warning = "⚠️ ",
  play = "▶️ ",
  time = "⏱️ ",
  save = "💾",
  load = "📂"
)

# UI Helper Functions
print_banner <- function(title, width = 50) {
  border <- strrep("═", width)
  padding <- floor((width - nchar(title) - 2) / 2)
  cat("\n", border, "\n")
  cat(strrep(" ", padding), " ", title, "\n")
  cat(border, "\n\n")
}

print_status_bar <- function(state) {
  hp_bar <- paste0("[", strrep("█", state$hp / 5), strrep("░", (100 - state$hp) / 5), "]")
  cat(UI_ICONS$health, "HP:", hp_bar, state$hp, "/100 | ")
  cat(UI_ICONS$ammo, "Ammo: ", state$ammo, " | ")
  cat(UI_ICONS$item, "Items: ", nrow(state$inventory), "\n")
}

print_divider <- function(char = "─", width = 50) {
  cat(strrep(char, width), "\n")
}

show_message <- function(type, message) {
  prefix <- switch(type,
    "success" = paste0(UI_ICONS$success, " "),
    "error" = paste0(UI_ICONS$error, " "),
    "warning" = paste0(UI_ICONS$warning, " "),
    "info" = "ℹ️ ",
    ""
  )
  cat("\n", prefix, message, "\n")
}

# Formato do inventário
inventory_tibble <- tibble(
  item = character(),
  quantity = integer(),
  type = character(),
  value = integer()
)
# Estado global inicial do jogo
state <- list(
  hp = 100,
  ammo = 10,
  inventory = inventory_tibble,
  active = TRUE,
  game_start = Sys.time()
)
# Explorar pode resultar em combate ou ganhar item
explore <- function(state) {
  event <- sample(c("zombie", "item"), 1)

  switch(event,
    "zombie" = combat(state),
    "item" = gain_item(state),
    {
      cat("\nnothing happens!")
      return(state)
    }
  )
}
# Combate: Se tiver munição, mata o zumbi. Se não, perde HP.
combat <- function(state) {
  cat("\n")
  print_banner("⚔️ ZOMBIE ENCOUNTER!", 50)
  
  if (state$ammo > 0) {
    state$ammo <- state$ammo - 1
    show_message("success", "💥 Shot the zombie!")
    cat("   Ammo remaining: ", state$ammo, "\n")
  } else {
    damage <- sample(5:15, 1)
    state$hp <- max(0, state$hp - damage)
    show_message("error", paste0("No ammo! Took ", damage, " damage!"))
  }
  
  state
}

#' @param inventory tibble do inventario
#' @param item_name character
#' @param quantity_add integer to add
add_to_inventory <- function(inventory, item_name,
                             quantity_add = 1, item_type = character,
                             item_value = 0) {
  existing <- inventory %>% filter(item == item_name)

  if (nrow(existing) > 0) {
    # Item existe no inventario, aumenta quantidade
    inventory <- inventory %>%
      mutate(quantity = if_else(
        item == item_name,
        quantity + quantity_add,
        quantity
      ))
  } else {
    # item não existe, adiciona novo item / linha
    inventory <- inventory %>%
      bind_rows(tibble(
        item = item_name,
        quantity = quantity_add,
        type = item_type,
        value = item_value
      ))
  }
  inventory
}

# Ganhar item: Verifica inventario e adiciona item ou aumenta quantidade. Se for munição, aumenta ammo.
gain_item <- function(state) {
  loot <- tibble(
    item = c("ammo", "red plant", "yellow plant", "green plant", "lab key", "pistol mod"),
    type = c("ammo", "heal", "heal", "heal", "key", "weapon mod"),
    value = c(5, 10, 10, 10, 0, 50)
  )

  found <- loot[sample(nrow(loot), 1), ]

  if (found$item == "ammo") {
    state$ammo <- state$ammo + 2
    show_message("success", paste0("Found ammo (+2) - Total: ", state$ammo))
    return(state)
  }

  state$inventory <- add_to_inventory(
    state$inventory, found$item,
    1, found$type, found$value
  )
  show_message("success", paste("Found:", found$item))
  state
}

game_start <- Sys.time()
# Calcula tempo de jogo, voltar nesta função para melhorar a formatação do tempo (minutos, segundos)
survival_time <- function(state) {
  time_mins <- difftime(Sys.time(), state$game_start, units = "mins")
  time_secs <- difftime(Sys.time(), state$game_start, units = "secs")
  
  cat("\n")
  print_banner("⏱️  SURVIVAL TIME")
  cat(UI_ICONS$time, "Minutes: ", round(as.numeric(time_mins), 1), "\n")
  cat(UI_ICONS$time, "Seconds: ", round(as.numeric(time_secs), 0), "\n\n")
  
  state
}
# Salva o jogo em um arquivo RDS. Estado de jogo é um Objeto R
save_game <- function(state) {
  saveRDS(state, "save.rds")
  show_message("success", "Game saved successfully!")
  state
}
# Carrega o estado do jogo
load_game <- function(state) {
  if (!file.exists("save.rds")) {
    show_message("warning", "No save file found. Starting new game.")
    return(state)
  }
  state <- readRDS("save.rds")
  show_message("success", "Save data loaded!")
  state
}
# Termina o jogo, definindo o estado como inativo
game_over <- function(state) {
  print_banner("💀 GAME OVER 💀")
  cat("You have not survived Raccoon City...\n")
  state$active <- FALSE
  state
}

reset_inventory <- function(state) {
  state$hp <- 100
  state$ammo <- 10
  state$inventory <- tibble(
    item = character(),
    quantity = integer(),
    type = character(),
    value = integer()
  )
  state$game_start <- Sys.time()
  state
}

print_inventory <- function(state) {
  inv <- state$inventory
  
  if (nrow(inv) == 0) {
    show_message("info", "Inventory is empty...")
    return(state)
  }
  
  cat("\n")
  print_banner("INVENTORY", 50)
  
  # Group by type with better formatting
  inv_display <- inv %>%
    group_by(type) %>%
    nest() %>%
    arrange(type)
  
  for (i in seq_len(nrow(inv_display))) {
    type_name <- inv_display$type[i]
    type_icon <- switch(type_name,
      "ammo" = UI_ICONS$ammo,
      "heal" = UI_ICONS$heal,
      "key" = UI_ICONS$key,
      "weapon mod" = "⚙️ ",
      "❓"
    )
    
    cat(type_icon, " ", toupper(type_name), "\n")
    
    items <- inv_display$data[[i]]
    for (j in seq_len(nrow(items))) {
      item_name <- items$item[j]
      qty <- items$quantity[j]
      value <- items$value[j]
      
      value_str <- if (value > 0) paste0(" (value: ", value, ")") else ""
      cat("   • ", item_name, " × ", qty, value_str, "\n")
    }
  }
  
  print_divider()
  stats <- list(
    total_items = sum(inv$quantity),
    total_value = sum(inv$quantity * inv$value)
  )
  cat("Total items: ", stats$total_items, 
      " | Total value: ", stats$total_value, "\n\n")
  
  state
}

analysis <- function(state) {
  if (nrow(state$inventory) == 0) {
    cat("\nNo items in inventory to analyze.\n")
    return(state)
  }
  p <- ggplot(state$inventory, aes(x = item, y = quantity, fill = type)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Inventory Analysis", x = "Item", y = "Quantity")

  print(p)
  state
}

commands <- list(
  "explore" = explore, "1" = explore,
  "inventory" = print_inventory, "i" = print_inventory,
  "time" = survival_time, "t" = survival_time,
  "save" = save_game, "s" = save_game,
  "load" = load_game, "l" = load_game,
  "quit" = game_over, "q" = game_over,
  "reset" = reset_inventory, "6" = reset_inventory,
  "analysis" = analysis, "7" = analysis
)

show_command_menu <- function() {
  cat("\n")
  print_divider("─")
  cat("ACTIONS:\n")
  cat("  [1] explore      ", UI_ICONS$zombie, "Search the area\n")
  cat("  [i] inventory    ", UI_ICONS$item, "View items\n")
  cat("  [t] time         ", UI_ICONS$time, "Survival time\n")
  cat("  [s] save         ", UI_ICONS$save, "Save game\n")
  cat("  [l] load         ", UI_ICONS$load, "Load game\n")
  cat("  [q] quit         ❌ Abandon mission\n")
  print_divider("─")
}

process_command <- function(input, state, commands) {
  input <- tolower(trimws(input))
  
  if (input %in% names(commands)) {
    state <- commands[[input]](state)
  } else {
    show_message("error", paste0("Unknown command: '", input, "'"))
  }
  
  state
}

# GAME LOOP
state <- tryCatch(load_game(state), error = function(e) state)
while (state$active) {
  # Clear screen illusion
  cat("\n\n")
  
  print_banner("⚔️ RACCOON CITY ⚔️", 50)
  print_status_bar(state)
  print_divider("─")
  
  state <- print_inventory(state)
  
  # Better menu display
  show_command_menu()
  input <- readline(UI_ICONS$play)
  
  # Process input
  state <- process_command(input, state, commands)
  
  # Check game over
  if (!is.null(state$hp) && state$hp <= 0) {
    print_banner("💀 GAME OVER 💀")
    cat("You have not survived Raccoon City...\n")
    state$active <- FALSE
  }
}