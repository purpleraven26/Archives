library(tidyverse)
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
  if (state$ammo > 0) {
    state$ammo <- state$ammo - 1
    cat("\nYou have killed a zombie \n -1 ammo")
  } else {
    damage <- sample(5:15, 1)
    state$hp <- state$hp - damage
    cat("\nNo ammo! You've lost", damage, "HP. \n")
  }
  return(state)
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
    cat("\nYou found ammo (+2)\n")
    cat("Total ammo: ", state$ammo)
    return(state)
  }


  state$inventory <- add_to_inventory(
    state$inventory, found$item,
    1, found$type, found$value
  )
  cat("Item added to inventory:", found$item, "\n")
  return(state)
}

game_start <- Sys.time()
# Calcula tempo de jogo, voltar nesta função para melhorar a formatação do tempo (minutos, segundos)
survival_time <- function(state) {
  time_mins <- difftime(Sys.time(), state$game_start, units = "mins")
  time_secs <- difftime(Sys.time(), state$game_start, units = "secs")
  cat("\nYou have survived for: ",  round(time_mins, 2), " minutes (", round(time_secs, 2), " seconds)\n")
  state
}
# Salva o jogo em um arquivo RDS. Estado de jogo é um Objeto R
save_game <- function(state) {
  saveRDS(state, "save.rds")
  cat("\nGame saved successfuly. \n")

  state
}
# Carrega o estado do jogo
load_game <- function(state) {
  if (!file.exists("save.rds")) {
    cat("\nNo save file found. Starting new game.\n")
    return(state)
  }
  state <- readRDS("save.rds")
  cat("\nSave data Loaded!\n")
  state
}
# Termina o jogo, definindo o estado como inativo
game_over <- function(state) {
  cat("You have not survived Raccoon City... \n")
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
    cat("Inventory Empty \n")
  } else {
    cat("Inventory:\n")
    for (i in 1:nrow(inv)) {
      cat("-", inv$item[i], "(x", inv$quantity[i], ")\n")
    }
  }
  return(state)
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
  "time" = survival_time, "2" = survival_time,
  "save" = save_game, "3" = save_game,
  "load" = load_game, "4" = load_game,
  "quit" = game_over, "5" = game_over,
  "reset" = reset_inventory, "6" = reset_inventory,
  "analysis" = analysis, "7" = analysis
)

# GAME LOOP
state <- tryCatch(load_game(state), error = function(e) state)
while (state$active) {
  # UI
  cat("\n\n--- RACCOON CITY ---\n")
  cat("HP:", state$hp, "| Ammo:", state$ammo, "\n")
  state <- print_inventory(state)

  # Interação com usuário
  input <- readline("Action (explore 1, time 2, save 3, load 4, quit 5, reset 6, analysis 7): ")
  if (input %in% names(commands)) {
    state <- commands[[input]](state)
  } else {
    cat("\nInvalid command. Please try again.\n")
  }

  if (!is.null(state$hp) && state$hp <= 0) {
    state <- game_over(state)
  }
}
