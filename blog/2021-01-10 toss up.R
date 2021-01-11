# game state:
# - current_player: who's turn it is to make a decision
# - scores: vec of length 2: scores for players
# - turn_points: points scored on the current turn so far (but not banked)
# - dice_rolls: vec of variable length: outcome of the dice rolls
#   - 0: red, 1: yellow, 2: green

# NUM_DICE: no. of dice in the game
# FACE_PROBS: vec of length 3: probabilities for red, yellow and green
# POINTS_TO_WIN: no. of points needed to win game
NUM_DICE <- 100
FACE_PROBS <- c(1/6, 2/6, 3/6)
POINTS_TO_WIN <- 100

DiceToString <- function(dice_rolls) {
  return(paste(sum(dice_rolls == 2), "Green,",
               sum(dice_rolls == 1), "Yellow,",
               sum(dice_rolls == 0), "Red"))
}

# Executes the current player's action and updates the state. Is also used if
# no greens and at least one red is rolled.
UpdateState <- function(state, action, verbose = FALSE) {
  if (verbose) {
    cat("Current roll:", DiceToString(state$dice_rolls))
    if (action != "rolled at least 1 red and no green")
      cat(" (bank", state$turn_points + sum(state$dice_rolls == 2), "pts?)",
          fill = TRUE)
    else
      cat("", fill = TRUE)
    cat(paste0("Player ", state$current_player, " ", action, "s"),
        fill = TRUE)
  }
    
  if (action == "bank") {
    # action = "bank": bank the points current player earned this turn, then
    # re-roll the dice.
    state$scores[state$current_player] <- state$scores[state$current_player] +
      state$turn_points + sum(state$dice_rolls == 2)
    state$turn_points <- 0
    state$dice_rolls <- sample(0:2, size = NUM_DICE, replace = TRUE, 
                               prob = FACE_PROBS)
    state$current_player <- 3 - state$current_player
  } else if (action == "roll") {
    # action = "roll": add the green dice to points earned this turn, then 
    # re-roll the remaining dice.
    state$turn_points <- state$turn_points + sum(state$dice_rolls == 2)
    ndice <- sum(state$dice_rolls != 2)
    if (ndice == 0) ndice <- NUM_DICE
    state$dice_rolls <- sample(0:2, size = ndice, replace = TRUE, 
                               prob = FACE_PROBS)
  } else if (action == "rolled at least 1 red and no green") {
    # action = "rolled at least 1 red and no green": 
    # no points banked, turn ends, re-roll dice.
    state$turn_points <- 0
    state$dice_rolls <- sample(0:2, size = NUM_DICE, replace = TRUE, 
                               prob = FACE_PROBS)
    state$current_player <- 3 - state$current_player
  } else {
    stop("action must be 'bank', 'roll', or 'rolled at least 1 red and no green'")
  }
  
  if (verbose) {
    if (action != "roll") {
      cat("Current scores:", state$scores, fill = TRUE)
      cat("", fill = TRUE)
    }
  }
  
  return(state)
}

SimulateGame <- function(Strategy1, Strategy2, verbose = FALSE) {
  # set up initial state
  state <- list(current_player = 1,
                scores = c(0, 0),
                turn_points = 0,
                dice_rolls = sample(0:2, size = NUM_DICE, replace = TRUE, 
                                    prob = FACE_PROBS))
  
  # check if no greens and at least one red, if so change player
  while (sum(state$dice_rolls == 2) == 0 && sum(state$dice_rolls == 0) > 0) {
    state <- UpdateState(state, "rolled at least 1 red and no green", verbose)
  }
  
  # while the game has not ended:
  # - get the next action from the current player's strategy
  # - update the state
  while (max(state$scores) < POINTS_TO_WIN) {
    if (state$current_player == 1) {
      action <- Strategy1(state)
    } else {
      action <- Strategy2(state)
    }
    state <- UpdateState(state, action, verbose)
    
    # check if no greens and at least one red
    while (sum(state$dice_rolls == 2) == 0 && sum(state$dice_rolls == 0) > 0) {
      state <- UpdateState(state, "rolled at least 1 red and no green", verbose)
    }
  }
  
  # game has ended: return winner
  if (verbose) {
    cat(paste("Game ends: Player", which.max(state$scores), "wins!"),
        fill = TRUE)
  }
  return(which.max(state$scores))
}

# strategy that stops after one roll
OneRoll <- function(state) {
  return("bank")
}

# strategy that stops only when the turn yields > `target` points
OverTargetPoints <- function(state, target = 10) {
  if (state$turn_points + sum(state$dice_rolls == 2) > target) {
    return("bank")
  } else {
    return("roll")
  }
}

#####
# small game examples
#####
NUM_DICE <- 10
FACE_PROBS <- c(1/6, 2/6, 3/6)
POINTS_TO_WIN <- 20
set.seed(1)
winner <- SimulateGame(OneRoll,
                       function(state) { OverTargetPoints(state, 19) },
                       verbose = TRUE)

NUM_DICE <- 10
FACE_PROBS <- c(0.5, 0.0, 0.5)
POINTS_TO_WIN <- 20
set.seed(1)
winner <- SimulateGame(OneRoll,
                       function(state) { OverTargetPoints(state, 19) },
                       verbose = TRUE)

#####
# SIMULATION: sanity check
#####
NUM_DICE <- 10
FACE_PROBS <- c(1/6, 2/6, 3/6)
POINTS_TO_WIN <- 100

nsim <- 10000
set.seed(1)
winners <- replicate(nsim, SimulateGame(OneRoll, OneRoll))
table(winners)  # not 50-50: player who starts first has an advantage

# for even numbered simulations, let player 2 start first
winners2 <- winners
winners2[2 * 1:(nsim/2)] <- 3 - winners2[2 * 1:(nsim/2)]
table(winners2)

#####
# SIMULATION: OneRoll vs. OverTargetPoints 20
#####
set.seed(1)
winners <- replicate(nsim, SimulateGame(
  OneRoll,
  function(state) { OverTargetPoints(state, 20) }))
table(winners)

#####
# SIMULATION: OverTargetPoints 20 vs. OverTargetPoints 50
#####
set.seed(1)
winners <- replicate(nsim, SimulateGame(
  function(state) { OverTargetPoints(state, 20) },
  function(state) { OverTargetPoints(state, 50) }))
table(winners)

# switch order of players
set.seed(1)
winners <- replicate(nsim, SimulateGame(
  function(state) { OverTargetPoints(state, 50) },
  function(state) { OverTargetPoints(state, 20) }))
table(winners)