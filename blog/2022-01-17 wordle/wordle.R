evaluateGuess <- function(guessVec, answerVec) {
  wordLength <- length(answerVec)

  resVec <- rep("-", wordLength)
  # first pass: mark exact matches (green)
  for (i in 1:wordLength) {
    if (guessVec[i] == answerVec[i]) {
      resVec[i] <- "G"
      answerVec[i] <- "-"  # mark unavailable for yellow
    }
  }
  
  # second pass: mark yellow
  for (i in 1:wordLength) {
    if (resVec[i] != "G") {
      idx <- match(guessVec[i], answerVec)
      if (!is.na(idx)) {
        resVec[i] <- "Y"
        answerVec[idx] <- "-"
      }
    }
  }
  
  resVec
}

# dictionary: a vector of allowable words
# wordLength: length of word to be guessed
# nGuesses: no. of guesses player has
playGame <- function(dictionary, wordLength = 5, nGuesses = 6) {
  # select an answer
  possibleAnswers <- dictionary[nchar(dictionary) == wordLength]
  answer <- sample(possibleAnswers, 1)
  answerVec <- strsplit(answer, "")[[1]]
  
  print(paste("You have", nGuesses, "chances to guess a word of length", 
              wordLength))
  
  guessCnt <- 0
  lettersLeft <- LETTERS
  while (guessCnt < nGuesses) {
    # display "keyboard"
    print(paste(c("Letters left:", lettersLeft), collapse = " "))
    
    # read in guess
    guessCnt <- guessCnt + 1
    guess <- readline(paste0("Enter guess ", guessCnt, ": "))
    while (nchar(guess) != wordLength) {
      guess <- readline(paste0("Guess must have ", wordLength, " characters: "))
    }
    guess <- toupper(guess)
    guessVec <- strsplit(guess, "")[[1]]
    
    # evaluate guess and update keyboard
    resVec <- evaluateGuess(guessVec, answerVec)
    
    # update keyboard
    lettersLeft <- setdiff(lettersLeft, guessVec)
    
    # print result
    print(paste(strsplit(guess, "")[[1]], collapse = " "))
    print(paste(resVec, collapse = " "))
    if (all(resVec == "G")) {
      print("You won!")
      return(guessCnt)
    }
  }
  print(paste("Sorry, you lost! Answer was ", answer))
  return(guessCnt)
}

# scrabble words
# https://boardgames.stackexchange.com/questions/38366/latest-collins-scrabble-words-list-in-text-file
dictionary <- read.csv("Collins Scrabble Words (2019).txt", 
                       header = FALSE, skip = 2)[, 1]
playGame(dictionary)

# 10k most common words
# https://github.com/first20hours/google-10000-english/blob/master/google-10000-english-usa-no-swears.txt
dictionary <- read.csv("google-10000-english-usa-no-swears.txt",
                       head = FALSE)[, 1]
dictionary <- toupper(dictionary)
playGame(dictionary)
