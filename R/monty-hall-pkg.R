#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Select a Door
#' 
#' @description
#' Randomly selects one of the the three doors in a Monty Hall Problem game.
#' 
#' @details
#' The function randomly selects one door from the three available doors.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a number between 1 and 3 indicating the selected door.
#' 
#' @examples
#' select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a Goat Door
#' 
#' @description
#' Host selects a door that contains a goat.
#' 
#' @details
#' If the contestant selected the car, the function randomly selects one of the two goat doors.
#' If the contestant selected a goat, the function selects the other goat door. 
#' 
#' @param game Represents the game, with two goats and one car.
#' @param a.pick A number between 1 and 3 representing the contestant's initial door selection.
#' 
#' @return The function returns a number between 1 and 3 indicating the door opened by the host.
#' 
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' open_goat_door(game, pick)
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change Door
#' 
#' @description
#' Determines the contestant's final door selection based on whether they stay or switch. 
#' 
#' @details
#' If the contestant stays, the function returns their original door selection. 
#' If the contestant switches, the function returns the remaining unopened door.
#' 
#' @param stay A value indicating whether the contestant stays with their original selectoin. 
#' @param opened.door A number between 1 and 3 representing the door opened by the host. 
#' @param a.pick A number between 1 and 3 representing the contestant's original door selection. 
#' 
#' @return The function returns a number between 1 and 3 indicating the contestant's final door selection. 
#' 
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' opened <- open_goat_door(game, pick)
#' change_door(stay = FALSE, opened.door = opened, a.pick = pick)
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine Winner
#' 
#' @description
#' Determines whether the contestant wins or loses the Monty Hall game. 
#' 
#' @details
#' The function compares the contestant's final door selection with the game setup.
#' The contestant wins if the final selection contains the car and loses if it contains a goat.
#'
#' @param final.pick A number between 1 and 3 representing the contestant's final door selection.
#' @param game A character vector representing the game, with two goats and one car.
#'
#' @return The function returns either "WIN" or "LOSE".
#'
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' opened <- open_goat_door(game, pick)
#' final.pick <- change_door(stay = FALSE, opened.door = opened, a.pick = pick)
#' determine_winner(final.pick, game)
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play a Monty Hall Game
#' 
#' @description
#' @description
#' Simulates one complete Monty Hall Problem game using both stay and switch strategies.
#'
#' @details
#' The function creates a new game, selects an initial door, opens a goat door,
#' determines the final selection for both staying and switching, and determines
#' the outcome of each strategy.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a data frame containing the strategy and outcome
#' for both staying and switching.
#'
#' @examples
#' play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play Multiple Monty Hall Games
#' 
#' @description
#' @description
#' Simulates multiple Monty Hall Problem games and summarizes the outcomes of the stay and switch strategies.
#'
#' @details
#' The function runs the Monty Hall game the specified number of times and returns
#' the results for each game. It also prints the proportion of wins and losses
#' for each strategy.
#'
#' @param n The number of games to simulate. The default is 100.
#'
#' @return The function returns a data frame containing the strategy and outcome for each game.
#'
#' @examples
#' play_n_games()
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
