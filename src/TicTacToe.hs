{-# LANGUAGE TemplateHaskell #-}

-- This is a demo on how to make a TicTacToe game without any callbacks, listeners
-- or mutable state. The programming technique we use here is called "Functional
-- Reactive Programming". You can think of it as a spreadsheet on steroids: we define
-- how values change based on existing changing values or events.
--
-- This implementationm relies on `reflex` and `reflex-dom`. The former is a general-
-- purpose FRP framework for Haskell and the latter connects it to the DOM API.
--
-- If you want to learn about these libraries, check out https://reflex-frp.org/ and
-- https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md. You can
-- use this as a minimal starter template for `reflex-dom` projects.
--
-- This TicTacToe application is functionally the same as https://codepen.io/gaearon/pen/gWWZgR.
-- I recommand trying it out to understand the code better.

module TicTacToe
  ( tictactoe,
  )
where

import Data.List ((!!))
import RIO
import Reflex.Dom
import TH (includeFileInSource)

$(includeFileInSource "styles/styles.css" "css")

-- Our de facto main function.
tictactoe :: IO ()
tictactoe = mainWidgetWithCss css widget

-- A cell contains an X, an O, or is empty.
data Cell = X | O | Empty deriving (Eq)

-- We have two players, PlayerX and PlayerO.
data Player = PlayerX | PlayerO deriving (Eq)

-- Our game board is a list of cells (always 9, in row-major order).
type Board = [Cell]

-- We keep track of three things in our game state: the number of turns taken so
-- far, the current board, and the winner if there is one.
data GameState
  = GameState
      { turnsTaken :: Int,
        board :: Board,
        winner :: Maybe Player
      }
  deriving (Eq)

-- Initially, the board consists of 9 empty cells.
initialBoard :: Board
initialBoard = replicate 9 Empty

-- Self-explanatory.
initialGameState :: GameState
initialGameState =
  GameState
    { turnsTaken = 0,
      board = initialBoard,
      winner = Nothing
    }

-- `widget` is a function that renders our page. It also contains our game logic.
widget :: MonadWidget t m => m ()
widget = do
  rec -- An `Event t a` is a source of occurences. Each time it occurs, it carries
      -- a value of type `a`.
      -- `evCellClick` is an `Event` that occurs each time a cell on the board has
      -- been clicked, carrying the index of the cell that has been clicked.
      evCellClick :: Event t Int <- leftmost . concat <$> el "table" do
        generateThree \rowIdx -> el "tr" $
          generateThree \columnIdx -> do
            let cellIdx = rowIdx * 3 + columnIdx
            (e, _) <-
              el' "td" $ dynText $
                fmap (cellText . (!! cellIdx)) (board <$> dynGameState)
            pure (domEvent Click e $> cellIdx)
      -- We create a `p` element containing a dynamic text depending on `dynGameState`.
      el "p" $ dynText $ ffor dynGameState \case
        -- If there is a winner, we display who it is.
        GameState {winner = Just p} -> "Winner: " <> playerText p
        -- If not, we show whose turn it is based on how many turns have been taken so far.
        -- See `whoseTurn`, line 179
        GameState {turnsTaken} -> "Next player: " <> (playerText . whoseTurn $ turnsTaken)
      -- `evGoToClicked` is an `Event` that occurs each time one of the "go to" buttons
      -- has been clicked, carrying the turn number to go to.
      evGoToClicked :: Event t Int <-
        switchHold never
          =<< ( el "ol" $ dyn $ ffor dynHistory \history ->
                  leftmost <$> flip traverse history \state -> el "li" do
                    evClick <-
                      button $
                        if turnsTaken state == 0
                          then "Go to game start"
                          else "Go to move #" <> textDisplay (turnsTaken state)
                    pure $ evClick $> turnsTaken state
              )
      -- Here, we create our first `Dynamic`. A `Dynamic t a` is simply a value of
      -- type `a` that changes over time.
      -- `holdDyn` is a function for creating a `Dynamic` from an `Event`. The first
      -- argument (`initialGameState`) is the value of the `Dynamic` before the
      -- event in the second argument has occurred for the first time. After that,
      -- the `Dynamic` holds whatever the value of the `Event` was when it last
      -- occured. In our case, this event is `evStateChange` (see below).
      dynGameState :: Dynamic t GameState <- holdDyn initialGameState evStateChange
      -- `evTookTurn` is an event that occurs each time a player has taken a
      -- turn, carrying the new GameState. This is different from `evCellClick`
      -- because the player may have clicked a cell that is already full, in
      -- case he has not taken a turn.
      let evTookTurn = attachWithMaybe (flip handleCellClick) (current dynGameState) evCellClick
      -- `evJumpTo` is similar to `evGoToClicked`, but it carries the new `GameState`
      -- after the jump rather than the turn number to jump to.
      -- `(!!)` is the list lookup function, so we define `evJumpTo` by looking
      -- up the turn number in the history (a list of `GameState`s) each time
      -- `evGoToClicked` occurs.
      let evJumpTo = attachWith (!!) (current dynHistory) evGoToClicked
      -- Here, we use `holdDyn` again: `dynTimeTraveling` starts out as `False`. When
      -- `evJumpTo` occurs, it is set to `True`, and when another turn is taken, it is
      -- set back to `False`.
      dynTimeTraveling :: Dynamic t Bool <- holdDyn False $ leftmost [evJumpTo $> True, evTookTurn $> False]
      -- Using `leftmost`, we can combine multiple `Event`s into a single one that
      -- occurs whenever any of the input events occur. In case multiple events occur
      -- at the same time, the value of the leftmost is used. We don't really need to
      -- worry about this since a human is unable to click "Go to turn X" and a cell
      -- on the board at simultaneously.
      let evStateChange = leftmost [evTookTurn, evJumpTo]
      -- `scanDyn` is a way of "recording" the value of a `Dynamic` (`dynGameState`
      -- in our case) over time. This comes in handy since we want to create a history
      -- of our `dynGameState`. Note that this history is also a `Dynamic` because it
      -- will grow as more turns are taken.
      dynHistory :: Dynamic t [GameState] <-
        scanDyn
          (pure . fst)
          ( \(state, timeTraveling) history ->
              if timeTraveling then history else take (turnsTaken state) history `snoc` state
          )
          (zipDyn dynGameState dynTimeTraveling)
  pure ()
  where
    generateThree f = traverse f [0 .. 2]
    snoc [] x = [x]
    snoc (x : xs) y = x : snoc xs y

-- Given the index of the cell that has been clicked and the current GameState,
-- `handleCellClick` creates a new `GameState` if the clicked cell is empty.
handleCellClick :: Int -> GameState -> Maybe GameState
handleCellClick clickedCellIdx (GameState {turnsTaken, board, winner}) =
  if board !! clickedCellIdx == Empty && winner == Nothing
    then
      let newBoard = replaceNth clickedCellIdx (playerCell . whoseTurn $ turnsTaken) board
       in Just $
            GameState
              { turnsTaken = turnsTaken + 1,
                board = newBoard,
                winner = computeWinner newBoard
              }
    else Nothing
  where
    replaceNth _ _ [] = []
    replaceNth n y (x : xs)
      | n == 0 = y : xs
      | otherwise = x : replaceNth (n - 1) y xs

cellText :: Cell -> Text
cellText = \case
  X -> "X"
  O -> "O"
  Empty -> ""

playerText :: Player -> Text
playerText = cellText . playerCell

whoseTurn :: Int -> Player
whoseTurn turn = if even turn then PlayerX else PlayerO

playerCell :: Player -> Cell
playerCell = \case
  PlayerX -> X
  PlayerO -> O

computeWinner :: Board -> Maybe Player
computeWinner board
  | hasWon board PlayerX = Just PlayerX
  | hasWon board PlayerO = Just PlayerO
  | otherwise = Nothing

hasWon :: Board -> Player -> Bool
hasWon board player =
  or
    ( ffor winLines \combination ->
        and (ffor combination (\idx -> board !! idx == playerCell player))
    )

winLines :: [[Int]]
winLines =
  [ [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6]
  ]
