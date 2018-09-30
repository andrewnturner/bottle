import Data.List
import System.Environment

data GameState = GameState Int Int Int
    deriving (Show)

possibleMoves :: GameState -> [GameState]
possibleMoves (GameState x y z) = xMoves ++ yMoves ++ zMoves
    where
        xMoves = [(GameState new_x y z) | new_x <- [0..(x-1)]]
        yMoves = [(GameState x new_y z) | new_y <- [0..(y-1)]]
        zMoves = [(GameState x y new_z) | new_z <- [0..(z-1)]]

isWinning :: GameState -> Bool
isWinning (GameState 0 0 0) = True
isWinning gs = any isLosing (possibleMoves gs)

isLosing :: GameState -> Bool
isLosing (GameState 0 0 0) = False
isLosing gs = all isWinning (possibleMoves gs)

winningMoves :: GameState -> [GameState]
winningMoves gs = filter isLosing (possibleMoves gs)

main :: IO ()
main = do
    [a, b, c] <- getArgs
    putStrLn $ show $ winningMoves (GameState (read a) (read b) (read c))
