module Bot.Red where

import Data.Board

import Data.Player




makeMove :: Board -> LookAhead -> Int
makeMove b i = case concat (board b) of
               []   -> (div (getWidth b) 2) + 1
               _:_  -> findMax (findIndex b i 0)

type Alpha = Int
type Beta  = Int
type Pair  = (Int, Score)



findIndex :: Board -> LookAhead -> Int -> [Pair]
findIndex b i a = zip movelist scorelist
    where
        movelist = filter (validMove b) [1..(getWidth b)]
        scorelist = map (scoreList i (turn b) a (-100000, 100000)) (nextboards b)


heuristic ::  Board -> Player -> Score
heuristic    b p = case p of
             BlueBot -> (blueScore b) - (redScore b)
             RedBot  -> (redScore b) - (blueScore b)
             Finished -> 0

scoreList :: LookAhead -> Player -> Int -> (Alpha,Beta) -> Board -> Score
scoreList   i mp a (alpha,beta) b = case (a==i || (nextboards b) == []) of
            True        -> heuristic b mp
            False
                | mp == (turn b)  -> maximise (nextboards b) (alpha, beta)
                | otherwise       -> minimise (nextboards b) (alpha, beta)

        where
            maximise:: [Board] -> (Alpha,Beta)-> Score
            maximise boards (alpha1,beta1) = case boards of
                     [] -> alpha1
                     x:xs
                         |  alpha1 >= beta1     -> beta1
                         |  (scoreList i mp (a+1) (alpha1,beta1) x) >= beta1 -> beta1
                         |  (scoreList i mp (a+1) (alpha1,beta1) x) > alpha1 -> maximise xs ((scoreList i mp (a+1) (alpha1,beta1) x),beta1)
                         |  otherwise -> maximise xs (alpha1,beta1)
                        -- |  (scoreList i mp (a+1) (alpha1,beta1) x) < alpha1  -> maximise xs  (alpha1,beta1)
                        --  |  (scoreList i mp (a+1) (alpha1,beta1) x) >=alpha1 && (scoreList i (a+1) (alpha1,beta1) x) <= beta1 -> maximise xs  ((heuristic x (turn x)),beta1)


            minimise:: [Board] -> (Alpha,Beta)-> Score
            minimise boards (alpha2,beta2) = case boards of
                     [] -> beta2
                     x:xs
                         |  alpha2 >= beta2    -> alpha2
                         | (scoreList i mp (a+1) (alpha2,beta2) x) <= alpha2 -> alpha2
                         | (scoreList i mp (a+1) (alpha2,beta2) x) <= beta2 -> minimise xs (alpha2,(scoreList i mp (a+1) (alpha2,beta2) x))
                         | otherwise -> minimise xs (alpha2,beta2)

nextboards :: Board -> [Board]
nextboards b = map (updateBoard b) (filter (validMove b) [1..width])
    where
        width = getWidth b

getWidth :: Board -> Int
getWidth    b = fst $ dimension b

findMax :: [Pair] -> Int
findMax list = fst(foldr maxPair (0, -100000) list)
    where
        maxPair :: Pair -> Pair -> Pair
        maxPair (a1, s1) (a2, s2)
            | s1 >= s2 = (a1, s1)
            | otherwise = (a2, s2)




