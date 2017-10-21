-- Assignment completed by
-- Name    :
-- UID     :
-- Tutor   :
-- Lab Time: 

module Bot.Blue where
    
import Data.Board

import Data.Player



makeMove :: Board -> LookAhead -> Int
makeMove b i = case concat (board b) of
                    []   -> (div 11 2)+1
                    _:_  -> hehe (findindex b i 0 (-100000,100000))


hehe ::[Pair]->  Int
hehe   p = case p of
               x:xs
                | maximum (map snd p) == snd x  -> fst x
                | otherwise                     -> hehe xs
               [] -> (div 11 2)+1



findindex :: Board -> LookAhead -> Int ->(Alpha,Beta) -> [Pair]
findindex b i a (alpha,beta) = zip (map (scorelist i a (alpha,beta)) (validboardlist b)) [1..11]



type Alpha = Int
type Beta = Int
type Pair = (Int,Score)

heuristic ::  Board -> Player -> Score
heuristic    b p = case p of
             BlueBot -> (blueScore b) - (redScore b)
             RedBot  -> (redScore b) - (blueScore b)
             Finished -> 0




scorelist :: LookAhead -> Int ->(Alpha,Beta) -> Board -> Score
scorelist   i a (alpha,beta) b = case (a==i) of
            True        -> heuristic b (turn b)
            False       -> case (turn b) of
                                      BlueBot -> maximise (validboardlist b) (alpha,beta)
                                      RedBot  -> minimise (validboardlist b) (alpha,beta)
                                      Finished -> 0
        where
            maximise:: [Board] -> (Alpha,Beta)-> Score
            maximise boards (alpha1,beta1) = case boards of
                     [] -> alpha1
                     x:xs
                         |  alpha1 >= beta1     -> alpha1
                         |  (scorelist i (a+1) (alpha1,beta1) x) < alpha1  -> maximise xs  (alpha1,beta1)
                         |  (scorelist i (a+1) (alpha1,beta1) x) >=alpha1 && (scorelist i (a+1) (alpha1,beta1) x) <= beta1 -> maximise xs  ((heuristic x (turn x)),beta1)


            minimise:: [Board] -> (Alpha,Beta)-> Score
            minimise boards (alpha2,beta2) = case boards of
                     [] -> beta2
                     x:xs
                         |  alpha2 >= beta2    -> beta2
                         |  (scorelist i (a+1) (alpha2,beta2) x) < beta2  -> minimise xs  ((heuristic x (turn x)),beta2)
                         |  (scorelist i (a+1) (alpha2,beta2) x) >= beta2 && (scorelist i (a+1) (alpha2,beta2) x) >= alpha2-> minimise xs  (alpha2,beta2)


validboardlist:: Board -> [Board]
validboardlist b = map (updateBoard b) (validmovelist b 1)

validmovelist:: Board -> Index -> [Index]
validmovelist   b i
            |i <= (fst $ dimension b) = case (validMove b i) of
                                True  -> [i]++(validmovelist b (i+1))
                                False -> []++ (validmovelist b (i+1))
            |otherwise = []

