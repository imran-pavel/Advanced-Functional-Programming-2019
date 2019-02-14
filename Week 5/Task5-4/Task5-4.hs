{-|

First run: ghc --make Task5-4.hs
Then run like this: Task5-4.exe 2 3   
            -- 2 is the number of players and 3 is the number of cards for each player. You can try with other numbers
            -- Output will show a list of tuples -> [(PlayerId, List of Cards for this player)]
            -- Last tuple of the list will show the remaining cards. Something like this: ("Remaining Cards", List of remaining cards)

-}



{-|

Write a program that shuffles cards. The program  is given as command line parameters the number of players and the number of cards for each player.
The program shuffles (using random numbers)  a normal playing card pack (4 suits: spades, hearts, diamonds, clubs, each having values 2..13 and A, 
plus two Jokers J1 and J2 that do not belong to any suit). and then deals cards for the players. Output the hands for the players and the leftover cards.

-}

import System.Environment
import System.Random
import qualified Data.List as DL


values :: [String]
values = "A" : ( map show [2..13] )

suits :: [String]
suits = ["Spade ", " Heart ", "Diamond ", "Club "]

createCardsOfThisSuit :: String -> [String]
createCardsOfThisSuit suit = zipWith (++) (repeat suit) values

cardPack :: [String]
cardPack = "J1" : "J2" : ( DL.intercalate [] $ foldl foldingFunction [] suits )
                                    where
                                        foldingFunction = (\acc suit -> (createCardsOfThisSuit suit) : acc)



dealCards :: (RandomGen g) => Int -> Int -> Int -> Int -> [String] -> g -> [String] -> [(String, [String])] -> [(String, [String])]                                       
dealCards 0 _ _ _ packOfCards _ _ outputList = outputList ++ [("Remaining Cards", packOfCards)]
dealCards nPlayers nthPlayer nCards 0 packOfCards randomGenerator chosenCardsForNthPlayer outputList = dealCards (nPlayers-1) (nthPlayer+1) nCards nCards packOfCards randomGenerator [] newOutputList
                                                                                                            where
                                                                                                                playerId = "Player " ++ (show nthPlayer)
                                                                                                                newOutputList = outputList ++ [(playerId, chosenCardsForNthPlayer)]

dealCards nPlayers nthPlayer nCards nCardsToGiveToNthPlayer packOfCards randomGenerator chosenCardsForNthPlayer outputList = dealCards nPlayers nthPlayer nCards (nCardsToGiveToNthPlayer-1) newPackOfCards newGenerator newchosenCardsForNthPlayer outputList
                                                                                                                                    where
                                                                                                                                        (chosenIndex, newGenerator) = randomR (0, ((length packOfCards)-1)) randomGenerator
                                                                                                                                        chosenCard = packOfCards !! chosenIndex
                                                                                                                                        newchosenCardsForNthPlayer = chosenCard : chosenCardsForNthPlayer
                                                                                                                                        newPackOfCards = DL.delete (packOfCards !! chosenIndex) packOfCards



main = do
            (nStrPlayers : nStrCards : []) <- getArgs
            let nPlayers = read nStrPlayers :: Int
            let nCards = read nStrCards :: Int
            gen <- getStdGen
            let packOfCards = cardPack
            let outputList = dealCards nPlayers 1 nCards nCards packOfCards gen [] []
            print outputList


