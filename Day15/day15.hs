import System.Random

rollDice :: IO Int
rollDice = getStdRandom (random (1,300))