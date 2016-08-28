import Control.Monad.State
import Data.Char
import System.IO
import System.Exit


pop :: State [Integer] ()
pop = state $ \(s:stack) -> ((),stack)

push :: Integer -> State [Integer] ()
push a = state $ \stack -> ((),a:stack)

swap :: State [Integer] ()
swap = state $ \stack -> ((), swapHelp stack)
  where
    swapHelp [] = []
    swapHelp [a] = [a]
    swapHelp (a:b:xs) = b:a:xs

add :: State [Integer]  ()
add = state $ \stack -> ((), biOp (+) stack)

sub :: State [Integer] ()
sub = state $ \stack -> ((), biOp (-) stack)

mult :: State [Integer] ()
mult = state $ \stack -> ((),biOp (*) stack)

divi :: State [Integer] ()
divi = state $ \stack -> ((), biOp quot stack)

leftOp :: State [Integer] ()
leftOp = state $ \stack  -> ((), biOp leftShift stack)

rightOp :: State [Integer] ()
rightOp = state $ \stack  -> ((), biOp rightShift stack)

idOp :: State [Integer] ()
idOp = state $ \stack -> ((), stack)

biOp :: (a -> a -> a) -> [a] -> [a]
biOp _ [] = []
biOp _ [a] = [a]
biOp f (a:b:stack) = f a b :stack

pow :: Integral a => a -> a -> a
pow a b | b > 0 = product $ replicate (fromIntegral b) a
pow _ _ = 1

leftShift :: Integral a => a -> a -> a
leftShift a b | b > 0 = a * pow 2 b
leftShift a b | b < 0 = rightShift a (-b)
leftShift a _ = a

rightShift :: Integral a => a -> a -> a
rightShift a b | b > 0 = floor $ fromIntegral a / fromIntegral (pow 2 b)
rightShift a b | b < 0 = leftShift a (-b)
rightShift a _ = a

switch :: Char -> Char
switch a
  | a == ',' = ' '
  | otherwise = a

replace = map switch

prin :: [Integer] -> String
prin stack = (replace . show . reverse) stack ++ " > "

isNum :: String -> Bool
isNum = all (\x -> isNumber x || (x == '-'))


readOp :: String -> State [Integer] ()
readOp a
  | a == "+" = add
  | a == "-" = sub
  | a == "*" = mult
  | a == "/" = divi
  | a == ">" = pop
  | a == "<<" = leftOp
  | a == ">>" = rightOp
  | a == "~" = swap
  | a == "" = idOp
  | isNum a = push ( read a :: Integer)
  | otherwise =  idOp

loop ::  State [Integer] () -> IO ()
loop st = do
  op <- getLine
  when (op == ":q") exitSuccess
  let f = readOp op
  let newSt = st >> f
  let res  = execState newSt []
  putStr $ prin res
  hFlush stdout
  loop newSt

main :: IO ()
main = putStr "[] > " >> hFlush stdout >> loop idOp
