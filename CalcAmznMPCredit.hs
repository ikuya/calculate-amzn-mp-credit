import System.Environment (getArgs)
import Control.Monad (when)
import Text.Read (readMaybe)

--------------------------------------------------

data Category = Book | CD deriving (Show, Eq)

type Price = Int
type Credit = Int

--------------------------------------------------

calculateCredit :: Category -> Price -> Credit
calculateCredit category price
  = price + (shippingCharge category)           -- Item price + Shipping chage
          - (variableClosingFee category)       -- Variable closing fee
          - (round $ fromIntegral price * 0.15) -- Referral fee
          - 100                                 -- Per item fee
  where
    shippingCharge category
      | category == Book = 257
      | category == CD   = 350
    variableClosingFee category
      | category == Book = 60
      | category == CD   = 140

creditPair :: Price -> (Credit, Credit)
creditPair price = (calculateCredit Book price
                   ,calculateCredit CD price)

--------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  when (not $ null args) $ do
    case readMaybe $ head args of
      Just price -> let credits = creditPair price
                    in do
                      putStrLn $ show Book ++ ": " ++ (show $ fst credits)
                      putStrLn $ show CD   ++ ": " ++ (show $ snd credits)
      Nothing    -> putStrLn "Error: the argument must be a numeric number"
