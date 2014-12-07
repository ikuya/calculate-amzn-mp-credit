import System.Environment (getArgs)
import Control.Monad (when)
import Text.Read (readMaybe)

data Category = Book | CD deriving (Show, Eq)

type Price = Int
type Credit = Int

calcCreditForCategory :: Category -> Price -> Credit
calcCreditForCategory category price
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

calcCredit :: Price -> (Credit, Credit)
calcCredit price = (calcCreditForCategory Book price,
                    calcCreditForCategory CD price)

main = do
  args <- getArgs
  when (not $ null args) $ do
    case readMaybe $ head args of
      Just price -> let creditPair = calcCredit price
                    in putStrLn $ "Book:" ++ (show $ fst creditPair) ++
                                  " CD/DVD:" ++ (show $ snd creditPair)
      Nothing    -> putStrLn "Error: the argument must be a numeric number"
