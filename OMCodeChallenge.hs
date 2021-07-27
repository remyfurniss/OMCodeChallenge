import Data.Char (isDigit)

-- MAIN FUNCTION --

main :: IO Integer
main = do
  putStrLn "Enter value"
  amount <- getLine
  if isValid amount
    then if isDigits $ removeLastPence amount
            --Just return the pence value
            then return $ read $ removeLastPence amount
            --Contains £s so plit
            else return $ poundsDecimalToPence $ removeFirstPound $ removeLastPence amount
    else
      --Not valid therefore retun 0
      return 0

-- HELPER FUNCTIONS --

--Check if the whole string is just digits
isDigits :: String -> Bool
isDigits str = all isDigit str

--Removes the last character if its is p
removeLastPence :: String -> String
removeLastPence ("")    = ""
removeLastPence ("p")   = ""
removeLastPence (c:cs)  = [c] ++ removeLastPence cs

--Removes the fist character if it is £
removeFirstPound :: String -> String
removeFirstPound ""       = ""
removeFirstPound ('£':cs) = cs
removeFirstPound str      = str

--Converts the string with no £ or p into the total pence value
poundsDecimalToPence :: String -> Integer
poundsDecimalToPence str = pounds * 100 + pence
  where
    (pounds,pence) = poundAndPenceToInt $ splitPoundsAndPence ("",str)

--Splits the pound and pence string at the decimal
splitPoundsAndPence :: (String,String) -> (String,String)
splitPoundsAndPence ("","") = ("","")
splitPoundsAndPence (pounds,"") = (pounds,"")
splitPoundsAndPence (pounds,c:cs) =
  if c == '.'
    then  (pounds,cs)
    else  splitPoundsAndPence (pounds++[c],cs)

--Converts the pounds and pence parts into there pence valhes
poundAndPenceToInt :: (String,String) -> (Integer,Integer)
poundAndPenceToInt (pounds,pence) = (pounds',pence'')
  where
    pounds' = if null pounds then 0 else read pounds
    pence'  = take 3 $ pence ++ "000"
    pence'' = read(take 2 pence') + zeroRound(read([pence'!!2])/10)

--Haskell uses an odd way of rounding and therefore this will do for now as the value is between 0 and 1
zeroRound :: Float -> Integer
zeroRound f = if f >= 0.5 then ceiling f else floor f

-- Helper Function Tests ----

isDigitsTests = ["asd","345","dsf342"]
runIsDigitsTest = map isDigits isDigitsTests == [False,True,False]

removeLastPenceTests = ["123p","p","123", "p234"]
runRemoveLastPenceTests = map removeLastPence removeLastPenceTests == ["123","","123","p234"]

removeFirstPoundTests = ["£123","123","£","123£"]
runRemoveFirstPoundTests = map removeFirstPound removeFirstPoundTests == ["123","123","","123£"]

poundsDecimalToPenceTests = ["123.12","123",".23",".265","00.123"]
runPoundsDecimalToPenceTests = map poundsDecimalToPence poundsDecimalToPenceTests == [12312,12300,23,27,12]

splitPoundsAndPenceTests = [("","123."),("",".3453"),("","134")]
runSplitPoundsAndPenceTests = map splitPoundsAndPence splitPoundsAndPenceTests == [("123",""),("","3453"),("134","")]

poundAndPenceToIntTests = [("123","34"),("","1"),("1",""),("13","1234"),("13","1250")]
runPoundAndPenceToIntTests = map poundAndPenceToInt poundAndPenceToIntTests == [(123,34),(0,10),(1,00),(13,12),(13,13)]

-- VALIDATION --

--Check if first is £ or int
isValidFirstVal :: String -> Bool
isValidFirstVal str = isDigit firstVal || firstVal == '£'
  where firstVal = head str

--Check if last val is p or int
isValidLastVal :: String -> Bool
isValidLastVal str = isDigit lastVal || lastVal == 'p'
  where lastVal = last str


--Check if midle vvals are . or an int
isValidMiddleVals :: String -> Bool
isValidMiddleVals (c:"") = isDigit c || c == '.'
isValidMiddleVals str    = all (\c-> isDigit c || c == '.') middleVals
  where
    strLen     = length str
    middleVals = tail $ take (strLen-1) str

--Count the number of decimals
countDecimals :: String -> Int
countDecimals = length . filter ('.'==)

--Make sure only 1 decimal
containsOneOrLesseDecimals :: String -> Bool
containsOneOrLesseDecimals str =  countDecimals str <= 1

--Make sure theres digits within the string/null
hasDigits :: String -> Bool
hasDigits = any isDigit


isValid :: String -> Bool
isValid str = not (null str) &&
 isValidFirstVal str &&
 isValidLastVal str &&
 isValidMiddleVals str  &&
 containsOneOrLesseDecimals str &&
 hasDigits str

-- Validation Tests --

isValidFirstValTests = ["£adfd3","3dfsadf","sfdsa"]
runIsValidFirstValTests = map isValidFirstVal isValidFirstValTests == [True,True,False]

isValidLastValTests = ["fdsgsfdgp","adsff4","asdff"]
runIsValidLastValTests = map isValidLastVal isValidLastValTests == [True,True,False]

isValidMiddleValsTests = ["d234r","d345.435g","dsfads"]
runIsValidMiddleValsTests = map isValidMiddleVals isValidMiddleValsTests == [True,True,False]

countDecimalsTests = ["4...45","..","132.4", "dsf"]
runCountDecimalsTests = map countDecimals countDecimalsTests == [3,2,1,0]

containsOneOrLesseDecimalsTests = ["4242","wf.345","..dfs"]
runContainsOneOrLesseDecimalsTests = map containsOneOrLesseDecimals containsOneOrLesseDecimalsTests == [True,True,False]

hasDigitsTests = ["adsfdf","3242sdfs","2342"]
runHasDigitsTests = map hasDigits hasDigitsTests == [False,True,True]

-- TESTS ---

testFunction :: String -> Integer
testFunction str =
  if isValid str
    then if isDigits $ removeLastPence str
            --Just return the pence value
            then  read $ removeLastPence str
            --Contains £s so plit
            else  poundsDecimalToPence $ removeFirstPound $ removeLastPence str
    else
      --Not valid therefore retun 0
       0


tests = ["6",
         "75",
         "167p",
         "4p",
         "1.97",
         "£1.33",
         "£2",
         "£20",
         "£1.97p",
         "£1p",
         "£1.p",
         "001.61p",
         "6.235p",
         "£1.256532677p",

         "",
         "1x",
         "£1x.0p",
         "£p",

         "£",
         "p",
         "x",
         "0.0001",
         "p3£5"]

testResults = [6,
               75,
               167,
               4,
               197,
               133,
               200,
               2000,
               197,
               100,
               100,
               161,
               624,
               126,

               0,
               0,
               0,
               0,

               0,
               0,
               0,
               0,
               0]

runTests = map testFunction tests == testResults

-- THE TASK ---

data Denominations =
  Denominations { twoPound    :: Int,
                  onePound    :: Int,
                  fiftyPence  :: Int,
                  twentyPence :: Int,
                  tenPence    :: Int,
                  fivePence   :: Int,
                  twoPence    :: Int,
                  onePence    :: Int } deriving (Show, Eq)

startingDenominations = Denominations 0 0 0 0 0 0 0 0

--Recursiveley subtract pence and add to denoms
penceToDenominations ::  Denominations -> Integer -> Denominations
penceToDenominations denoms pence
      | pence >= 200 = penceToDenominations denoms {twoPound = (twoPound denoms) + 1 } (pence - 200)
      | pence >= 100 = penceToDenominations denoms {onePound = (onePound denoms) + 1 } (pence - 100)
      | pence >= 50  = penceToDenominations denoms {fiftyPence = (fiftyPence denoms) + 1 } (pence - 50)
      | pence >= 20  = penceToDenominations denoms {twentyPence = (twentyPence denoms) + 1 }  (pence - 20)
      | pence >= 10  = penceToDenominations denoms {tenPence = (tenPence denoms) + 1 } (pence - 10)
      | pence >= 5   = penceToDenominations denoms {fivePence = (fivePence denoms) + 1 } (pence - 5)
      | pence >= 2   = penceToDenominations denoms {twoPence = (twoPence denoms) + 1 } (pence - 2)
      | pence >= 1   = penceToDenominations denoms {onePence = (onePence denoms) + 1 } (pence - 1)
      | otherwise    = denoms


denominationsTests = [234,100,96,21,1,0]

denominationsResults = [Denominations 1 0 0 1 1 0 2 0,
                        Denominations 0 1 0 0 0 0 0 0,
                        Denominations 0 0 1 2 0 1 0 1,
                        Denominations 0 0 0 1 0 0 0 1,
                        Denominations 0 0 0 0 0 0 0 1,
                        Denominations 0 0 0 0 0 0 0 0]

runDenominationsTests = map (penceToDenominations  startingDenominations) denominationsTests == denominationsResults

--- THE FULL RUN ---

rullFullTest = map ((penceToDenominations  startingDenominations) . testFunction) tests
