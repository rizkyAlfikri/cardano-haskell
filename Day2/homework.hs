type Digit = Int
type CardNumber = [Int]
type ReversedCardNumber = CardNumber
type Index = Int
type CardNumberWithIndex = [(Digit, Index)]

ccNumber = [4,5,5,6,7,3,7,5,8,6,8,9,9,8,5,5]
defaultIndex = 0

constructWithIndex :: CardNumber -> Digit -> CardNumberWithIndex
constructWithIndex [] _ = []
constructWithIndex (x:xs) index = (x, index) : constructWithIndex xs (index + 1)

-- 1. Drop the last digit number. Wrap single droped number and output list wiht tuple 
dropLastDigit :: CardNumber -> (CardNumber, Digit)
dropLastDigit xs = (init xs , last xs)

outputDropLastDigitVer1 = fst $ dropLastDigit ccNumber
outputDropLastDigitVer2 = snd $ dropLastDigit ccNumber


-- 2. Reverse the number
reverseCardNumber :: CardNumber -> ReversedCardNumber
reverseCardNumber = reverse

outputReverseCardNumber = reverseCardNumber outputDropLastDigitVer1


-- 3. Multiply digit in odd position
multiplyOddPos:: CardNumberWithIndex -> CardNumber
multiplyOddPos = map (\x -> if odd . (+1) $ snd x then fst x *2 else fst x)

outputMultiplyOddPos = multiplyOddPos $ constructWithIndex outputReverseCardNumber defaultIndex


-- 4. Substract with 9 if the element of list greater than 9
substractWithNine :: CardNumber -> CardNumber
substractWithNine = map(\x -> if x > 9 then x - 9 else x)

outputSubstractWithNine = substractWithNine outputMultiplyOddPos


-- 5. Sum all element into single digit integer
sumAllDigitNumber :: CardNumber -> Digit
sumAllDigitNumber = sum

outputSumAllDigitNumber = sumAllDigitNumber outputSubstractWithNine


-- 6. Add the check digit to the sum
addResultSumWithDropLastDigit :: Digit -> Digit -> Digit
addResultSumWithDropLastDigit sumNumber lastDropNum = sumNumber + lastDropNum

outputAddResultSumWithDropLastDigit = outputSumAllDigitNumber + outputDropLastDigitVer2


-- 7. Check if total sum can be divided by 9
isTotalSumCanDividedWithTen:: Digit -> Bool
isTotalSumCanDividedWithTen = even  . (`mod` 10)

outputIsTotalSumCanDividedWithTen = isTotalSumCanDividedWithTen outputAddResultSumWithDropLastDigit

verifiedCardNumber :: CardNumber -> Bool
verifiedCardNumber cardNumber = resultIsTotalSumCanDividedWithNine
    where
        resultDropLastDigit = dropLastDigit cardNumber
        resultReverseCardNumber = reverseCardNumber $ fst resultDropLastDigit
        resultMultiplyOddPos = multiplyOddPos $ constructWithIndex resultReverseCardNumber defaultIndex
        resultSubstractWithNine = substractWithNine resultMultiplyOddPos
        resultSumAllDigitNumber = sumAllDigitNumber resultSubstractWithNine
        resultAddResultSumWithDropLastDigit = addResultSumWithDropLastDigit resultSumAllDigitNumber $ snd resultDropLastDigit
        resultIsTotalSumCanDividedWithNine = isTotalSumCanDividedWithTen resultAddResultSumWithDropLastDigit