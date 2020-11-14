module Task3 where
import Data.List as L
import Data.Char as C
import Task3Message

---haskel executable
    ---executable parameters
---stdin
---stdout
    ---halts
---exit code


data JsonLikeValue = JLString String | JLInt Int | JLMap [(String, JsonLikeValue)] | JLArray [JsonLikeValue] deriving (Show, Eq)
data InvalidState = Order | Duplicates deriving (Show, Eq)
type To = [[(Int, Char)]]

getLilBoard :: To
getLilBoard = case convert 3 (either error id (parse message)) of
    Right a -> a
    Left a -> error $ "Ivalid state" ++ show a

parse :: String -> Either String JsonLikeValue
parse str = 
    case parseJLMap str str of
        Left a -> Left a
        Right (b, "") -> Right b
        Right _ -> Left "Some values are outside of map"

parseJLMap :: String -> String -> Either String (JsonLikeValue, String)
parseJLMap ('d':t) orgStr = 
    case parseAllMapedJLValues t orgStr of
        Left a -> Left a
        Right (mapBody, rest) -> Right (mapBody, rest)
parseJLMap dnStartD orgStr = Left ("Error around character " ++ show errPos ++ " received string: " ++ dnStartD ++ " map has to start with a 'd'")
    where 
        errPos = lenDiff orgStr dnStartD

lenDiff :: String -> String -> Int
lenDiff str1 str2 = (length str1) - (length str2)

parseAllMapedJLValues :: String -> String -> Either String (JsonLikeValue, String)
parseAllMapedJLValues ('e':t) _ = Right (JLMap [], t)
parseAllMapedJLValues str orgStr =
    case parseMapedJLValue str orgStr of
        Left a -> Left a
        Right ((key, value), rest) ->
            case parseAllMapedJLValues rest orgStr of
                Left a -> Left a
                Right (JLMap acc, rest1) -> Right $ (JLMap ([(key, value)] ++ acc), rest1)

parseMapedJLValue :: String -> String -> Either String ((String, JsonLikeValue), String)
parseMapedJLValue str orgStr = 
    case parseString str orgStr of
        Left a -> Left a
        Right (key, rest) ->
            case parseJLValue rest orgStr of
                Left a -> Left a
                Right (value, rest') -> Right ((key, value), rest')

parseString :: String -> String -> Either String (String, String)
parseString str orgStr =
    let
        errPos = lenDiff orgStr str
        strLen = if C.isDigit $ head str
            then L.takeWhile C.isDigit str
            else "not declared"
        postfix = L.drop (length strLen) str
    in
        case strLen of 
            "not declared" -> Left ("Error around character " ++ show errPos ++ " received string: " ++ str ++ " Length of the string was not declared")
            _ ->
                case postfix of
                (':':r) -> Right (L.take (read strLen) r, L.drop (read strLen) r)
                _ -> Left ("Error around character " ++ show errPos ++ " received string: " ++ str ++ " Invalid string")

parseJLValue :: String -> String -> Either String (JsonLikeValue, String)
parseJLValue ('d':t) orgStr =
    case parseJLMap('d':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLValue ('l':t) orgStr = 
    case parseJLArray [] ('l':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLValue ('i':t) orgStr = 
    case parseJLInt ('i':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLValue (h:t) orgStr = 
    let
        errPos = lenDiff orgStr (h:t)
    in
    if C.isDigit h
    then 
        case parseJLString (h:t) orgStr of
            Left a -> Left a
            Right (a, b) -> Right (a, b)
    else Left ("Error around character " ++ show errPos ++ " received string: " ++ (h:t) ++ " JsonLikeValue has to start with a 'd' or a 'l' or an 'i' or a digit")
parseJLValue [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty JLValue")
    where
        errPos = lenDiff orgStr []

parseJLArray :: [JsonLikeValue] -> String -> String -> Either String (JsonLikeValue, String)
parseJLArray [] ('l':t) orgStr =
    case parseJLValue t orgStr of
        Left a -> Left a
        Right (value, (fstCh : rest)) ->
            case fstCh of
                'e' -> Right (JLArray ([] ++ [value]), rest)
                _ -> parseJLArray ([] ++ [value]) (fstCh : rest) orgStr
parseJLArray parsedArrEls (h:t) orgStr =
    case parseJLValue (h:t) orgStr of
        Left a -> Left a
        Right (value, (fstCh : rest)) ->
            case fstCh of
                'e' -> Right (JLArray (parsedArrEls ++ [value]), rest)
                _ -> parseJLArray (parsedArrEls ++ [value]) (fstCh : rest) orgStr
parseJLArray [] [] orgStr = Left ("Error around character " ++ show errPos ++ "Empty Array")
    where
        errPos = lenDiff orgStr []
parseJLArray _ dnStartL orgStr = Left ("Error around character " ++ show errPos ++ " received string: " ++ dnStartL ++ " list has to start with an 'l'")
    where
        errPos = lenDiff orgStr dnStartL

parseJLInt :: String -> String -> Either String (JsonLikeValue, String)
parseJLInt ('i':t) orgStr = 
        let 
            errPos = lenDiff orgStr ('i':t)
        in
        case C.isDigit $ head t of
            False -> Left ("Error around character " ++ show errPos ++ ", Integer has 0 digits")
            True ->
                    let
                        prefix = L.takeWhile C.isDigit t
                        postfix = L.drop (length prefix) t
                    in
                        case postfix of
                            ('e':r) -> Right (JLInt (read prefix), r)
                            _ -> Left ("Error around character " ++ show errPos ++ " received string: " ++ ('i':t) ++ " Integer has to end with an 'e'")
parseJLInt [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty Int")
    where
        errPos = lenDiff orgStr []
parseJLInt dnStartI orgStr = Left ("Error around character " ++ show errPos ++ " received string: " ++ dnStartI ++ " Integer has to start with an 'i'")
    where
        errPos = lenDiff orgStr dnStartI

parseJLString :: String -> String -> Either String (JsonLikeValue, String)
parseJLString str orgStr =
    let
        errPos = lenDiff orgStr str
    in
        if C.isDigit $ head str
            then case L.drop (length (L.takeWhile C.isDigit str)) str of
                (':':r) -> Right (JLString $ L.take (read (L.takeWhile C.isDigit str)) r, L.drop (read (L.takeWhile C.isDigit str)) r)
                _ -> Left ("Error around character " ++ show errPos ++ ", Invalid string")
            else Left ("Error around character " ++ show errPos ++ ", Length of the string was not declared")

-----------------------------------------------------------------

convert :: Int -> JsonLikeValue -> Either InvalidState To
convert size wholeMap =
    case getAllTurnsArr wholeMap ([], [], []) of
        Left a -> Left a
        Right allTurnsArr -> parseArrToLIL allTurnsArr (createEmptyLILArr size [])
        
createEmptyLILArr :: Int -> [[(Int, Char)]] -> [[(Int, Char)]]
createEmptyLILArr 0 arr = arr
createEmptyLILArr size arr = createEmptyLILArr (size-1) (arr ++ [[]])

parseArrToLIL :: ([Int], [Int], [Char]) -> [[(Int, Char)]] -> Either InvalidState [[(Int, Char)]]
parseArrToLIL ([], [], []) lilArr = Right lilArr
parseArrToLIL (xsArr, ysArr, vsArr) lilArr = 
    let
        ythArr = lilArr !! head ysArr
        newMove = (head xsArr, head vsArr)
    in 
        case addMoveToArr ythArr newMove of
            Left a -> Left a
            Right a -> parseArrToLIL (tail xsArr, tail ysArr, tail vsArr) (replace a (head ysArr) lilArr)

replace :: a -> Int -> [a] -> [a]
replace newEl 0 arr = newEl: (tail arr)
replace newEl i (a:arr) = (a : replace newEl (i-1) arr)

addMoveToArr :: [(Int, Char)] -> (Int, Char) -> Either InvalidState [(Int, Char)]
addMoveToArr allMovesArr (x, v) = 
    case findPosToInsert allMovesArr x 0 of
        Left a -> Left a
        Right pos -> Right $ insertAt (x,v) pos allMovesArr

insertAt :: a -> Int -> [a] -> [a]
insertAt newEl _ [] = [newEl]
insertAt newEl 0 arr = (newEl:arr)
insertAt newEl i (a:arr) = (a : insertAt newEl (i - 1) arr)

findPosToInsert :: [(Int, Char)] -> Int -> Int -> Either InvalidState Int
findPosToInsert [] _ _ = Right 0
findPosToInsert arr cord ptr = 
    if ((length arr) - 1 < ptr)
    then Right (ptr)
    else
        if (fst (arr !! ptr)) == cord
        then Left Duplicates
        else
            if (fst (arr !! ptr)) > cord
            then Right (ptr)
            else 
                case findPosToInsert arr cord (ptr+1) of
                    Left a -> Left a
                    Right a -> Right a

getAllTurnsArr :: JsonLikeValue -> ([Int], [Int], [Char]) -> Either InvalidState ([Int], [Int], [Char])
getAllTurnsArr wholeMap allTurnsArr = 
    case mapFind wholeMap "last" of
        Just lstLast -> 
            let
                allTurnsArr' = addTurn lstLast allTurnsArr
            in
                case isCorrOrder allTurnsArr' of 
                    False -> Left Order
                    True ->
                        case delFromMap wholeMap ("last", lstLast) of
                            JLMap (h:_) -> getAllTurnsArr (snd h) allTurnsArr'
                            JLMap [] -> Right allTurnsArr'

mapFind :: JsonLikeValue -> String -> Maybe JsonLikeValue 
mapFind (JLMap []) _ = Nothing
mapFind (JLMap (h:t)) needed = 
    if (fst h) == needed
    then Just $ snd h
    else mapFind (JLMap t) needed

addTurn :: JsonLikeValue -> ([Int],[Int],[Char]) -> ([Int],[Int],[Char])
addTurn turn (xsArr,ysArr,vsArr) = 
    case turn of
        JLArray [] -> (xsArr,ysArr,vsArr)
        JLArray (JLMap (turnTuple) : []) -> 
                    let
                        turnArr = snd $ head turnTuple
                        xsParsedInt = parseJLIntToInt $ (parseJLArrayToArray turnArr) !! 0
                        ysParsedInt = parseJLIntToInt $ (parseJLArrayToArray turnArr) !! 1
                        vsParsedStr = parseJLStringToString $ (parseJLArrayToArray turnArr) !! 2
                    in
                        (xsArr ++ [xsParsedInt], ysArr ++ [ysParsedInt], vsArr ++ vsParsedStr)
        a -> error $ "Turn has to be in an array, but received " ++  show a

parseJLIntToInt :: JsonLikeValue -> Int
parseJLIntToInt theInt =
    case theInt of
        JLInt a -> a

parseJLStringToString :: JsonLikeValue -> String
parseJLStringToString str =
    case str of
        JLString a -> a
        a -> error $ show a

parseJLArrayToArray :: JsonLikeValue -> [JsonLikeValue]
parseJLArrayToArray arr =
    case arr of
        JLArray a -> a

isCorrOrder :: ([Int], [Int], [Char]) -> Bool
isCorrOrder (_, _, []) = True
isCorrOrder (_, _, (_:[])) = True
isCorrOrder (_, _, vsArr) = 
    if head (reverse vsArr) == head (drop 1 (reverse vsArr))
    then False
    else True

delFromMap :: JsonLikeValue -> (String, JsonLikeValue) -> JsonLikeValue
delFromMap wholeMap itemDel = 
    case wholeMap of
        JLMap arrayOfTuples -> JLMap $ delete itemDel arrayOfTuples



---------------------------------------

start :: IO ()
start =
    let
        board = populateBlankVals getLilBoard
    in
        if (isBoardFilled board || (isWin board /= 'b'))
            then printStatusMessage $ (board, "I cannot perform any moves because game is already ended (board is full or there is a winner")
            else printStatusMessage $ (makeNextStep board, "")

printStatusMessage :: (To, String) -> IO ()
printStatusMessage (board, msg) = putStr $ unlines $ [msg, getStrToDrawBoard board]

makeNextStep :: To -> To
makeNextStep board =
        case findWinStep board of
            Just b -> b
            Nothing -> findNonDoomedBoard $ genAllPossibleMoves board board []

findNonDoomedBoard :: [To] -> To
findNonDoomedBoard [] = error "The board is doomed for me to lose, there is no possible move that would save me, lord Aurelion Sol forgive me for have a sined and save me from the Yasuo's torture"
findNonDoomedBoard (board:remBoards) =
    case isBoardDoomed board of
        False -> board
        True -> findNonDoomedBoard remBoards

isBoardDoomed :: To -> Bool
isBoardDoomed board = 
    case findWinStep board of
        Nothing -> False
        Just _ -> True

findWinStep :: To -> Maybe To
findWinStep board = 
    let
        player
            | isXTurn board = ('X', 10)
            | otherwise = ('O', -10)
        allPossMoves = genAllPossibleMoves board board []
        allPossScores = calcAllBoardsScore allPossMoves []
    in
        case findIndex (== (snd player)) allPossScores of
            Nothing -> Nothing
            Just i -> Just $ allPossMoves !! i

isXTurn :: To -> Bool
isXTurn ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) =
    let
        allTurnsValue = [snd sq1, snd sq2, snd sq3, snd sq4, snd sq5, snd sq6, snd sq7, snd sq8, snd sq9]
        xNmTurns = calcPlayerTurns allTurnsValue 'X' 0
        oNmTurns = calcPlayerTurns allTurnsValue 'O' 0
    in
        if (oNmTurns < xNmTurns)
        then False
        else True
isXTurn a = error $ "Cant check if X turn: Invalid board" ++ show a

calcPlayerTurns :: [Char] -> Char -> Int -> Int
calcPlayerTurns [] _ acc = acc
calcPlayerTurns (h:t) player acc
    | h == player = calcPlayerTurns t player (acc + 1)
    | otherwise = calcPlayerTurns t player acc

calcAllBoardsScore :: [To] -> [Int] -> [Int]
calcAllBoardsScore [] acc = acc
calcAllBoardsScore (h:t) acc =
    calcAllBoardsScore t (acc ++ [calcScore h])

calcScore :: To -> Int
calcScore board = 
    let
        board' = populateBlankVals board
    in
        case isWin board' of
            'X' -> 10
            'O' -> -10
            'b' 
               | (isBoardFilled board') -> 0
               | otherwise -> 50

genAllPossibleMoves :: To -> To -> [To] -> [To]
genAllPossibleMoves board genBoard acc = 
    case genPossibleMove board genBoard of
            Left _ -> acc
            Right (newMove, newGenBoard) -> genAllPossibleMoves board newGenBoard (acc ++ [newMove])

genPossibleMove :: To -> To -> Either String (To, To)
genPossibleMove orgBoard board = 
    case whichSqBlank board of
        Left _ -> Left "No more possible moves"
        Right (row, col) -> 
            let 
                player
                    | isXTurn orgBoard = 'X'
                    | otherwise = 'O'
                newRowForMove = replace (col, player) col (orgBoard !! row)
                newMove = replace newRowForMove row orgBoard
                newRowForBoard = replace (col, player) col (board !! row)
                newBoard = replace newRowForBoard row board
            in
                Right (newMove, newBoard)

populateBlankVals :: To -> To
populateBlankVals (row1 : row2 : row3 : []) 
    | (length row1 /= 3) = populateBlankVals ((addLowestBlank row1 0) : row2 : row3 : [])
    | (length row2 /= 3) = populateBlankVals (row1 : (addLowestBlank row2 0) : row3 : [])
    | (length row3 /= 3) = populateBlankVals (row1 : row2 : (addLowestBlank row3 0) : [])
    | otherwise = (row1 : row2 : row3 : []) 
populateDummyVals :: Show a1 => a1 -> a2
populateDummyVals b = error $ "Cannot populate board with blank values: Invalid board " ++ show b

addLowestBlank :: [(Int, Char)] -> Int -> [(Int, Char)]
addLowestBlank row acc  
    | (length row < acc+1) = insertAt (acc, 'b') acc row
    | (fst (row !! acc) == acc) = addLowestBlank row (acc+1)
    | otherwise = insertAt (acc, 'b') acc row

isWin :: To -> Char
isWin ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) 
    --Diagonals
    | (snd sq1 == snd sq5 && snd sq5 == snd sq9 && snd sq9 /= 'b') = snd sq9
    | (snd sq3 == snd sq5 && snd sq5 == snd sq7 && snd sq7 /= 'b') = snd sq7
    --Horizontals
    | (snd sq1 == snd sq2 && snd sq2 == snd sq3 && snd sq3 /= 'b') = snd sq3
    | (snd sq4 == snd sq5 && snd sq5 == snd sq6 && snd sq6 /= 'b') = snd sq6
    | (snd sq7 == snd sq8 && snd sq8 == snd sq9 && snd sq9 /= 'b') = snd sq9
    --Verticals
    | (snd sq1 == snd sq4 && snd sq4 == snd sq7 && snd sq7 /= 'b') = snd sq7
    | (snd sq2 == snd sq5 && snd sq5 == snd sq8 && snd sq8 /= 'b') = snd sq8
    | (snd sq3 == snd sq6 && snd sq6 == snd sq9 && snd sq9 /= 'b') = snd sq9
    | otherwise = 'b'
isWin b = error $ "Cannot check if player won diagonaly: Invalid board " ++ show b

isBoardFilled :: To -> Bool
isBoardFilled ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : []) : [])
    | (snd sq1 /= 'b' && snd sq2 /= 'b' && snd sq3 /= 'b' && snd sq4 /= 'b' && snd sq5 /= 'b' &&
       snd sq6 /= 'b' && snd sq7 /= 'b' && snd sq8 /= 'b' && snd sq9 /= 'b') = True
    | otherwise = False
isBoardFilled b = error $ "Cannot check if board is filled: Invalid board " ++ show b

calcBlankSq :: To -> Int
calcBlankSq ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : []) : []) = 
    let 
        b1 = isSqBlank sq1
        b2 = isSqBlank sq2
        b3 = isSqBlank sq3
        b4 = isSqBlank sq4
        b5 = isSqBlank sq5
        b6 = isSqBlank sq6
        b7 = isSqBlank sq7
        b8 = isSqBlank sq8
        b9 = isSqBlank sq9
    in 
        b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9

isSqBlank :: (Int, Char) -> Int
isSqBlank sq
    | (snd sq == 'b') = 1
    | otherwise = 0

whichSqBlank :: To -> Either String (Int, Int) 
whichSqBlank ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) 
    | (snd sq1 == 'b') = Right (0, 0)
    | (snd sq2 == 'b') = Right (0, 1)
    | (snd sq3 == 'b') = Right (0, 2)
    | (snd sq4 == 'b') = Right (1, 0)
    | (snd sq5 == 'b') = Right (1, 1)
    | (snd sq6 == 'b') = Right (1, 2)
    | (snd sq7 == 'b') = Right (2, 0)
    | (snd sq8 == 'b') = Right (2, 1)
    | (snd sq9 == 'b') = Right (2, 2)
    | otherwise = Left "no blank sq"

getStrToDrawBoard :: To -> String
getStrToDrawBoard board=
    let
        board' = replaceAllBWithSpace board
        ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) = board'
    in
        unlines $ [[snd sq1] ++  " | " ++  [snd sq2] ++ " | " ++ [snd sq3] ,
                   "---------",
                   [snd sq4] ++  " | " ++  [snd sq5] ++ " | " ++ [snd sq6] ,
                   "---------",
                   [snd sq7] ++  " | " ++  [snd sq8] ++ " | " ++ [snd sq9]]

replaceAllBWithSpace :: To -> To
replaceAllBWithSpace ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) = 
    let
        sq1' = (fst sq1 ,ifBSpace $ snd sq1)
        sq2' = (fst sq2 ,ifBSpace $ snd sq2)
        sq3' = (fst sq3 ,ifBSpace $ snd sq3)
        sq4' = (fst sq4 ,ifBSpace $ snd sq4)
        sq5' = (fst sq5 ,ifBSpace $ snd sq5)
        sq6' = (fst sq6 ,ifBSpace $ snd sq6)
        sq7' = (fst sq7 ,ifBSpace $ snd sq7)
        sq8' = (fst sq8 ,ifBSpace $ snd sq8)
        sq9' = (fst sq9 ,ifBSpace $ snd sq9)
    in
        [[sq1', sq2', sq3'], [sq4', sq5', sq6'], [sq7', sq8', sq9']]

ifBSpace :: Char -> Char
ifBSpace ch
    |(ch == 'b') = ' '
    | otherwise = ch
