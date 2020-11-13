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

--minimax
    --return every posibility [posibilities]
        --goes 9 times, checks every sq
            --if sq null, create move
    --calc score
        -- if game end
            -- player * sqLeft+1; where player is -1 or 1
        -- else calc score




data JsonLikeValue = JLString String | JLInt Int | JLMap [(String, JsonLikeValue)] | JLArray [JsonLikeValue] deriving (Show, Eq)
data InvalidState = Order | Duplicates deriving (Show, Eq)
type To = [[(Int, Char)]]

-- message' = "d4:prevd4:prevd4:prevd4:lastd2:ysli1ee2:vsl1:Oe2:xsli0eee4:prevd4:lastd2:ysli2ee2:vsl1:Xe2:xsli0eee4:prevd4:lastd2:ysli0ee2:vsl1:Oe2:xsli2eee4:prevd4:prevd4:lastd2:ysli2ee2:vsl1:Oe2:xsli2eee4:prevd4:lastd2:ysli0ee2:vsl1:Xe2:xsli0eeeee4:lastd2:ysli1ee2:vsl1:Xe2:xsli2eeeeeee4:lastd2:ysli2ee2:vsl1:Xe2:xsli1eeee4:lastd2:ysli0ee2:vsl1:Oe2:xsli1eeee4:lastd2:ysli1ee2:vsl1:Xe2:xsli1eeee"

p = parse message
c = convert (either error id (parse message))

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

convert :: JsonLikeValue -> Either InvalidState ([Int], [Int], [Char])
convert wholeMap = getAllTurnsArr wholeMap ([], [], [])

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

--calcScore :: 