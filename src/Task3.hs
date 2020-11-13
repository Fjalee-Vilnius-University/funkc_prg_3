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
parseJLMap dnStartD orgStr = Left ("Error around character " ++ show errPos ++ ", map has to start with a 'd'")
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
            "not declared" -> Left ("Error around character " ++ show errPos ++ ", Length of the string was not declared")
            _ ->
                case postfix of
                (':':r) -> Right (L.take (read strLen) r, L.drop (read strLen) r)
                _ -> Left ("Error around character " ++ show errPos ++ ", Invalid string")

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
    if (C.isDigit h)
    then 
        case parseJLString (h:t) orgStr of
            Left a -> Left a
            Right (a, b) -> Right (a, b)
    else Left ("Error around character " ++ show errPos ++ ", JsonLikeValue has to start with a 'd' or a 'l' or an 'i' or a digit")
parseJLValue [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty JLValue")
    where
        errPos = lenDiff orgStr []

parseJLArray :: [JsonLikeValue] -> String -> String -> Either String (JsonLikeValue, String)
parseJLArray [] ('l':t) orgStr =
    case parseJLIntOrString t orgStr of
        Left a -> Left a
        Right (value, (fstCh : rest)) ->
            case fstCh of
                'e' -> Right (JLArray ([] ++ [value]), rest)
                _ -> parseJLArray ([] ++ [value]) (fstCh : rest) orgStr
parseJLArray parsedArrEls (h:t) orgStr =
    case parseJLIntOrString (h:t) orgStr of
        Left a -> Left a
        Right (value, (fstCh : rest)) ->
            case fstCh of
                'e' -> Right (JLArray (parsedArrEls ++ [value]), rest)
                _ -> parseJLArray (parsedArrEls ++ [value]) (fstCh : rest) orgStr
parseJLArray [] [] orgStr = Left ("Error around character " ++ show errPos ++ "Empty Array")
    where
        errPos = lenDiff orgStr []
parseJLArray _ dnStartL orgStr = Left ("Error around character " ++ show errPos ++ ", list has to start with an 'l'")
    where
        errPos = lenDiff orgStr dnStartL

parseJLIntOrString :: String -> String -> Either String (JsonLikeValue, String)
parseJLIntOrString ('i':t) orgStr = 
    case parseJLInt ('i':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLIntOrString (h:t) orgStr =
    let
        errPos = lenDiff orgStr (h:t)
    in
    if C.isDigit h
    then case parseJLString (h:t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
    else Left ("Error around character " ++ show errPos ++ ", Value is nether an Int or a String")
parseJLIntOrString [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty Int or String")
    where
        errPos = lenDiff orgStr []

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
                            _ -> Left ("Error around character " ++ show errPos ++ ", Integer has to end with an 'e'")
parseJLInt [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty Int")
    where
        errPos = lenDiff orgStr []
parseJLInt dnStartI orgStr = Left ("Error around character " ++ show errPos ++ ", Integer has to start with an 'i'")
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