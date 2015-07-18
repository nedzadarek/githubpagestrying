module Keys where 

import Graphics.Element exposing (show)
import Mouse
import Keyboard
import Set 
import Char
import Dict
import String
main =
    Signal.map show 
    <| Signal.map String.concat 
    <| Signal.map List.reverse
    --<| Signal.map (\keys -> if keys == [73, 76, 89] then "I love you" else "I don't know you")
    --<| Signal.foldp (
    --    \keys hash -> 
    --        if | List.length keys > 3 -> hash
    --           | List.length keys == 0 -> {hash | current_keys <- [], keys <- hash.current_keys :: hash.keys }
    --           | List.length keys > List.length hash.current_keys -> {hash | current_keys <- keys}
    --           | otherwise -> hash) 
        --{current_keys = [], keys =[]}
    <| Signal.map (\codes -> List.map list_to_chars_hardcoded codes)
    <| Signal.map (\hash -> .keys hash)
    <| Signal.foldp (
        \keys hash -> 
            if | List.length keys > 5 -> hash
               | List.length keys == 0 -> {hash | current_keys <- [], keys <- hash.current_keys :: hash.keys }
               | List.length keys > List.length hash.current_keys -> {hash | current_keys <- keys}
               | otherwise -> hash) 
        {current_keys = [], keys =[]}
    
    <| Signal.map Set.toList Keyboard.keysDown

    --<| Signal.map (\x -> if x%2==0 then "mod 2" else "not mod 2")
    --<| Signal.foldp (\click total -> total + 1) 0 Mouse.clicks
    --Signal.map show sig1


list_to_chars codes = List.map Char.fromCode codes
list_to_chars2 codes =
  if | codes == [73,76,89] -> "I love you"
     | codes == [72,73,89] -> "I hate you"
     | otherwise -> "Who are you"

--codes = Dict.fromList [(1, 32), (2, 73), (3, 79), (4, 80), (5, 85)]
--alphabet = 
--  Dict.fromList 
--    [
--    ([codes[5]])
--    ]
-- 

--nths = 

nth l n = if n>1 then nth (mtail l) (n-1) else mhead l 

mhead l = case List.head l of Maybe.Just x -> x
mtail l = case List.tail l of Maybe.Just x -> x 

-- [32, 73, 79, 80, 85]
--" "  U   I   O   P
--[32, 85, 73, 79, 80]
list_to_chars_hardcoded codes = 
  case codes of
    [80] -> "A"
    [79] -> "B"
    [79, 80] -> "C"
    [73] -> "D"
    [73, 80] -> "E"
    [73, 79] -> "F"
    [73, 79, 80] -> "G"
    [85] -> "H"
    [80, 85] -> "I"
    [79, 85] -> "J"
    [79, 80, 85] -> "K"
    [73, 85] -> "L"
    [73, 80, 85] -> "M"
    [73, 79, 85] -> "N"
    [73, 79, 80, 85] -> "O"
    [32] -> "P"
    [32, 80] -> "Q"
    [32, 79] -> "R"
    [32, 79, 80] -> "S"
    [32, 73] -> "T"
    [32, 73, 80] -> "U"
    [32, 73, 79] -> "V"
    [32, 73, 79, 80] -> "W"
    [32, 85] -> "X"
    [32, 80, 85] -> "Y" -- [32, 85, 80]
    [32, 79, 85] -> "Z" -- [32, 85, 79] 

    otherwise -> " "
