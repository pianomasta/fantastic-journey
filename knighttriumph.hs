-- A proposito de generalisar le sequentia de movimentos de pecias

import Data.List
import System.Environment
import Text.Read

type Ver = Bool
si, no :: Ver
si = True
no = False

type Position = (Int, Int)

mover :: Position -> Int -> Position
mover (filo, rango) =
  \verso ->
    (\(l, d) -> (filo + l, rango + d))
    (case verso of
       1 -> (1, 2)
       2 -> (2, 1)
       3 -> (2, -1)
       4 -> (1, -2)
       5 -> (-1, -2)
       6 -> (-2, -1)
       7 -> (-2, 1)
       8 -> (-1, 2)
    )

passo :: Int -> Position -> [Position]
passo n pos = passo' n [pos]
passo' 0 pos = pos
passo' n pos = passo' (n - 1) (concatMap passo1 pos)
  where passo1 pos = pos : filter valide (map (mover pos) [1 .. 8])

valide :: Position -> Ver
valide (filo, rango) = valide1 filo && valide1 rango
  where valide1 a = 1 <= a && a < 9

visualisar :: [Position] -> IO ()
visualisar = putStr . visualisar' [(a, b) | a <- [8, 7 .. 1], b <- [1 .. 8]]
visualisar' pada pos
  | [(1, 8)] <- pada = check (1, 8) pos : '8' : '\n' : "abcdefgh\n"
  | (a, 8) : pada <- pada = check (a, 8) pos : ((head . show) a) : '\n' : visualisar' pada pos
  | (a, b) : pada <- pada = check (a, b) pos : visualisar' pada pos
  where check pos1 pos = if elem pos1 pos then 'x' else ' '

readPosition :: String -> Position
readPosition [filo, rango] = (readFilo filo, readRango rango)
  where
    readFilo filo =
      case filo of
        'a' -> 1; 'b' -> 2; 'c' -> 3; 'd' -> 4
        'e' -> 5; 'f' -> 6; 'g' -> 7; 'h' -> 8
        _ -> error ("Error leger filo: " ++ [filo])
    readRango rango =
      case rango of
        '1' -> 1; '2' -> 2; '3' -> 3; '4' -> 4
        '5' -> 5; '6' -> 6; '7' -> 7; '8' -> 8
        _ -> error ("Error leger rango: " ++ [rango])
readPosition input = error ("Error leger position: " ++ input)

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
      [position, passos] ->
        case readMaybe passos :: Maybe Int of
          Just passos ->
            if 0 <= passos
              then visualisar $ passo passos (readPosition position)
             else error "Numero de passos debe esser nonnegative."
          Nothing ->
            error ("Error leger numero de passos: " ++ passos)
      _ ->
        error ("usage: knighttriumph [position] [passos]")
