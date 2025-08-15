import System.Cmd

-- constants
indent :: Int
indent = 2

type Name = String
data Wikiwiki =
  Cat {name :: Name, w :: [Wikiwiki]} |
  Node {name :: Name}
itemize :: Wikiwiki -> String
itemize (Cat name w) = "cat " ++ name
itemize (Node name) = "    " ++ name

type YAML = String
format :: Wikiwiki -> YAML
format (Cat "root" w) = concat (map format w)
format (Cat name w) =
  name ++ ":\n" ++
  (unlines . map (replicate indent ' ' ++) . lines) (concat (map format w))
format (Node name) = name ++ "\n"

browse :: Wikiwiki -> IO ()
browse root@(Cat "root" w) = browse1 [] root
browse _ = error "Didn't start browsing at root."
browse1 d (Cat name w) =
  do
  system "clear"
  putStr
    (name ++ "\n" ++
     "\n" ++
     unlines (map (replicate 2 ' ' ++) (zipWith (++) (map ((++ "> ") . show) [1 ..]) (map itemize w))) ++
     "\n"
    )
  putStr "> "
  number <- getLine
  if all (\c -> elem c ['0' .. '9']) number
    then let r = read number in
         if 0 /= r then finger (r - 1) else exit
   else retry
  where
    exit =
      case d of
        _ : _ -> browse1 (tail d) (head d)
        _ -> return ()
    retry = browse1 d (Cat name w)
    finger number = finger1 number w
    finger1 0 (w1 : wall) = browse1 (Cat name w : d) w1
    finger1 _ [] = retry
    finger1 number (_ : w) = finger1 (number - 1) w
browse1 d (Node name) =
  do
  system ("mg -nR '" ++ name ++ "'")
  browse1 (tail d) (head d)
