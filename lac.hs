import Text.Read

-- After every expression is this result.
data Result =
  Console String |
  Result Number
type Number = Int

readMaybeNumber :: String -> Maybe Number
readMaybeNumber = readMaybe
data Operation = Plus | Minus | Times | Over
-- These operations must function for whatever underlying type is Number.
op :: Operation -> Number -> Number -> Number
op Plus = (+)
op Minus = (-)
op Times = (*)
op Over = div
-- -- If one wants to:
-- instance Read Operation where
--   readsPrec 0 "+" = [(Plus, [])]
--   readsPrec 0 "-" = [(Minus, [])]
--   readsPrec 0 "*" = [(Times, [])]
--   readsPrec 0 "/" = [(Over, [])]
-- -- Then you are using readMaybe :: String -> Maybe Operation.
-- Otherwise:
readMaybeOperation :: String -> Maybe Operation
readMaybeOperation "+" = Just Plus
readMaybeOperation "-" = Just Minus
readMaybeOperation "*" = Just Times
readMaybeOperation "/" = Just Over
readMaybeOperation _ = Nothing

data E = It | N Number | Op Operation
expression :: Maybe Number -> String -> Result
expression it input =
  let parse = (mapM parse1 . words) input
  in
  case parse of
    Left error -> error
    Right ex -> expression' [] ex
  where
    parse1 "it"
      | Just it <- it = Right (N it)
      | otherwise = Left (Console "No last result")
    parse1 input1
      | Just n <- readMaybeNumber input1 = Right (N n)
      | Just op <- readMaybeOperation input1 = Right (Op op)
      | otherwise = Left (Console "Parse error")
expression' [] [N n] = Result n
expression' _ [] = Console "Bad expression"
expression' left (N a : N b : Op c : ex) =
  expression' [] (left ++ N (op c a b) : ex)
expression' left (a : ex) = expression' (left ++ [a]) ex

rpn :: IO ()
rpn = rpn' Nothing
rpn' it =
  do
  input <- getLine
  case expression it input of
    Console s ->
      do
      putStrLn s
      retry
    Result n ->
      do
      print n
      rpn' (Just n)
  where retry = rpn' it
