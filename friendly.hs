import System.Environment
import System.Cmd

name :: Char -> String
name ' ' = "white"
name '!' = "control"
name '"' = "bunny"
name '#' = "hash"
name '$' = "ching"
name '%' = "mod"
name '&' = "address"
name '\x27' = "irk"
name '(' = "wax"
name ')' = "wane"
name '*' = "aster"
name '+' = "cross"
name ',' = "tail"
name '-' = "option"
name '.' = "hidden"
name '/' = "on"
name ':' = "type"
name ';' = "break"
name '<' = "leftoid"
name '=' = "equals"
name '>' = "rightoid"
name '?' = "query"
name '@' = "whorl"
name '[' = "lbrick"
name '\x5c' = "off"
name ']' = "rbrick"
name '^' = "hat"
name '_' = "under"
name '`' = "tick"
name '{' = "lsik"
name '|' = "pipe"
name '}' = "rsik"
name '~' = "knot"

friendly0 (c@'-' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly0 (c@'~' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly0 rest = friendly rest
friendly "" = ""
-- conservative
friendly (c@' ' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'!' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@'"' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'#' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'$' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'%' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'&' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'\x27' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'(' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@')' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@'*' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@'/' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@':' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@';' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@'<' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'=' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@'>' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@'?' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'[' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@'\x5c' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@']' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'^' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'`' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'{' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c@'|' : rest) = "_" ++ name c ++ "_" ++ friendly rest
-- conservative
friendly (c@'}' : rest) = "_" ++ name c ++ "_" ++ friendly rest
friendly (c : rest) = c : friendly rest

-- import System.Exit (ExitCode (ExitSuccess, ExitFailure)) -- way at the top though, /not/ down here
-- This would return its native return, an exit code from the process you run:
-- rename :: FilePath -> IO ExitCode
-- rename file = rawSystem "mv" ["-i", file, friendly0 file]

-- This does an action and doesn't return anything,
-- because you're changing the internal (not in IO, but /in/ IO) value to the unit, i.e. ():
rename :: FilePath -> IO ()
rename file = fmap (const ()) (rawSystem "mv" ["-i", file, friendly0 file])

main :: IO ()
main = getArgs >>= mapM_ rename
