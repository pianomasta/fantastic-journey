import Data.List
 (intersperse)
import System.Environment
 (getArgs)
import System.Cmd
 (system)
import System.Exit
 (ExitCode (ExitSuccess, ExitFailure), exitWith)

defaultDelay :: Int
defaultDelay = 3

run :: String -> IO ExitCode
run chars =
  do
  system ("echo " ++ chars ++ " | MORSE_PLAY=no morse")
  noise
  system ("ffplay -loglevel 8 -nodisp -autoexit /tmp/out.tmp.flac > /dev/null")
  noise

generateNoise, noise :: IO ExitCode
generateNoise = system "ffmpeg -y -loglevel 8 -f s32le -i /dev/urandom -af volume=0.1 -t 0.15 -compression_level 12 /tmp/noise.tmp.flac"
noise = system "ffplay -loglevel 8 -nodisp -autoexit /tmp/noise.tmp.flac > /dev/null"

practice :: String -> IO ()
practice chars = practiceWithDelay defaultDelay chars

practiceWithDelay :: Int -> String -> IO ()
practiceWithDelay delay chars =
  do
  sleep delay
  sequence_ (
    intersperse (sleep delay) (
      map (run . (: [])) chars
      )
    )

sleep :: Int -> IO ExitCode
sleep delay = system ("sleep " ++ show delay)

review :: String -> IO ExitCode
review chars = system ("echo -n " ++ chars ++ " | spell")

defaultMagnitude :: Int
defaultMagnitude = 20

practiceSet :: String -> Integer -> IO ExitCode
practiceSet set seed = practiceSetWithMagnitude defaultMagnitude set seed

practiceSetWithMagnitude :: Int -> String -> Integer -> IO ExitCode
practiceSetWithMagnitude magnitude set seed =
  do
  -- DEBUG
  -- print seeds
  -- DEBUG
  -- print chars
  practice chars
  sequence_ [
    sleep defaultDelay,
    system "echo \"Time for review.\" | flite",
    sleep defaultDelay,
    system "echo \"What you should've gotten is:\" | flite"
    ]
  review chars
  where
  seeds = (take magnitude . tail) (iterate randomize seed)
  chars = map ((!!) set . fromInteger . flip mod (toInteger maximum)) seeds
  maximum = length set

type Seed = Integer
randomize :: Seed -> Seed
randomize seed = mod (a * seed + c) m
  where
  m, a, c :: Integer
  m = 2 ^ 64
  a = 6364136223846793005
  c = 1
rolld60 :: Seed -> (Seed, Int)
rolld60 seed =
  let newseed = randomize seed in
  (newseed, fromEnum (mod newseed 60 + 1))

main :: IO ()
main =
  do
  args <- getArgs
  case args of
    [] -> putStr (usageString ++ "\n")
    [set, seed] ->
      do
      generateNoise
      -- DEBUG
      -- print seed
      practiceSet set (read seed)
      exitWith ExitSuccess
    _ ->
      do
      putStr ("error: Unrecognized usage.\n" ++ usageString ++ "\n")
      exitWith (ExitFailure 1)
  where
  usageString = "usage: morsereview [letters] [seed]"
