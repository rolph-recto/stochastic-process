-- RandomWalk.hs
-- simulate random walks
--
-- we use the list monad to model the "superposition" of
-- every possible path of the process. however, for `runWalk` to calculate
-- efficiently the probability of being at some state after n timesteps,
-- we must destroy path information by marginalizing the probability of being
-- in some state at every step. this ensures that the time complexity of
-- simulation grows only with the number of states that the process can
-- possibly occupy
-- (i.e., states with nonzero probability after n timesteps)
--
-- this allows for an elegant implementation of transition probabilities,
-- which acts on an item on the distribution (possible state and probability
-- of being in that state) and returns the possible states to which process
-- has transitioned

import Control.Monad
import qualified Data.Map.Strict as M
import Data.List (foldl')

-- probability of being in a state
type StateProb = (Int, Double)
-- the probabilities in the list should add up to 1!
type Distribution = [StateProb]

-- transition rule
type TransitionRule = StateProb -> Distribution

distSum :: Distribution -> Double
distSum dist = foldr (+) 0.0 $ map snd dist

-- take a list of stateprobs [(x,px1),(y,py1),(x,px2),...]
-- and return [(x,px1+px2),(y,py1),...]
-- each (x,px) entry states the probability of going through a path
-- that ends in x; so we marginalize the probability of ending up
-- at x at the current timestep by adding the probability of all
-- the paths that end at x
marginalize :: Distribution -> Distribution
marginalize dist = M.toList $ foldr addPath M.empty dist
  where sumProb (x,p) acc = maybe p (+p) (M.lookup x acc)
        addPath (x,p) acc = M.insert x (sumProb (x,p) acc) acc

-- simple random walk
srw :: TransitionRule
srw (x,p) = [(x-1,p*0.5), (x+1,p*0.5)]

-- simple random walk with absorbing boundaries
absorb :: Int -> Int -> TransitionRule
absorb lo hi (x,p)
  | x == lo   = [(x,p)]
  | x == hi   = [(x,p)]
  | otherwise = [(x-1,p*0.5), (x+1,p*0.5)]

-- simple random walk with reflecting boundaries
reflect :: Int -> Int -> TransitionRule
reflect lo hi (x,p)
  | x == lo   = [(x+1,p)]
  | x == hi   = [(x-1,p)]
  | otherwise = [(x-1,p*0.5), (x+1,p*0.5)]

-- simulate a random walk using some rule for n timesteps
-- use foldl' here to force evaluation and marginalize at every time step
-- if we don't do this, there will a lot of redundant entries
-- in the list, making subsequent timesteps slow to compute
runWalk :: Int -> TransitionRule -> Distribution -> Distribution
runWalk n step init = foldl' apm init (replicate n step)
  where apm acc f = marginalize (acc >>= f)

-- uses Kleisi composition to compactly represent repeated bindings
-- this is equivalent to
-- x >>= step >>= step >>= ... >>= step
-- where step is binded n times
-- while clever, this is really inefficient because evaluation is not forced
-- until a huge composed function (stepN) has been calculated
runWalk' :: Int -> TransitionRule -> Distribution -> Distribution
runWalk' n step init = init >>= stepN
  where stepN = foldr (>=>) (return) (replicate n step)

main = do
  putStrLn "SRW to 10 timesteps"
  let dist = runWalk 10 srw [(0,1.0)]
  print dist

  putStrLn "SRW with absorbing boundaries to 1000 timesteps"
  let dist2 = runWalk 1000 (absorb (-5) 5) [(0,1.0)]
  print dist2

  putStrLn "SRW with reflecting boundaries to 1000 timesteps"
  let dist2 = runWalk 1000 (reflect (-5) 5) [(0,1.0)]
  print dist2
