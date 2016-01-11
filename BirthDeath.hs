-- BirthDeath.hs
-- birth and death processes

import Control.Monad.State
import Data.Random
import Data.RVar
import Data.Random.Source.IO
import Data.Random.Distribution.Exponential

-- (state, time)
type BDProcess = (Int, Double)

-- time-dependent
-- takes in population size, returns rate
type Rate = Int-> Double

-- simulate birth and death processes up to time e by using its exponential
-- interarrival rate to determine the number of arrivals/deaths
runBDProcess :: Rate -> Rate -> Double -> StateT BDProcess IO ()
runBDProcess birth death e = do
  (n, t) <- get
  let birthRate = birth n
  let deathRate = death n
  let rate = 1.0 / (birthRate + deathRate)

  -- sample to get next arrival time
  v <- lift $ sampleRVar (exponential rate)
  let t' = t + v
  
  -- next arival is past the simulation period; do nothing
  -- or, alternatively, the rate is 0, which means an arrival is impossible
  if t' > e-0.0001 || rate == 0.0
  then return ()

  -- arrival is within simulation period; add a birth or death
  else do
    p <- lift $ sampleRVar (stdUniform :: RVar Double)
    if p <= birthRate / (birthRate + deathRate)
    -- birth has arrived!
    then do
      put (n+1, t')
      runBDProcess birth death e
    -- death has arrived!
    else do
      put (n-1, t')
      runBDProcess birth death e
    
main = do
  (n, t) <- execStateT (runBDProcess (const 1.0) (const 0.0) 10.0) (0,0)
  putStrLn "Poisson process with lambda = 1.0"
  putStrLn $ "x = " ++ show n

  (n, t) <- execStateT (runBDProcess (\x -> (fromIntegral x :: Double) * 1.0) (const 0.0) 10.0) (1,0)
  putStrLn "Yule process with lambda = 1.0"
  putStrLn $ "x = " ++ show n
