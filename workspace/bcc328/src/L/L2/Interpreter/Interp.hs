module L.L2.Interpreter.Interp where 

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map 

import L.L2.Frontend.Syntax

import Utils.Pretty
import Utils.Value 
import Utils.Var 

-- Dual environment: phi for immutable variables, sigma for mutable variables
type ImmutableEnv = Map Var Value  -- φ (phi)
type MutableEnv = Map Var Value    -- σ (sigma)
type DualEnv = (ImmutableEnv, MutableEnv)

evalL2 :: L2 -> IO (Either String DualEnv) 
evalL2 (L2 ss)
  = foldM step (Right (Map.empty, Map.empty)) ss 
  where 
    step ac@(Left _) _ = pure ac 
    step (Right env) s2 = evalS2 env s2 
      

evalS2 :: DualEnv -> S2 -> IO (Either String DualEnv) 
evalS2 env@(phi, sigma) (LRead s v)
  = do 
      putStr s 
      val <- readValue
      pure (Right (phi, Map.insert v val sigma))

evalS2 env@(phi, sigma) (LPrint e)
  = case evalE2 env e of 
      Left err -> pure $ Left err 
      Right val -> do 
        putStrLn (pretty val)
        pure (Right env) 

evalS2 env@(phi, sigma) (LAssign v e)
  = case evalE2 env e of 
      Left err -> pure $ Left err 
      Right val -> pure (Right (phi, Map.insert v val sigma))

evalS2 env@(phi, sigma) (Def v e ss)
  = case evalE2 env e of 
      Left err -> pure $ Left err 
      Right val -> do 
        let phi' = Map.insert v val phi
            newEnv = (phi', sigma)
        result <- foldM step (Right newEnv) ss
        case result of 
          Left err -> pure $ Left err 
          Right (_, sigma') -> pure (Right (phi, sigma'))  -- Restore original phi
  where 
    step ac@(Left _) _ = pure ac 
    step (Right env) s = evalS2 env s


readValue :: IO Value 
readValue = (VInt . read) <$> getLine 

evalE2 :: DualEnv -> E2 -> Either String Value 
evalE2 _ (LVal v) = Right v 

evalE2 (phi, sigma) (LVar v) 
  = case Map.lookup v phi of 
      Just val -> Right val  -- Immutable variable found
      Nothing -> case Map.lookup v sigma of 
        Just val -> Right val  -- Mutable variable found
        Nothing -> Left ("Undefined variable: " ++ pretty v)

evalE2 env (LAdd l1 l2) 
  = do 
      v1 <- evalE2 env l1
      v2 <- evalE2 env l2 
      v1 .+. v2

evalE2 env (LMinus l1 l2) 
  = do 
      v1 <- evalE2 env l1
      v2 <- evalE2 env l2 
      v1 .-. v2

evalE2 env (LMul l1 l2) 
  = do 
      v1 <- evalE2 env l1
      v2 <- evalE2 env l2
      v1 .*. v2

evalE2 env (LDiv l1 l2) 
  = do 
      v1 <- evalE2 env l1
      v2 <- evalE2 env l2
      v1 ./. v2
