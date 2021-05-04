module Infer where

import Common

import Control.Monad.Writer (listen)
import Control.Monad.Except (throwError)
import Data.Map.Strict as H (Map, insert, lookup, empty, fromList, singleton)

  {- question 1: fresh instance function -}

freshInst :: PolyTy -> Infer MonoTy
freshInst (Forall [] tau)           = return tau
freshInst (Forall _ (TyVar i))      = return (TyVar i)
freshInst (Forall (x:xs) tau) = 
  do t <- freshTau
     let newtau = apply (substInit x t) tau
     freshInst (Forall xs newtau)


  {- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = case tau of
    TyVar x           -> i == x
    TyConst _ []      -> False
    TyConst s (x:xs)  -> occurs i x || occurs i (TyConst s xs)

  {- question 3: unification -}

unify :: [Constraint] -> Infer Substitution
unify [] = return H.empty
unify constraintList = unify' constraintList where
-- Delete rule: If s and t are are equal, discard the pair, and unify φ
  unify' ((s:~:t):phi) 
    | s == t      = unify phi
  unify' ((s:~:t):phi) = case s:~:t of
    -- Orient rule: If t is a variable, and s is not, then discard (s,t), and unify {t∼s} ∪ φ′.
    s@(TyConst _ _):~:t@(TyVar _)   -> unify ((t:~:s):phi)
    -- Decompose rule
    s@(TyConst c1 ss):~:t@(TyConst c2 ts)
      | c1 == c2    -> unify (phi ++ st) -- st ++ phi ????
      | otherwise   -> throwError $ Can'tMatch (TyConst c1 ss) (TyConst c2 ts)
                          where st = zipWith (:~:) ss ts
    -- Eliminate rule                      
    s@(TyVar s'):~:t
      | not $ occurs s' t  -> do e <- unify (map (\(s'':~:t'') -> apply (substInit s' t) s'' :~: apply (substInit s' t) t'') phi)
                                 return $ H.insert s' (apply e t) e                      
      | otherwise -> throwError $ InfiniteType s' t

  {- question 4: type inference -}
funcReturn :: MonoTy -> MonoTy
funcReturn (TyConst "->" list) = funcReturn $ last list
funcReturn m = m

infer :: TypeEnv -> Exp -> Infer MonoTy
-- constants (problem 1)
infer env (ConstExp c) = freshInst sig where sig = constTySig c
-- vars (problem 2)
infer env (VarExp x) = case H.lookup x env of
                            Nothing -> throwError (LookupError x)
                            Just x' -> freshInst x'
-- let bindings (problem 3)
-- TODO: WHAT?? HOW DOES THIS WORK I COPY PASTED IT LMAO
infer env (LetExp x e1 e2) = do   (tau1, constraints) <- listen $ infer env e1
                                  substitution <- unify constraints
                                  let env' = H.insert x (gen env (apply substitution tau1)) env
                                  infer env' e2

-- operator
-- infer env (MonOpExp op e1) = do   freshin <- freshInst $ monopTySig op
--                                   tau <- freshTau
--                                   tau1 <- infer env e1
--                                   constrain freshin $ funTy tau1 tau
--                                   return tau
infer env (MonOpExp op e1) = do   t1 <- freshInst $ monopTySig op
                                  t2 <- freshTau
                                  constrain t1 t2
                                  return $ funcReturn t1

infer env (BinOpExp op e1 e2) = do  freshin <- freshInst $ binopTySig op
                                    tau <- freshTau
                                    tau1 <- infer env e1
                                    tau2 <- infer env e2
                                    constrain freshin $ binopFunTy tau1 tau2 tau
                                    return tau

infer env (IfExp e1 e2 e3) = do (tau1, cons1) <- listen $ infer env e1
                                (tau2, cons2) <- listen $ infer env e2
                                (tau3, cons3) <- listen $ infer env e3
                                subs <- unify (cons1 ++ cons2)
                                constrain tau1 boolTy 
                                constrain tau2 tau3
                                return tau3
                          
infer env (FunExp x e) = do   tau1 <- freshTau --infer env e
                              let env' = H.insert x ( polyTyOfMonoTy tau1) env
                              tau2 <- infer env' e
                              return $ funTy tau1 tau2

infer env (AppExp f e ) = do  (tau1, cons1) <- listen $ infer env f
                              (tau2, cons2) <- listen $ infer env e
                              let tau1' = funcReturn tau1
                              subs <- unify (cons1 ++ cons2)
                              constrain tau1' tau2    -- tau2 tau1' or the other way ?? idk
                              return tau1'

infer env (LetRecExp f x e1 e) = do ftau <- freshTau 
                                    xtau <- freshTau
                                    let env' = H.insert f ( polyTyOfMonoTy ftau) env
                                    let env'' = H.insert x ( polyTyOfMonoTy xtau) env'
                                    (tau3, cons) <- listen $ infer env'' e1
                                    substitution <- unify cons
                                    let env''' = H.insert x (gen env (apply substitution tau3)) env''
                                    infer env''' e
                                    



inferInit :: TypeEnv -> Exp -> Infer PolyTy
inferInit env e = do
  (tau, constraints) <- listen $ infer env e
  substitution <- unify constraints
  return $ quantifyMonoTy $ apply substitution tau

inferDec :: TypeEnv -> Dec -> Infer (TypeEnv, PolyTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x tau env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f tau env, tau)