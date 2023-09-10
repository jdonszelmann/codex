import Debug.Trace
import Data.List (lookup, elemIndex)

-- The common values
data Val = B Bool | I Int | Unit
    deriving (Show)

-- The handle language
data Han = Handler String Han | Handle Han Han | ValH Val | VarH String | Resume Han
    deriving (Show)

-- The lambda calculus
data Lam = Lambda String Lam | App Lam Lam | ValL Val | VarL String
    deriving (Show)

translate :: Lam -> Han
translate x = Handle (translate' x) (VarH "$val")

translate' :: Lam -> Han
translate' (Lambda x e) = Handler "$val" $ Resume $ Handle (Handler x (Resume $ VarH "$arg")) (translate e)
translate' (App e1 e2) =
    Handle (translate e1) $
    Handle (Handler "$fun" (Resume $ VarH "$val")) $
    Handle (translate e2) $
    Handle (Handler "$arg" (Resume $ VarH "$val")) $
        VarH "$fun"
translate' (ValL v) = Handler "$val" (Resume $ ValH v)
translate' (VarL x) = VarH x

type CtxL = [(String, Lam)]

evalLam :: CtxL -> Lam -> Lam
evalLam ctx (App e1 e2) = case evalLam ctx e1 of
    Lambda x e -> evalLam ((x, evalLam ctx e2):ctx) e
    _ -> error "Applied non-lambda"
evalLam ctx (VarL x) = case lookup x ctx of
    Just v -> v
    Nothing -> error $ "variable not bound: " ++ x
evalLam ctx x = x

evalL = evalLam []

type CtxH = [Either (String, Han) Han]

evalHan :: CtxH -> CtxH -> Han -> Han
evalHan ctx k e = traceShow e (evalHan' ctx k e) 

-- The context represent the "handle" constructs that we are in
-- If Left then we have a binding
-- If Right then we have an expression still to evaluate when we get to it
-- The head is the innermost binding
-- This means that any prefix is a valid continuation, which we also keep track of as k

evalHan' ctx k (Handle e1 e2) = evalHan (Right e2:ctx) k e1
evalHan' ctx k (VarH x) = case getHandler x [] ctx of
    Just (ctx', k', e) -> evalHan ctx' k' e
    Nothing -> error $ "variable not bound: " ++ x
evalHan' ctx k (Resume x) = evalHan (k ++ ctx) [] (evalHan ctx k x)
evalHan' (Right _:ctx) k (ValH v) = error "Needed a handler got a value"
evalHan' (Right e2:ctx) k (Handler x e) = evalHan (Left (x, e):ctx) k e2
evalHan' (Left _:ctx) k x = evalHan ctx k x
evalHan' [] _ x = x

-- decompose (Handle e1 e2) = (Right e2:ctx, e)
--     where (ctx, e) = decompose e1
-- decompose x = ([], x)

-- reduce :: (CtxH, CtxH, Han) -> (CtxH, CtxH, Han)
-- reduce (Right e2:ctx, k, ValH v) = error "Needed a handler got a value"
-- reduce (Right e2:ctx, k, Handler x e) = (Left (x, e):ctx, k, e)
-- reduce (ctx, k, VarH x) = case getHandler x [] ctx of
--     Just (ctx', k', e) -> (ctx', k', e)
--     Nothing -> error $ "variable not bound: " ++ x
-- reduce (ctx, k, Resume e) = (k ++ ctx, e)
-- reduce x = error $ "Stuck at: " ++ show x

evalH = evalHan [] []

getHandler :: String -> CtxH -> CtxH -> Maybe (CtxH, CtxH, Han)
getHandler _ _ [] = Nothing
getHandler x pre post@(Left (x', e):_) | x == x' = Just (post, pre, e)
getHandler x pre (curr:post) = getHandler x (pre ++ [curr]) post  

example = App (Lambda "x" (VarL "x")) (ValL $ I 0)

ex2 = Handle (Handler "x" (Resume $ ValH $ I 0)) (VarH "x")

evalBoth :: Lam -> (Lam, Han)
evalBoth x = (evalL x, evalH $ translate x)

showL :: Lam -> String
showL (Lambda x e) = "(λ" ++ x ++ "." ++ showL e ++ ")"
showL (App e1 e2) = showL e1 ++ " " ++ showL e2
showL (VarL x) = x
showL (ValL v) = showV v

showV :: Val -> String
showV (B b) = show b
showV (I i) = show i
showV Unit = "()"

showH :: Han -> String
showH (Handler x e) = "{" ++ x ++ ": " ++ showH e ++ "}"
showH (Handle e1 e2) = "(" ++ showH e1 ++ " " ++ showH e2 ++ ")"
showH (VarH x) = x
showH (Resume e) = "resume " ++ showH e
showH (ValH v) = showV v