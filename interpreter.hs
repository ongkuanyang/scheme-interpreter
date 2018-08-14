import Control.Applicative
import Control.Monad (liftM, ap)

data Exp = Num Int | Var Name | Quote String | Assign Name Exp | Define Name Exp |
           If Exp Exp Exp | Lambda [Name] Exp | Begin [Exp] | Cond [CondP] |
           App Exp [Exp] | Prim ([Value] -> Value)

data Value = Fun (EvalM [Value] -> EvalM Value) | PrimV ([Value] -> Value) |
             NumV Int | QuoteV String | BoolV Bool

data CondP = Exp Exp

type Name = String

type EvalM a = EnvT Env (ErrorT Id) a

data Env = Env [(Name, EvalM Value)]

class MonadT t where
    lift :: (Monad m) => m a -> t m a

eval :: Exp -> EvalM Value

eval (Num a)  = return (NumV a)

eval (Var name) = readEnv >>= \env ->
                                case lookupEnv name env of
                                  Just val -> val
                                  Nothing -> err ("Unbound variable: " ++ name)

eval (Quote a) = return (QuoteV a)

eval (Assign name exp) = readEnv >>= \env ->
                                       inEnv (extendEnv ([name], [inEnv env (eval exp)]) env)
                                           (return (QuoteV "ok"))

eval (Define name exp) = readEnv >>= \env ->
                                       inEnv (let newenv = (extendEnv ([name], [inEnv newenv (eval exp)]) env) in newenv)
                                           (return (QuoteV "ok"))

eval (If pred consq alt) = eval pred >>= \p -> if true p then eval consq else eval alt

eval (Lambda vars exp) =
  readEnv >>= \env ->
                return (Fun (\args -> args >>= \vs ->
                                                if length vs == length vars
                                                then
                                                inEnv (extendEnv (vars, map return vs)
                                                                 env)
                                                      (eval exp)
                                                else err "Incorrect no. of arguments supplied."))

eval (Begin (exp:[])) = eval exp
eval (Begin (exp:exps)) = eval exp >> eval (Begin exps)

--eval s@(Cond _) = eval (condtoif s)

eval (App op operands) =
  readEnv >>= \env ->
                eval op >>= \f ->
                              case f of
                                Fun func -> func $ mapM (inEnv env . eval) operands
                                PrimV func -> (mapM (inEnv env . eval) operands)
                                             >>= \vals -> (return . func) vals

eval (Prim x) = return (PrimV x)

eval _ = err "Invalid expression!"

true (BoolV False) = False
true _ = True

-- Identity Monad
newtype Id a = Id { runIdentity :: a }

instance Monad Id where
  return x = Id x
  m >>= k = k (runIdentity m)

instance Functor Id where
  fmap = liftM

instance Applicative Id where
  pure = return
  (<*>) = ap

-- Environment Monad Trasformer

data EnvT env m a = EnvM (env -> (m (env, a)))

instance Monad m => Monad (EnvT env m) where
  return x = EnvM (\s -> return (s, x))
  EnvM m >>= k = EnvM (\s0 -> m s0 >>= \(s1, a) -> let EnvM x = k a in x s1)

instance MonadT (EnvT env) where
  lift m = EnvM (\s -> m >>= \x -> return (s, x))

inEnv :: (Monad m) => Env -> EnvT Env m a -> EnvT Env m a
readEnv :: (Monad m) => EnvT Env m Env

inEnv env (EnvM m) = EnvM (\_ -> m env)
readEnv  = EnvM (\env -> return (env, env))
  
lookupEnv :: Name -> Env -> Maybe (EvalM Value)
lookupEnv x (Env []) = Nothing
lookupEnv x (Env ((y, z):xs)) = if x == y then Just z else lookupEnv x (Env xs)

extendEnv :: ([Name], [EvalM Value]) -> Env -> Env
extendEnv ((x:xs), (y:ys)) (Env env) = Env ((x,y):env)
extendEnv ([], []) env = env

instance Monad m => Functor (EnvT env m) where
  fmap = liftM

instance Monad m => Applicative (EnvT env m) where
  pure = return
  (<*>) = ap

-- Error Monad Transformer

data Error a = Ok a | Error String

data ErrorT m a = ErrorM (m (Error a))

instance Monad m => Monad (ErrorT m) where
  return = ErrorM . return . Ok
  ErrorM m >>= k = ErrorM (m >>= \a ->
                              case a of
                                (Ok x) -> let ErrorM m = k x in m
                                (Error msg) -> return (Error msg))

instance MonadT ErrorT where
  lift m = ErrorM (m >>= \x -> return $ Ok x)

class Monad m => ErrMonad m where
  err :: String -> m a

instance Monad m => ErrMonad (ErrorT m) where 
  err  = ErrorM . return . Error

instance Monad m => Functor (ErrorT m) where
  fmap = liftM

instance Monad m => Applicative (ErrorT m) where
  pure = return
  (<*>) = ap  

-- Lifting of err

instance (ErrMonad m) => ErrMonad (EnvT env m) where
  err = lift . err

-- Test run eval
test :: Exp -> String
test x = let EnvM a = eval x
             ErrorM b = a (Env [])
             Id c = b
         in case c of
              Error x -> "Error : " ++ x
              Ok (env, val) -> showval val

--Show val

showval (Fun _) = "Function"
showval (PrimV _) = "Primitive function"
showval (NumV x) = show x
showval (QuoteV x) = x
showval (BoolV x) = show x

-- Primitive functions

add (NumV x : xs) = let NumV y = add xs in NumV (x + y)
add [] = NumV 0

multiply (NumV x : xs) = let NumV y = add xs in NumV (x * y)
multiply [] = NumV 1

difference (NumV x : xs) = let NumV y = difference xs in NumV (x - y)
difference [] = NumV 0

eq (NumV x : NumV y : _) = if x == y then BoolV True else BoolV False
