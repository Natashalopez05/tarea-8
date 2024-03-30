import qualified Data.Map.Strict as Map

-- Definición de tipos de datos para expresiones aritméticas
data Expr = Var String
          | Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

-- Definición de tipos de datos para comandos (asignación y expresión)
data Command = Assign String Expr
             | ExprCmd Expr
             deriving (Show)

-- Definición de un entorno que mapea nombres de variables a sus valores
type Env = Map.Map String Int

-- Evaluación de expresiones
eval :: Env -> Expr -> Int
eval _ (Val n) = n
eval env (Var x) = Map.findWithDefault (error $ "Variable no definida: " ++ x) x env
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2

-- Ejecución de comandos (asignación o expresión)
execute :: Env -> Command -> (Env, Maybe Int)
execute env (Assign var expr) = (Map.insert var (eval env expr) env, Nothing)
execute env (ExprCmd expr) = (env, Just (eval env expr))

-- Función principal del REPL
repl :: Env -> IO ()
repl env = do
    putStr "> "
    input <- getLine
    let (newEnv, result) = case parseCommand input of
                             Just cmd -> execute env cmd
                             Nothing -> (env, Just (error "Error de sintaxis"))
    case result of
      Just val -> putStrLn $ show val
      Nothing -> return ()
    repl newEnv

-- Analizador sintáctico de comandos
parseCommand :: String -> Maybe Command
parseCommand str =
    case words str of
      [var, "=", expr] -> Just $ Assign var (parseExpr expr)
      _ -> Just $ ExprCmd (parseExpr str)

-- Analizador sintáctico de expresiones aritméticas
parseExpr :: String -> Expr
parseExpr = parse . words
  where
    parse [x] = if all (`elem` ['0'..'9']) x
                then Val (read x)
                else Var x
    parse (x:"+":y:rest) = Add (parseExpr x) (parseExpr y)
    parse (x:"-":y:rest) = Sub (parseExpr x) (parseExpr y)
    parse (x:"*":y:rest) = Mul (parseExpr x) (parseExpr y)
    parse (x:"/":y:rest) = Div (parseExpr x) (parseExpr y)
    parse _ = error "Expresión inválida"

-- Función principal
main :: IO ()
main = repl Map.empty
