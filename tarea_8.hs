import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

-- Definición de tipos de datos para expresiones aritméticas
data Expr = Var String
          | Val Int
          | BinOp BinOperator Expr Expr
          deriving (Show)

data BinOperator = Add | Sub | Mul | Div deriving (Show)

-- Definición de tipos de datos para comandos (asignación y expresión)
data Command = Assign String Expr
             | ExprCmd Expr
             | Quit
             deriving (Show)

-- Definición de un entorno que mapea nombres de variables a sus valores
type Env = Map.Map String Int

-- Evaluación de expresiones
eval :: Env -> Expr -> Maybe Int
eval _ (Val n) = Just n
eval env (Var x) = Map.lookup x env
eval env (BinOp op e1 e2) = do
    v1 <- eval env e1
    v2 <- eval env e2
    case op of
        Add -> pure (v1 + v2)
        Sub -> pure (v1 - v2)
        Mul -> pure (v1 * v2)
        Div -> if v2 /= 0 then Just (v1 `div` v2) else Nothing

-- Ejecución de comandos (asignación, expresión o salir)
execute :: Env -> Command -> (Env, Maybe Int)
execute env (Assign var expr) = case eval env expr of
                                    Just val -> (Map.insert var val env, Nothing)
                                    Nothing -> (env, Just $ error "Error de evaluación")
execute env (ExprCmd expr) = (env, eval env expr)
execute _ Quit = (Map.empty, Nothing)

-- Función principal del REPL
repl :: Env -> IO ()
repl env = do
    putStr "> "
    input <- getLine
    let (newEnv, result) = case parseCommand input of
                                Just cmd -> execute env cmd
                                Nothing -> (env, Just $ error "Error de sintaxis")
    case result of
        Just val -> putStrLn $ show val
        Nothing -> return ()
    if result /= Just "quit" then repl newEnv else return ()

-- Analizador sintáctico de comandos
parseCommand :: String -> Maybe Command
parseCommand str
    | str == "quit" = Just Quit
    | length parts == 3 && parts !! 1 == "=" = do
        let var = parts !! 0
        expr <- parseExpr (unwords $ drop 2 parts)
        Just $ Assign var expr
    | otherwise = ExprCmd <$> parseExpr str
    where parts = words str

-- Analizador sintáctico de expresiones aritméticas
parseExpr :: String -> Maybe Expr
parseExpr str = case words str of
    [x] -> Var <$> Just x
    [x] -> Val <$> readMaybe x
    (x:op:y:rest) -> case op of
                        "+" -> BinOp Add <$> parseExpr x <*> parseExpr (unwords (y:rest))
                        "-" -> BinOp Sub <$> parseExpr x <*> parseExpr (unwords (y:rest))
                        "*" -> BinOp Mul <$> parseExpr x <*> parseExpr (unwords (y:rest))
                        "/" -> BinOp Div <$> parseExpr x <*> parseExpr (unwords (y:rest))
    _ -> Nothing

-- Función principal
main :: IO ()
main = repl Map.empty
