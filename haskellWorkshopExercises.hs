--exercise 1
type Column = (String, Integer)
type Row = [Column]
type Table = [Row]
type Value = Integer



--exercise 2

c1 :: Column --another possibility
c1 = ("x",64)

t1 :: Table
t1 = [[("x",2),("y",3)],[("x",5),("y",88)],[("x",444),("y",2)],[("x",5),("y",5)]]

--exercise 3
-- functions come from the DB.Utils
main = putStrLn (checkIfValid t1)
--can use verifyAndPPTable
checkIfValid t1 = 
  if (checkTable t1)
    then ppTable t1
    else "Inconsistent data"
	
--exercise 4
type Database = [(String, Table)]

db1 :: Database
db1 = [("table1", t1), ("tableWHAT", t1)]

myMap f list = if (null list)
  then []
  else (f (head list)) : (myMap f (tail list))

ppTableNames dbs = unwords (myMap getName)

--exercise 5

getTable tableMaybe = if (isJus tableMaybe)
  then ppTable (fromJust tableMaybe)
  else "Table was not found"

prompt :: IO ()
prompt = do
  putStrLn (ppTableNames db1)
  tableName <- getLine
  let tableMaybe = lookup tableName db1
  putStrLn (getTable tableMaybe)
  
-- could also use
--let res = lookup input myDB
--if isJust res
--  then putStrLn (verifyAndPPTable (fromJust res))
--  else putStrLn "Table not found"

--exercise 6

--exercise 7

--exercise 8
data MathExpr
  = Number Int
  | Plus
      MathExpr
      MathExpr
  | Mult
      MathExpr
	  MathExpr
-- (3 + 7) * (-1) + (-9)
expr :: Expr
expr = 
  Mult (Plus (Number 3) (Number 7)) (Plus (Number (-1)) (Number (-9)))
  
--exercise 9
evaluate expr =
  case expr of
    Plus expression expression -> evaluate(expr) + evaluate(expr)
	Mult expression expression -> evaluate(expr) * evaluate(expr)
	Number num -> num