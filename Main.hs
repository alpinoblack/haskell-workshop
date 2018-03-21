#!/usr/bin/env stack
-- stack runghc

--------------------------------

-- HOW TO RUN:

-- stack runghc Main.hs

--------------------------------


-- Declaration of the module name
module Main
-- List of declarations to export
where

-- import other modules
import DB.Utils

-- Module body - declarations


-- Replace `ex1` with the exercise you want to print

--main = putStrLn ex1
--main = print t1
--main = putStrLn (checkIfValid t1)
--main = print (myMap increment intList)
--main = putStrLn (ppTableNames db1)
main = print (evaluate expr)

---------------------
-- YOUR WORK BELOW --
---------------------

type Column = (String, Integer)
type Row = [Column]
type Table = [Row]
type Value = Integer

--exercise 2
t1 :: Table
t1 = [[("x",2),("y",3)],[("x",5),("y",88)],[("x",444),("y",2)],[("x",5),("y",5)]]

increment n = n + 1

checkIfValid t1 = if (checkTable t1)
  then ppTable t1
  else "Inconsistent data"

myMap f list = if (null list)
  then []
  else (f (head list)) : (myMap f (tail list))

intList :: [Integer]
intList = [1,2,3,4,5]

ppTableNames db = unwords (myMap getName db)

type Database = [(String, Table)]
db1 :: Database
db1 = [("table1", t1), ("tableWHAT", t1)]

ex1 = "Hello World!"

getTable tableMaybe = if (isJust tableMaybe)
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

data MathExpr
  = Number Int
  | Plus
      MathExpr
      MathExpr
  | Mult
      MathExpr
	  MathExpr

	  
expr :: MathExpr
expr = 
  Mult (Plus (Number 3) (Number 7)) (Plus (Number (-1)) (Number (-9)))
  
--exercise 9
evaluate expr =
  case expr of
    Plus expression1 expression2 -> (evaluate expression1) + (evaluate expression2)
    Mult expression1 expression2 -> (evaluate expression1) * (evaluate expression2)
    Number num -> num
