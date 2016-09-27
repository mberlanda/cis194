module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st str int = st'
  where st' y
          | y == str = int
          | otherwise = st y

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Val x) = x
evalE st (Var n) = st n
evalE st (Op exp1 op exp2) =
  case op of
    Plus -> a + b
    Minus -> a - b 
    Times -> a * b
    Divide -> a `div` b
    Gt -> if a > b then 1 else 0
    Ge -> if a >= b then 1 else 0   
    Lt -> if a < b then 1 else 0
    Le -> if a <= b then 1 else 0
    Eql -> if a == b then 1 else 0
  where a = evalE st exp1
        b = evalE st exp2


-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Skip) = DSkip
desugar (Assign st exp) = DAssign st exp
desugar (If exp st' st'') = DIf exp (desugar st') (desugar st'')
desugar (While exp st) = DWhile exp (desugar st)
desugar (Sequence st' st'') = DSequence (desugar st') (desugar st'')
desugar (Incr st) = DAssign st (Op (Var st) Plus (Val 1))
desugar (For init exp update loop) = DSequence (desugar init) (DWhile exp (DSequence (desugar loop) (desugar update)))

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign s exp) = extend state s (evalE state exp)
evalSimple state (DIf exp dst1 dst2)
  | val == 1 = evalSimple state dst1
  | otherwise = evalSimple state dst2
  where
    val = evalE state exp
evalSimple state while@(DWhile exp dst)
  | val == 1 = evalSimple (evalSimple state dst) while
  | otherwise = state
  where
    val = evalE state exp
evalSimple state (DSequence dst1 dst2) = evalSimple (evalSimple state dst1) dst2
evalSimple state DSkip = state

run :: State -> Statement -> State
run state st = evalSimple state (desugar st)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
