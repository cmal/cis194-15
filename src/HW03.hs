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
-- >>> (extend empty "A" 5) "A" == 5
-- True
-- >>> (extend (extend empty "A" 5) "A" 6) "A" == 6
-- True
extend :: State -> String -> Int -> State
extend st0 k v = st1
  where
    st1 k1 = if k1 == k then v else st0 k1

-- >>> empty "A"
-- 0
empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------
-- >>> evalE empty (Var "A") == 0
-- True
-- >>> evalE empty (Op (Val 1) Eql (Val 2)) == 0
-- True
-- >>> evalE (extend empty "A" 5) (Var "A") == 5
-- True
-- >>> evalE (extend empty "A" 5) (Op (Var "A") Plus (Val 1)) == 6
-- True

evalE :: State -> Expression -> Int
evalE st0 exp = case exp of
   Var s -> st0 s
   Val i -> i
   Op exp1 bop exp2 -> 
      let v1 = evalE st0 exp1
          v2 = evalE st0 exp2
      in case bop of 
            Plus -> v1 + v2
            Minus -> v1 - v2
            Times -> v1 * v2
            Divide -> div v1 v2
            Gt -> if v1 > v2 then 1 else 0
            Ge -> if v1 >= v2 then 1 else 0
            Lt -> if v1 < v2 then 1 else 0
            Le -> if v1 <= v2 then 1 else 0
            Eql -> if v1 == v2 then 1 else 0
                          
   
-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)
-- >>> desugar (Incr "A") == DAssign "A" (Op (Var "A") Plus (Val 1))
-- DAssign "A" (Op (Var "A") Plus (Val 1))
-- >>> desugar (For (Assign "A" (Val 0)) (Op (Var "A") Lt (Var "N")) (Incr "A") (Incr "A"))
-- DSequence (DAssign "A" (Val 0)) (DWhile (Op (Var "A") Lt (Var "N")) (DSequence (DAssign "A" (Op (Var "A") Plus (Val 1))) (DAssign "A" (Op (Var "A") Plus (Val 1)))))
desugar :: Statement -> DietStatement
desugar st = case st of 
  Assign s ex -> DAssign s ex
  Incr s -> DAssign s (Op (Var s) Plus (Val 1))
  If ex state state' -> DIf ex (desugar state) (desugar state')
  While ex state -> DWhile ex (desugar state)
  For state ex state' state2 -> DSequence (desugar state) $ DWhile ex $ DSequence (desugar state2) (desugar state')
  Sequence state state' -> DSequence (desugar state) (desugar state')
  Skip -> DSkip

-- Exercise 4 -----------------------------------------
-- >>> let s = evalSimple empty (DAssign "A" (Val 10)) in s "A" == 10
-- True
-- >>> let s = evalSimple empty (DIf (Val 0) (DAssign "A" (Val 1)) (DAssign "A" (Val 2))) in s "A" == 2
-- True
-- >>> desugar (Incr "A")
-- DAssign "A" (Op (Var "A") Plus (Val 1))

-- >>> evalSimple (extend empty "A" 5) (desugar (Incr "A")) "A"
-- 6

-- >>> run (extend empty "A" 5) (Incr "A") "A" == 6
-- False
evalSimple :: State -> DietStatement -> State
evalSimple st0 ds = case ds of 
  DAssign s ex -> extend st0 s $ evalE st0 ex
  DIf ex ds' ds2 -> if evalE st0 ex == 0 then evalSimple st0 ds2 else evalSimple st0 ds'
  DWhile ex ds' -> if evalE st0 ex == 0 then st0 else evalSimple (evalSimple st0 ds') (DWhile ex ds')
  DSequence ds' ds2 -> evalSimple (evalSimple st0 ds') ds2
  DSkip -> st0

-- >>> let s = run empty (If (Val 0) (Assign "A" (Val 1)) (Assign "A" (Val 2))) in s "A" == 2
-- True
-- 
-- >>> let s = run (extend empty "In" 5) factorial in s "Out"
-- 120
-- >>> let s = run (extend empty "A" 15) squareRoot in s "B"
-- 3
-- >>> let s = run (extend empty "In" 11) fibonacci in s "Out"
-- 144
run :: State -> Statement -> State
run st0 stmt = evalSimple st0 (desugar stmt)


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
