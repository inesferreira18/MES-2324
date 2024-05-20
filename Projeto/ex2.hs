{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.Generics.Zipper
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import Data.Maybe (Maybe(Nothing))

instance StrategicData Sequence
instance StrategicData Action
instance StrategicData a => StrategicData [a]


data Sequence = Sequence [Action]
        deriving (Show, Data)


data Action = Ops Action Action
            | Move (Int, Int)
            | Store Int
            | Pickup Int
            | Wait Int
            | Nil
            deriving (Show, Data)




-- Main function Innermost
optIM :: Sequence -> Sequence
optIM program = 
        let pProgram = toZipper program
            (Just newProgram) = applyTP (innermost step) pProgram
            step = failTP `adhocTP` optActions
        in fromZipper newProgram

optActions :: Action -> Maybe Action
optActions (Ops (Move (x1, y1)) (Move (x2,y2))) = Just (Move (x1+x2, y1+y2))
optActions (Ops (Store x) (Store y)) = Just (Store (x+y))
optActions (Ops (Pickup x) (Pickup y)) = Just (Pickup (x+y)) 
optActions (Ops act (Wait _)) = Just act
optActions (Ops (Wait _) act) = Just act
optActions (Ops act Nil) = Just act
optActions (Ops Nil act) = Just act
optActions (Ops (Store x) (Pickup y)) = if x > y then Just (Store (x-y)) else Just (Pickup (y-x))
optActions (Ops (Pickup x) (Store y)) = if x > y then Just (Pickup (x-y)) else Just (Store (y-x))  
optActions _ = Nothing


-- sequence = Sequence[Ops (Move(1,1)) (Ops (Move(3,2)) (Ops (Store 5) (Ops (Wait 10) (Ops (Pickup 4) Nil))))]