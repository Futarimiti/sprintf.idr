module Sprintf

%access export
%default total

data Printf = PPercent
            | PInteger
            | PDouble
            | PChar
            | PReg Char

-- NOTE: other specifiers such as %s are considered
-- normal characters and will be treated as-is
toPrintf : List Char -> List Printf
toPrintf [] = []
toPrintf ('%' :: 'd' :: xs) = PInteger :: toPrintf xs
toPrintf ('%' :: 'f' :: xs) = PDouble :: toPrintf xs
toPrintf ('%' :: 'c' :: xs) = PChar :: toPrintf xs
toPrintf ('%' :: '%' :: xs) = PPercent :: toPrintf xs
toPrintf (x :: xs) = PReg x :: toPrintf xs

-- deduce type of args passed to printf
corresArgs : List Printf -> List Type
corresArgs [] = []
corresArgs (PPercent :: xs) = corresArgs xs
corresArgs (PInteger :: xs) = Int :: corresArgs xs
corresArgs (PDouble :: xs) = Double :: corresArgs xs
corresArgs (PChar :: xs) = Char :: corresArgs xs
corresArgs (PReg x :: xs) = corresArgs xs

-- convert a list of types to printf function type
-- example: [Int, Double, Char] -> (Int -> Double -> Char -> List Char)
--          [] -> (List Char)  -- no args
typeListToFunc : Type -> List Type -> Type
typeListToFunc ending lst = case lst of
                                 [] => ending
                                 (x :: xs) => x -> typeListToFunc ending xs

fmt : (acc: String)
   -> (p: List Printf)
   -> typeListToFunc String (corresArgs p)
fmt acc [] = acc
fmt acc (PPercent :: ps) = fmt (acc ++ "%") ps
fmt acc (PInteger :: ps) = \i => fmt (acc ++ show i) ps
fmt acc (PDouble :: ps) = \d => fmt (acc ++ show d) ps
fmt acc (PChar :: ps) = \c => fmt (acc ++ pack [c]) ps
fmt acc (PReg x :: ps) = fmt (acc ++ pack [x]) ps

sprintf : (s: String)
       -> typeListToFunc String (corresArgs (toPrintf (unpack s)))
sprintf s = fmt "" (toPrintf (unpack s))

