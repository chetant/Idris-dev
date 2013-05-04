module Data.Vect.Quantifiers

data Any : (P : a -> Type) -> Vect a n -> Type where
  Here  : {P : a -> Type} -> {xs : Vect a n} -> P x -> Any P (x :: xs)
  There : {P : a -> Type} -> {xs : Vect a n} -> Any P xs -> Any P (x :: xs)

anyNilAbsurd : {P : a -> Type} -> Any P Nil -> _|_
anyNilAbsurd Here impossible
anyNilAbsurd There impossible

anyElim : {xs : Vect a n} -> {P : a -> Type} -> (Any P xs -> b) -> (P x -> b) -> Any P (x :: xs) -> b
anyElim _ f (Here p) = f p
anyElim f _ (There p) = f p

any : {P : a -> Type} -> ((x : a) -> Dec (P x)) -> (xs : Vect a n) -> Dec (Any P xs)
any _ Nil = No anyNilAbsurd
any p (x::xs) with (p x)
  | Yes prf = Yes (Here prf)
  | No prf =
    case any p xs of
      Yes prf' => Yes (There prf')
      No prf' => No (anyElim prf' prf)

data All : (P : a -> Type) -> Vect a n -> Type where
  Nil : {P : a -> Type} -> All P Nil
  (::) : {P : a -> Type} -> {xs : Vect a n} -> P x -> All P xs -> All P (x :: xs)

negAnyAll : {P : a -> Type} -> {xs : Vect a n} -> Not (Any P xs) -> All (\x => Not (P x)) xs
negAnyAll {xs=Nil} _ = Nil
negAnyAll {xs=(x::xs)} f = (\x => f (Here x)) :: negAnyAll (\x => f (There x))

notAllHere : {P : a -> Type} -> {xs : Vect a n} -> Not (P x) -> All P (x :: xs) -> _|_
notAllHere _ Nil impossible
notAllHere np (p :: _) = np p

notAllThere : {P : a -> Type} -> {xs : Vect a n} -> Not (All P xs) -> All P (x :: xs) -> _|_
notAllThere _ Nil impossible
notAllThere np (_ :: ps) = np ps

all : {P : a -> Type} -> ((x : a) -> Dec (P x)) -> (xs : Vect a n) -> Dec (All P xs)
all _ Nil = Yes Nil
all d (x::xs) with (d x)
  | No prf = No (notAllHere prf)
  | Yes prf =
    case all d xs of
      Yes prf' => Yes (prf :: prf')
      No prf' => No (notAllThere prf')
