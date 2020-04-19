{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Lecture07.Money
  ( Money
  -- ^ здесь мы экспортируем только тип без конструктора Money и eliminator `getMoney`
  , getMoney
  , USD
  , RUB
  , mkRubbles
  , mkDollars
  -- ^ вместо конструктора Money экспортируем смарт-конструкторы
  ) where

{-
  Здесь мы объявляем типы метки для представления видов валют.
  У них нет конструкторов, а значит что они не представимы во время
  выполнения программ и существуют только на этапе проверки типов.
-}
data USD
data RUB

{-
  Теперь мы определяем тип Money с параметром типа `a`.

  Особенность типа Money заключается в том, что
  мы не используем параметр `a` в определении типа.

  То есть этот параметр не используется во время выполнения программы
  и существует только во время компиляции. Зачем это нужно?

  Это позволяет нам создавать типы для разных валют и избегать операции между ними.
-}
newtype Money a = Money { getMoney :: Integer } deriving (Eq, Show)

mkDollars :: Integer -> Money USD
mkDollars = Money

mkRubbles :: Integer -> Money RUB
mkRubbles = Money

{-
  Разные конструкторы типов позволяют нам строить значения конкретных типов.
-}
russianDebt :: Money RUB
russianDebt = mkRubbles 16200000000000 -- 16.2 trillion

americanDebt :: Money USD
americanDebt = mkDollars 22700000000000 -- 22.7 trillion

{-
  Если мы реализуем instance Semigroup для `Money a`, то сможем складывать
  деньги без валюты:
-}
badBehaviour :: Money a
badBehaviour =
  let
    (<>) = undefined -- нужно только для примера
    rubbles = Money 34
    dollars = Money 23
  in rubbles <> dollars

{-
  `badBehaviour` имеет тип `Money a`, который можно параметризовать как угодно.

  Поэтому мы и не экспортируем конструктор `Money`, чтобы не создавать значения типа `Money a`, а
  экспортируем конкретные конструкторы, что позволяет нам избеждать такого поведения.
-}