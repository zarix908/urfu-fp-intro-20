{-# LANGUAGE EmptyDataDeriving #-}

module Lecture04 where


{-
  04: Алгебраические типы данных (ADTs)

  1. ADTs и `data`
  2. Expression problem
  3. Records
  4. `newtype`
  5. Polymorphic types and `type`
  6. Product and sum types, cardinality
-}

{-
  Типы данных в языках программирования — это гибкий инструмент,
  который помогает решать сложные задачи.

  Типы позволяют строить абстракции, предоставляя разную семантику и уменьшая
  ментальную нагрузку на разработчика.

  Например, никто не мешает оперировать данными как байтами, но тогда
  разработчику нужно отслеживать по ходу всей программы, что именно эти
  байты представляют: числа, строки или что-то еще.

  Сегодня мы научимся определять свои типы данных в Haskell и как с ними
  эффективно работать.
-}

{-
  В объекто-ориентированных языках программирования новый тип объявляется
  с помощью классов. Например:

    class Animal {
      int age;
      string name;

      Animal Animal() { }
    }

  В Haskell для объявления нового типа используется ключевое слово `data`:
-}

data Animal = Animal Int String deriving (Eq, Show) 
{-                   ^--------^ параметры конструктора
              ^ конструктор типа
     ^ имя типа
   
  В Haskell необязательно давать имена параметрам типа в конструкторе.
  Типы с именованными полями называются records или записи.
  Познакомимся с ними чуть позже.

  Используя объявленнный тип и конструктор, мы можем создавать животных:

    let lion = Animal 13 "Leo"
    let cat = Animal 2 "Xavier"

  Важно понимать, что `Animal` это тоже функция:

    :t Animal
    Animal :: Int -> String -> Animal

  На самом деле, класс Animal, который мы объявили в самом начале не эквивалентен
  его аналогу на C#. В C# каждый класс наследует от object несколько методов, в том числе
  ToString() и Equals(). В Haskell нет наследования и общего предка, поэтому мы явно указываем,
  что наши объекты можно будет сравнивать и преобразовать в строку с помощью deriving (Eq, Show).
-}

-- Напишем функцию, которая принимает в качестве аргумента Animal
nameOfAnimal :: Animal -> String
-- Мы можем использовать pattern matching так же, как делали со списками
nameOfAnimal (Animal _ name) = name
--                   ^ игнорируем возраст во время паттерн матчинга по конструктору

{-
  Допустим, нам захотелось написать функцию, которая говорит, умеет ли животное мяукать.
  Сейчас мы не сможем по аргументу понять, что за животное внутри.
-}
canMeow :: Animal -> Bool
canMeow (Animal _ _) = error "Непонятно что за зверь"
-- возраст и имя ничего не говорят о виде

{-
  В ООП (C#, Java, Kotlin, whatever) языке, много способов сделать желаемое:
    1. Использовать наследование Cat : Animal, Lion : Animal.
       Определить `canMeow` в Animal и переопределить в наследниках.
    2. Использовать typeof и смотреть на тип объекта в рантайме.
       При этом Cat и Lion могут быть связаны или несвязаны иерархией наследования.
    3. Определить интерфейс IMeowable с методом `canMeow` 
    4. Определить Enum AnimalType и добавить его в Animal, а потом
       использовать switch/if.
    5. Использовать Church encoding.
-}

-- Вот так на Haskell выглядит способ 4. Это называется sum type:
data AnimalType = Lion | Cat | Dog | Duck deriving (Eq, Show)

data AnimalWithType = AnimalWithType Int String AnimalType deriving (Eq, Show)

canMeow' :: AnimalWithType -> Bool
canMeow' (AnimalWithType _ _ animalType) = case animalType of
  Lion -> False -- оказываются львы не мяукают, на ютубе таких видео не нашлось...
  Cat -> True
  Dog -> False
  Duck -> False -- удалить строчку и показать compile error при -Wall -Werror

-- А можем сделать решение, похожее на 1 вариант, это тоже sum type:
data Animal'
  = Lion' Int String
  | Cat' Int String
  | Dog' Int String
  | Duck' Int String
  deriving (Eq, Show)
-- Глядя на AnimalType можно было подумать, что это просто способ объявить enum из императивных языков.
-- В случае с Animal видно, что `data` более мощный инструумент и enum только его частный случай.

{-
  У нас есть один тип `Animal'` с четырьмя конструкторами.
  Мы вынуждены добавлять `'` в названия типа и конструкторов, потому что они
  уже объявлены раньше в AnimalType. Это так, потому что конструкторы типов
  работают как обычные функции, а функции не могут называться одинаково.

  Тогда canMeow будет выглядить так:
-}

canMeow'' :: Animal' -> Bool
canMeow'' (Lion' _ _) = False
canMeow'' (Cat' _ _) = True
canMeow'' (Dog' _ _) = False
canMeow'' (Duck' _ _) = False

{-
  Одно из важных свойств хорошо поддерживаемого кода -- расширяемость.
  Допустим у нас есть тип с набором функций и необходимо добавить к нему новую функцию `getFoodPrice`.
  Это очень легко сделать в функциональном языке и в Haskell в частности. Например:

    getFoodPrice :: Animal -> Int
    getFoodPrice = ...

  Мы ничего не меняли в старом коде.
  
  В ООП языке такой тип выглядел бы как абстрактный класс с несколькими наследниками.
  Значит, если мы хотим добавить новый метод в иерархию, мы вынуждены пойти
  в каждого наследника и написать реализацию этого метода для него.

  С другой стороны, если мы захотим добавить в `Animal` новый конструктор, то,
  например, в С# нам нужно будет просто добавить нового наследника, старый код трогать не нужно.
  А вот в Haskell нам придётся дописать каждую из уже написанных функций.

  Получается, что язык позволяет легко решать только одну из этих задач,
  решение второй нам нужно как-то придумать. Это проблема носит название Expression Problem.

  У этой проблемы есть множество разных решений. Например, для добавления
  новых функция в иерархию классов в ООП языках используют:
    - Паттерн Visitor
    - Multimethods
    - Open classes

  В функциональных языках есть свои интересные решения:
    - Tagless-final
    - Typeclasses
    - Coproducts of functors

  Подробнее:
    - https://en.wikipedia.org/wiki/Expression_problem
    - https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions/

  На самом деле паттерн Visitor одна из возможных реализаций pattern matching в ООП.
  А IVisitor представляет собой какую-то новую функциональность, которую мы хотим добавить
  в исходную иерархию классов, т.е. попросту функция в функциональном языке.

  Пусть у нас есть дерево выражений (abstract syntax tree) в некотором языке.
  Узел этого дерева представим интерфейсом:

    public interface IStatement { }

    // У IStatement есть несколько наследников:
    public class PrintVariable : IStatement { ... }
    public class AssignVariable : IStatement { ... }
    ...

    // Сделаем класс Visitor, в котором каждому наследнику Statement
    // будет соответствовать перегрузка функции Visit:

    public interface IVisitor<out T>
    {
        T Visit(PrintVariable statement);
        T Visit(AssignVariable statement);
        ...
    }

    // Теперь добавим функцию Accept в Statement и её реализацию в каждого наследника
    
    public interface IStatement
    {
        T Accept<T>(IVisitor<T> visitor);
    }

    public class PrintVariable : IStatement
    {
        ...

        public T Accept<T>(IVisitor<T> visitor)
        {
            return visitor.Visit(this);
        }
    }

    Теперь мы можем легко расширить IStatement, для этого достаточно реализовать интерфейс IVisitor:

    class BeautyPrinter : IVisitor<string>
    {
      string PrintBeautiful(IStatement root)
      {
        root.Accept(this);
      }

      string Visit(VariableDeclaration statement);
      ...
    }

    <=>

    data Statement = VariableDeclaration | AssignVariable | ...

    printBeautiful :: Statement -> String
    printBeautiful VariableDeclaration = "..."
    ...

  Подробнее:
    - https://blog.ploeh.dk/2018/06/18/church-encoded-payment-types/
    - https://blog.ploeh.dk/2018/06/25/visitor-as-a-sum-type/
    - https://koerbitz.me/posts/Sum-Types-Visitors-and-the-Expression-Problem.html
-}

{-
  Теперь посмотрим на records (ADTs с именованными полями).

  В начале лекции мы определили класс с двумя полями:

    class Animal {
      int age;
      string name;

      Animal Animal() { }
    }

  В Haskell мы можем получить похожее поведение:
-}

data RAnimal = RAnimal
  { age :: Int
  , name :: String
  } deriving (Eq, Show)

{-
  Что это нам даёт?

  1. Создавать данные, явно указывая поля:

    leo = RAnimal { name = "Leo", age = 2 }

  2. Получать значение по именю поля:

    >name leo
    "Leo"

    Язык сгенерировал нам функции с названием поля, для получения значений из типа:

    > :t age
    age :: RAnimal -> Int

    > :t name
    name :: RAnimal -> String

  3. Менять значение по именю поля:

    hovard = leo { name = "Hovard" }

    Важно заметить, что leo не изменился. Модификация поля вернула нам
    новое выражение с новым значением:

    > leo
    RAnimal {age = 2, name = "Leo"}

    >hovard
    RAnimal {age = 2, name = "Hovard"}
-}

{-
  Новые типы можно объявлять не только с помощью `data`, но и с помощью
  `newtype`:

    newtype Commission = Commission Double

  Они также поддерживают синтаксис records:

    newtype Commission = Commission { getCommission :: Double }

  Тогда мы получаем следующие функции:

    > :t Commission
    Commission :: Double -> Commission

    > :t getCommission 
    getCommission :: Commission -> Double

  Как можно заметить, `newtype` создает изоморфизм между типами.
  После проверки типов, значения типов `Double` и `Commission` становятся
  взаимозаменяемы в рантайме, что позволяет компилятору оптимизировать код.
  Поэтому `newtype` всегда должен иметь только один конструктор с одним полем.

  На практике `newtype` позволяет создавать дешевые дополнительные абстракции.
  В будущем мы захотим научиться приручать животных. Мы можем создать тип для
  прирученных животных:
-}
newtype AdoptedAnimal = AdoptedAnimal
  { getAdoptedAnimal :: AnimalWithType }
  deriving (Eq, Show)

-- <Задачи для самостоятельного решения>

{-
  Вам нужно отрефакторить функцию, написав типы-обертки
  для `Int` с помощью `newtype`:
    - Day
    - Month
    - Year
-}
showDate :: Int -> Int -> Int -> String
showDate day month year =
  "Day " ++ show day ++ " of " ++ show month ++ " month, year " ++ show year

-- </Задачи для самостоятельного решения>

{-
  В Haskell в отличии от многих императивных языков
  отсутствует тип и значение null. Вместо этого используется тип `Maybe`,
  который похож на Nullable<T> в С#. У `Maybe` есть два конструктора:
    1. Nothing - когда значение отсутствует
    2. Just a - когда значение присутствует

  data Maybe a = Just a | Nothing
    deriving (Eq, Show)

  Это избавляет нас от NullReferenceException (решает так называемую "billion-dollar mistake").

  `Maybe a` — это полиморфный тип, который принимает любой тип `a`.

  Помимо `newtype` в языке есть синонимы типов:

    type MaybeAge = Maybe Int
    type MaybeLocation = Maybe String

  Это не создает новые типы, а просто создает синонимы, чтобы можно было указывать
  в сигнатурах. В отличии от newtype, они не ограничивают функции. То есть мы
  можем передавать значение типа `MaybeAge` в функцию, которая принимает `Maybe Int`.
-}

-- <Задачи для самостоятельного решения>

{-
  uncons возвращает пару, где первым элементом идет первый элемент списка,
  если он присутствует, и остаток списка

  - uncons [1] ~> (Just 1, [])
  - uncons [] ~> (Nothing, [])
  - uncons [1,2,3] ~> (Just 1, [2, 3])
-}
uncons :: [a] -> (Maybe a, [a])
uncons l = error "not implemented"

{-
  zipMaybe возвращает пару значений, если оба значения не Nothing:

  - zipMaybe Nothing (Just 2) ~> Nothing
  - zipMaybe (Just "hello") Nothing ~> Nothing
  - zipMaybe (Just "hey") (Just 2) ~> Just ("hey", 2)
-}
zipMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
zipMaybe a b = error "not implemented"

-- </Задачи для самостоятельного решения>

{-
  К сожалению `Maybe a` скрывает причину, по которой значение может быть недоступно.
  Использовать этот тип для операций, где хотелось бы знать причину ошибки, неудобно.
  Поэтому есть тип `Either a b`:

  data Either a b = Left a | Right b

  По договоренности левый тип `a` используется для ошибок, а правый `b` - для корректных значений.
-}

-- <Задачи для самостоятельного решения>

{-
  `adopt` приручает животное, а если это нельзя сделать,
  то возвращает причину.

  Животные приручаются по следующим правилам:
  
    - если это кот:
      - можно приручить, если младше 5 лет и имя не начинается с "D"
      - иначе сообщать "Can't adopt cat"
    - если это собака:
      - можно приручить, если старше 1 года
      - иначе сообщать "Can't adopt dog"
    - если это утка:
      - можно приручать, если ее зовут "Daisy"
      - иначу сообщать "Quack"
    - если это лев:
      - сообщать "Can't adopt lions :("
-}
adopt :: AnimalWithType -> Either String AdoptedAnimal
adopt = error "not implemented"

-- </Задачи для самостоятельного решения>

{-
  Как можно заметить из примеров выше бывает два вида типов: когда мы определяем
  поля конструктора и когда перечисляем варианты. Product и sum types соответственно.

  Почему они так называются?

  Давайте рассмотрим подробнее как устроены типы. У каждого типа есть
  набор конструкторов (значений). Например у типа `AnimalType` есть
  всего 4 конструктора: Lion, Cat, Dog и Duck. Число конструкторов
  у типа называется cardinality.

  Может ли cardinality быть нулевым? Да, такой тип называется Void:

  data Void

  Тип с одним конструктором обычно называется Unit:

  data Unit = Unit

  Можно ввести обозначение для cardinality:

    |Void| = 0
    |Unit| = 1
    |AnimalType| = 4

  Типы с одинаковыми cardinality изоморфны.
  
  Какой cardinality у полиморфных ADT?

  1. Sum type

    data Either a b = Left a | Right b

    |Either a b| = |a| + |b|

  2. Product type

    data (a, b) = (a, b)

    |(a, b)| = |a| x |b|

  3. Arrow type

    |a -> b| = |b|^|a|

    Для каждого элемента типа `a` можно вернуть любой элемент типа `b`.
    Поэтому |a| раз умножаются cardinality `b`: |b| x |b| x ... x |b|

  Посчитайте cardinality для:

  1. |Bool| = 

  2. |(Bool, Bool)| =

    data (a, b) = (a, b)

  3. |Maybe a| =

    data Maybe a = Nothing | Just a

  4. |Bool -> Bool| =

  5. |Bool -> (Bool, Bool)| =

  6. |Bool -> (Bool, a)| =

-}

-- <Задачи для самостоятельного решения>

{-
  В этом задании вам необходимо самостоятельно написать конструкторы бинарного дерева
  и вспомогательные функции. Тесты написаны так, что вспомогательные функции
  зависят друг друга. 
-}
data Tree a
  {-
    Определите конструкторы для бинарного дерева:
      - лист
      - узел с значением и левой и правой ветками
  -}
  deriving (Eq, Show)

-- Возвращает пустое дерево
empty :: Tree a
empty = error "not implemented"

-- Возвращает True, если дерево - это лист
isLeaf :: Tree a -> Bool
isLeaf t = error "not implemented"

-- Возвращает True, если дерево - не лист
isNode :: Tree a -> Bool
isNode = not . isLeaf

-- Если дерево это нода, то возвращает текущее значение ноды
getValue :: Tree a -> Maybe a
getValue t = error "not implemented"

-- Если дерево это нода, то возвращает левое поддерево
getLeft :: Tree a -> Maybe (Tree a)
getLeft t = error "not implemented"

-- Если дерево это нода, то возвращает правое поддерево
getRight :: Tree a -> Maybe (Tree a)
getRight t = error "not implemented"

{-
  Вставка значения в дерево:

    - insert 3 empty ~> Node Leaf 3 Leaf 
    - insert 1 $ insert 3 $ insert 2 empty ~> Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
    - insert 3 $ insert 2 $ insert 1 empty ~> Node Leaf 1 (Node Leaf 2 (Node Leaf 3 Leaf))
    - insert 1 $ insert 2 $ insert 3 empty ~> Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 Leaf
    
   Обратите внимание, что требуется именно бинарное дерево поиска.
   https://en.wikipedia.org/wiki/Binary_search_tree

   В этом задании вам поможет функция `compare`. Она умеет возвращать
   три значения: GT, EQ, LT. Попробуйте поиграться в repl.
-} 
insert :: Ord a => a -> Tree a -> Tree a
insert v t = error "not implemented"

{-
  Проверка наличия значения в дереве:

    - isElem 2 empty ~> False
    - isElem 1 $ insert 1 empty ~> True
    - isElem 1 $ insert 1 $ insert 3 $ insert 2 empty ~> True
    - isElem 2 $ insert 1 $ insert 3 $ insert 2 empty ~> True
    - isElem 4 $ insert 1 $ insert 3 $ insert 2 empty ~> False
-}
isElem :: Ord a => a -> Tree a -> Bool
isElem v tree = error "not implemented"

-- </Задачи для самостоятельного решения>
