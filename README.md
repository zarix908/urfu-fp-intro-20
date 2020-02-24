# urfu-fp-intro-20

Спецкурс "Введение в функциональное программирование" в УрФУ в 2020.

# Подготовка среды

## Установите Haskell

### MacOS

https://www.haskell.org/platform/mac.html

или

```
brew install ghc cabal-install
```

### Linux

https://www.haskell.org/downloads/linux/

### Windows

https://www.haskell.org/platform/windows.html.

## Проверьте, что всё работает

Чтобы убедиться, что всё установлено верно, запустите тест:

```
cd fpcourse
cabal new-run spec -- --match "Lecture00"
```

Вы должны увидеть, что "всё работает":

![setup is ok](https://github.com/ak3n/fpcourse/blob/master/assets/SetUpIsDone.jpg)

По любым вопросам можно обратиться к официальной [документации](https://www.haskell.org/documentation/) или к преподавателям в чате [https://teleg.run/urfu_fp_intro_20_chat](https://teleg.run/urfu_fp_intro_20_chat).

# Расписание

Неделя | Дата   | Тема 
-------|--------|------
1      | 27 фев | Введение в Haskell
2      | 05 мар | Списки и строки
3      | 12 мар | Лямбда-исчисление (untyped/stlc)
4      | 19 мар | ADTs
5      | 26 мар | Ленивость
6      | 02 апр | Параметрический полиморфизм
7      | 09 апр | Структуры данных
8      | 16 апр | Классы типов
9      | 23 апр | Монады IO и Random
10     | 30 апр | Остальные монады
11     | 07 мая | Трансформеры
12     | 14 мая | * выходной *
13     | 21 мая | Архитектура функциональных приложений
14     | 28 мая | ?
