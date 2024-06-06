{-Functional Programming Coursework
By Dawud Abdullah Iqbal
---- Part 1 ----
a) Simpler programming - Functional programming is declarative rather than imperative. 
This means that rather than having to specify the algorithm and creating a long step-by-step process that is required to solve a problem
or providing the means for solving a problem, all the programmer must do in this case is only describe what the problem is and rely on the 
computer to solve this. As a result, in functional programming all that must be done is to create the application and circumstance of the functions application to the arguments, 
rather than having to build up the functions from the very basics. 
For example, in an imperative programming language you would have to define what the Fibonacci sequence is and create its algorithm on your own if it is to be used. 
However, in functional programming languages like Haskell, all that must be done is create the situation and application of where the Fibonacci sequence is to be used. 
This is the same for summing numbers and many other functions. 

Debugging and testing is easier - in functional languages like Haskell you call a function without the need for parenthesis. 
This allows the programmer to simply enter the function’s name followed by the arguments in the terminal. 
Also, because Haskell is purely functional, functions have no side effects. This is due to lazy evaluation; 
where functions are not evaluated until they are run. This allows testing and debugging separate functions to be much simpler as they can all be separately tested, 
each with their own arguments and the errors can be corrected specific to the function. This allows the program itself to flow better and maintainability is easier as a result. 


Greater Readability - functional programming also comes with the benefit of enhanced readability of values due to the use of pure functions as there is always a pure value as the output. 
The value makes it easier to read and understand how different programs work making re-usability another key advantage of functional programming. 
This is also useful for it making easier for the developer to comprehend the code and develop the program more.
Also, as the user can treat every function as a value, the functions can be implemented and incorporated with other functions very easily as the functions result can be passed through as an argument.


b) A mathematical function is a binary relation associating each value of a set with values from another set of elements. 
It takes an input from one set to perform a certain task (which is also the relationship between the 2 sets) on the input and map it to a value from another set.
Haskell functions are very similar in the sense that they take some predefined sets of inputs and outputs and use a function to relate the inputs to a given set of outputs. For example:
add  :: Int -> Int -> Int
add x y = x+y

Here you can see the type(which can also be interpreted as the set of values) being defined for the function in the first line for the inputs and outputs 
and the second line defines the relation between the 2 inputs (x and y) and the output being x+y. 
Functional programming contains referential transparency - a concept whereby a function can be replaced with its equivalent value. This is something that is shared with maths e.g.
1+2 can be replaced with the value 3.

In Haskell you don’t need to specify that you’re returning a value. In Haskell, functions must return a value, so there’s never a need to make this explicit just like in math.

However, Haskell is able to perform more operations than what is able in maths while also being able to take different data values as inputs. For example, list operations and the
concatenation in lists is not possible in maths as haskell is able to take 2 same or different data types (depending on what the type signature implies) and can output a completely
different data type e.g. [[Int]]->[[Int]]->[Int] (2 list of lists being inputted and being outputted as a single list is possible in haskell but not with mathematical functions.



c) A higher order function is a function that is able to take functions as arguments/parameters or can return a function as a value. e.g.
dotwice :: (a -> a) -> a -> a  
dotwice f x = f (f x)
If this is applied to function like (+4)
dotwice(+4) 6 => this would output 14
This shows a function being taken as a parameter hence making this a higher order function. 

-}
---- Part 2 ----
--a--
--creates line of *--
liness :: Int -> String
liness x
  | x == 0 = ""
  | otherwise = (concat["*" | n <- [0..x-1]])


--creates lines that are longer for step effect--
altline :: Int -> Int -> String
altline x n
  | x == 0 = ""
  | otherwise = (concat[liness x | f <- [0..n-1]])


--put it all together to create a step--
altstep :: Int -> Int -> Int -> String
altstep x y z
  | x == 0 = ""
  | otherwise = (concat[altline x y ++ ("\n") | f <- [0..z-1]])

--this allows for downward steps--
otherstep :: Int -> Int -> Int -> String
otherstep x y z
  | x == 0 = ""
  | y == 0 = ""
  | otherwise = altstep x y z ++ otherstep x (y-1) z


--final function combining everything--
steps :: Int -> Int -> Int -> String
steps z x y
  | x == 0 = ""
  | y == 0 = ""
  | otherwise = (concat[altstep x n z | n <- [0..y]])++otherstep x y z

--b--

--regular line of *s--
line :: Int -> String
line x
  | x == 0 = ""
  | otherwise = (concat["*" | n <- [0..x-1]])++("\n")

--This creates a spaced line of *s for the lines before flag x intersection--
spacedch :: Int -> String
spacedch x
  | x == 0 = "**"
  | otherwise = ("*"++(spaces x)++"*")

--creates a line of n strings, used for concatenation with the previous function--
spaces :: Int -> String
spaces x
  | x == 0 = ""
  | otherwise = (concat[" " | n <- [0..x-1]])

--used for 2nd line after regular line of *s--
otroves :: Int -> String
otroves x = spaces (x-4)


--Putting top half of pattern together --
patternspace :: Int -> Int -> String
patternspace x n
  | x == 2 = ""
  | otherwise = spacedch n ++ otroves x ++ spacedch n ++ "\n" ++ patternspace (x-2) (n+1)

----Putting top half of pattern together but for odd numbers as intersection acts differently--
patternspaceodd :: Int -> Int -> String
patternspaceodd x n
  | x == 1 = ""
  | x == 3 = spacedch n ++ otroves x ++ drop 1 (spacedch n) ++ "\n" ++ patternspaceodd (x-2) (n+1)
  | otherwise = spacedch n ++ otroves x ++ spacedch n ++ "\n" ++ patternspaceodd (x-2) (n+1)


--Bottom half of pattern--
revpatternspace :: Int -> Int -> String
revpatternspace x n
  | n == -1 = ""
  | otherwise = spacedch n ++ otroves x ++ spacedch n ++ "\n" ++ revpatternspace (x+2) (n-1)

--putting all together--
flagpatternb :: Int -> String
flagpatternb x
  | mod x 2 == 0 = line x ++ patternspace x 0 ++ revpatternspace 4 (x`div`3) ++ line x
  | otherwise = line x ++ patternspaceodd x 0 ++ revpatternspace 5 ((x`div`2)-2) ++ line x

--final function compiling everything--
flagpattern :: Int -> Int -> String
flagpattern x n
  | x < 5 = ""
  | n == 0 = ""
  | n == 1 = flagpatternb x
  | otherwise = flagpatternb x ++ flagpattern x (n-1)

---- Part 3 ----
--delete first instance of charachter appearance--
deleteFirst _ [] = [] 
deleteFirst a (b:bc) | a == b    = bc 
                     | otherwise = b : deleteFirst a bc

--look at elements of x and compare with y--
checr :: Int -> String -> String -> String
checr n x y  = deleteFirst (x!!n) y

--make it recurse through the words charachters till all similar charachters are removed--
compares :: Int -> String -> String -> String
compares n x y
  |n == length(x) = ""
  |otherwise = checr n x y ++" "++ (compares (n+1) x (checr n x y))

--get the name with all similar charachters removed--
getword :: Int -> String -> String -> String
getword n x y = last(words(compares n x y))


spaceremove :: String -> String
spaceremove x = [x!!n|n<-[0..(length(x)-1)], not(x!!n `elem` " ")]


combine = zipWith (++)

--zip the name with string of lphi--
rotation :: String -> [String]
rotation x = combine (map (:[]) x) (map (:[]) ("lphilphilphilphilphilphilphilphi"))

--checks what the last charachter is in the zip so relation can be decided--
getrelation :: String -> String
getrelation x
  |tail(last (rotation x)) == "l" = " loves "
  |tail(last (rotation x)) == "p" = " is physical to "
  |tail(last (rotation x)) == "h" = " hates "
  |otherwise = " is indifferent to "

--everything combined so relation string can be outputted--
compatibility :: String -> String -> String
compatibility x y 
  |x==y = x++" is indifferent to "++y++" and "++y++" is indifferent to "++x
  |otherwise = x++getrelation(getword 0 (spaceremove(y)) (spaceremove(x)))++y++" and "++y++getrelation(getword 0 (spaceremove(x)) (spaceremove(y)))++x

---- Part 4 ----
--use of 'a' as type could be both charachter or integer, check if charachter is found while incrementing the number--
splitt :: (Eq a) => [a] -> a -> Int -> [Int]
splitt [] _ n = [n]
splitt (x:xs) y n =
  if (x:xs)==[] && n/=0 then [n]
  else if x==y then conditionchecc (x:xs) y n
  else splitt xs y (n+1)

--check if n is 0 when charachter is found--
conditionchecc :: (Eq a) => [a] -> a -> Int -> [Int]
conditionchecc (x:xs) y n = 
  if n==0 then splitt xs y 0
  else [n] ++ splitt xs y 0

--combine for split function
lsplit :: (Eq a) => [a] -> a -> [Int]
lsplit (x:xs) y = splitt (x:xs) y 0
