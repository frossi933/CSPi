      
data A = B | A String A | ARec A
a = A "1" (g "2" a)
    where g s t = (A s (ARec t))
b = A "1" (A "2" B)



f::A -> String
f B = "fin"
f (A s a) = s++(f a)
f (ARec a) = "rec"
              
f':: Int -> A -> String
f' 0 a = f a
f' n (ARec a) = f' (n-1) a
f' n B = "fin"
f' n (A s a) = s++(f' n a)


g s = print s

h f = do f "hola"
         f "hol"
         return ()
         
         {-
data Tree a = L a | N (Tree a) (Tree a) deriving(Show)

instance Monad Tree where
    return x = L x
    (L a) >>= f = f a
    (N l r) >>= f = N (l >>= f) (r >>= f)
    
arbol :: () -> Tree Int
arbol () = do a <- N (L 1) (L 2)
              b <- L (a + 2)
              return b
              
f [x] = L x
f (x:(y:ys))-}