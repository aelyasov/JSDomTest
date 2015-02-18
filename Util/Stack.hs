-- | Copy of stack implementation from http://www.haskell.org/haskellwiki/Abstract_data_type
module Util.Stack (Stack, empty, isEmpty, push, top, pop, stack2list) where
 

newtype Stack a = StackImpl [a] 
    deriving Show

empty :: Stack a
empty = StackImpl []

isEmpty :: Stack a -> Bool
isEmpty (StackImpl s) = null s

push :: a -> Stack a -> Stack a
push x (StackImpl s) = StackImpl (x:s)

top :: Stack a -> a
top (StackImpl s) = head s

pop :: Stack a -> (a,Stack a)
pop (StackImpl (s:ss)) = (s,StackImpl ss)


stack2list :: Stack a -> [a]
stack2list (StackImpl ss) = ss
