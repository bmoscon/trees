{--
   Splay Tree - Copyright (c) 2013 Bryant Moscon - bmoscon@gmail.com 
 
 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to 
 deal in the Software without restriction, including without limitation the 
 rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 1. Redistributions of source code must retain the above copyright notice, 
    this list of conditions, and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice, 
    this list of conditions and the following disclaimer in the documentation 
    and/or other materials provided with the distribution, and in the same 
    place and form as other copyright, license and disclaimer information.

 3. The end-user documentation included with the redistribution, if any, must 
    include the following acknowledgment: "This product includes software 
    developed by Bryant Moscon (http://www.bryantmoscon.org/)", in the same 
    place and form as other third-party acknowledgments. Alternately, this 
    acknowledgment may appear in the software itself, in the same form and 
    location as other such third-party acknowledgments.

 4. Except as contained in this notice, the name of the author, Bryant Moscon,
    shall not be used in advertising or otherwise to promote the sale, use or 
    other dealings in this Software without prior written authorization from 
    the author.


 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
 THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
 THE SOFTWARE.








 Example Usage (in hugs):
 Hugs> :l splay.hs
 Main> splaySearch 5 (Node 10 (Node 5 Nil Nil) Nil)
 (1,(5, Nil, (10, Nil, Nil)))
 Main> 

 Note that splaySearch returns 1 (5 was found) and then the newly splayed tree
 
 -}

-- define the tree/node
data Tree a = Nil | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
 show Nil = "Nil"
 show (Node x t1 t2)
  = "(" ++ show x ++ ", " ++ show t1 ++ ", " ++ show t2 ++ ")"

-- =================
-- preorder
-- =================

preorderH Nil ys = ys
preorderH (Node x t0 t1) ys = preorderH t1 (preorderH t0 (x:ys))

preorder (Node x t0 t1) = reverse(preorderH (Node x t0 t1) [])


-- ==================
-- treeInsertWithPath
-- ==================

tIWP x Nil ys = ((Node x Nil Nil), ys)

tIWP x (Node y t0 t1) ys
 | x <= y = ((Node y (fst (tIWP x t0 (0:ys))) t1), (snd (tIWP x t0 (0:ys))))
 | x > y = ((Node y t0 (fst (tIWP x t1 (1:ys)))), (snd (tIWP x t1 (1:ys))))

treeInsertWithPath x Nil = ((Node x Nil Nil), [])

treeInsertWithPath x (Node y t0 t1) = (a, reverse b)
 where (a, b) = tIWP x (Node y t0 t1) []


-- +++++++++++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++++++++++
-- ++    SPLAY                      ++
-- +++++++++++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++++++++++   

{- + splay3 is the base cases
   + splay2 is the rebuilding function; it builds each tree for the path with length two or less
   + splay1 is the control function; it calls splay2 and does the actual recursion
   + splay is the function the user calls, or is called by other functions (like search)
-}

-- ===============================
-- Splay 3
-- ===============================

{- these are the base cases when the path length is equal to 2.
   in each case, the Node x is the root of the subtree and
   the Node z is the one we want to move the the root of the tree
-}
-- zigzig steps
splay3 (Node x (Node y (Node z t0 t1) t2 ) t3) [0, 0]
 = (Node z t0 (Node y t1 (Node x t2 t3)))

splay3 (Node x t0 (Node y t1 (Node z t2 t3)))  [1, 1]
 = (Node z (Node y (Node x t0 t1) t2 ) t3)

-- zigzag steps
splay3 (Node x (Node y t0 (Node z t1 t2)) t3) [0, 1]
 = (Node z (Node y t0 t1) (Node x t2 t3))

splay3 (Node x t0 (Node y (Node z t1 t2) t3)) [1, 0]
 = (Node z (Node x t0 t1) (Node y t2 t3))

{- these are the base cases when the path length is equal to 1.
   in each case, the Node x is the root of the subtree and
   the Node z is the one we want to move the the root of the tree
-}

splay3 (Node x t0 (Node z t1 t2)) [1]
 = (Node z (Node x t0 t1) t2)

splay3 (Node x (Node z t0 t1) t2) [0]
 = (Node z t0 (Node x t1 t2))

{- this handles the case where the node in question is already the root.
   this function should never be called by splay2, only by splay1
-}

splay3 (Node z t0 t1) []
 = (Node z t0 t1)

-- ==============================
-- splay2
-- ==============================
{-  splay2 builds a new tree after the desired node is moved up the tree two levels.
    splay2 should not be called if there is one or less levels left to move the node.
    In those cases splay1 should directly call splay 3
-}  

splay2 (Node r t0 t1) (x:xs)
 | length xs > 1 && x == 0 = (Node r (splay2 t0 xs) t1)
 | length xs > 1 && x == 1 = (Node r t0 (splay2 t1 xs))
 | otherwise               = splay3 (Node r t0 t1) (x:xs)

-- ==============================
-- splay1
-- ==============================
-- splay1 takes in the path in REVERSE for ease of removing the last two elements

splay1 (Node r t0 t1) xs
 | length xs > 2 = splay1 (splay2 (Node r t0 t1) (reverse xs)) (remove2 xs)
 | otherwise     = splay3 (Node r t0 t1) (reverse xs) 

remove2 (x:y:zs) = zs

-- ==============================
-- splay
-- ==============================

splay t0 xs = splay1 t0 (reverse xs)


-- ++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++
-- ++ end of SPLAY             ++
-- ++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++

-- ===============
-- splayInsert
-- ===============
-- insert a node

splayInsert x Nil = (Node x Nil Nil)
splayInsert x (Node y t0 t1) = splay (fst z) (snd z)
    where z = treeInsertWithPath x (Node y t0 t1)


-- ================
-- splaySearch
-- ================
-- search for a value
-- returns 0 or 1, and the new tree that results from the splay operation if the value is found

splaySearch x Nil = (0, Nil)

splaySearch x (Node y t0 t1) = ((fst z), (splay (Node y t0 t1) (snd z)))  
   where z = pathfinder x (Node y t0 t1) []

-- helper function
pathfinder x (Node y t0 t1) ys
 | x==y       = (1, reverse ys)
 | x< y       = pathfinder x t0 (0:ys) 
 | x> y       = pathfinder x t1 (1:ys)
 | otherwise  = (0,reverse ys)

pathfinder x Nil ys
  = (0, reverse( minus1 ys))

-- helper function
minus1 (y:ys) = ys 
