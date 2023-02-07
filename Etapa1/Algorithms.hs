module Algorithms where

import qualified Data.Set as S
import Data.List
import Data.Maybe
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}
search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = faux f node graph [] []

faux :: Ord a
     => ([a] -> [a] -> [a])  
     -> a                    
     -> Graph a 
	 -> [a] -- lista nodurilor vizitate
	 -> [a] -- stiva/ coada
     -> [a]
faux f node graph visited struct = 
    if take 1 (f struct (S.toList (S.difference (outNeighbors node graph) (S.fromList visited)))) == []
        then [node]
		else [node] ++ (faux f newnode graph ([node] ++ visited) newstruct)
    where
	    newnode = head (f struct (S.toList (S.difference (outNeighbors node graph) (S.fromList visited))))
	    newstruct = tail(f struct (S.toList (S.difference (outNeighbors node graph) (S.fromList visited))))

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
mouse :: Ord a => [a] -> [a] -> [a]
mouse struct kids = struct ++ (filter (\x -> not (elem x struct)) kids) 

bfs :: Ord a => a -> Graph a -> [a]
bfs = search mouse

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}

cats :: Ord a => [a] -> [a] -> [a]
cats struct kids = kids ++ (filter (\x -> not (elem x kids)) struct)
dfs :: Ord a => a -> Graph a -> [a]
dfs = search cats

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}
countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph = 
	if elem to (dfs from graph)
    then Just (fromJust (elemIndex to (bfs from graph)) - 1, fromJust (elemIndex to (dfs from graph)) - 1)
	else Nothing