module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node a) = S.fromList [a]
nodes (Overlay g1 g2) = S.union (nodes g1) (nodes g2) 
nodes (Connect g1 g2) = S.union (nodes g1) (nodes g2)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node a) = S.empty
edges (Overlay g1 g2) = S.union (edges g1) (edges g2)
edges (Connect g1 g2) = (S.union (edges g1) (edges g2))`S.union` S.cartesianProduct (nodes g1) (nodes g2)

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty
outNeighbors node (Node a) = S.empty
outNeighbors node (Overlay g1 g2)   
    | (S.member node (nodes g1)) && (S.notMember node (nodes g2)) = outNeighbors node g1 
	| (S.member node (nodes g2)) && (S.notMember node (nodes g1)) = outNeighbors node g2 
    | (S.notMember node (nodes g2)) && (S.notMember node (nodes g1)) = S.empty
	| otherwise = S.union (outNeighbors node g1) (outNeighbors node g2)
outNeighbors node (Connect g1 g2)
    | (S.member node (nodes g1)) && (S.notMember node (nodes g2)) = S.union (outNeighbors node g1) (nodes g2)
    | (S.notMember node (nodes g1)) && (S.notMember node (nodes g2)) = S.empty
    | (S.member node (nodes g1)) && (S.member node (nodes g2)) = S.union (outNeighbors node g1) (outNeighbors node g2) `S.union` (nodes g2)
	| otherwise = outNeighbors node g2
{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node a) = S.empty
inNeighbors node (Overlay g1 g2)   
    | (S.member node (nodes g1)) && (S.notMember node (nodes g2)) = inNeighbors node g1 
	| (S.member node (nodes g2)) && (S.notMember node (nodes g1)) = inNeighbors node g2 
    | (S.notMember node (nodes g2)) && (S.notMember node (nodes g1)) = S.empty
	| otherwise = S.union (inNeighbors node g1) (inNeighbors node g2)
inNeighbors node (Connect g1 g2)
    | (S.member node (nodes g2)) && (S.notMember node (nodes g1)) = S.union (inNeighbors node g2) (nodes g1)
    | (S.notMember node (nodes g2)) && (S.notMember node (nodes g1)) = S.empty
    | (S.member node (nodes g1)) && (S.member node (nodes g2)) = S.union (inNeighbors node g1) (inNeighbors node g2) `S.union` (nodes g1)
	| otherwise = inNeighbors node g1
{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node Empty = Empty
removeNode node (Node a) = if (node == a)
                            then Empty
                            else (Node a)
removeNode node (Overlay g1 g2) = Overlay (removeNode node g1) (removeNode node g2)
removeNode node (Connect g1 g2) = Connect (removeNode node g1) (removeNode node g2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news Empty = Empty
splitNode old news (Node a) = if (old == a)
                               then (foldl Overlay Empty (map Node news)) 
							   else (Node a)
splitNode old news (Overlay g1 g2) = Overlay (splitNode old news g1) (splitNode old news g2)
splitNode old news (Connect g1 g2) = Connect (splitNode old news g1) (splitNode old news g2)
{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node Empty = Empty
mergeNodes prop node (Node a) = if prop a 
                                 then (Node node)
								 else (Node a)
mergeNodes prop node (Overlay g1 g2) = Overlay (mergeNodes prop node g1) (mergeNodes prop node g2)
mergeNodes prop node (Connect g1 g2) = Connect (mergeNodes prop node g1) (mergeNodes prop node g2)