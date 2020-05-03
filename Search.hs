{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import Data.Maybe

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

--   Node s a = Node stare actiune parinte adancime lista_de_copii
data Node s a = Node s (Maybe a) (Maybe (Node s a)) Int [Node s a]

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Node stare actiune parinte adancime copii) = stare

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node stare actiune parinte adancime copii) = parinte

nodeDepth :: Node s a -> Int
nodeDepth (Node stare actiune parinte adancime copii) = adancime

nodeAction :: Node s a -> Maybe a
nodeAction (Node stare actiune parinte adancime copii) = actiune

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node stare actiune parinte adancime copii) = copii

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createNodeForState :: (ProblemState s a, Eq s) => s -> Maybe a -> Maybe (Node s a) -> Int -> Node s a
createNodeForState stare actiune parinte adancime = 
    let current_node = Node stare actiune parinte adancime lista_de_copii
        lista_de_actiuni_si_noduri_viitoare = successors stare
        lista_de_copii = if isGoal stare then [] else map (\x -> createNodeForState (snd x) (Just (fst x)) (Just current_node) (adancime + 1)) lista_de_actiuni_si_noduri_viitoare 
    in
        current_node

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace stare = createNodeForState stare Nothing Nothing 0

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

--         :: frontiera curenta -> (noduri nou-adaugate, noua frontiera)
unPasDeBfs :: [Node s a] -> ([Node s a], [Node s a])
unPasDeBfs [] = ([], [])
unPasDeBfs frontiera =
    let 
        nod_curent = head frontiera
        copii = nodeChildren nod_curent
    in
        (copii, (tail frontiera) ++ copii)

-- aplicaBfs :: frontiera curenta -> [(nou-adaugate, noua frontiera)]
aplicaBfs [] = []
aplicaBfs frontiera = 
    let 
        -- pnf = pereche noduri noi si frontiera noua
        pnf = unPasDeBfs frontiera
        noduri_noi = fst pnf
        noua_frontiera = snd pnf
        rest_lista_bfs = aplicaBfs noua_frontiera
    in
        (noduri_noi, noua_frontiera):rest_lista_bfs

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs nod = aplicaBfs [nod]

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}


extractPath :: Node s a -> [(Maybe a, s)]
extractPath (Node stare actiune parinte adancime copii) = 
    if (isNothing parinte) then [(Nothing, stare)]
    else (extractPath (fromJust parinte)) ++ [(actiune, stare)]



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
