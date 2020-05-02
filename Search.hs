{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where
import RollTheBall as Rtb
import qualified Data.Set as S
import qualified Data.Array as A 

import ProblemState
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

data Node s a = Node {
    getState :: s,
    getAction :: Maybe a,
    getParent :: Maybe (Node s a),
    getDepth :: Int,
    getChildren :: [Node s a]
} deriving (Eq, Show)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState node = getState node

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = getParent node

nodeDepth :: Node s a -> Int
nodeDepth node = getDepth node

nodeAction :: Node s a -> Maybe a
nodeAction node = getAction node

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = getChildren node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

-- levelToString :: s -> String
-- levelToString node = map category $ A.elems (cells (getState node))

-- createNode :: (ProblemState s a, Eq s) => Node s a -> s ->Node s a
-- createNode node@(Node state action parent depth c)   = new_node
--                     where
--                     pos_dir_levels = ProblemState.successors state
--                     childrens = [(Node lev (Just pd) (Just new_node) (depth + 1) [])  | (pd, lev) <- pos_dir_levels]
--                     new_node = Node state action parent depth childrens


-- createNode :: (ProblemState s a, Eq s)  => Node s a -> a -> s ->Node s a
-- createNode parinte@(Node state actionn parent depth c) action lv  = Node lv (Just action) parent 0 childrenRec
--                                                         where 
--                                                         succs_pos_dir_levels = ProblemState.successors state
--                                                         childrenRec = [(createNode (createNode parinte action lv) pd lev) | (pd, lev) <- succs_pos_dir_levels]


createNode ::  (ProblemState s a, Eq s)  => Node s a -> s ->Node s a
createNode parinte@(Node state actionn parent depth c) lv = Node lv Nothing (Just parinte) 0 childRec
                                                            where
                                                            succs_pos_dir_levels = ProblemState.successors state
                                                            this_node = createNode parinte lv
                                                            childRec = [(Node ll (Just pd)) (Just this_node) 0 (map (createNode (createNode this_node ll)) (map snd (ProblemState.successors ll))) | (pd, ll) <- succs_pos_dir_levels]

createStateSpace :: (ProblemState s a, Eq s)  => s -> Node s a
createStateSpace lv =   createNode startNode  lv
                        where
                        startNode = Node lv Nothing Nothing 0 []
                        -- dummyAc = ProblemState.getSameAction 
                        -- dummyLv = ProblemState.getSameAction lv
              

              

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}



notMemberInSet :: (Ord s) => S.Set s -> s -> Bool
notMemberInSet visited lv = S.notMember lv visited

bfsHelper ::(Ord s)  => [Node s a] -> [Node s a] -> [([Node s a], [Node s a])] -> S.Set s -> [([Node s a], [Node s a])]
bfsHelper recentlyAdded frontier last_result visited =  if (length frontier) == 0 then last_result
                                                        else bfsHelper new_recentlyAdded new_frontier new_last_result new_visited
                                                        where
                                                        new_last_result = last_result ++ [(recentlyAdded, frontier)]
                                                        node = head frontier
                                                        children = getChildren node
                                                        valid_children = filter ((notMemberInSet visited) . getState) children
                                                        visited_valid_children = S.fromList (map getState valid_children)
                                                        new_recentlyAdded = valid_children
                                                        new_frontier = (tail frontier) ++ new_recentlyAdded
                                                        new_visited = S.union visited visited_valid_children
                                                        
                                                        

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs startNode = bfsHelper [startNode] [startNode] [] S.empty 

startNode = createStateSpace lv4

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
extractPath = undefined



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
