module Language.Newspeak.Core.Eval where

import Language.Newspeak.Core
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (mapAccumL)
import Prettyprinter

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type Addr = Int
data TiDump = DummyTiDump deriving Show
type TiHeap = Heap Node
data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int
          -- | NInd Addr
          -- | NPrim Name Primitive
          -- | NData Int [Addr]
          -- | NMarked Node
          deriving Show

type TiGlobals = Map Name Addr
type TiStats = Int
data Heap a = Heap Addr (Map Addr a) deriving Show

initialTiDump = DummyTiDump
tiStatInitial :: TiStats
tiStatInitial = 0
tiStaIncSteps :: TiStats -> TiStats
tiStaIncSteps s = s + 1
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s


initialHeap :: TiHeap
initialHeap = Heap 0 Map.empty
heapAlloc :: TiHeap -> Node -> (TiHeap, Addr)
heapAlloc (Heap addr heap) node = (Heap (addr + 1) (Map.insert addr node heap), addr)
heapLookup :: TiHeap -> Addr -> Node
heapLookup (Heap _ heap) addr = heap Map.! addr

compile :: CoreProgram -> TiState
compile program = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program -- ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = [addressOfMain]
    addressOfMain :: Addr
    addressOfMain = Map.findWithDefault (error "main is not defined") "main" globals  



showResults :: [TiState] -> Doc ann
showResults states = vsep (map showState states) <> showStats (last states)

showState :: TiState -> Doc ann
showState (stack, dump, heap, globals, stats) = 
  vsep [align $ pretty "Stack:[" <> showStack heap stack <> rbracket,
        pretty "Heap:" <> showHeap heap,
        pretty "Globals:" <> showGlobals globals]

showStack :: TiHeap -> TiStack -> Doc ann
showStack heap stack = hsep (map (showStackItem heap) stack)

showStackItem :: TiHeap -> Addr -> Doc ann
showStackItem heap addr = showNode heap (heapLookup heap addr)

showNode :: TiHeap -> Node -> Doc ann
showNode heap ap@(NAp funAddr argAddr) = viaShow ap
  --showNode heap (heapLookup heap funAddr) <+> showNode heap (heapLookup heap argAddr)

showNode heap (NSupercomb name args body) =
  pretty "Supercomb:" <+> pretty name <+> hsep (map pretty args) <> line <> viaShow body -- showNode heap  body

showNode _ (NNum n) = pretty n

showHeap :: TiHeap -> Doc ann
showHeap h@(Heap _ heap) = vsep (map (showHeapItem h) (Map.toList heap))

showHeapItem :: TiHeap -> (Addr, Node) -> Doc ann
showHeapItem heap (addr, node) = pretty addr <> colon <+> showNode heap node

showGlobals :: TiGlobals -> Doc ann
showGlobals globals = vsep (map showGlobal (Map.toList globals))

showGlobal :: (Name, Addr) -> Doc ann
showGlobal (name, addr) = pretty name <> colon <+> pretty addr

showStats :: TiState -> Doc ann
showStats (_, _, _, _, stats) = pretty "Steps:" <+> pretty stats


--showState :: TiState -> Dapp/Language/Newspeak/Core/boc a
--showState (stack, dump, heap, globals, stats) = encloseSep lbrace rbrace semi (map showStackItem stack) <> line <> showHeap heap <> line <> showGlobals globals


applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, globals, stats) =
  (stack, dump, heap, globals, f stats)

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap1, Map.fromList scAddrs)
  where
    (heap1, scAddrs) = mapAccumL allocateSc initialHeap scDefs
    -- (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives

  
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = heapAlloc heap (NSupercomb name args body)
    
eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates | tiFinal state = []
               | otherwise = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStaIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], dump, heap, globals, stats) = isDataNode (heapLookup heap soleAddr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

step :: TiState -> TiState
step state = dispatch (heapLookup heap (head stack))
  where
    (stack, dump, heap, globals, stats) = state
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) scName argNames body = (newStack, dump, newHeap, globals, stats)
  where
    newStack = resultAddr : drop (length argNames + 1) stack
    (newHeap, resultAddr) = instantiate body heap env
    env = Map.union (Map.fromList argBindings) globals
    argBindings = zip argNames (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) = map getArg stack
  where
    getArg addr = arg
      where
        (NAp fun arg) = heapLookup heap addr

instantiate :: CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiate (ENum n) heap env = heapAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = heapAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, Map.findWithDefault (error ("Undefined name " ++ show v ++ "   " ++ show env)) v env)
instantiate (EConstr tag arity) heap env = undefined -- instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = undefined -- instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"


