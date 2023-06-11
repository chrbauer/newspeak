module Newspeak.Lang.TemplateInstantiation.Eval where

import Newspeak.Lang.Core
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (mapAccumL)

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

initialTiDump = DummyTiDump
tiStatInitial :: TiStats
tiStatInitial = 0
tiStaIncSteps :: TiStats -> TiStats
tiStaIncSteps s = s + 1
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s



compile :: CoreProgram -> TiState
compile program = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = [addressOfMain]
    addressOfMain = Map.findWithDefault globals "main" (error "main is not defined")

eval :: TiState -> [TiState]

showResults :: [TiState] -> String


applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, globals, stats) =
  (stack, dump, heap, globals, f stats)

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap2, scAddrs ++ primAddrs)
  where
    (heap1, scAddrs) = mapAccumL allocateSc hInitial scDefs
    -- (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives



  
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

eval state = state : restStates
  where
    restStates | tiFinal state = []
               | otherwise = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStaIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], dump, heap, globals, stats) = isDataNode (hLookup heap soleAddr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
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
    newStack = resultAddr : (drop (length argNames + 1) stack)
    (newHeap, resultAddr) = instantiate body heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) = map getArg stack
  where
    getArg addr = arg
      where
        (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"


