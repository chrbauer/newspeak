module Language.Newspeak.Core.Eval where

import Language.Newspeak.Core
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (mapAccumL)
import Prettyprinter

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type Addr = Int
type TiDump = [TiStack]
type TiHeap = Heap Node
data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int
          | NInd Addr
          | NPrim Name Primitive
          -- | NData Int [Addr]
          -- | NMarked Node
          deriving Show

type TiGlobals = Map Name Addr
type TiStats = Int
data Heap a = Heap Addr (Map Addr a) deriving Show

initialTiDump :: TiDump
initialTiDump = []
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
heapUpdate :: TiHeap -> Addr -> Node -> TiHeap
heapUpdate (Heap addr heap) addr' node = Heap addr (Map.insert addr' node heap)

compile :: CoreProgram -> TiState
compile program = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program -- ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = [addressOfMain]
    addressOfMain :: Addr
    addressOfMain = Map.findWithDefault (error "main is not defined") "main" globals  



showSteps :: [TiState] -> Doc ann
showSteps states =
  vsep (map showStep $ zip [0..] states) <> line <> showStats (last states)

showStep :: (Int, TiState) -> Doc ann
showStep (nr, state) = nest 4 $ pretty "Step" <+> pretty nr <> colon <> line <> showState state

showState :: TiState -> Doc ann
showState (stack, dump, heap, globals, stats) = 
  vsep [pretty "Stack:" <+> showStack heap stack,
        nest 4 $ pretty "Heap:" <> line <> showHeap heap,
        nest 4 $ pretty "Globals:" <> line <> showGlobals globals]

showStack :: TiHeap -> TiStack -> Doc ann
showStack heap stack = encloseSep lbracket rbracket (pretty ", ") (map (showStackItem heap) stack)

showStackItem :: TiHeap -> Addr -> Doc ann
showStackItem heap addr = showNode heap (heapLookup heap addr)

showNode :: TiHeap -> Node -> Doc ann
showNode heap ap@(NAp funAddr argAddr) = viaShow ap
  --showNode heap (heapLookup heap funAddr) <+> showNode heap (heapLookup heap argAddr)

showNode heap (NSupercomb name args body) =
  pretty "Supercomb:" <+> pretty name <+> hsep (map pretty args) <+> equals  <+> viaShow body -- showNode heap  body

showNode _ (NNum n) = pretty n
showNode heap (NInd addr) = pretty "Indirection:" <+> pretty addr

showNode _ (NPrim name p) = pretty "PrimOp:" <+> pretty name

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
buildInitialHeap scDefs = (heap2, Map.fromList $ scAddrs ++ primAddrs)
  where
    (heap1, scAddrs) = mapAccumL allocateSc initialHeap scDefs
    (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives


primitives :: [(Name, Primitive)]
primitives = [ ("negate", Neg),
               ("+", Add),
               ("-", Sub),
               ("*", Mul),
               ("/", Div)              
             ]

  
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = heapAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
  where
    (heap', addr) = heapAlloc heap (NPrim name prim)
    
eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates | tiFinal state = []
               | otherwise = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStaIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], [], heap, globals, stats) = isDataNode (heapLookup heap soleAddr)
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
    dispatch (NInd a1) = indStep state a1
    dispatch (NPrim _ p) = primStep state p
    

primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div

primNeg :: TiState -> TiState
primNeg  (stack@(root:arg:[]), dump, heap, globals, stats) = 
  case heapLookup heap b of
    NNum n ->
      let heap' = heapUpdate heap arg  (NNum (-n))
      in ([arg], dump, heap', globals, stats)
    _ -> ([b], [arg]:dump, heap, globals, stats)
  where
    [b] = getArgs heap stack

    
primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith  (stack@(root:arg1:arg2:[]), dump, heap, globals, stats) f = 
  case (heapLookup heap a1, heapLookup heap a2) of
    (NNum n1, NNum n2) ->
      let heap' = heapUpdate heap arg2  (NNum (f n1 n2))
      in ([arg2], dump, heap', globals, stats)
    (NNum _, _) -> ([a2], [arg1, arg2]:dump, heap, globals, stats)
    _ -> ([a1], [arg1, arg2]:dump, heap, globals, stats)
  where
    [a1, a2] = getArgs heap stack


indStep :: TiState -> Addr -> TiState
indStep (a:stack, dump, heap, globals, stats) a1 = (a1 : stack, dump, heap, globals, stats)

numStep :: TiState -> Int -> TiState
numStep (stack, [], heap, globals, stats) n = error "Number applied as a function!"
numStep (stack, s:dump, heap, globals, stats) n = (s, dump, heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack@(a:_), dump, heap, globals, stats) a1 a2 =
  case heapLookup heap a2 of
    (NInd a3) -> (stack, dump, heapUpdate heap a (NAp a1 a3), globals, stats)
    _ -> (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) scName argNames body = (newStack, dump, newHeap, globals, stats)
  where
    newStack@(aN:_) = drop (length argNames) stack
    newHeap = instantiateAndUpdate body aN heap env
    env = Map.union (Map.fromList argBindings) globals
    argBindings = zip argNames (getArgs heap stack)

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_sc:stack) = map getArg stack
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
instantiate (EVar v) heap env = (heap, Map.findWithDefault (error ("Undefined name " ++ show v)) v env)
instantiate (EConstr tag arity) heap env = undefined -- instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"
instantiate (EPrim p) heap env = heapAlloc heap (NPrim (show p) p)


instantiateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateLet isrec defs body heap env = instantiate body heap' env'
  where
    (heap', extraBindings) = mapAccumL instantiateDef heap defs
    env' = if isrec then Map.union (Map.fromList extraBindings) env else env
    instantiateDef heap (name, expr) = (heap', (name, addr))
      where
        (heap', addr) = instantiate expr heap env'

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiateAndUpdate (EAp e1 e2) updAddr heap env = heap3
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
    heap3 = heapUpdate heap2 updAddr (NAp a1 a2)
instantiateAndUpdate var@(EVar v) updAddr heap env = newHeap
  where 
     (heap', resultAddr) = instantiate var heap env
     newHeap = heapUpdate heap updAddr (NInd resultAddr) 

instantiateAndUpdate (ENum n) updAddr heap env = heapUpdate heap updAddr (NNum n)
instantiateAndUpdate (EConstr tag arity) updAddr heap env = undefined -- instantiateConstr tag arity heap env
instantiateAndUpdate (ELet isrec defs body) updAddr heap env = heapUpdate heap' updAddr (NInd a)
  where (heap', a) = instantiateLet isrec defs body heap env
instantiateAndUpdate (ECase e alts) updAddr heap env = error "Can't instantiate case"


