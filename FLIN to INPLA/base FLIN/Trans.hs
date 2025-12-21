module Trans where

import Data.Char (ord, chr)
import Data.List (nub, group, sort,isPrefixOf)
import Parser
import Text.Parsec (ParseError)
import qualified Data.Map.Strict as Map

-- ------------------------
-- Data structures for fully explicit INPLA nets
-- ------------------------
type Port   = String
type Symbol = String
type Agent  = (Symbol, Port, [Port]) -- (agent label, principal port, aux ports)
type Wire   = (Port,Port)
type Net    = ([Agent],[Wire])
type LUT    = [(String,Int)]         -- (func label, number outputs)

main :: FilePath -> IO ()
main filename = do
    inputFile  <- readFile filename
    let inputLines  = lines inputFile
        ruleLines   = filter (not . ("--" `isPrefixOf`)) inputLines
        parsedRules = map parseRule ruleLines
        rules       = [r | Right r <- parsedRules]
        errors      = [e | Left e <- parsedRules]
    if not (null errors)
       then error ("Parse errors: " ++ show errors)
       else do
           let lut        = makeLUT rules []
               transRules = transRuleList rules lut
           -- debug
           putStrLn "LUT:"
           putStrLn (show lut)
           -- print FLIN rules
           putStrLn "FLIN rules:"
           putStrLn inputFile
           putStrLn "\nINPLA rules:"
           putStrLn transRules
           putStrLn ""
           putStr "Enter a FLIN term to translate, or :Q to quit"
           putStrLn ""
           -- start interactive loop to translate terms
           replLoop lut

replLoop :: LUT -> IO ()
replLoop lut = do
    putStr "> "
    line <- getLine
    if line == ":Q"
       then return ()
       else do
           let result = inpla lut line
           case result of
               Left err  -> putStrLn ("Parse error: " ++ show err)
               Right str -> putStrLn str
           replLoop lut


-- trans
trans :: Term -> VarName -> Net -> LUT -> Net
-- Variables
trans (Var v) root (agents, wires) _ =
    (agents, wires ++ [(v, root)])

-- Empty net vanishes
trans Empty _ (agents, wires) _ =
    (agents, wires)

-- Constructors
trans (Constr constr args) root (agents, wires) lut =
  let numIns = length args

      -- generate auxiliary ports from the root
      auxPorts = take numIns $ tail $ iterate fresh root

      -- argument roots: each aux port concatenated with its fresh increment
      argRoots = map (\x -> x ++ fresh x) auxPorts

      -- create the constructor agent
      newAgent = (constr, root, auxPorts)

      -- recursively translate each argument and connect its principal port
      (agents', wires') =
        foldl
          (\(as, ws) (arg, aroot, auxPort) ->
             let (as', ws') = trans arg aroot (as, ws) lut
                 newWire    = (aroot, auxPort)  -- principal first
             in (as', ws' ++ [newWire])
          )
          (agents ++ [newAgent], wires)
          (zip3 args argRoots auxPorts)
  in (agents', wires')

-- functions
trans (Func fName args) root net lut =
  let numOuts = funcNumOuts fName lut
      pp       = fresh root

      -- output ports: first is root, rest are root ++ fresh iterated
      outPorts = 
        if numOuts == 0 then []
        else root : map (root ++) (take (numOuts - 1) $ tail $ iterate fresh root)

      -- input ports (excluding pp itself)
      numInPortsExPP = length args - 1
      inPortsExPP    = map (pp ++) (take numInPortsExPP $ tail $ iterate fresh pp)

      -- the function agent itself
      newAgent       = (fName, pp, outPorts ++ inPortsExPP)

      -- now handle arguments
      (allAgents, allWires) =
        foldr
          (\(arg, port) (as, ws) ->
             let freshP       = fresh port
                 (as', ws')  = trans arg freshP net lut
                 newWire      = (freshP, port)  -- connect arg result to function input
             in  (as' ++ as, newWire : (ws' ++ ws)))
          ([], [])
          (zip args (pp : inPortsExPP))
  in
      (newAgent : allAgents, allWires)

-- let
trans (Let t1 vars t2) root net lut =
  let
      -- 1. Translate t1
      (agents1, wires1) = trans t1 root net lut

      -- 2. Rename outputs of t1 to match 'vars'
      -- *** need to change this do deal with Par: need to do a case on shape of t1 e.g. x,y~A|B in x|y***
      (agents1',wires1') = 
        if null agents1 then (agents1, renameWire t1 (head wires1) (head vars)) -- only handles 1 wire
        else ((renameOutputs t1 (head agents1) vars lut) : tail agents1, wires1)  -- only handles 1 agent

      -- 3. Translate t2 with updated net
      -- (agents2,wires2) = 
      --   if null agents1' then (agents1',wires1')
      --   else trans t2 (root++"L"++(fresh root)) (agents1',wires1') lut
      (agents2,wires2) = 
        if null agents1' then (agents1',wires1')
        else 
          let (agents3,wires3) = trans t2 (root++"L"++(fresh root)) ([],[]) lut
          in (agents1'++agents3,wires1'++wires3)
  in
      -- Output ex duplications created
      (nub agents2, nub wires2)

-- ||
trans (Par t1 t2) root (agents,wires) lut =
  let (agents1, wires1) = trans t1 root (agents,wires) lut
      usedPorts = map (\(_,pp,aux) -> pp:aux) agents1 ++ [[a,b] | (a,b) <- wires1]
      usedSet   = concat usedPorts
      newRoot   = head (dropWhile (`elem` usedSet) (iterate fresh root))
      (agents2, wires2) = trans t2 newRoot (agents1,wires1) lut
  in (agents2, wires2)

renameOutputs :: Term -> Agent -> [VarName] -> LUT -> Agent
-- need the Term to know whether is Constr or Func
renameOutputs (Var v) agent vars lut = 
  undefined
renameOutputs Empty agent vars lut = 
  undefined
renameOutputs (Constr _ _) (name,out,ins) vars lut = 
  let [newOut] = vars   -- should only be 1
  in (name,newOut,ins)
renameOutputs (Func fName inputs) (name,pp,auxs) vars lut = 
  let numOuts = funcNumOuts name lut
  in (name,pp,vars++(drop numOuts auxs))

renameWire :: Term -> Wire -> VarName -> [Wire]
renameWire (Var v) wire var = 
  let (p1,p2) = wire
  in
    if p1==v then [(p1,var)] else [(var,p2)]

netToINPLA :: Net -> String
netToINPLA (agents, wires)
  | null inplaAgents = inplaWires ++ ";"
  | null inplaWires  = inplaAgents ++ ";"
  | otherwise        = inplaAgents ++ "," ++ inplaWires ++ ";"
  where
    inplaAgents = agentsToINPLA agents
    inplaWires  = wiresToINPLA wires

agentsToINPLA :: [Agent] -> String
agentsToINPLA []     = ""
agentsToINPLA [(symbol, pp, auxPorts)]    = symbol++"("++(listPorts auxPorts)++")"++"~"++pp
agentsToINPLA ((symbol, pp, auxPorts):as) = symbol++"("++(listPorts auxPorts)++")"++"~"++pp++","++ agentsToINPLA as

listPorts :: [Port] -> String
listPorts []  = ""
listPorts [p] = p
listPorts (p:ps) = p++","++ listPorts ps

wiresToINPLA :: [Wire] -> String
wiresToINPLA []              = ""
wiresToINPLA [(p1,p2)]       = p1++"~"++p2
wiresToINPLA ((p1,p2):wires) = p1++"~"++p2++","++ wiresToINPLA wires

funcNumOuts :: String -> LUT -> Int
funcNumOuts symbol [] = error ("funcNumOuts reaches end of LUT for " ++ symbol)
funcNumOuts symbol ((name, num):rest) = 
    if symbol == name then num else funcNumOuts symbol rest

-- build the LUT
makeLUT :: [Rule] -> LUT -> LUT
makeLUT [] lut = lut
makeLUT (Rule t1 t2 : rs) lut =
    case t1 of
        Func fName _ ->
            if any (\(name, _) -> name == fName) lut
               then makeLUT rs lut  -- already in LUT, skip
               else
                   let numOuts = countOuts t2 lut
                       lut'    = lut ++ [(fName, numOuts)]
                   in makeLUT rs lut'
        _ -> error "First term of Rule must be Func"


countOuts :: Term -> LUT -> Int
-- count number non-empty terms
countOuts Empty _        = 0
countOuts (Var _) _       = 1
countOuts (Constr _ _) _  = 1
countOuts (Func f _) lut   = funcNumOuts f lut
countOuts (Let t1 _ t2) lut = countOuts t2 lut
countOuts (Par t1 t2) lut   = (countOuts t1 lut) + (countOuts t2 lut)

-- fresh port names
fresh :: String -> String
fresh s 
  | last s == 'z' = s ++ "a"  -- append 'a' when last char is 'z'
  | otherwise = init s ++ [chr (ord (last s) + 1)]  -- increment last char

-- clean up intermediate links e.g. a~b,b~c
cleanNet :: Net -> Net
cleanNet (agents, wires) = --(agents, wires)
    let intermediates     = intermediatePorts wires
        collapsedWires    = collapseChains wires intermediates
    in collapseWiresToPorts (updateAgentsWithWires agents collapsedWires)

intermediatePorts :: [Wire] -> [Port]
intermediatePorts ws =
    let ports = concatMap (\(a,b) -> [a,b]) ws
        counts = map (\g -> (head g, length g)) . group . sort $ ports
    in [p | (p,n) <- counts, n > 1]

-- Collapse a single intermediate port
collapseIntermediate :: [Wire] -> Port -> [Wire]
collapseIntermediate wires ip =
    let -- select wires that contain the intermediate port
        relevant = filter (\(a,b) -> a == ip || b == ip) wires
        -- get the ports on the other side of ip
        endpoints = map (\(a,b) -> if a == ip then b else a) relevant
    in case endpoints of
         [p1,p2] -> [(p1,p2)]  -- connect them directly
         _        -> []         -- ignore other cases for now
         
-- Collapse all intermediate ports
collapseChains wires intermediates =
    let isIntermediate p = p `elem` intermediates

        -- map each port to its directly connected port(s)
        adj = Map.fromListWith (++) [(a,[b]) | (a,b) <- wires] `Map.union`
              Map.fromListWith (++) [(b,[a]) | (a,b) <- wires]

        -- find canonical port for a given port, skipping intermediates
        canonical p visited
          | isIntermediate p =
              case filter (`notElem` visited) (Map.findWithDefault [] p adj) of
                []    -> p
                (q:_) -> canonical q (p:visited)
          | otherwise = p

        ports = nub $ concatMap (\(a,b) -> [a,b]) wires

        -- build new wires for non-intermediate ports only
        newWires = nub [ (canonical a [], canonical b []) 
                       | (a,b) <- wires
                       , not (isIntermediate a && isIntermediate b)]
    in nub [(x,y) | (x,y) <- newWires, x /= y]

-- if a~b and a appears in port, replace a with b
collapseWiresToPorts :: Net -> Net
collapseWiresToPorts (agents, wires) = 
  let (from,to) = unzip wires
      newAgents = map (updateAgent from to) agents
      freeWires = makeFreeWires from to newAgents
  in (newAgents, freeWires)

makeFreeWires :: [Port] -> [Port] -> [Agent] -> [Wire]
makeFreeWires from to agents =
    [(f, t) | (f, t) <- zip from to, not (portInAgents t agents)]
  where
    portInAgents p ags = any (portInAgent p) ags

-- Check if a port occurs in an agent (principal or auxiliary)
portInAgent :: Port -> Agent -> Bool
portInAgent p (_, principal, auxPorts) =
    p == principal || p `elem` auxPorts

-- Check if either port from wire occurs in at least one agent
portOccurs :: Wire -> [Agent] -> Bool
portOccurs (p1, p2) agents =
    any (\agent -> portInAgent p1 agent || portInAgent p2 agent) agents

-- Get all ports that appear in the agents
agentPorts :: [Agent] -> [Port]
agentPorts agents = nub $ concatMap (\(_,pp,aux) -> pp:aux) agents

-- Update agents and wires based on collapsed wires
updateAgentsWithWires :: [Agent] -> [Wire] -> Net
updateAgentsWithWires agents wires =
    let aPorts = agentPorts agents
        processWire (as, ws) (a,b)
          | a `elem` aPorts && b `notElem` aPorts =
                let as' = map (replacePort a b) as
                in (as', ws)  -- drop wire
          | b `elem` aPorts && a `notElem` aPorts =
                let as' = map (replacePort b a) as
                in (as', ws)  -- drop wire
          | otherwise = (as, (a,b):ws)  -- keep wire
        (agents', wires') = foldl processWire (agents, []) wires
    in (nub agents', nub wires')

-- Replace port pOld with pNew in a single agent
replacePort :: Port -> Port -> Agent -> Agent
replacePort pOld pNew (label, pp, aux) =
    let pp' = if pp == pOld then pNew else pp
        aux' = map (\x -> if x == pOld then pNew else x) aux
    in (label, pp', aux')


-- Rules; needs LUT
transRule :: Rule -> LUT -> String
transRule (Rule t1 t2) lut =
    let lhs = cleanNet $ trans t1 "r" ([], []) lut
        rhs = case t2 of
                Par t1' t2' -> 
                    let flatRHS = flatPar (Par t1' t2')
                        numOuts  = length flatRHS
                        outsList = take numOuts $
                                   case lhs of
                                     (agents, _) -> concatMap (\(_,_,aux) -> aux) agents
                        -- translate each element of flatRHS with the corresponding port
                        combineNet (agentsAcc, wiresAcc) (term, port) =
                            let (a', w') = trans term port ([], []) lut
                            in (agentsAcc ++ a', wiresAcc ++ w')
                        rhsNet = foldl combineNet ([], []) (zip flatRHS outsList)
                    in rhsNet
                Let _ _ _ -> cleanUpLetOutputs lhs (cleanNet $ trans t2 "r" ([], []) lut) lut
                _         -> cleanNet $ trans t2 "r" ([], []) lut
    in (transActivePair lhs) ++ (netToINPLA rhs)

cleanUpLetOutputs :: Net -> Net -> LUT -> Net
-- take lhs and rhs of rule and ensure outputs are consistent
cleanUpLetOutputs lhs rhs lut = -- rhs
  let outputs     = uniqueOnly $ inLHSonly lhs rhs  -- outputs from LHS
      danglingRHS = unconnectedPorts rhs            -- anything not connected on RHS
      outputsRHS  = notInLHS lhs danglingRHS
  in updateOutputs rhs outputsRHS outputs 

updatePortName :: [Port] -> [Port] -> Port -> Port
updatePortName from to port =
    case lookup port (zip from to) of
        Just newPort -> newPort
        Nothing      -> port

-- Update ports in an agent
updateAgent :: [Port] -> [Port] -> Agent -> Agent
updateAgent from to (symbol, principal, auxPorts) =
    (symbol, updatePortName from to principal, map (updatePortName from to) auxPorts)

-- Update ports in a wire
updateWire :: [Port] -> [Port] -> Wire -> Wire
updateWire from to (p1, p2) =
    (updatePortName from to p1, updatePortName from to p2)

-- Main function: update all ports in a Net
updateOutputs :: Net -> [Port] -> [Port] -> Net
updateOutputs (agents, wires) from to =
    (map (updateAgent from to) agents, map (updateWire from to) wires)

transRuleList :: [Rule] -> LUT -> String
transRuleList rules lut =
    concatMap (\r -> transRule r lut ++ "  ") rules

transActivePair :: Net -> String
transActivePair (agents, wire) = 
  let [(a1name, a1pp, a1auxs) ,(a2name, a2pp, a2auxs)] = agents
      a1auxsList       = listPorts a1auxs
      a2auxsList       = listPorts a2auxs
      a1auxsListString = "(" ++ a1auxsList ++ ")" -- INPLA needs empty parentheses for lower case agent symbols (but can call them wihtout eg. need eps()><Z but can do eps~Z)
      a2auxsListString = if null a2auxsList then "" else "(" ++ a2auxsList ++ ")"             
  in
    a1name ++ a1auxsListString ++ " >< " ++ a2name ++ a2auxsListString ++ " => "

flatPar :: Term -> [Term]
flatPar (Par t1 t2) = flatPar t1 ++ flatPar t2
flatPar t           = [t]

--- **** 
tr :: LUT -> String -> Either ParseError Net
tr lut term =
  case parseTerm term of
    Left err -> Left err
    Right t  -> Right (cleanNet $ trans t "r" ([], []) lut) 

inpla :: LUT -> String -> Either ParseError String
inpla lut term =
  case tr lut term of
    Left err  -> Left err
    Right net -> Right (netToINPLA net)

-- net = ([("dup","s",["x1","x2"]),("S","rs",["rt"]),("S","ru",["rv"])],
--        [("t","s"),("x","t"),("x1","rtru"),("rtru","rt"),("x2","rvrw"),("rvrw","rv")])
-- (a,w) = (fst net, snd net)

-- example
-- funcPortInfoLUT = [("add",1), ("dup",2), ("inc",1), ("eps",0)] :: LUT
-- rules = [Rule (Func "add" [Constr "Z" [],Var "y"]) (Var "y"), 
--          Rule (Func "dup" [Constr "Z" []]) (Par (Constr "Z" []) (Constr "Z" [])),
--          Rule (Func "eps" [Constr "Z" []]) Empty]
--- **** 


-- ensure correct interface LHS and RHS by fixing up RHS
-- syncOutputs :: Net -> Net -> LUT -> Net
-- syncOutputs lhs rhs lut =
--   let 
--       func        = head $ fst lhs
--       (fName,_,_) = func
--       numOuts     = funcNumOuts fName lut
--   in
--       if numOuts < 2 then rhs
--       else -- need to deal only with output ports
--       ---- I think only way to do this is to rewrite let rule and do it all manually 
--         let
--           (_,_,auxPorts) = func
--           lhsAgent = [(fName,head auxPorts,take numOuts auxPorts)] -- strip out unwanted info; set pp to an aux so removed by "nub" (next line)
--           p1 = nub $ inLHSonly (lhsAgent,[]) rhs
--           p2 = nub $ unconnectedPorts rhs
--           p3 = notInLHS (lhsAgent,[]) p2
--           (rhsAgents, rhsWires) = rhs 
--         in (replacePortAgents rhsAgents (zip p1 p3), replacePortsWires rhsWires p1 p3)

inLHSonly :: Net -> Net -> [String]
inLHSonly (lhsAgents, lhsWires) (rhsAgents, rhsWires) =
  let lhsPorts = map (\(_,pp,aux) -> pp:aux) lhsAgents ++ [[a,b] | (a,b) <- lhsWires]
      rhsPorts = map (\(_,pp,aux) -> pp:aux) rhsAgents ++ [[a,b] | (a,b) <- rhsWires]
  in filter (`notElem` concat rhsPorts) (concat lhsPorts)

uniqueOnly :: Eq a => [a] -> [a]
-- if an element occurs more than once, remove it. So only unique elements remain.
uniqueOnly xs = [x | x <- nub xs, count x xs == 1]
  where
    count y = length . filter (== y)

unconnectedPorts :: Net -> [Port]
unconnectedPorts (agents, wires) =
  let agentPorts = concatMap (\(_,pp,aux) -> pp:aux) agents
      wirePorts  = concatMap (\(a,b) -> [a,b]) wires
      attempt1 =  filter (`notElem` wirePorts) agentPorts
      -- attempt2 = unconnectedPortsAgents (agents,wires)
  in uniqueOnly attempt1

unconnectedPortsAgents :: Net -> [Port]
unconnectedPortsAgents (agents, wires) = uniqueOnly $ concatMap portList agents

portList :: Agent -> [Port]
portList (symbol,pp,auxList) = pp:auxList

notInLHS :: Net -> [String] -> [String]
notInLHS (lhsAgents, lhsWires) ports =
  let lhsPorts = concatMap (\(_,pp,aux) -> pp:aux) lhsAgents ++ concatMap (\(a,b) -> [a,b]) lhsWires
  in filter (`notElem` lhsPorts) ports

-- replacePortAgent :: Agent -> Port -> Port -> [Agent]
-- replacePortAgent (symbol, pp, aux) new old =
--   let pp'  = if pp == old then new else pp
--       aux' = map (\p -> if p == old then new else p) aux
--   in (symbol, pp', aux')

replacePortAgents :: [Agent] -> [(Port,Port)] -> [Agent]
replacePortAgents []          _        = []
replacePortAgents (ag:agents) portList =
  (replacePortAgents1 ag portList) : (replacePortAgents agents portList) 

replacePortAgents1 :: Agent -> [(Port,Port)] -> Agent
replacePortAgents1 agent []       = agent
replacePortAgents1 agent portList =
  let ((new,old):rest) = portList 
  in replacePortAgents1 (replacePortAgent agent (new,old)) rest

replacePortAgent :: Agent -> (Port,Port) -> Agent
replacePortAgent (symbol, pp, aux) (new,old) =
  let pp'  = if pp == old then new else pp
      aux' = map (\p -> if p == old then new else p) aux
  in (symbol, pp', aux')




-- replacePortAgents :: [Agent] -> [Port] -> [Port] -> [Agent]
-- replacePortAgents []          _    _    = []
-- replacePortAgents (ag:agents) news olds = 
--   map (replacePortAgent 
  -- zip news olds
   


  -- | length olds /= length news = error "replacePortAgents: lists must have equal length"
  -- | otherwise = foldl (\ags o n -> map (\a -> replacePortAgent a n o) ags) agents olds news

replacePortsWires :: [Wire] -> [Port] -> [Port] -> [Wire]
replacePortsWires wires olds news
  | length olds /= length news = error "replacePortsWires: lists must have equal length"
  | otherwise = foldl (\ws (o,n) -> map (\(a,b) -> (if a==o then n else a, if b==o then n else b)) ws) wires (zip olds news)

