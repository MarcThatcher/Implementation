module Types where
import Parser
import Data.Maybe (isJust)

data Type
  = Bot
  | VarType    String
  | FuncType   [Type] Type   -- [input types] -> output type
  | ConstrType String [Type] -- type name and parameter type names  
  | ParType    [Type]
  | UserType   String
  deriving (Show, Eq)

type TypeDecl = [(String,Int)] -- name of type, number parameters
type Context  = [(String,Type)]

userTypes   = [("Nat",0), ("Bool",0), ("List",1), ("Pair",2)]
-- context = X+F+C
constrTypes = [("Z", ConstrType "Nat" []), 
               ("S", ConstrType "Nat" [ConstrType "Nat" []]),
               ("Nil", ConstrType "List" []),
               ("Cons", ConstrType "List" [ConstrType "Nat" [], ConstrType "List" [ConstrType "Nat" []]]),
               ("True", ConstrType "Bool" []),
               ("False", ConstrType "Bool" []) ]
funcTypes   = [("add", FuncType [ConstrType "Nat" [], ConstrType "Nat" []] (ConstrType "Nat" [])), 
               ("isZ", FuncType [ConstrType "Nat" []] (ConstrType "Bool" [])),
               ("e", FuncType [VarType "A"] Bot),
               ("d", FuncType [VarType "A"] (ParType [VarType "A", VarType "A"]))]
varTypes    = [("x", VarType "A"), ("x1", VarType "B"), ("x2", VarType "C"),
               ("y", VarType "D"), ("y1", VarType "E"), ("y2", VarType "F"),
               ("z", VarType "G"), ("z1", VarType "H"), ("z2", VarType "I")
              ]

d :: String -> Maybe Type
d string = let Right term = parseTerm string in deriveType term

deriveType :: Term -> Maybe Type
deriveType term = derive term (funcTypes ++ constrTypes ++ varTypes)

derive :: Term -> Context -> Maybe Type
-- term then context
derive Empty        _              = Just Bot
derive (Var x)             context = typeVar x context
derive (Constr name terms) context = deriveConstr name terms context
derive (Func name terms)   context = deriveFunc name terms context
derive (Let t vars body)   context = deriveLet t vars body context
derive (Par t1 t2)         context = derivePar t1 t2 context

typeVar :: String -> Context -> Maybe Type
-- can just return type based on variable name as variable names unique
typeVar x context = lookUpType x context

deriveConstr :: ConstrName -> [Term] -> Context -> Maybe Type 
deriveConstr constr paramList context = case paramList of
    -- nullary
    []      -> lookUpType constr context
    -- unary
    [p1]    -> do
                ConstrType typeName paramTypes <- lookUpType constr context
                derivedType                    <- derive p1 context
                case paramTypes of
                    [paramType] -> if isJust (unify (derivedType, paramType))
                                   then Just (ConstrType typeName [])
                                   else Nothing
                    _ -> Nothing
    -- binary
    [p1,p2] -> do
            ConstrType typeName paramTypes <- lookUpType constr context
            derivedType1 <- derive p1 context
            derivedType2 <- derive p2 context
            case paramTypes of
                [paramType1, paramType2] -> if isJust (unify (derivedType1, paramType1)) && 
                                               isJust (unify (derivedType2, paramType2))
                                            then Just (ConstrType typeName [])
                                            else Nothing
                _                        -> Nothing
    _       -> error "More than 2 parameters to constructor. Please don't do that."

deriveFunc :: FuncName -> [Term] -> Context -> Maybe Type
deriveFunc func paramList context = case paramList of
    [p1]    -> do
            FuncType [inputType] outputType <- lookUpType func context
            derivedType                     <- derive p1 context
            case unify (inputType, derivedType) of
                Nothing   -> Nothing
                Just subs -> Just (sub (outputType, subs))
    [p1,p2] -> do
            FuncType [inputType1, inputType2] outputType <- lookUpType func context
            derivedType1 <- derive p1 context
            derivedType2 <- derive p2 context
            case (unify (inputType1, derivedType1), unify (inputType2, derivedType2)) of
                (Just subs1, Just subs2) -> Just (sub (sub (outputType, subs1), subs2))
                _ -> Nothing
    _       -> error "More than 2 parameters to function. Please don't do that."

deriveLet :: Term -> [VarName] -> Term -> Context -> Maybe Type
deriveLet t1 vars t2 context = do
    type1 <- derive t1 context
    let argTypes = case type1 of
            ParType ts -> ts
            t -> [t]
        newContext      = zip vars argTypes
        -- need to update content with newContent
        filteredContext = filter (\(k, _) -> k `notElem` map fst newContext) context
        combinedContext = newContext ++ filteredContext
    type2 <- derive t2 combinedContext
    case type2 of
        Bot                            -> Just Bot
        VarType a                      -> Just (VarType a) 
        FuncType inputTypes outputType -> 
            if argTypes == inputTypes
               then Just outputType
               else Nothing
        ConstrType c p                 -> Just (ConstrType c p)
        ParType p                      -> Just (ParType p)
        UserType u                     -> Just (UserType u)

derivePar :: Term -> Term -> Context -> Maybe Type
derivePar t1 t2 context = do
    type1 <- derive t1 context
    type2 <- derive t2 context
    return (ParType [type1, type2])

lookUpType :: String -> Context -> Maybe Type
lookUpType name typeList = lookup name typeList

inputs :: Type -> [Type]
inputs Bot                   = []
inputs (VarType x)           = [VarType x]
inputs (FuncType params _)   = params
inputs (ConstrType _ params) = params
inputs (ParType types)       = concatMap inputs types
inputs (UserType t)          = []

unify :: (Type,Type) -> Maybe [(Type,Type)]
unify (Bot,_)               = Nothing
unify (_,Bot)               = Nothing
unify (VarType a,VarType b) = if a==b then Just [(VarType a,VarType b)] else Just [(VarType b,VarType a)]
unify (VarType a, t)        = Just [(t, VarType a)]   -- should have occurs check here
unify (t, VarType a)        = Just [(t, VarType a)]
unify (ConstrType c1 c1param, ConstrType c2 c2param) 
    | c1 /= c2 = Nothing
    | length c1param /= length c2param = Nothing
    | length c1param == 0 = Just []
    | length c1param == 1 = 
        let [t1] = c1param
            [t2] = c2param
        in unify (t1, t2)
    | length c1param == 2 =
        let [t1,t2] = c1param
            [t3,t4] = c2param
        in do
            s1 <- unify (t1, t3)
            s2 <- unify (sub (t2, s1), sub (t4, s1))
            return (subUnifiers (s2, s1))
    | otherwise = Nothing

sub :: (Type, [(Type,Type)]) -> Type
sub (t, subs) = foldl (\acc s -> subOne (acc, s)) t subs

subOne :: (Type, (Type,Type)) -> Type
-- do single sub
subOne (Bot, _)                                     = Bot
subOne (VarType x,(tau, VarType y))                 = if x==y then tau else VarType x
subOne (FuncType inputTypes outputType, sub)        = FuncType (map (\t -> subOne (t, sub)) inputTypes) 
                                                               (subOne (outputType, sub))
subOne (ConstrType name paramTypes, sub)            = ConstrType name (map (\t -> subOne (t, sub)) paramTypes)
subOne (ParType types, sub)                         = ParType (map (\t -> subOne (t, sub)) types)
subOne (UserType t, _)                              = UserType t

subUnifiers :: ([(Type,Type)],[(Type,Type)]) -> [(Type,Type)]
subUnifiers (pairs, subs) = 
    let pairs' = map (\(t1, t2) -> (sub (t1, subs), sub (t2, subs))) pairs
    in if pairs' == pairs 
       then pairs ++ subs
       else pairs'