module Types where
import Parser

-- note this uses "base" FLIN! So let has [,] not {,} and no attributes or syntactic sugar

data Type
  = Bot
  | VarType    String
  | FuncType   [Type] Type   -- [input types] -> output type
  | ConstrType String [Type] -- type name and parameter type names  
  | ParType    [Type]
  | UserType   String
  deriving (Show, Eq)

type TypeDecl = [(String,Int)]      -- name of type, number parameters
type FCDecl   = [(String,Type)]     -- function/constructor name, type

userTypes   = [("Nat",0), ("Bool",0), ("List",1), ("Pair",2)]
constrTypes = [("Z", ConstrType "Nat" []), 
               ("S", ConstrType "Nat" [ConstrType "Nat" []]),
               ("Nil", ConstrType "List" [ConstrType "Nat" []]),
               ("Cons", ConstrType "List" [ConstrType "Nat" [], ConstrType "List" [ConstrType "Nat" []]])]
funcTypes   = [("add", (FuncType [ConstrType "Nat" [], ConstrType "Nat" []]) (ConstrType "Nat" [])), 
               ("isZ", (FuncType [ConstrType "Nat" []]) (ConstrType "Bool" [])),
               ("e", (FuncType [ConstrType "Nat" []]) Bot),
               ("d", (FuncType [ConstrType "Nat" []]) (ParType [ConstrType "Nat" [], ConstrType "Nat" []]))
              ]


derive :: Term -> FCDecl -> FCDecl -> Maybe Type
-- term then function type list then construtor type list
-- Nothing means fails
derive Empty               _         _         = Just Bot
derive (Var x)             _         _         = Just (deriveVar x)
derive (Constr name terms) fTypeList cTypeList = deriveConstr name terms fTypeList cTypeList 
derive (Func name terms)   fTypeList cTypeList = deriveFunc   name terms fTypeList cTypeList
derive (Let t vars body)   fTypeList cTypeList = deriveLet    t vars body fTypeList cTypeList
derive (Par t1 t2) fTypeList cTypeList         = derivePar t1 t2 fTypeList cTypeList

deriveVar :: String -> Type
-- can just return type based on variable name as variable names unique
deriveVar x = ConstrType "Nat" []
--VarType ('T':x)

deriveConstr :: ConstrName -> [Term] -> FCDecl -> FCDecl -> Maybe Type 
deriveConstr constr paramList fTypeList cTypeList = case paramList of
    -- nullary
    []      -> lookUpType constr cTypeList
    -- unary
    [p1]    -> do
                ConstrType typeName [paramType] <- lookUpType constr cTypeList
                derivedType                     <- derive p1 fTypeList cTypeList
                if derivedType == paramType
                    then Just (ConstrType typeName [])
                    else Nothing
    -- binary
    [p1,p2] -> do
                ConstrType typeName paramTypes <- lookUpType constr cTypeList
                derivedType1 <- derive p1 fTypeList cTypeList
                derivedType2 <- derive p2 fTypeList cTypeList
                case paramTypes of
                    [paramType1, paramType2] -> if derivedType1 == paramType1 && derivedType2 == paramType2
                                                then Just (ConstrType typeName paramTypes)
                                                else Nothing
                    _                        -> Nothing

deriveFunc :: FuncName -> [Term] -> FCDecl -> FCDecl -> Maybe Type
deriveFunc func paramList fTypeList cTypeList = case paramList of
    [p1]    -> do
                FuncType inputTypes outputTypes <- lookUpType func fTypeList
                derivedType                     <- derive p1 fTypeList cTypeList
                case inputTypes of
                    [paramType] -> if derivedType == paramType
                                   then Just outputTypes
                                   else Nothing
                    _           -> Nothing
    [p1,p2] -> do
                FuncType inputTypes outputType <- lookUpType func fTypeList
                derivedType1 <- derive p1 fTypeList cTypeList
                derivedType2 <- derive p2 fTypeList cTypeList
                case inputTypes of
                    [paramType1, paramType2] -> if derivedType1 == paramType1 && derivedType2 == paramType2
                                                then Just outputType
                                                else Nothing
                    _                        -> Nothing
    _       -> Nothing 

deriveLet :: Term -> [VarName] -> Term -> FCDecl -> FCDecl -> Maybe Type
deriveLet t1 vars t2 fTypeList cTypeList = do
    type1 <- derive t1 fTypeList cTypeList
    type2 <- derive t2 fTypeList cTypeList
    case type2 of
        FuncType inputTypes outputType -> 
            let argTypes = case type1 of
                    ParType ts -> ts
                    t          -> [t]
            in 
            --
                Just outputType
            -- if argTypes == inputTypes
            --    then 
               
            --    else Nothing
        _ -> Just type2

derivePar :: Term -> Term -> FCDecl -> FCDecl -> Maybe Type
derivePar t1 t2 fTypeList cTypeList = do
    type1 <- derive t1 fTypeList cTypeList
    type2 <- derive t2 fTypeList cTypeList
    return (ParType [type1, type2])

lookUpType :: String -> FCDecl -> Maybe Type
lookUpType name typeList = lookup name typeList

inputs :: Type -> [Type]
inputs Bot                   = []
inputs (VarType x)           = [VarType x]
inputs (FuncType params _)   = params
inputs (ConstrType _ params) = params
inputs (ParType types)       = concatMap inputs types
inputs (UserType t)          = [    ]

outputs :: Type -> [Type]
outputs Bot                   = []
outputs (VarType x)           = [VarType x]
outputs (FuncType _ result)   = [result]
outputs (ConstrType c _)      = [UserType c]
outputs (ParType types)       = concatMap outputs types
outputs (UserType t)          = [UserType t]

-- derive (Constr "S" [Constr "Z" []]) funcTypes constrTypes
-- derive (Constr "Z" []) funcTypes constrTypes
-- derive (Func "isZ" [Constr "Z" []]) funcTypes constrTypes
-- derive (Func "add" [Constr "Z" [],Constr "Z" []]) funcTypes constrTypes
-- derive (Let (Func "d" [Var "x"]) ["x1","x2"] (Func "add" [Var "x1",Var "x2"])) funcTypes constrTypes
-- derive (Let (Func "d" [Var "x"]) ["x1","x2"] (Par (Constr "S" [Var "x1"]) (Constr "S" [Var "x2"]))) funcTypes constrTypes
-- derive (Let (Constr "Z" []) ["x"] (Var "x")) funcTypes constrTypes