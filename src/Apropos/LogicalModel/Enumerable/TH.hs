module Apropos.LogicalModel.Enumerable.TH (
  gen_enumerable,
) where

import Language.Haskell.TH

-- cribbed from here https://wiki.haskell.org/Template_haskell/Instance_deriving_example

gen_enumerable :: Name -> Q [Dec]
gen_enumerable typName = do
  (TyConI d) <- reify typName
  (type_name, _, _, constructors) <- typeInfo (return d)
  i_dec <-
    gen_instance
      (mkName "Enumerable")
      (conT type_name)
      constructors
      (mkName "enumerated", gen_enumerated)
  return [i_dec]
  where
    gen_enumerated = appE (varE (mkName "join")) . listE

type Constructor = (Name, [(Maybe Name, Type)])
type Function_body = ExpQ
type Gen_func = [ExpQ] -> Function_body
type Func_name = Name
type Func = (Func_name, Gen_func)

gen_instance :: Name -> TypeQ -> [Constructor] -> Func -> DecQ
gen_instance class_name for_type constructors func =
  instanceD
    (cxt [])
    (appT (conT class_name) for_type)
    [func_def func]
  where
    func_def (func_name, gen_func) =
      funD
        func_name
        [(gen_clause gen_func) constructors]

gen_clause :: ([ExpQ] -> ExpQ) -> [Constructor] -> ClauseQ
gen_clause func_body data_cons =
  do
    let comprehensions = map makeComprehension data_cons
    (clause [] (normalB (func_body comprehensions)) [])
  where
    componentName (i, _) = mkName $ "f" <> show i
    makeComprehension (con_name, components) =
      compE (mkComp con_name (componentName <$> (zip ([0 ..] :: [Int]) components)))
    mkComp con_name components =
      reverse ((noBindS $ appCon con_name components) : (mkBinds con_name components))
    appCon con_name [] = conE con_name
    appCon con_name (e : es) = appE (appCon con_name es) (varE e)
    mkBinds _ [] = []
    mkBinds con_name (e : es) =
      (bindS (varP e) (varE $ mkName "enumerated")) : (mkBinds con_name es)

typeInfo :: DecQ -> Q (Name, [Name], [(Name, Int)], [(Name, [(Maybe Name, Type)])])
typeInfo m =
  do
    d' <- m
    case d' of
      d@(DataD _ _ _ _ _ _) ->
        return $ (simpleName $ name d, paramsA d, consA d, termsA d)
      d@(NewtypeD _ _ _ _ _ _) ->
        return $ (simpleName $ name d, paramsA d, consA d, termsA d)
      _ -> error ("derive: not a data type declaration: " ++ show d')
  where
    consA (DataD _ _ _ _ cs _) = map conA cs
    consA (NewtypeD _ _ _ _ c _) = [conA c]
    consA d = error $ show d

    paramsA (DataD _ _ ps _ _ _) = map nameFromTyVar ps
    paramsA (NewtypeD _ _ ps _ _ _) = map nameFromTyVar ps
    paramsA d = error $ show d

    nameFromTyVar (PlainTV a) = a
    nameFromTyVar (KindedTV a _) = a

    termsA (DataD _ _ _ _ cs _) = map termA cs
    termsA (NewtypeD _ _ _ _ c _) = [termA c]
    termsA d = error $ show d

    termA (NormalC c xs) = (c, map (\x -> (Nothing, snd x)) xs)
    termA (RecC c xs) = (c, map (\(n, _, t) -> (Just $ simpleName n, t)) xs)
    termA (InfixC t1 c t2) = (c, [(Nothing, snd t1), (Nothing, snd t2)])
    termA d = error $ show d

    conA (NormalC c xs) = (simpleName c, length xs)
    conA (RecC c xs) = (simpleName c, length xs)
    conA (InfixC _ c _) = (simpleName c, 2)
    conA d = error $ show d

    name (DataD _ n _ _ _ _) = n
    name (NewtypeD _ n _ _ _ _) = n
    name d = error $ show d

simpleName :: Name -> Name
simpleName nm =
  let s = nameBase nm
   in case dropWhile (/= ':') s of
        [] -> mkName s
        _ : [] -> mkName s
        _ : t -> mkName t
