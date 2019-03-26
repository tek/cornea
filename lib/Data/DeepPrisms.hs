{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.DeepPrisms where

import Control.Lens (Prism', makeClassyPrisms)
import qualified Control.Lens as Lens (preview, review)
import Control.Monad (filterM)
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype (
  ConstructorInfo(constructorName, constructorFields),
  DatatypeInfo(datatypeCons),
  reifyDatatype,
  )
import Language.Haskell.TH.Syntax (
  ModName(..),
  Name(Name),
  NameFlavour(NameQ, NameS, NameG),
  NameSpace(VarName),
  OccName(..),
  )

class DeepPrisms e e' where
  prism :: Prism' e e'

hoist :: DeepPrisms e e' => e' -> e
hoist =
  Lens.review prism

retrieve :: DeepPrisms e e' => e -> Maybe e'
retrieve =
  Lens.preview prism

data Ctor =
  Ctor {
    ctorName :: Name,
    ctorType :: Name
  }
  deriving (Eq, Show)

data SubError =
  SubError {
    seCtor :: Name,
    seWrapped :: Name
  }
  deriving (Eq, Show)

data PrismsInstance =
  PrismsInstance {
    prismInstanceName :: Name,
    prismInstanceDec :: Dec
  }
  deriving (Eq, Show)

ctor :: ConstructorInfo -> Maybe Ctor
ctor info =
  cons (constructorFields info)
  where
    cons [ConT tpe] =
      Just $ Ctor (constructorName info) tpe
    cons _ =
      Nothing

dataType :: Name -> Q [Ctor]
dataType =
  fmap (mapMaybe ctor . datatypeCons) . reifyDatatype

mkHoist :: TypeQ -> TypeQ -> BodyQ -> DecQ
mkHoist _ _ body = do
  (VarE name) <- [|prism|]
  funD name [clause [] body []]

deepPrismsInstance :: TypeQ -> TypeQ -> BodyQ -> DecQ
deepPrismsInstance top local body =
  instanceD (cxt []) (appT (appT [t|DeepPrisms|] top) local) [mkHoist top local body]

idInstance :: Name -> DecQ
idInstance name =
  deepPrismsInstance nt nt body
  where
    nt = conT name
    body = normalB [|id|]

typeHasDeepPrisms :: Ctor -> Q Bool
typeHasDeepPrisms (Ctor _ tpe) = do
  (ConT name) <- [t|DeepPrisms|]
  isInstance name [ConT tpe, ConT tpe]

modName :: NameFlavour -> Maybe ModName
modName (NameQ mod') =
  Just mod'
modName (NameG _ _ mod') =
  Just mod'
modName _ =
  Nothing

sameModule :: NameFlavour -> NameFlavour -> Bool
sameModule f1 f2 =
  case (modName f1, modName f2) of
    (Just a, Just b) | a == b -> True
    _ -> False

-- |Convert a constructor's NameFlavour to one for a prism
-- The NameSpace field is DataName for the constructor and must be VarName
-- Curiously, this only surfaces as a bug when having a certain nesting level across modules
prismFlavour :: NameFlavour -> NameFlavour
prismFlavour (NameG _ pkg mod') =
  NameG VarName pkg mod'
prismFlavour n =
  n

prismName :: Name -> Name -> ExpQ
prismName (Name _ topFlavour) (Name (OccName n) localFlavour) =
  varE (Name (OccName ('_' : n)) flavour)
  where
    flavour
      | sameModule topFlavour localFlavour = NameS
      | otherwise = prismFlavour localFlavour

constructorPrism :: Name -> [Name] -> Ctor -> Q PrismsInstance
constructorPrism top intermediate (Ctor name tpe) = do
  inst <- deepPrismsInstance (conT top) (conT tpe) (normalB body)
  return (PrismsInstance tpe inst)
  where
    compose = appE . appE [|(.)|] . prismName top
    body = foldr compose (prismName top name) (reverse intermediate)

filterDuplicates :: [Ctor] -> [PrismsInstance] -> [PrismsInstance]
filterDuplicates created = filter (not . (`elem` (ctorType <$> created)) . prismInstanceName)

prismsForData :: Name -> [Name] -> Name -> Q [PrismsInstance]
prismsForData top intermediate local = do
  cons <- filterM typeHasDeepPrisms =<< dataType local
  localInstances <- traverse (constructorPrism top intermediate) cons
  deepInstances <- traverse recurse cons
  return (localInstances ++ (deepInstances >>= filterDuplicates cons))
  where
    recurse (Ctor name tpe) = prismsForData top (name : intermediate) tpe

prismsForMainData :: Name -> DecsQ
prismsForMainData name = do
  idInst <- idInstance name
  insts <- prismsForData name [] name
  return (idInst : (prismInstanceDec <$> insts))

deepPrisms :: Name -> DecsQ
deepPrisms name = do
  basic <- makeClassyPrisms name
  deep <- prismsForMainData name
  return $ basic ++ deep
