{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Class.Closed.TH where

import Data.Class.Closed          (Closed)
import Data.Maybe                 (fromMaybe)
import Data.Proxy                 (Proxy (..))
import Language.Haskell.TH        (Dec (..), Name, Q, TyVarBndr (..), Type (..),
                                   appT, classD, clause, conT, defaultSigD,
                                   funD, instanceWithOverlapD, nameBase,
                                   newName, normalB, sigD, varT)
import Language.Haskell.TH.Syntax (getQ, putQ)

-- | Closes all declared classes. Any instances must be given in the same quote.
close ∷ Q [Dec] -> Q [Dec]
close = (>>= mapM modify) where
  modify ∷ Dec → Q Dec
  modify (ClassD context name tvs deps decs) = do
    let con = conT name
        ty  = foldl (\t b → appT t (binderT b)) con tvs
    private ← getPrivateName name
    classD (pure context) name tvs deps                  -- class ... ⇒ C ... where
      $ defaultSigD private [t|Closed $con ⇒ Proxy $ty|] --   default private ∷ Closed C ⇒ Proxy (C ...)
      : sigD private [t|Proxy $ty|]                      --   private ∷ Proxy (C ...)
      : funD private [clause [] (normalB [|Proxy|]) []]  --   private = Proxy
      : map pure decs                                    --   ...
  modify (InstanceD overlap context ty@(conName → Just name) decs) = do
    private ← getPrivateName name
    instanceWithOverlapD overlap (pure context) (pure ty) -- instance ... where
      $ funD private [clause [] (normalB [|Proxy|]) []]   --   private = Proxy
      : map pure decs                                     --   ...
  modify dec = pure dec

#if MIN_VERSION_template_haskell(2, 17, 0)
  binderT ∷ TyVarBndr flag → Q Type
  binderT (PlainTV  name _)   = varT name
  binderT (KindedTV name _ _) = varT name
#else
  binderT ∷ TyVarBndr → Q Type
  binderT (PlainTV  name)   = varT name
  binderT (KindedTV name _) = varT name
#endif

  getPrivateName ∷ Name → Q Name
  getPrivateName name = do
    cache ← fromMaybe [] <$> getQ
    case lookup name cache of
      Just private → return private
      Nothing      → do
        temp    ← newName ("_" ++ nameBase name)
        private ← newName (show temp)
        putQ ((name, private):cache)
        return private

  conName ∷ Type → Maybe Name
  conName (ConT n)        = Just n
  conName (AppT x _)      = conName x
  conName (AppKindT x _)  = conName x
  conName (SigT x _)      = conName x
  conName (InfixT _ n _)  = Just n
  conName (UInfixT _ n _) = Just n
  conName (ParensT x)     = conName x
  conName _               = Nothing
