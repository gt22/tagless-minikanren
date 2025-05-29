{-# LANGUAGE TemplateHaskell #-}
module TFKanren.Utils.Template.TypeTemplate(generateLogicType, module TFKanren.Utils.Type, sequenceA_) where

import TFKanren.Core.Internal.Logic as L
import TFKanren.Utils.Type
import Language.Haskell.TH as TH
import Debug.Trace 
import Data.List (singleton)
import Control.Monad (replicateM)
import Data.Foldable (sequenceA_)
import qualified Data.Set as Set
import Data.Char (toLower)

asBang :: Q Type -> Q BangType
asBang = bangType (bang noSourceUnpackedness noSourceStrictness)

data TypeInfo = TypeInfo {
        typeName :: Name,
        boundVars :: [Name],
        constructors :: [ConstructorInfo]
    } deriving Show

data ConstructorInfo = ConstructorInfo {
        constructorName :: Name,
        constructorFields :: [Type]
    } deriving Show

data GenerateAccessors = GenerateAccessors | NoGenerateAccessors deriving (Eq, Show)

generateLogicType :: Name -> Q [Dec]
generateLogicType = generateLogicType_ GenerateAccessors

generateLogicType_ :: GenerateAccessors -> Name -> Q [Dec]
generateLogicType_ acc underlyingType = do
    (Just t) <- parseTypeInfo <$> TH.reify underlyingType
    concat <$> sequence 
        [ if acc == GenerateAccessors then makeAccessors t else pure []
        , makeInstance t
        ]

makeNames :: ConstructorInfo -> String -> Q [Name]
makeNames c n = replicateM (length (constructorFields c)) (newName n)

logicCons :: ConstructorInfo -> Name
logicCons n = mkName $ "Logic" ++ nameBase (constructorName n)

fullType :: TypeInfo -> Q Type
fullType t = foldl appT (conT $ typeName t) (varT <$> boundVars t)

makeAccessors :: TypeInfo -> Q [Dec]
makeAccessors t = sequence $ concatMap (\c -> [makeTypeSig c, makeAccessor c]) $ constructors t
    where

        makeTypeSig c = do
            let var = mkName "var"
            sigD (accessorName c) $ foldr (\a b -> [t| Logic $(pure a) $(varT var) -> $b |]) [t| Logic $(fullType t) $(varT var) |] (constructorFields c)

        makeAccessor c = do
            names <- makeNames c "x"
            funD (accessorName c) [clause (varP <$> names) (normalB $ [| Ground $(constructTerm c names) |]) []]

        constructTerm c names = foldl appE (conE $ logicCons c) (varE <$> names)

        accessorName c = mkName $ case nameBase (constructorName c) of
            (x:xs) -> toLower x : xs
            [] -> error "Empty constructor name"
makeInstance :: TypeInfo -> Q [Dec]
makeInstance t = singleton <$> (instanceD logicConstraints [t| LogicType $(fullType t) |] makeBody)
    where
        var = VarT $ mkName "var"

        logicConstraints = sequence ((\v -> [t| LogicType $(varT v) |]) <$> boundVars t)
        
        makeBody = 
            [ makeLogicType
            , makePragma 'project, makeProject
            , makePragma 'L.reify, makeReify
            , makePragma 'quote, makeQuote
            , makePragma 'unifyVal, makeUnify
            , makePragma 'derefVal, makeDeref
            , makePragma 'generate, makeGenerate
            ]

        makeLogicType = dataInstD (pure []) ''WithLogic [fullType t, pure var] Nothing (makeLogicConstructor <$> constructors t) []
            where
                makeLogicConstructor c = normalC (logicCons c) (logicField <$> constructorFields c)
 
                logicField x = asBang [t| Logic $(pure x) $(pure var) |]

        makeProject = funD 'project (projectClause <$> constructors t)
            where
                projectClause c = do
                    names <- makeNames c "x"
                    clause [conP (constructorName c) $ varP <$> names] (normalB $ projectBody c names) []
                
                projectBody c names = foldl appE (conE $ logicCons c) $ (\v -> [| Ground (project $(varE v)) |]) <$> names

        makeReify = funD 'L.reify $ (reifyClause <$> constructors t) ++ [nothingClause]
            where
                reifyClause c = do
                    names <- makeNames c "x"
                    clause [conP (logicCons c) $ (\v -> [p| Ground ($(varP v)) |]) <$> names] (normalB $ reifyBody c names) []
                
                reifyBody c names = foldl (\a b -> [| $a <*> $b |]) [| Just $(conE $ constructorName c) |] $ (\v -> [| L.reify $(varE v) |]) <$> names
                nothingClause = clause [wildP] (normalB [| Nothing |]) []

        makeQuote = funD 'quote (quoteClause <$> constructors t)
            where
                quoteClause c = do
                    names <- makeNames c "x"
                    clause [conP (logicCons c) $ varP <$> names] (normalB $ quoteBody c names) []
                
                quoteBody c names = foldl appE quoteBase (varE <$> names)
                    where
                        quoteBase = [| $(varE $ mkName $ "quote" ++ show (length names)) $(stringE $ nameBase $ constructorName c) $(conE $ logicCons c) |]
                
        makeUnify = funD 'unifyVal $ (unifyClause <$> constructors t) ++ [emptyClause]
            where
                unifyClause c = do
                    let unif = mkName "_unif"
                    x_names <- makeNames c "x"
                    y_names <- makeNames c "y"
                    clause [varP unif, logicPat c x_names, logicPat c y_names] (normalB $ unifyBody unif x_names y_names) []
                
                logicPat c names = conP (logicCons c) $ varP <$> names

                unifyBody unif x_names y_names = [| sequenceA_ $(listE $ unifyElem <$> (zip x_names y_names)) |]
                    where
                        unifyElem (x, y) = [| $(varE unif) $(varE x) $(varE y) |]
                
                emptyClause = clause [wildP, wildP, wildP] (normalB [| empty |]) []
        makeDeref = funD 'derefVal (derefClause <$> constructors t)
        derefClause c = do
            let deref = mkName "_deref"
            names <- makeNames c "x"
            clause [varP deref, conP (logicCons c) $ varP <$> names] (normalB $ derefBody deref names) []
            where
                derefBody deref names = foldl (\a b -> [| $a <*> $b |]) [| pure $(conE $ constructorName c) |] $ (\v -> [| $(varE deref) $(varE v) |]) <$> names

        makeGenerate = funD 'generate [clause [] (normalB generateBody) []]
            where
                generateBody = [| asum $(listE $ generateCon <$> constructors t) |]
                generateCon c = foldl (\a b -> [| $a <*> $b |]) [|pure $(conE $ constructorName c)|] $ (replicate (length (constructorFields c)) [| generate |])


        makePragma n = pragInlD n Inlinable FunLike AllPhases
parseTypeInfo :: Info -> Maybe TypeInfo
parseTypeInfo (TyConI (DataD _ name_ vars_ _ cons_ _)) = TypeInfo name_ (unpackVar <$> vars_) <$> sequence (parseConstructor <$> cons_)
    where
        unpackVar (PlainTV n _) = n
        unpackVar (KindedTV n _ _) = n
parseTypeInfo (TyConI (NewtypeD cxt_ name_ vars_ kind_ con_ deriv_)) = parseTypeInfo (TyConI (DataD cxt_ name_ vars_ kind_ [con_] deriv_))
parseTypeInfo _ = Nothing

parseConstructor :: Con -> Maybe ConstructorInfo
parseConstructor (NormalC name_ fields_) = Just $ ConstructorInfo name_ (map snd fields_)
parseConstructor (RecC name_ fields_) = Just $ ConstructorInfo name_ (map (\(_, _, t) -> t) fields_)
parseConstructor _ = Nothing