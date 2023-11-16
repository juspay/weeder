{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}

module Weeder
  ( -- * Analysis
    Analysis(..)
  , analyseHieFiles
  , emptyAnalysis
  , allDeclarations

    -- ** Reachability
  , Root(..)
  , reachable

    -- * Declarations
  , Declaration(..)
  )
   where

-- algebraic-graphs
import Algebra.Graph ( Graph, edge, empty, overlay, vertex, vertexList )
import Algebra.Graph.ToGraph ( dfs )

-- base
import Control.Applicative ( Alternative )
import Control.Monad ( guard, msum, when, unless )
import Data.Foldable ( for_, traverse_ )
import Data.List ( intercalate )
import Data.Monoid ( First( First ), getFirst )
import GHC.Generics ( Generic )
import Prelude hiding ( span )

-- containers
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Sequence ( Seq )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Traversable ( for )

-- generic-lens
import Data.Generics.Labels ()

-- ghc
import Avail ( AvailInfo( Avail, AvailTC ) )
import FieldLabel ( FieldLbl( FieldLabel, flSelector ) )
import HieTypes
  ( BindType( RegularBind )
  -- , EvVarSource ( EvInstBind, cls )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl, ClassTyDecl )
  , DeclType( DataDec, ClassDec, ConDec, SynDec, FamDec )
  , HieAST( Node, nodeInfo, nodeChildren, nodeSpan )
  , HieASTs( HieASTs, getAsts )
  , HieFile( HieFile, hie_asts, hie_exports, hie_module, hie_hs_file )
  , IdentifierDetails( IdentifierDetails, identInfo )
  , NodeInfo( NodeInfo, nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  , IdentifierDetails( IdentifierDetails, identInfo, identType )
  , TypeIndex
  , HieTypeFix( Roll )
  , HieArgs( HieArgs )
  , HieType( HTyVarTy, HAppTy, HTyConApp, HForAllTy, HFunTy, HQualTy, HLitTy, HCastTy, HCoercionTy )
  , Span
  , Identifier
  , HieFile( HieFile, hie_asts, hie_exports, hie_module, hie_hs_file, hie_types )
  )
import Module ( Module, moduleStableString )
import Name ( Name, nameModule_maybe, nameOccName )
import OccName
  ( OccName
  , isDataOcc
  , isDataSymOcc
  , isTcOcc
  , isTvOcc
  , isVarOcc
  , occNameString
  )
import SrcLoc ( RealSrcSpan, realSrcSpanEnd, realSrcSpanStart )
import IfaceType
  ( ShowForAllFlag (ShowForAllWhen)
  , pprIfaceSigmaType
  , IfaceTyCon (IfaceTyCon, ifaceTyConName)
  )
-- lens
import Control.Lens ( (%=) )
import qualified Data.Map as M
-- mtl
import Control.Monad.State.Class ( MonadState )

-- transformers
import Control.Monad.Trans.Maybe ( runMaybeT )
import Weeder.Config ( Config( Config, typeClassRoots, unusedTypes ) )
import Control.Monad.Reader.Class ( MonadReader, asks, ask)
import HieUtils
import Control.Monad.Trans.Reader ( runReaderT )
import qualified FastString
import Outputable (showSDocUnsafe)

data Declaration =
  Declaration
    { declModule :: Module
      -- ^ The module this declaration occurs in.
    , declOccName :: OccName
      -- ^ The symbol name of a declaration.
    }
  deriving
    ( Eq, Ord )


instance Show Declaration where
  show =
    declarationStableName


declarationStableName :: Declaration -> String
declarationStableName Declaration { declModule, declOccName } =
  let
    namespace
      | isVarOcc declOccName     = "var"
      | isTvOcc declOccName      = "tv"
      | isTcOcc declOccName      = "tc"
      | isDataOcc declOccName    = "data"
      | isDataSymOcc declOccName = "dataSym"
      | otherwise                = "unknown"

    in
    intercalate "$" [ namespace, moduleStableString declModule, "$", occNameString declOccName ]


-- | All information maintained by 'analyseHieFile'.
data Analysis =
  Analysis
    { dependencyGraph :: Graph Declaration
      -- ^ A graph between declarations, capturing dependencies.
    , declarationSites :: Map Declaration ( Set RealSrcSpan )
      -- ^ A partial mapping between declarations and their definition site.
      -- This Map is partial as we don't always know where a Declaration was
      -- defined (e.g., it may come from a package without source code).
      -- We capture a set of spans, because a declaration may be defined in
      -- multiple locations, e.g., a type signature for a function separate
      -- from its definition.
    , implicitRoots :: Set Root
      -- ^ The Set of all Declarations that are always reachable. This is used
      -- to capture knowledge not yet modelled in weeder, such as instance
      -- declarations depending on top-level functions.
    , exports :: Map Module ( Set Declaration )
      -- ^ All exports for a given module.
    , modulePaths :: Map Module FilePath
      -- ^ A map from modules to the file path to the .hs file defining them.
    , prettyPrintedType :: Map Declaration String
      -- ^ Used to match against the types of instances and to replace the
      -- appearance of declarations in the output
    }
  deriving
    ( Generic )
type RefMap a = M.Map Identifier [(Span, IdentifierDetails a)]
data AnalysisInfo =
  AnalysisInfo
    { currentHieFile :: HieFile
    , weederConfig :: Config
    , refMap :: RefMap TypeIndex
    }

-- | The empty analysis - the result of analysing zero @.hie@ files.
emptyAnalysis :: Analysis
emptyAnalysis = Analysis empty mempty mempty mempty mempty mempty

data Root
  = -- | A given declaration is a root.
    DeclarationRoot Declaration
  | -- | We store extra information for instances in order to be able
    -- to specify e.g. all instances of a class as roots.
    InstanceRoot Declaration
      OccName -- ^ Name of the parent class
  | -- | All exported declarations in a module are roots.
    ModuleRoot Module
  deriving
    ( Eq, Ord )


-- | Determine the set of all declaration reachable from a set of roots.
reachable :: Analysis -> Set Root -> Set Declaration
reachable Analysis{ dependencyGraph, exports } roots =
  Set.fromList ( dfs ( foldMap rootDeclarations roots ) dependencyGraph )

  where

    rootDeclarations = \case
      DeclarationRoot d -> [ d ]
      ModuleRoot m -> foldMap Set.toList ( Map.lookup m exports )


-- | The set of all known declarations, including usages.
allDeclarations :: Analysis -> Set Declaration
allDeclarations Analysis{ dependencyGraph } =
  Set.fromList ( vertexList dependencyGraph )

analyseHieFiles :: (Foldable f, MonadState Analysis m) => Config -> f HieFile -> m ()
analyseHieFiles weederConfig hieFiles = do
  for_ hieFiles \hieFile -> do
    let info = AnalysisInfo hieFile weederConfig rf
    runReaderT analyseHieFile info

  where

    asts = concatMap (Map.elems . getAsts . hie_asts) hieFiles

    rf = generateReferencesMap asts

-- | Incrementally update 'Analysis' with information in a 'HieFile'.
analyseHieFile :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => m ()
analyseHieFile = do
  HieFile{ hie_asts = HieASTs hieASTs, hie_exports, hie_module, hie_hs_file } <- asks currentHieFile
  #modulePaths %= Map.insert hie_module hie_hs_file

  for_ hieASTs \ast -> do
    addAllDeclarations ast
    topLevelAnalysis ast

  for_ hie_exports ( analyseExport hie_module )


analyseExport :: MonadState Analysis m => Module -> AvailInfo -> m ()
analyseExport m = \case
  Avail name ->
    for_ ( nameToDeclaration name ) addExport

  AvailTC name pieces fields -> do
    for_ ( nameToDeclaration name ) addExport
    for_ pieces ( traverse_ addExport . nameToDeclaration )
    for_ fields \FieldLabel{ flSelector } -> for_ ( nameToDeclaration flSelector ) addExport

  where

    addExport :: MonadState Analysis m => Declaration -> m ()
    addExport d = #exports %= Map.insertWith (<>) m ( Set.singleton d )


-- | @addDependency x y@ adds the information that @x@ depends on @y@.
addDependency :: MonadState Analysis m => Declaration -> Declaration -> m ()
addDependency x y =
  #dependencyGraph %= overlay ( edge x y )


addImplicitRoot :: MonadState Analysis m => Declaration -> m ()
addImplicitRoot x =
  #implicitRoots %= Set.insert (DeclarationRoot x)

define :: MonadState Analysis m => Declaration -> RealSrcSpan -> m ()
define decl span =
  when ( realSrcSpanStart span /= realSrcSpanEnd span ) do
    #declarationSites %= Map.insertWith Set.union decl ( Set.singleton span )
    #dependencyGraph %= overlay ( vertex decl )


addDeclaration :: MonadState Analysis m => Declaration -> m ()
addDeclaration decl =
  #dependencyGraph %= overlay ( vertex decl )


-- | Try and add vertices for all declarations in an AST - both
-- those declared here, and those referred to from here.
addAllDeclarations :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
addAllDeclarations n = do
  Config{ unusedTypes } <- asks weederConfig
  for_ ( findIdentifiers' ( const True ) n )
    \(d, IdentifierDetails{ identType },_) -> do
      addDeclaration d
      when unusedTypes $
        case identType of
          Just t -> do
            hieType <- lookupType t
            let names = typeToNames hieType
            traverse_ (traverse_ (addDependency d) . nameToDeclaration) names
          Nothing -> pure ()

typeToNames :: HieTypeFix -> Set Name
typeToNames (Roll t) = case t of
  HTyVarTy n -> Set.singleton n

  HAppTy a (HieArgs args) ->
    typeToNames a <> hieArgsTypes args

  HTyConApp (IfaceTyCon{ifaceTyConName}) (HieArgs args) ->
    Set.singleton ifaceTyConName <> hieArgsTypes args

  HForAllTy _ a -> typeToNames a

  HFunTy b c ->
    typeToNames b <> typeToNames c

  HQualTy a b ->
    typeToNames a <> typeToNames b

  HLitTy _ -> mempty

  HCastTy a -> typeToNames a

  HCoercionTy -> mempty

  where

    hieArgsTypes :: [(Bool, HieTypeFix)] -> Set Name
    hieArgsTypes = foldMap (typeToNames . snd) . filter fst

lookupType :: MonadReader AnalysisInfo m => TypeIndex -> m HieTypeFix
lookupType t = recoverFullType t . hie_types <$> asks currentHieFile

topLevelAnalysis :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
topLevelAnalysis n@Node{ nodeChildren } = do
  Config{ unusedTypes } <- asks weederConfig
  analysed <-
    runMaybeT
      ( msum $
          [
            analyseStandaloneDeriving n
          , analyseInstanceDeclaration n
          , analyseBinding n
          , analyseRewriteRule n
          , analyseClassDeclaration n
          , analyseDataDeclaration n
          , analysePatternSynonyms n
          ] ++ if unusedTypes then
          [ analyseTypeSynonym n
          , analyseFamilyDeclaration n
          , analyseFamilyInstance n
          , analyseTypeSignature n
          ] else []
      )

  case analysed of
    Nothing ->
      -- We didn't find a top level declaration here, check all this nodes
      -- children.
      traverse_ topLevelAnalysis nodeChildren

    Just () ->
      -- Top level analysis succeeded, there's nothing more to do for this node.
      return ()

analyseStandaloneDeriving :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseStandaloneDeriving n@Node{ nodeSpan } = do
  guard $ annsContain n ("DerivDecl", "DerivDecl")
  pure ()

  -- for_ (findEvInstBinds n) \(d, cs, ids, _) -> do
  --   define d nodeSpan


  --   for_ (uses n) $ addDependency d

  --   case identType ids of
  --     Just t -> for_ cs (addInstanceRoot d t)
  --     Nothing -> pure ()

lookupPprType :: MonadReader AnalysisInfo m => TypeIndex -> m String
lookupPprType = fmap renderType . lookupType

  where

    renderType = showSDocUnsafe . pprIfaceSigmaType ShowForAllWhen . hieTypeToIface

addInstanceRoot :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => Declaration -> TypeIndex -> Name -> m ()
addInstanceRoot x t cls = do
  #implicitRoots %= Set.insert (InstanceRoot x (nameOccName cls))

  -- since instances will not appear in the output if typeClassRoots is True
  Config{ typeClassRoots } <- asks weederConfig
  unless typeClassRoots $ do
    str <- lookupPprType t
    #prettyPrintedType %= Map.insert x str

analyseTypeSynonym :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseTypeSynonym n@Node{ nodeSpan } = do
  guard $ annsContain n ("SynDecl", "TyClDecl")

  for_ ( findIdentifiers isTypeSynonym n ) $ \d -> do
    define d nodeSpan

    for_ (uses n) (addDependency d)

  where

    isTypeSynonym =
      any \case
        Decl SynDec _ -> True
        _             -> False

analyseFamilyDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseFamilyDeclaration n@Node{ nodeSpan } = do
  guard $ annsContain n ("FamDecl", "TyClDecl")

  for_ ( findIdentifiers isFamDec n ) $ \d -> do
    define d nodeSpan

    for_ (uses n) (addDependency d)

  where

    isFamDec =
      any \case
        Decl FamDec _ -> True
        _             -> False

unNodeAnnotation :: (FastString.FastString, FastString.FastString) -> (String, String)
unNodeAnnotation (x,y) = (FastString.unpackFS x, FastString.unpackFS y)

getSourceNodeInfo :: NodeInfo a -> Map String (NodeInfo a)
getSourceNodeInfo node = Map.singleton "" node

annsContain :: HieAST a -> (String, String) -> Bool
annsContain Node{ nodeInfo } ann =
  any (Set.member ann . Set.map unNodeAnnotation . nodeAnnotations) $ getSourceNodeInfo nodeInfo

analyseFamilyInstance :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseFamilyInstance n = do
  guard $ annsContain n ("TyFamInstD", "InstDecl")

  for_ ( uses n ) addImplicitRoot

analyseTypeSignature :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseTypeSignature n = do
  guard $ annsContain n ("TypeSig", "Sig")

  for_ (findIdentifiers isTypeSigDecl n) $
    for_ ( uses n ) . addDependency

  where

    isTypeSigDecl =
      any \case
        TyDecl -> True
        _      -> False

analyseBinding :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseBinding n@Node{ nodeSpan, nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard $ ( "FunBind", "HsBindLR" ) `Set.member` nodeAnnotations

  for_ ( findDeclarations n ) \d -> do
    define d nodeSpan

    for_ ( uses n ) $ addDependency d


analyseRewriteRule :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseRewriteRule n@Node{ nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "HsRule", "RuleDecl" ) `Set.member` nodeAnnotations )

  for_ ( uses n ) addImplicitRoot


analyseInstanceDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseInstanceDeclaration n@Node{ nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "ClsInstD", "InstDecl" ) `Set.member` nodeAnnotations )

  traverse_ addImplicitRoot ( uses n )


analyseClassDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseClassDeclaration n@Node{ nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "ClassDecl", "TyClDecl" ) `Set.member` nodeAnnotations )

  for_ ( findIdentifiers isClassDeclaration n ) $
    for_ ( findIdentifiers ( const True ) n ) . addDependency

  where

    isClassDeclaration =
      not . Set.null . Set.filter \case
        Decl ClassDec _ ->
          True

        _ ->
          False

analyseDataDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseDataDeclaration n = do
  guard $ annsContain n ("DataDecl", "TyClDecl")

  Config{ unusedTypes } <- asks weederConfig

  for_
    ( foldMap
        ( First . Just )
        ( findIdentifiers ( any isDataDec ) n )
    )
    \dataTypeName -> do
      when unusedTypes $
        define dataTypeName (nodeSpan n)

      -- Without connecting constructors to the data declaration TypeAliasGADT.hs 
      -- fails with a false positive for A
      conDecs <- for ( constructors n ) \constructor ->
        for ( foldMap ( First . Just ) ( findIdentifiers ( any isConDec ) constructor ) ) \conDec -> do
          addDependency conDec dataTypeName
          pure conDec

      -- To keep acyclicity in record declarations
      let isDependent d = Just d `elem` fmap getFirst conDecs

      for_ ( uses n ) (\d -> unless (isDependent d) $ addDependency dataTypeName d)

  for_ ( derivedInstances n ) \(d, cs, ids, ast) -> do
    define d (nodeSpan ast)

    -- followEvidenceUses ast d

    for_ ( uses ast ) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRoot d t)
      Nothing -> pure ()

  where

    isDataDec = \case
      Decl DataDec _ -> True
      _              -> False

    isConDec = \case
      Decl ConDec _ -> True
      _             -> False

derivedInstances :: HieAST a -> Seq (Declaration, Set Name, IdentifierDetails a, HieAST a)
derivedInstances n = findNodeTypes "HsDerivingClause" n >>= findEvInstBinds


findNodeTypes :: FastString.FastString -> HieAST a -> Seq ( HieAST a )
findNodeTypes name n@Node{ nodeChildren,  nodeInfo = NodeInfo{ nodeAnnotations } } =
  if any ( \( _, t ) -> t == name ) nodeAnnotations then
    pure n

  else
    foldMap (findNodeTypes name) nodeChildren

-- constructors n@Node { nodeChildren, nodeInfo = NodeInfo{ nodeAnnotations } } =
--   if any ( \( _, t ) -> t == "ConDecl" ) nodeAnnotations then
--     pure n

--   else
--     foldMap constructors nodeChildren

findEvInstBinds :: HieAST a -> Seq (Declaration, Set Name, IdentifierDetails a, HieAST a)
findEvInstBinds n = (\(d, ids, ast) -> (d, getClassNames ids, ids, ast)) <$>
  findIdentifiers'
    (   not
      . Set.null
    ) n

  where

    -- getEvVarSources :: Set ContextInfo -> Set EvVarSource
    -- getEvVarSources = foldMap (maybe mempty Set.singleton) .
    --   Set.map \case
    --     -- EvidenceVarBind a@EvInstBind{} ModuleScope _ -> Just a
    --     _ -> Nothing

    getClassNames :: IdentifierDetails a -> Set Name
    getClassNames a = Set.empty


constructors :: HieAST a -> Seq ( HieAST a )
constructors n@Node { nodeChildren, nodeInfo = NodeInfo{ nodeAnnotations } } =
  if any ( \( _, t ) -> t == "ConDecl" ) nodeAnnotations then
    pure n

  else
    foldMap constructors nodeChildren

analysePatternSynonyms :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analysePatternSynonyms n@Node{ nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard $ ( "PatSynBind", "HsBindLR" ) `Set.member` nodeAnnotations

  for_ ( findDeclarations n ) $ for_ ( uses n ) . addDependency

findDeclarations :: HieAST a -> Seq Declaration
findDeclarations =
  findIdentifiers
    (   not
      . Set.null
      . Set.filter \case
          -- Things that count as declarations
          ValBind RegularBind ModuleScope _ -> True
          PatternBind ModuleScope _ _       -> True
          Decl _ _                          -> True
          TyDecl                            -> True
          ClassTyDecl{}                     -> True

          -- Anything else is not a declaration
          _ -> False
    )

-- findEvInstBinds :: HieAST a -> Seq (Declaration, Set Name, IdentifierDetails a, HieAST a)
-- findEvInstBinds n = (\(d, ids, ast) -> (d, getClassNames ids, ids, ast)) <$>
--   findIdentifiers'
--     (   not
--       . Set.null
--     ) n

  -- where

    -- getEvVarSources :: Set ContextInfo -> Set EvVarSource
    -- getEvVarSources = foldMap (maybe mempty Set.singleton) .
    --   Set.map \case
    --     -- EvidenceVarBind a@EvInstBind{} ModuleScope _ -> Just a
    --     _ -> Nothing

    -- getClassNames :: IdentifierDetails a -> Set Name
    -- getClassNames = identInfo

findIdentifiers
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq Declaration
findIdentifiers f Node{ nodeInfo = NodeInfo{ nodeIdentifiers }, nodeChildren } =
     foldMap
       ( \case
           ( Left _, _ ) ->
             mempty

           ( Right name, IdentifierDetails{ identInfo } ) ->
             if f identInfo then
               foldMap pure ( nameToDeclaration name )

             else
               mempty
           )

       ( Map.toList nodeIdentifiers )
  <> foldMap ( findIdentifiers f ) nodeChildren

findIdentifiers'
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq (Declaration, IdentifierDetails a, HieAST a)
findIdentifiers' f n@Node{ nodeInfo = NodeInfo{ nodeIdentifiers }, nodeChildren } =
     foldMap
       (\case
           ( Left _, _ ) ->
             mempty

           ( Right name, ids@IdentifierDetails{ identInfo } ) ->
             if f identInfo then
               (, ids, n) <$> foldMap pure (nameToDeclaration name)

             else
               mempty
           )
       ((Map.toList nodeIdentifiers))
  <> foldMap ( findIdentifiers' f ) nodeChildren


uses :: HieAST a -> Set Declaration
uses =
    foldMap Set.singleton
  . findIdentifiers \identInfo -> Use `Set.member` identInfo


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <- nameModule_maybe name
  return Declaration { declModule = m, declOccName = nameOccName name }

-- followEvidenceUses :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST a -> Declaration -> m ()
-- followEvidenceUses n d = do
--   Config{ typeClassRoots } <- asks weederConfig
--   AnalysisInfo{ refMap } <- ask

--   let getEvidenceTrees = mapMaybe (getEvidenceTree refMap)
--       evidenceInfos = concatMap Tree.flatten (getEvidenceTrees names)
--       instanceEvidenceInfos = evidenceInfos & filter \case
        -- EvidenceInfo _ _ _ (Just (EvInstBind _ _, ModuleScope, _)) -> True
  --       _ -> False

  -- -- If type-class-roots flag is set then we don't need to follow evidence uses
  -- -- as the binding sites will be roots anyway
  -- unless typeClassRoots $ for_ instanceEvidenceInfos \ev -> do
  --   let name = nameToDeclaration (evidenceVar ev)
  --   mapM_ (addDependency d) name

  -- where

  --   names = concat . Tree.flatten $ evidenceUseTree n

  --   evidenceUseTree :: HieAST a -> Tree [Name]
  --   evidenceUseTree Node{ nodeInfo, nodeChildren } = Tree.Node
  --     { Tree.rootLabel = concatMap ( Map.toList nodeIdentifiers) (nodeInfo)
  --     , Tree.subForest = map evidenceUseTree nodeChildren
  --     }