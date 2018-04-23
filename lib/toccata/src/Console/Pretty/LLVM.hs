{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2018, (c) Stephen Diehl 2014-2017, (c) Cedric Shoc 2015
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Console.Pretty.LLVM (
  PP(..),
  ppll,
  ppll_
) where

import Console.Pretty
import Control.Monad (guard)
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Array.MArray hiding (index)
import Data.Array.ST hiding (index)
import Data.Array.Unsafe
import qualified Data.ByteString.Char8 as BL
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short as SBF
import Data.Char (chr, ord, isAscii, isControl, isLetter, isDigit)
import Data.Default.Class
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Maybe (isJust)
import Data.String
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal as RenderTerminal
import GHC.Word
import LLVM.AST
import qualified LLVM.AST.AddrSpace as AS
import LLVM.AST.Attribute
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.COMDAT
import qualified LLVM.AST.Constant as C
import LLVM.AST.DataLayout
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.FunctionAttribute as FA
import LLVM.AST.Global
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Linkage as L
import LLVM.AST.ParameterAttribute as PA
import qualified LLVM.AST.RMWOperation as RMW
-- import LLVM.AST.Type
import LLVM.AST.Typed
-- import qualified LLVM.AST.Visibility as V
import LLVM.DataLayout
import Numeric (showHex)
import Prelude hiding ((<$>))
import Text.Printf

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- parensIf ::  Bool -> Doc a -> Doc a
-- parensIf True = parens
-- parensIf False = id

commas :: [Doc a] -> Doc a
commas  = hsep . punctuate (pretty ',')

-- colons :: [Doc a] -> Doc a
-- colons  = hcat . intersperse (pretty ':')

hlinecat :: [Doc a] -> Doc a
hlinecat = vcat . intersperse softline

wrapbraces :: Doc a -> Doc a -> Doc a
wrapbraces leadIn x = (leadIn <+> pretty '{') <$> x <$> pretty '}'

(<$>) :: Doc a -> Doc a -> Doc a
x <$> y = x <> line <> y

angleBrackets :: Doc a -> Doc a
angleBrackets x = pretty '<' <> x <> pretty '>'

spacedbraces :: Doc a -> Doc a
spacedbraces x = pretty '{' <+> x <+> pretty '}'

local :: Ann a => Doc a -> Doc a
local a = annotate annIdentifier $ "%" <> a

global :: Ann a => Doc a -> Doc a
global a = annotate annIdentifier $ "@" <> a

label :: Ann a => Doc a -> Doc a
label a = annotate annType "label" <+> annotate annIdentifier ("%" <> a)

cma :: Doc a -> Doc a -> Doc a -- <,> does not work :(
a `cma` b = a <> "," <+> b

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

class Ann a where
  annIdentifier, annType, annStmt, annKeyword, annComment :: a

instance Ann () where
  annIdentifier = ()
  annType = ()
  annStmt = ()
  annKeyword = ()
  annComment = ()

instance Ann AnsiStyle where
  annIdentifier = color Green
  annType = color Blue
  annStmt = colorDull Yellow
  annKeyword = underlined
  annComment = color Magenta

astmt :: Ann a => Doc a -> Doc a
astmt = annotate annStmt

atype :: Ann a => Doc a -> Doc a
atype = annotate annType

akw :: Ann a => Doc a -> Doc a
akw = annotate annKeyword

acomment :: Ann a => Doc a -> Doc a
acomment = annotate annComment

class PP p where
  pp :: Ann a => p -> Doc a
  default pp :: Pretty p => p -> Doc a
  pp = pretty

ppMaybe :: (PP a, Ann b) => Maybe a -> Doc b
ppMaybe (Just x) = pp x
ppMaybe Nothing = mempty

ppBool :: Doc a -> Bool -> Doc a
ppBool x True = x
ppBool _ False = mempty

-- XXX: horrible hack
unShort :: BS.ShortByteString -> [Char]
unShort xs = fmap (toEnum . fromIntegral) $ BS.unpack xs

short :: Ann a => BS.ShortByteString -> Doc a
short x = pretty (unShort x)

decodeShortUtf8 :: SBF.ShortByteString -> Text
decodeShortUtf8 = decodeUtf8 . fromStrict . SBF.fromShort

instance PP Word32

instance PP Word64

instance PP Integer where

instance PP BS.ShortByteString where
  pp = pp . unShort

instance PP [Char]

instance PP Name where
  pp (Name nm)
   | BS.null nm = dquotes mempty
    | isFirst first && all isRest name = pretty (pack name)
    | otherwise = dquotes . hcat . map escape $ name
    where
        name = unShort nm
        first = head name
        isFirst c = isLetter c || c == '-' || c == '_' || c == '$' || c == '.'
        isRest c = isDigit c || isFirst c
  pp (UnName x) = pretty x

instance PP Parameter where
  pp (Parameter ty (UnName _) attrs) = hsep (pp ty : fmap pp attrs)
  pp (Parameter ty name attrs) = hsep (pp ty : fmap pp attrs) <+> local (pp name)

instance PP [ParameterAttribute] where
  pp x = hsep $ fmap pp x

instance PP ([Parameter], Bool) where
  pp (params, False) = commas (fmap pp params)
  pp (_params, True) = "TODO" -- XXX: variadic case

instance PP (Operand, [ParameterAttribute]) where
  pp (op, attrs) = pp (typeOf op) <+> pp attrs <+> pp op

instance PP UnnamedAddr where
  pp LocalAddr = "local_unnamed_addr"
  pp GlobalAddr = "unnamed_addr"

instance PP Type where
  pp (IntegerType w) = atype $ "i" <> pp w
  pp (FloatingPointType HalfFP)      = atype "half"
  pp (FloatingPointType FloatFP )    = atype "float"
  pp (FloatingPointType DoubleFP)    = atype "double"
  pp (FloatingPointType FP128FP)     = atype "fp128"
  pp (FloatingPointType X86_FP80FP)  = atype "x86_fp80"
  pp (FloatingPointType PPC_FP128FP) = atype "ppc_fp128"

  pp VoidType = atype "void"
  pp (PointerType ref (AS.AddrSpace addr))
    | addr == 0 = atype $ pp ref <> "*"
    | otherwise = atype $ pp ref <+> akw "addrspace" <> parens (pp addr) <> "*"
  pp ft@(FunctionType {..}) = atype $ pp resultType <+> ppFunctionArgumentTypes ft
  pp (VectorType {..}) = atype $ "<" <> pp nVectorElements <+> "x" <+> pp elementType <> ">"
  pp (StructureType {..}) = atype $ if isPacked
                               then "<{" <> (commas $ fmap pp elementTypes ) <> "}>"
                               else  "{" <> (commas $ fmap pp elementTypes ) <> "}"
  pp (ArrayType {..}) = atype $ brackets $ pp nArrayElements <+> "x" <+> pp elementType
  pp (NamedTypeReference name) = atype "%" <> pp name
  pp MetadataType = atype "metadata"
  pp TokenType = atype "token"
  pp LabelType = atype "label"

instance PP Global where
  pp Function {..} =
      case basicBlocks of
        [] -> hsep 
           $ pre
          ++ [pp returnType, global (pp name) <> ppParams (pp . typeOf) parameters]
          ++ post
        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] -> hsep (
             pre
          ++ [pp returnType, global (pp name) <> ppParams pp parameters]
          ++ post
          ) `wrapbraces` (indent 2 $ ppSingleBlock b)
        bs -> hsep (
             pre
          ++ [pp returnType, global (pp name) <> ppParams pp parameters]
          ++ post
          ) `wrapbraces` (vcat $ fmap pp bs)
    where
      pre = akw "declare" : pp linkage : pp callingConvention : fmap pp returnAttributes
      post = fmap pp functionAttributes ++ align' ++ gcName ++ foldMap (\con -> [ akw "prefix", ppTyped con ]) prefix
      align' = guard (alignment /= 0) *> [akw "align", pp alignment]
      gcName = foldMap (\n -> [akw "gc", dquotes (pretty $ unShort n)]) garbageCollectorName

  pp GlobalVariable {..} = global (pp name) <+> "=" <+> ppLinkage hasInitializer linkage <+> ppMaybe unnamedAddr
                             <+> addrSpace' <+> kind <+> pp type' <+> ppMaybe initializer <> ppAlign alignment
    where
      hasInitializer = isJust initializer
      addrSpace' =
        case addrSpace of
          AS.AddrSpace addr
            | addr == 0 -> mempty
            | otherwise -> "addrspace" <> parens (pp addr)
      kind | isConstant = "constant"
           | otherwise  = "global"

  pp GlobalAlias {..} = global (pp name) <+> "=" <+> pp linkage <+> ppMaybe unnamedAddr <+> "alias" <+> pp typ `cma` ppTyped aliasee
    where
      typ = getElementType type'

ppMetadata :: Ann a => Maybe Metadata -> Doc a
ppMetadata Nothing = "null"
ppMetadata (Just m) = pp m

instance PP Definition where
  pp (GlobalDefinition x) = pp x
  pp (TypeDefinition nm ty) = local (pp nm) <+> "=" <+> "type" <+> maybe "opaque" pp ty
  pp (FunctionAttributes gid attrs) = "attributes" <+> pp gid <+> "=" <+> braces (hsep (fmap ppAttrInGroup attrs))
  pp (NamedMetadataDefinition nm meta) = "!" <> short nm <+> "=" <+> "!" <> braces (commas (fmap pp meta))
  pp (MetadataNodeDefinition node meta) = pp node <+> "=" <+> "!" <> braces (commas (fmap ppMetadata meta))
  pp (ModuleInlineAssembly asm) = "module asm" <+> dquotes (pretty (pack (BL.unpack asm)))
  pp (COMDAT name selKind) = "$" <> short name <+> "=" <+> "comdat" <+> pp selKind

instance PP SelectionKind where
  pp Any = "any"
  pp ExactMatch = "exactmatch"
  pp Largest = "largest"
  pp NoDuplicates = "noduplicates"
  pp SameSize = "samesize"

ppAttrInGroup :: Ann a => FunctionAttribute -> Doc a
ppAttrInGroup = \case
  StackAlignment n -> "alignstack=" <> pp n
  attr -> pp attr

instance PP FunctionAttribute where
  pp = \case
   NoReturn            -> "noreturn"
   NoUnwind            -> "nounwind"
   FA.ReadNone         -> "readnone"
   FA.ReadOnly         -> "readonly"
   FA.WriteOnly        -> "writeonly"
   NoInline            -> "noinline"
   AlwaysInline        -> "alwaysinline"
   MinimizeSize        -> "minsize"
   OptimizeForSize     -> "optsize"
   OptimizeNone        -> "optnone"
   SafeStack           -> "safestack"
   StackProtect        -> "ssp"
   StackProtectReq     -> "sspreq"
   StackProtectStrong  -> "sspstrong"
   NoRedZone           -> "noredzone"
   NoImplicitFloat     -> "noimplicitfloat"
   Naked               -> "naked"
   InlineHint          -> "inlinehint"
   StackAlignment n    -> "alignstack" <> parens (pp n)
   ReturnsTwice        -> "returns_twice"
   UWTable             -> "uwtable"
   NonLazyBind         -> "nonlazybind"
   Builtin             -> "builtin"
   NoBuiltin           -> "nobuiltin"
   Cold                -> "cold"
   JumpTable           -> "jumptable"
   NoDuplicate         -> "noduplicate"
   SanitizeAddress     -> "sanitize_address"
   SanitizeThread      -> "sanitize_thread"
   SanitizeMemory      -> "sanitize_memory"
   NoRecurse           -> "norecurse"
   Convergent          -> "convergent"
   ArgMemOnly          -> "argmemonly"
   InaccessibleMemOnly -> "inaccessiblememonly"
   AllocSize a Nothing -> "allocsize" <> parens (pp a)
   AllocSize a (Just b) -> "allocsize" <> parens (commas [pp a, pp b])
   InaccessibleMemOrArgMemOnly -> "inaccessiblemem_or_argmemonly"
   FA.StringAttribute k v -> dquotes (short k) <> "=" <> dquotes (short v)
   Speculatable        -> "speculatable"

instance PP ParameterAttribute where
  pp = \case
    ZeroExt                    -> "zeroext"
    SignExt                    -> "signext"
    InReg                      -> "inreg"
    SRet                       -> "sret"
    Alignment word             -> "align" <+> pp word
    NoAlias                    -> "noalias"
    ByVal                      -> "byval"
    NoCapture                  -> "nocapture"
    Nest                       -> "nest"
    PA.ReadNone                -> "readnone"
    PA.ReadOnly                -> "readonly"
    PA.WriteOnly               -> "writeonly"
    InAlloca                   -> "inalloca"
    NonNull                    -> "nonnull"
    Dereferenceable word       -> "dereferenceable" <> parens (pp word)
    DereferenceableOrNull word -> "dereferenceable_or_null" <> parens (pp word)
    Returned                   -> "returned"
    SwiftSelf                  -> "swiftself"
    SwiftError                 -> "swifterror"
    PA.StringAttribute k v -> dquotes (short k) <> "=" <> dquotes (short v)

instance PP CC.CallingConvention where
  pp = \case
   CC.Numbered word -> "cc" <+> pp word
   CC.C             -> "ccc"
   CC.Fast          -> "fastcc"
   CC.Cold          -> "coldcc"
   CC.GHC           -> "cc 10"
   CC.HiPE          -> "cc 11"
   CC.WebKit_JS     -> "webkit_jscc"
   CC.AnyReg        -> "anyregcc"
   CC.PreserveMost  -> "preserve_mostcc"
   CC.PreserveAll   -> "preserve_allcc"
   CC.Swift         -> "swiftcc"
   CC.CXX_FastTLS   -> "cxx_fast_tlscc"
   CC.X86_StdCall   -> "cc 64"
   CC.X86_FastCall  -> "cc 65"
   CC.ARM_APCS      -> "cc 66"
   CC.ARM_AAPCS     -> "cc 67"
   CC.ARM_AAPCS_VFP -> "cc 68"
   CC.MSP430_INTR   -> "cc 69"
   CC.X86_ThisCall  -> "cc 70"
   CC.PTX_Kernel    -> "cc 71"
   CC.PTX_Device    -> "cc 72"
   CC.SPIR_FUNC     -> "cc 75"
   CC.SPIR_KERNEL   -> "cc 76"
   CC.Intel_OCL_BI  -> "cc 77"
   CC.X86_64_SysV   -> "cc 78"
   CC.Win64         -> "cc 79"
   CC.X86_Intr      -> "x86_intrcc"
   CC.X86_RegCall   -> "x86_regcallcc"
   CC.X86_VectorCall -> "x86_vectorcallcc"
   CC.AVR_Intr      -> "avr_intrcc"
   CC.AVR_Signal    -> "avr_signalcc"
   CC.AVR_Builtin   -> "cc 86"
   CC.HHVM          -> "hhvmcc"
   CC.HHVM_C        -> "hhvm_ccc"
   CC.AMDGPU_VS     -> "amdgpu_vs"
   CC.AMDGPU_GS     -> "amdgpu_gs"
   CC.AMDGPU_PS     -> "amdgpu_ps"
   CC.AMDGPU_CS     -> "amdgpu_cs"
   CC.AMDGPU_HS     -> "amdgpu_hs"
   CC.AMDGPU_Kernel -> "amdgpu_kernel"
   CC.MSP430_Builtin -> "msp430"

instance PP L.Linkage where
    pp = ppLinkage False

ppLinkage :: Ann a => Bool -> L.Linkage -> Doc a
ppLinkage omitExternal = \case
   L.External | omitExternal -> mempty
              | otherwise    -> akw "external"
   L.Private                 -> akw "private"
   L.Internal                -> akw "internal"
   L.ExternWeak              -> akw "extern_weak"
   L.AvailableExternally     -> akw "available_externally"
   L.LinkOnce                -> akw "linkonce"
   L.Weak                    -> akw "weak"
   L.Common                  -> akw "common"
   L.Appending               -> akw "appending"
   L.LinkOnceODR             -> akw "linkonce_odr"
   L.WeakODR                 -> akw "weak_odr"

instance PP InstructionMetadata where
  pp meta = commas ["!" <> pp x <> "!" <> ("{" <> pp y <> "}") | (x,y) <- meta]

instance PP MetadataNodeID where
  pp (MetadataNodeID x) = "!" <> pretty x

instance PP GroupID where
  pp (GroupID x) = "#" <> pretty x

instance PP BasicBlock where
  pp (BasicBlock nm instrs term) =
    lbl <$> indent 2 (vcat $ (fmap pp instrs) ++ [pp term])
    where
      lbl = case nm of
        UnName _ -> acomment $ "; <label>:" <> pp nm <> ":"
        _ -> pp nm <> ":"

instance PP Terminator where
  pp = \case
    Br dest meta -> astmt "br" <+> label (pp dest) <+> ppInstrMeta meta

    Ret val meta -> astmt "ret" <+> maybe "void" ppTyped val <+> ppInstrMeta meta

    CondBr cond tdest fdest meta ->
     astmt "br" <+> ppTyped cond
     `cma` label (pp tdest)
     `cma` label (pp fdest)
     <+> ppInstrMeta meta

    Switch {..} -> astmt "switch" <+> ppTyped operand0'
                 `cma` label (pp defaultDest)
                 <+> brackets (hsep [ ppTyped v `cma` label (pp l) | (v,l) <- dests ])
                 <+> ppInstrMeta metadata'

    Unreachable {..} -> astmt "unreachable" <+> ppInstrMeta metadata'

    IndirectBr op dests meta -> astmt "indirectbr" <+> ppTyped op `cma`
     brackets (hsep [ label (pp l) | l <- dests ])
     <+> ppInstrMeta meta

    e @ Invoke {..} ->
     ppInvoke e
     <+> "to" <+> label (pp returnDest)
     <+> "unwind" <+> label (pp exceptionDest)
     <+> ppInstrMeta metadata'

    Resume op meta -> astmt "resume "<+> ppTyped op <+> ppInstrMeta meta

    CleanupRet pad dest meta ->
      astmt "cleanupret" <+> "from" <+> pp pad <+> "unwind" <+> maybe "to caller" (label . pp) dest
      <+> ppInstrMeta meta

    CatchRet catchPad s meta ->
      astmt "catchret" <+> "from" <+> pp catchPad <+> "to" <+> label (pp s)
      <+> ppInstrMeta meta

    CatchSwitch {..} ->
      astmt "catchswitch" <+> "within" <+> pp parentPad' <+>
      brackets (commas (map (label . pp) (toList catchHandlers))) <+>
      "unwind" <+> "to" <+> maybe "caller" pp defaultUnwindDest
      <+> ppInstrMeta metadata'

instance PP Instruction where
  pp = \case
    Add {..}    -> astmt "add"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Sub {..}    -> astmt "sub"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Mul {..}    -> astmt "mul"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Shl {..}    -> astmt "shl"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    AShr {..}   -> astmt "ashr" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    LShr {..}   -> astmt "lshr" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    And {..}    -> astmt "and"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Or {..}     -> astmt "or"   <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Xor {..}    -> astmt "xor"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    SDiv {..}   -> astmt "sdiv"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    UDiv {..}   -> astmt "udiv"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    SRem {..}   -> astmt "srem"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    URem {..}   -> astmt "urem"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

    FAdd {..}   -> astmt "fadd" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FSub {..}   -> astmt "fsub" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FMul {..}   -> astmt "fmul" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FDiv {..}   -> astmt "fdiv" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FRem {..}   -> astmt "frem" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FCmp {..}   -> astmt "fcmp" <+> pp fpPredicate <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

    Alloca {..} -> astmt "alloca" <+> pp allocatedType <> num <> ppAlign alignment <+> ppInstrMeta metadata
      where num   = case numElements of Nothing -> mempty
                                        Just o -> "," <+> ppTyped o
    Store {..}  -> astmt "store" <+> ppTyped value `cma` ppTyped address <> ppAlign alignment
    Load {..}   -> astmt "load" <+> pp argTy `cma` ppTyped address <> ppAlign alignment <+> ppInstrMeta metadata
      where PointerType argTy _ = typeOf address
    Phi {..}    -> astmt "phi" <+> pp type' <+> commas (fmap phiIncoming incomingValues) <+> ppInstrMeta metadata

    ICmp {..}   -> astmt "icmp" <+> pp iPredicate <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

    c@Call {..} -> ppCall c  <+> ppInstrMeta metadata
    Select {..} -> astmt "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue] <+> ppInstrMeta metadata
    SExt {..}   -> astmt "sext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    ZExt {..}   -> astmt "zext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    FPExt {..}   -> astmt "fpext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    Trunc {..}  -> astmt "trunc" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    FPTrunc {..}  -> astmt "fptrunc" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata

    GetElementPtr {..} -> astmt "getelementptr" <+> bounds inBounds <+> commas (pp argTy : fmap ppTyped (address:indices)) <+> ppInstrMeta metadata
      where argTy = getElementType $ typeOf address
    ExtractValue {..} -> astmt "extractvalue" <+> commas (ppTyped aggregate : fmap pp indices') <+> ppInstrMeta metadata

    BitCast {..} -> astmt "bitcast" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    FPToUI {..} -> astmt "fptoui" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    FPToSI {..} -> astmt "fptosi" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    UIToFP {..} -> astmt "uitofp" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    SIToFP {..} -> astmt "sitofp" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    PtrToInt {..} -> astmt "ptrtoint" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    IntToPtr {..} -> astmt "inttoptr" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata

    InsertElement {..} -> astmt "insertelement" <+> commas [ppTyped vector, ppTyped element, ppTyped index] <+> ppInstrMeta metadata
    ShuffleVector {..} -> astmt "shufflevector" <+> commas [ppTyped operand0, ppTyped operand1, ppTyped mask] <+> ppInstrMeta metadata
    ExtractElement {..} -> astmt "extractelement" <+> commas [ppTyped vector, ppTyped index] <+> ppInstrMeta metadata
    InsertValue {..} -> astmt "insertvalue" <+> commas (ppTyped aggregate : ppTyped element : fmap pp indices') <+> ppInstrMeta metadata

    Fence {..} -> astmt "fence" <+> pp atomicity <+> ppInstrMeta metadata
    AtomicRMW {..} -> astmt "atomicrmw" <+> ppVolatile volatile <+> pp rmwOperation <+> ppTyped address `cma` ppTyped value <+> pp atomicity  <+> ppInstrMeta metadata
    CmpXchg {..} -> astmt "cmpxchg" <+> ppVolatile volatile <+> ppTyped address `cma` ppTyped expected `cma` ppTyped replacement
      <+> pp atomicity <+> pp failureMemoryOrdering <+> ppInstrMeta metadata

    AddrSpaceCast {..} -> astmt "addrspacecast" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    VAArg {..} -> astmt "va_arg" <+> ppTyped argList `cma` pp type' <+> ppInstrMeta metadata

    LandingPad {..} ->
      astmt "landingpad" <+> pp type' <+> ppBool "cleanup" cleanup <+> ppInstrMeta metadata
      <+> commas (fmap pp clauses)
    CatchPad {..} -> astmt "catchpad" <+> "within" <+> pp catchSwitch <+> brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata
    CleanupPad {..} -> astmt "cleanuppad" <+> "within" <+> pp parentPad <+> brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata

    where
      bounds True = "inbounds"
      bounds False = mempty

instance PP CallableOperand where
  pp (Left _asm) = error "CallableOperand"
  pp (Right op) = pp op

instance PP LandingPadClause where
  pp = \case
    Catch c  -> "catch" <+> ppTyped c
    Filter c -> "filter" <+> ppTyped c

instance PP [Either GroupID FunctionAttribute] where
  pp x = hsep $ fmap pp x

instance PP (Either GroupID FunctionAttribute) where
  pp (Left gid) = pp gid
  pp (Right fattr) = pp fattr

instance PP Operand where
  pp (LocalReference _ nm) = local (pp nm)
  pp (ConstantOperand con) = pp con
  pp (MetadataOperand mdata) = pp mdata

instance PP Metadata where
  pp (MDString str) = "!" <> dquotes (pretty (decodeShortUtf8 str))
  pp (MDNode node) = pp node
  pp (MDValue operand) = pp operand

instance PP MetadataNode where
  pp (MetadataNode xs) = "!" <> braces (commas (fmap ppMetadata xs))
  pp (MetadataNodeReference ref) = pp ref

instance PP C.Constant where
  pp (C.Int _width val) = pp val
  pp (C.Float (F.Double val))      =
    if specialFP val
      then "0x" <> (pretty . pack) (showHex (doubleToWord val) "")
      else pretty (printf "%6.6e" val :: String)
  pp (C.Float (F.Single val))      =
    if specialFP val
      then "0x" <> (pretty . pack) (showHex (floatToWord val) "")
      else pretty (printf "%6.6e" val :: String)
  pp (C.Float (F.Half val))        = pretty (printf "%6.6e" val :: String)
  pp (C.Float (F.Quadruple val _)) = pretty (printf "%6.6e" val :: String)
  pp (C.Float (F.X86_FP80 val _))  = pretty (printf "%6.6e" val :: String)
  pp (C.Float (F.PPC_FP128 val _)) = pretty (printf "%6.6e" val :: String)

  pp (C.GlobalReference _ty nm) = "@" <> pp nm
  pp (C.Vector args) = "<" <+> commas (fmap ppTyped args) <+> ">"

  pp (C.Add {..})    = "add"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.Sub {..})    = "sub"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.Mul {..})    = "mul"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.Shl {..})    = "shl"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.AShr {..})   = "ashr" <+> ppTyped operand0 `cma` pp operand1
  pp (C.LShr {..})   = "lshr" <+> ppTyped operand0 `cma` pp operand1
  pp (C.And {..})    = "and"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.Or {..})     = "or"   <+> ppTyped operand0 `cma` pp operand1
  pp (C.Xor {..})    = "xor"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.SDiv {..})   = "sdiv"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.UDiv {..})   = "udiv"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.SRem {..})   = "srem"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.URem {..})   = "urem"  <+> ppTyped operand0 `cma` pp operand1

  pp (C.FAdd {..})   = "fadd" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FSub {..})   = "fsub" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FMul {..})   = "fmul" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FDiv {..})   = "fdiv" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FRem {..})   = "frem" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FCmp {..})   = "fcmp" <+> pp fpPredicate <+> ppTyped operand0 `cma` pp operand1
  pp C.ICmp {..}     = "icmp" <+> pp iPredicate <+> ppTyped operand0 `cma` pp operand1

  pp (C.Select {..})  = "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue]
  pp (C.SExt {..})    = "sext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.ZExt {..})    = "zext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.FPExt {..})   = "fpext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.Trunc {..})   = "trunc" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.FPTrunc {..}) = "fptrunc" <+> ppTyped operand0 <+> "to" <+> pp type'

  pp C.FPToUI {..} = "fptoui" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.FPToSI {..} = "fptosi" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.UIToFP {..} = "uitofp" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.SIToFP {..} = "sitofp" <+> ppTyped operand0 <+> "to" <+> pp type'

  pp (C.Struct _ packed elems) =
    let struct = spacedbraces $ commas $ fmap ppTyped elems
    in if packed
         then angleBrackets struct
         else struct

  pp (C.Null constantType) = ppNullInitializer constantType

#if MIN_VERSION_llvm_hs_pure(5,1,3)
  pp (C.AggregateZero _constantType) = "zeroinitializer"
#endif

  pp (C.Undef {}) = "undef"
  pp (C.TokenNone {}) = "none"
  pp (C.BlockAddress fn blk) = "blockaddress" <> parens (commas (fmap pp [fn, blk]))

  pp C.Array {..}
    | memberType == (IntegerType 8) = "c" <> (dquotes $ hcat [ppIntAsChar val | C.Int _ val <- memberValues])
    | otherwise = brackets $ commas $ fmap ppTyped memberValues

  pp C.GetElementPtr {..} = "getelementptr" <+> bounds inBounds <+> parens (commas (pp argTy : fmap ppTyped (address:indices)))
    where
      PointerType argTy _ = typeOf address
      bounds True = "inbounds"
      bounds False = mempty

  pp C.BitCast {..} = "bitcast" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
  pp C.PtrToInt {..} = "ptrtoint" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
  pp C.IntToPtr {..} = "inttoptr" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
  pp C.AddrSpaceCast {..} = "addrspacecast" <+> parens (ppTyped operand0 <+> "to" <+> pp type')

instance PP a => PP (Named a) where
  pp (nm := a) = local (pp nm) <+> "=" <+> pp a
  pp (Do a) = pp a

instance PP Module where
  pp Module {..} = hlinecat (header : layout ++ target ++ fmap pp moduleDefinitions) where
    header = acomment $ fromString $ printf "; ModuleID = '%s'" (unShort moduleName)
    target = foldMap (\tgt -> [ "target triple =" <+> dquotes (pp tgt)]) moduleTargetTriple
    layout = foldMap (\lyt -> [ "target datalayout =" <+> dquotes (pp lyt) ]) moduleDataLayout

instance PP FP.FloatingPointPredicate where
  pp op = case op of
   FP.False -> "false"
   FP.OEQ   -> "oeq"
   FP.OGT   -> "ogt"
   FP.OGE   -> "oge"
   FP.OLT   -> "olt"
   FP.OLE   -> "ole"
   FP.ONE   -> "one"
   FP.ORD   -> "ord"
   FP.UEQ   -> "ueq"
   FP.UGT   -> "ugt"
   FP.UGE   -> "uge"
   FP.ULT   -> "ult"
   FP.ULE   -> "ule"
   FP.UNE   -> "une"
   FP.UNO   -> "uno"
   FP.True  -> "true"

instance PP IP.IntegerPredicate where
  pp op = case op of
   IP.EQ  -> "eq"
   IP.NE  -> "ne"
   IP.UGT -> "ugt"
   IP.UGE -> "uge"
   IP.ULT -> "ult"
   IP.ULE -> "ule"
   IP.SGT -> "sgt"
   IP.SGE -> "sge"
   IP.SLT -> "slt"
   IP.SLE -> "sle"

instance PP Atomicity where
  pp (scope, order) =
    pp scope <+> pp order

instance PP SynchronizationScope where
  pp = \case
    SingleThread -> "syncscope(\"singlethread\")"
    System -> mempty

instance PP MemoryOrdering where
  pp = \case
    Unordered              -> "unordered"
    Monotonic              -> "monotonic"
    Acquire                -> "acquire"
    Release                -> "release"
    AcquireRelease         -> "acq_rel"
    SequentiallyConsistent -> "seq_cst"

instance PP RMW.RMWOperation where
  pp = \case
    RMW.Xchg -> "xchg"
    RMW.Add -> "add"
    RMW.Sub -> "sub"
    RMW.And -> "and"
    RMW.Nand -> "nand"
    RMW.Or -> "or"
    RMW.Xor -> "xor"
    RMW.Max -> "max"
    RMW.Min -> "min"
    RMW.UMax -> "umax"
    RMW.UMin -> "umin"

instance PP DataLayout where
  pp x = pp (BL.unpack (dataLayoutToString x))

-------------------------------------------------------------------------------
-- Special Case Hacks
-------------------------------------------------------------------------------

escape :: Char -> Doc a
escape '"'  = "\\22"
escape '\\' = "\\\\"
escape c    = if isAscii c && not (isControl c)
              then pretty c
              else "\\" <> hex c
    where
        hex :: Char -> Doc a
        hex = pad0 . ($ []) . showHex . ord
        pad0 :: String -> Doc a
        pad0 [] = "00"
        pad0 [x] = "0" <> pretty x
        pad0 xs = pretty xs

ppVolatile :: Bool -> Doc a
ppVolatile True = "volatile"
ppVolatile False = mempty

ppIntAsChar :: Integral a => a -> Doc b
ppIntAsChar = escape . chr . fromIntegral

ppAlign :: Ann a => Word32 -> Doc a
ppAlign x | x == 0    = mempty
          | otherwise = ", align" <+> pp x

-- print an operand and its type
ppTyped :: (PP a, Typed a, Ann b) => a -> Doc b
ppTyped a = pp (typeOf a) <+> pp a

-- ppCommaTyped :: (PP a, Typed a, Ann b) => a -> Doc b
-- ppCommaTyped a = pp (typeOf a) `cma` pp a

phiIncoming :: Ann b => (Operand, Name) -> Doc b
phiIncoming (op, nm) = brackets (pp op `cma` (local (pp nm)))

ppParams :: (a -> Doc b) -> ([a], Bool) -> Doc b
ppParams ppParam (ps, varrg) = parens . commas $ fmap ppParam ps ++ vargs
    where
        vargs = if varrg then ["..."] else []

ppFunctionArgumentTypes :: Ann a => Type -> Doc a
ppFunctionArgumentTypes FunctionType {..} = ppParams pp (argumentTypes, isVarArg)
ppFunctionArgumentTypes _ = error "Non-function argument. (Malformed AST)"

ppNullInitializer :: Ann a => Type -> Doc a
ppNullInitializer PointerType {..} = "zeroinitializer"
ppNullInitializer StructureType {..} = "zeroinitializer"
ppNullInitializer FunctionType {..} = "zeroinitializer"
ppNullInitializer ArrayType {..} = "zeroinitializer"
ppNullInitializer _ = error "Non-pointer argument. (Malformed AST)"

ppCall :: Ann a => Instruction -> Doc a
ppCall Call { function = Right f,..}
  = tl <+> "call" <+> pp callingConvention <+> pp returnAttributes <+> pp resultType <+> ftype
    <+> pp f <> parens (commas $ fmap pp arguments) <+> pp functionAttributes
    where
      (functionType@FunctionType {..}) = referencedType (typeOf f)
      ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else mempty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t

      tl = case tailCallKind of
        Just Tail -> "tail"
        Just MustTail -> "musttail"
        Just NoTail -> "notail"
        Nothing -> mempty
ppCall _ = error "Non-callable argument. (Malformed AST)"

-- Differs from Call in record name conventions only so needs a seperate almost
-- identical function. :(
ppInvoke :: Ann a => Terminator -> Doc a
ppInvoke Invoke { function' = Right f,..}
  = "invoke" <+> pp callingConvention' <+> pp resultType <+> ftype
    <+> pp f <> parens (commas $ fmap pp arguments') <+> pp functionAttributes'
    where
      (functionType@FunctionType {..}) = referencedType (typeOf f)
      ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else mempty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t
ppInvoke _ = error "Non-callable argument. (Malformed AST)"

ppSingleBlock :: Ann a => BasicBlock -> Doc a
ppSingleBlock (BasicBlock _nm instrs term) = vcat $ fmap pp instrs ++ [pp term]

-- According to <https://stackoverflow.com/a/7002812/3877993> this is
-- the best way to cast floats to words.

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

specialFP :: RealFloat a => a -> Bool
specialFP f = isNaN f || f == 1 / 0 || f == - 1 / 0

ppInstrMeta :: Ann a => InstructionMetadata -> Doc a
ppInstrMeta [] = mempty
ppInstrMeta xs = "," <> pp xs

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

ppll :: (MonadIO m, PP a) => FancyOptions -> a -> m ()
ppll opts a = putFancy opts (pp a <> line')

-- for quick testing
ppll_ :: (MonadIO m, PP a) => a -> m ()
ppll_ = ppll def
