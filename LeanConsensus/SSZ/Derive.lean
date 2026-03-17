/-
  SSZ Derive Handlers

  Custom `deriving` handlers that auto-generate `SszType`, `SszEncode`,
  `SszDecode`, and `SszHashTreeRoot` instances for structure types.

  These replace ~300 lines of hand-written instances in Consensus/Types.lean
  with simple `deriving` clauses.
-/

import Lean
import LeanConsensus.SSZ.Types
import LeanConsensus.SSZ.Encode
import LeanConsensus.SSZ.Decode
import LeanConsensus.SSZ.Merkleization

open Lean Elab Command Term Meta

namespace LeanConsensus.SSZ.Derive

-- ════════════════════════════════════════════════════════════════
-- Shared helpers
-- ════════════════════════════════════════════════════════════════

inductive FieldClass where
  | fixed (size : Nat)
  | variable

structure FieldInfo where
  fieldName : Name
  fieldType : Expr
  cls : FieldClass

instance : Inhabited FieldInfo where
  default := { fieldName := .anonymous, fieldType := .const .anonymous [], cls := .variable }

/-- Extract a Nat from a UInt32 expression.
    After whnf, a UInt32 value is typically `@OfNat.ofNat UInt32 n inst`
    where `n` is a raw nat literal. -/
private def extractUInt32Nat (e : Expr) : Nat :=
  if e.isAppOf ``OfNat.ofNat then
    let args := e.getAppArgs
    if h : args.size > 1 then
      match args[1] with
      | .lit (.natVal n) => n
      | _ => findNatLit e |>.getD 0
    else findNatLit e |>.getD 0
  else findNatLit e |>.getD 0
where
  findNatLit (e : Expr) : Option Nat :=
    match e with
    | .lit (.natVal n) => some n
    | .app f a =>
      match findNatLit a with
      | some n => some n
      | none => findNatLit f
    | _ => none

/-- Classify a field type by synthesizing SszType and evaluating sszFixedSize. -/
def classifyFieldType (fieldType : Expr) : TermElabM FieldClass := do
  let inst ← synthInstance (← mkAppM ``SszType #[fieldType])
  let expr ← mkAppOptM ``SszType.sszFixedSize #[fieldType, inst]
  -- Use full transparency to reduce through defs like XMSS_PUBKEY_SIZE
  let val ← withTransparency .all <| whnf expr
  if val.isAppOf ``Option.none then
    return .variable
  else if val.isAppOf ``Option.some then
    let arg ← withTransparency .all <| whnf val.appArg!
    return .fixed (extractUInt32Nat arg)
  else
    return .variable

def analyzeStruct (structName : Name) : TermElabM (Array FieldInfo) := do
  let env ← getEnv
  let some info := getStructureInfo? env structName
    | throwError s!"'{structName}' is not a structure"
  let mut result : Array FieldInfo := #[]
  -- Use fieldNames for correct declaration order (fieldInfo may be unordered)
  for fieldName in info.fieldNames do
    let some decl := env.find? (structName ++ fieldName)
      | throwError s!"field projection '{structName ++ fieldName}' not found"
    let fieldType ← forallTelescopeReducing decl.type fun _ body => pure body
    let cls ← classifyFieldType fieldType
    result := result.push { fieldName, fieldType, cls }
  return result

def nextPow2 (n : Nat) : Nat := Id.run do
  if n <= 1 then return 1
  let mut p := 1
  while p < n do
    p := p * 2
  return p

/-- Convert a field type Expr to Syntax for type annotation using delaboration. -/
def fieldTypeToSyntax (e : Expr) : TermElabM (TSyntax `term) :=
  Lean.PrettyPrinter.delab e

-- ════════════════════════════════════════════════════════════════
-- SszType deriving handler
-- ════════════════════════════════════════════════════════════════

def mkSszTypeHandler (declNames : Array Name) : CommandElabM Bool := do
  if declNames.size != 1 then return false
  let declName := declNames[0]!
  let fields ← liftTermElabM <| analyzeStruct declName
  let mut allFixed := true
  let mut totalSize : Nat := 0
  for fi in fields do
    match fi.cls with
    | .fixed size => totalSize := totalSize + size
    | .variable => allFixed := false
  let typeName := mkIdent declName
  if allFixed then
    let sizeStx := Syntax.mkNumLit (toString totalSize)
    elabCommand (← `(instance : LeanConsensus.SSZ.SszType $typeName where
      sszFixedSize := some $sizeStx))
  else
    elabCommand (← `(instance : LeanConsensus.SSZ.SszType $typeName where
      sszFixedSize := none))
  return true

-- ════════════════════════════════════════════════════════════════
-- SszEncode deriving handler
-- ════════════════════════════════════════════════════════════════

def mkSszEncodeHandler (declNames : Array Name) : CommandElabM Bool := do
  if declNames.size != 1 then return false
  let declName := declNames[0]!
  let fields ← liftTermElabM <| analyzeStruct declName
  let typeName := mkIdent declName
  let xId := mkIdent `x
  let encId := mkIdent `enc

  let mut body : TSyntax `term ← `(LeanConsensus.SSZ.SszEncoder.finalize $encId)
  for fi in fields.reverse do
    let fieldAccess := mkIdent fi.fieldName
    body ← `(let $encId := LeanConsensus.SSZ.SszEncoder.addField $encId ($xId).$fieldAccess; $body)
  body ← `(let $encId := { : LeanConsensus.SSZ.SszEncoder }; $body)

  elabCommand (← `(instance : LeanConsensus.SSZ.SszEncode $typeName where
    sszEncode := fun $xId => $body))
  return true

-- ════════════════════════════════════════════════════════════════
-- SszDecode deriving handler
-- ════════════════════════════════════════════════════════════════

/-- Build struct literal syntax: TypeName.mk f1 f2 ... -/
def mkStructLit (typeName : TSyntax `ident) (fieldNames : Array Name) :
    CommandElabM (TSyntax `term) := do
  let ctorId := mkIdent (typeName.getId ++ `mk)
  -- Build nested application: ((((ctor f1) f2) f3) ...)
  let mut result : TSyntax `term := ⟨ctorId⟩
  for fn in fieldNames do
    let fId : TSyntax `term := ⟨mkIdent fn⟩
    result ← `($result $fId)
  return result

def mkSszDecodeHandler (declNames : Array Name) : CommandElabM Bool := do
  if declNames.size != 1 then return false
  let declName := declNames[0]!
  let fields ← liftTermElabM <| analyzeStruct declName
  let typeName := mkIdent declName

  -- Pre-compute field type syntax for type annotations in sszDecode calls
  let fieldTypeSyntaxes ← liftTermElabM do
    fields.mapM fun fi => fieldTypeToSyntax fi.fieldType

  let mut allFixed := true
  let mut fixedRegionSize : Nat := 0
  for fi in fields do
    match fi.cls with
    | .fixed size => fixedRegionSize := fixedRegionSize + size
    | .variable =>
      allFixed := false
      fixedRegionSize := fixedRegionSize + 4

  let dataId := mkIdent `data
  let decId := mkIdent `dec
  let fieldNames := fields.map (·.fieldName)

  if allFixed then
    let totalSizeStx := Syntax.mkNumLit (toString fixedRegionSize)

    let structLit ← mkStructLit typeName fieldNames
    let mut body : TSyntax `term ← `(Except.ok $structLit)

    -- Build Except.bind chain from last field to first
    for i' in [:fields.size] do
      let i := fields.size - 1 - i'
      let fi : FieldInfo := fields[i]!
      let ftStx := fieldTypeSyntaxes[i]!
      let fId := mkIdent fi.fieldName
      let pairId := mkIdent (.mkSimple s!"{fi.fieldName}_pair")
      match fi.cls with
      | .fixed size =>
        let sizeStx := Syntax.mkNumLit (toString size)
        body ← `(
          Except.bind (LeanConsensus.SSZ.SszDecoder.readFixed $decId $sizeStx) fun $pairId =>
          let $decId := Prod.snd $pairId
          Except.bind (LeanConsensus.SSZ.SszDecode.sszDecode (α := $ftStx) (Prod.fst $pairId)) fun $fId =>
          $body)
      | .variable => pure ()  -- unreachable in allFixed path

    body ← `(
      let $decId := LeanConsensus.SSZ.SszDecoder.new $dataId
      $body)

    elabCommand (← `(instance : LeanConsensus.SSZ.SszDecode $typeName where
      sszDecode := fun $dataId =>
        if ByteArray.size $dataId != $totalSizeStx then
          Except.error (LeanConsensus.SSZ.SszError.invalidLength $totalSizeStx (ByteArray.size $dataId))
        else
          $body))
  else
    -- Mixed: Phase 1 reads fixed fields + offsets, Phase 2 decodes variable fields
    let fixedSizeStx := Syntax.mkNumLit (toString fixedRegionSize)
    let varSlicesId := mkIdent `varSlices

    let mut varCount : Nat := 0
    for fi in fields do
      match fi.cls with
      | .variable => varCount := varCount + 1
      | _ => pure ()

    let structLit ← mkStructLit typeName fieldNames
    let mut body : TSyntax `term ← `(Except.ok $structLit)

    -- Phase 2: decode variable fields from varSlices (in reverse)
    let mut varIdx := varCount
    for i' in [:fields.size] do
      let i := fields.size - 1 - i'
      let fi : FieldInfo := fields[i]!
      match fi.cls with
      | .variable =>
        varIdx := varIdx - 1
        let fId := mkIdent fi.fieldName
        let ftStx := fieldTypeSyntaxes[i]!
        let idxStx := Syntax.mkNumLit (toString varIdx)
        body ← `(
          Except.bind (LeanConsensus.SSZ.SszDecode.sszDecode (α := $ftStx) ($varSlicesId[$idxStx]!)) fun $fId =>
          $body)
      | _ => pure ()

    -- extractVariableSlices
    body ← `(
      Except.bind (LeanConsensus.SSZ.SszDecoder.extractVariableSlices $decId) fun $varSlicesId =>
      $body)

    -- Phase 1: readFixed for fixed fields, readOffset for variable fields (in reverse)
    for i' in [:fields.size] do
      let i := fields.size - 1 - i'
      let fi : FieldInfo := fields[i]!
      match fi.cls with
      | .fixed size =>
        let fId := mkIdent fi.fieldName
        let ftStx := fieldTypeSyntaxes[i]!
        let pairId := mkIdent (.mkSimple s!"{fi.fieldName}_pair")
        let sizeStx := Syntax.mkNumLit (toString size)
        body ← `(
          Except.bind (LeanConsensus.SSZ.SszDecoder.readFixed $decId $sizeStx) fun $pairId =>
          let $decId := Prod.snd $pairId
          Except.bind (LeanConsensus.SSZ.SszDecode.sszDecode (α := $ftStx) (Prod.fst $pairId)) fun $fId =>
          $body)
      | .variable =>
        body ← `(
          Except.bind (LeanConsensus.SSZ.SszDecoder.readOffset $decId) fun $decId =>
          $body)

    body ← `(
      let $decId := LeanConsensus.SSZ.SszDecoder.new $dataId
      $body)

    elabCommand (← `(instance : LeanConsensus.SSZ.SszDecode $typeName where
      sszDecode := fun $dataId =>
        if ByteArray.size $dataId < $fixedSizeStx then
          Except.error (LeanConsensus.SSZ.SszError.unexpectedEndOfInput $fixedSizeStx (ByteArray.size $dataId))
        else
          $body))

  return true

-- ════════════════════════════════════════════════════════════════
-- SszHashTreeRoot deriving handler
-- ════════════════════════════════════════════════════════════════

def mkSszHashTreeRootHandler (declNames : Array Name) : CommandElabM Bool := do
  if declNames.size != 1 then return false
  let declName := declNames[0]!
  let fields ← liftTermElabM <| analyzeStruct declName
  let typeName := mkIdent declName
  let xId := mkIdent `x
  let limit := nextPow2 fields.size
  let limitStx := Syntax.mkNumLit (toString limit)

  let hashExprs : Array (TSyntax `term) ← fields.mapM fun fi => do
    let fieldAccess := mkIdent fi.fieldName
    `(LeanConsensus.SSZ.SszHashTreeRoot.hashTreeRoot ($xId).$fieldAccess)

  let arrayStx ← `(#[$[$hashExprs],*])

  elabCommand (← `(instance : LeanConsensus.SSZ.SszHashTreeRoot $typeName where
    hashTreeRoot := fun $xId =>
      LeanConsensus.SSZ.merkleize $arrayStx $limitStx))
  return true

-- ════════════════════════════════════════════════════════════════
-- Registration
-- ════════════════════════════════════════════════════════════════

initialize registerDerivingHandler ``LeanConsensus.SSZ.SszType mkSszTypeHandler
initialize registerDerivingHandler ``LeanConsensus.SSZ.SszEncode mkSszEncodeHandler
initialize registerDerivingHandler ``LeanConsensus.SSZ.SszDecode mkSszDecodeHandler
initialize registerDerivingHandler ``LeanConsensus.SSZ.SszHashTreeRoot mkSszHashTreeRootHandler

end LeanConsensus.SSZ.Derive
