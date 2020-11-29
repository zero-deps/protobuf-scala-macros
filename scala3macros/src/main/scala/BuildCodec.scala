package zd
package proto

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._, report._
import scala.internal.quoted.showName
import scala.collection.immutable.ArraySeq
import zd.proto.Bytes

trait BuildCodec3 extends Common3 {
  implicit val qctx: QuoteContext
  import qctx.reflect.{_, given}
  import qctx.reflect.defn._

  def prepareTrait[A: quoted.Type](a: Expr[A], params: List[FieldInfo])(using ctx: Context): Expr[Prepare] =
    val aTpe = a.unseal.tpe
    val ifBranches: List[(Term, Term)] = params.map { p =>
      val condition: Term = 
        if p.tpe.typeSymbol.isTerm then
          Select.unique(a.unseal, "==").appliedTo(p.getter(a.unseal))
        else
          Select.unique(a.unseal, "isInstanceOf").appliedToType(p.tpe)
      val action: Term = prepareImpl(a, List(p)).unseal
      condition -> action
    }
    val error = s"Wrong type of child of sealed trait: ${aTpe}"
    val elseBranch: Term = '{ throw new RuntimeException(${Expr(error)}) }.unseal
    mkIfStatement(ifBranches, elseBranch).seal.cast[Prepare]

  def prepareImpl[A: quoted.Type](a: Expr[A], params: List[FieldInfo])(using ctx: Context): Expr[Prepare] =
    val sizeAccSym = Symbol.newVal(Symbol.currentOwner, "sizeAcc", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
    val sizeAccRef = Ref(sizeAccSym)
    val sizeAccValDef = ValDef(sizeAccSym, Some(Literal(Constant.Int(0))))
    val xs = params.flatMap(p => size(a, p, sizeAccRef))
    val newPrepare = '{
      new Prepare {
        val size: Int = ${ sizeAccRef.seal.asExprOf[Int] }
        def write(os: CodedOutputStream): Unit = ${ writeImpl(a, params, 'os) }
      }
    }.unseal
    Block(
      sizeAccValDef :: xs
    , newPrepare
    ).seal.asExprOf[Prepare]

  def writeImpl[A: quoted.Type](a: Expr[A], params: List[FieldInfo], os: Expr[CodedOutputStream]): Expr[Unit] =
    Expr.block(
      params.flatMap(p =>
        if p.tpe.isCommonType then writeCommon(a, os, p)
        else if p.tpe.isOption then writeOption(a, os, p)
        else if p.tpe.isIterable then writeCollection(a, os, p)
        else writeMessage(a, os, p)
      )
    , unitExpr)

  def writeCommon[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , writeFun(os, field.tpe, field.getter(a.unseal))
    )
  
  def writeOption[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe = field.tpe.optionArgument
    val getter = field.getter(a.unseal)
    val getterOption = Select.unique(getter, "get")
    if tpe.isCommonType then
      List(
        '{
          if ${Select.unique(getter, "isDefined").seal.asExprOf[Boolean]} then {
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${writeFun(os, tpe, getterOption)}
          }
        }
      )
    else
      val prepareOptionRef = Ref(field.prepareOptionSym).seal.asExprOf[Option[Prepare]]
      List(
        '{
          if ${prepareOptionRef}.isDefined then {
            val p = ${prepareOptionRef}.get
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${os}.writeUInt32NoTag(p.size)
            p.write(${os})
          }
        }
      )

  def writeCollection[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe1 = field.tpe.iterableArgument
    val getter = field.getter(a.unseal)
    val pType = tpe1.seal.asInstanceOf[quoted.Type[_]]
    val sizeRef = Ref(field.sizeSym)
    if tpe1.isCommonType then
      pType match
        case '[$V] =>
          if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then
            val expr = 
              Select.unique(getter, "foreach")
                .appliedToType(unitLiteral.tpe)
                .appliedTo('{ (v: V) => {
                      ${os}.writeUInt32NoTag(${Expr(field.tag)})
                      ${writeFun(os, tpe1, 'v.unseal)}
                    }
                  }.unseal)
                .seal.asExprOf[Unit]
              // a.field.foreach((v: V) => {
              //   os.writeUInt32NoTag(field.tag)
              //   writeFun(os, v)
              // })
            List(expr)
          else
            List(
              '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
            , '{ ${os}.writeUInt32NoTag(${sizeRef.seal.asExprOf[Int]}) }
            , Select.unique(getter, "foreach")
                .appliedToType(unitLiteral.tpe)
                .appliedTo('{ (v: V) => ${writeFun(os, tpe1, 'v.unseal)} }.unseal)
                .seal.asExprOf[Unit]
              // a.field.foreach((v: V) => writeFun(os, v))
            )
    else
      val prepareArrayRef = Ref(field.prepareArraySym).seal.asExprOf[Array[Prepare]]
      val counterName = s"${field.name}Counter"
      List(
        '{
          @showName(${Expr(counterName)})
          var counter = 0
          while (counter < ${prepareArrayRef}.length) {
            val p = ${prepareArrayRef}(counter)
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${os}.writeUInt32NoTag(p.size)
            p.write(${os}) 
            counter = counter + 1
          }
        }
      )

  def writeMessage[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val prepareRef = Ref(field.prepareSym).seal.asExprOf[Prepare]
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , '{ ${os}.writeUInt32NoTag(${prepareRef}.size) }
    , '{ ${prepareRef}.write(${os}) }
    )

  def size[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
   if field.tpe.isCommonType then sizeCommon(a, field, sizeAcc)
   else if field.tpe.isOption then sizeOption(a, field, sizeAcc)
   else if field.tpe.isIterable then sizeCollection(a, field, sizeAcc)
   else sizeMessage(a, field, sizeAcc)

  def sizeCommon[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val fun = sizeFun(field.tpe, field.getter(a.unseal))
    val sum = '{ ${Expr(CodedOutputStream.computeTagSize(field.num))} + ${fun} }
    List(increment(sizeAcc, sum))
  
  def sizeOption[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val tpe = field.tpe.optionArgument
    val getter: Term = field.getter(a.unseal)
    val getterOption: Term = Select.unique(getter, "get")//getterOptionTerm(a, field)
    if (tpe.isCommonType) then
      val fun = sizeFun(tpe, getterOption)
      val sum = '{ ${Expr(CodedOutputStream.computeTagSize(field.num))} + ${fun} }
      val incrementSize = increment(sizeAcc, sum)
      val isDefined = Select(getter, OptionClass.method("isDefined").head)
      List(If(isDefined, incrementSize, unitLiteral))
    else
      val prepareOptionRhs = '{
        if (${Select.unique(getter, "isDefined").seal.asExprOf[Boolean]}) {
          val p: Prepare = ${Select.unique(findCodec(tpe), "prepare").appliedTo(getterOption).seal.asExprOf[Prepare]}
          ${
            increment(
              sizeAcc
            , '{${Expr(CodedOutputStream.computeTagSize(field.num))} + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
            ).seal
          }
          Some(p)
        } else None
      }
      List(
        ValDef(field.prepareOptionSym, Some(prepareOptionRhs.unseal))
      )

  def sizeCollection[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] = 
    val tpe1 = field.tpe.iterableArgument
    val getter = field.getter(a.unseal)
    val pType = tpe1.seal.asInstanceOf[quoted.Type[_]]
    pType match
      case '[$V] =>
        if tpe1.isCommonType then
          val sizeRef = Ref(field.sizeSym)
          val sizeValDef = ValDef(field.sizeSym, Some(Literal(Constant.Int(0))))
          if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then
            val tagSizeName = s"${field.name}TagSize"
            val sizeExpr = '{ 
              @showName(${Expr(tagSizeName)})
              val tagSize = ${Expr(CodedOutputStream.computeTagSize(field.num))}
              ${
                Select.unique(getter, "foreach")
                  .appliedToType(unitLiteral.tpe)
                  .appliedTo('{ (v: V) => ${ increment(sizeRef, '{ ${sizeFun(tpe1, 'v.unseal)} + tagSize }).seal.asExprOf[Unit] } }.unseal)
                  .seal
              } // a.field.foreach((v: V) =>  sizeRef = sizeRef + sizeFun(v) + tagSize)
            }
            val incrementAcc = increment(sizeAcc, sizeRef.seal.asExprOf[Int])
            List(sizeValDef, sizeExpr.unseal, incrementAcc)
          else
            // a.field.foreach((v: V) =>  sizeRef = sizeRef + sizeFun(v))
            val sizeExpr = 
              Select.unique(getter, "foreach")
                .appliedToType(unitLiteral.tpe)
                .appliedTo('{ (v: V) => ${ increment(sizeRef, sizeFun(tpe1, 'v.unseal)).seal.asExprOf[Unit] } }.unseal)
            val sizeRefExpr = sizeRef.seal.asExprOf[Int]
            val sum = '{ 
              ${Expr(CodedOutputStream.computeTagSize(field.num))} + 
              CodedOutputStream.computeUInt32SizeNoTag(${sizeRefExpr}) +
              ${sizeRefExpr}
            }
            val incrementAcc = increment(sizeAcc, sum)
            List(sizeValDef, sizeExpr, incrementAcc)
        else
          val prepareArrayRef = Ref(field.prepareArraySym)
          val prepareArrayRhs = '{ new Array[Prepare](${Select.unique(getter, "size").seal.asExprOf[Int]}) }
          val counterName = s"${field.name}Counter"
          val sizeExpr = '{
            @showName(${Expr(counterName)})
            var counter = 0
            ${ Select.unique(getter, "foreach")
                  .appliedToType(unitLiteral.tpe)
                  .appliedTo('{ (v: V) =>
                      val p: Prepare = ${Select.unique(findCodec(tpe1), "prepare").appliedTo('v.unseal).seal.asExprOf[Prepare]}
                      ${prepareArrayRef.seal.asExprOf[Array[Prepare]]}(counter) = p
                      ${
                        increment(
                          sizeAcc
                        , '{${Expr(CodedOutputStream.computeTagSize(field.num))} + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
                        ).seal
                      }
                      counter = counter + 1                    
                    }.unseal
                  )
                  .seal 
                  // a.field.foreach((v: V) => {
                  //   val p: Prepare = implicitly[MessageCodec[V]].prepare(v)
                  //   prepareArra[counter] = p
                  //   sizeAcc = sizeAcc + CodedOutputStream.computeTagSize(field.num) + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size
                  //   counter = counter + 1
                  // })
            }
          }
          List(
            ValDef(field.prepareArraySym, Some(prepareArrayRhs.unseal))
          , sizeExpr.unseal
          )

  def sizeMessage[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val getter = field.getter(a.unseal)
    val prepare = Select.unique(findCodec(field.tpe), "prepare").appliedTo(getter)
    val prepareValDef = ValDef(field.prepareSym, Some(prepare))
    val prepareRef = Ref(field.prepareSym).seal.asExprOf[Prepare]
    val sum = '{ 
      ${Expr(CodedOutputStream.computeTagSize(field.num))} + 
      CodedOutputStream.computeUInt32SizeNoTag(${prepareRef}.size) +
      ${prepareRef}.size
    }
    val incrementAcc = increment(sizeAcc, sum)
    List(prepareValDef, incrementAcc)

  def readImpl(t: TypeRepr, params: List[FieldInfo], is: Expr[CodedInputStream])(using ctx: Context): Expr[Any] = {

    val (initStatements, readRefs, resExp): (List[Statement], List[Term], Term) =
      if t.isSealedTrait then
        val _none = Ref(NoneModule)
        val sym = Symbol.newVal(Symbol.currentOwner, "readRes", OptionType.appliedTo(t), Flags.Mutable, Symbol.noSymbol)
        val init = ValDef(sym, Some(_none))
        val ref = Ref(sym)
        val error = s"missing one of required field for ${t.typeSymbol.fullName}"
        val exception = '{ throw new RuntimeException(${Expr(error)}) }.unseal
        val res = ref.select(OptionClass.method("getOrElse").head)
          .appliedToType(t)
          .appliedTo(exception) // ref.getOrElse[t](exception)
        (List(init), List.fill(params.size)(ref), res)
      else
        val xs = params.map(p => {
          val (init, ref) = initValDef(p)
          val res = resTerm(ref, p)
          (init, ref, res)
        }).unzip3
        (xs._1, xs._2, classApply(t, xs._3))

    val tagMatch: Statement = '{
      var done = false
      while (done == false) {
        val tag: Int = ${is}.readTag
        var tagMatch: Boolean = false
        if (tag == 0) { done = true; tagMatch = true }
        ${
          Expr.block(params.zip(readRefs).map{ case (p, ref) => {
            val paramTag = Expr(p.tag)
            val readContent = readContentImpl(p, ref, is)
            '{  if (tag == ${paramTag}) { 
                  tagMatch = true
                  ${readContent}
                }
            }
          }}, unitExpr)
        }
        if (tagMatch == false) ${is}.skipField(tag)
      }
    }.unseal

    val statements =
      if (params.size > 0) then initStatements :+ (tagMatch) 
      else Nil
    
    Block(
      statements
    , resExp
    ).seal
  }

  def readContentImpl(p: FieldInfo, readRef: Term, is: Expr[CodedInputStream]): Expr[Any] =
    if (p.tpe.isCommonType) then
      val fun: Term = readFun(p.tpe, is)
      Assign(
        readRef
      , Some_Apply(tpe=p.tpe, value=fun)
      ).seal
    else if p.tpe.isOption && p.tpe.optionArgument.isCommonType then
      val tpe1 = p.tpe.optionArgument
      val fun: Term = readFun(tpe1, is)
      Assign(
        readRef
      , Some_Apply(tpe=tpe1, value=fun)
      ).seal
    else if p.tpe.isOption then
      val tpe1 = p.tpe.optionArgument
      val fun: Term = Select.unique(findCodec(tpe1), "read").appliedTo(is.unseal)
      putLimit(
        is
      , Assign(
          readRef
        , Some_Apply(tpe=tpe1, value=fun)
        ).seal      
      )
    else if p.tpe.isIterable && p.tpe.iterableArgument.isCommonType then
      val tpe1 = p.tpe.iterableArgument
      val fun: Term = readFun(tpe1, is)
      val addOneApply = Apply(
        Select(readRef, readRef.tpe.termSymbol.method("addOne").head)
      , List(fun)
      ).seal
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then 
        addOneApply
      else
        putLimit(
          is
        , '{ while (${is}.getBytesUntilLimit > 0) ${addOneApply} }
        )
    else if p.tpe.isIterable then
      val tpe1 = p.tpe.iterableArgument
      val fun: Term = Select.unique(findCodec(tpe1), "read").appliedTo(is.unseal)
      val addOneApply = Apply(
        Select(readRef, readRef.tpe.termSymbol.method("addOne").head)
      , List(fun)
      ).seal
      putLimit(
        is
      , addOneApply
      )
    else
      val fun: Term = Select.unique(findCodec(p.tpe), "read").appliedTo(is.unseal)
      putLimit(
        is
      , Assign(
          readRef
        , Some_Apply(tpe=p.tpe, value=fun)
        ).seal 
      )

  def putLimit(is: Expr[CodedInputStream], read: Expr[Any]): Expr[Unit] =
    '{
      val readSize: Int = ${is}.readRawVarint32
      val limit = ${is}.pushLimit(readSize)
      ${read}
      ${is}.popLimit(limit)
    }

  def initValDef(field: FieldInfo)(using ctx: Context): (ValDef, Ref) =
    if field.tpe.isOption then
      val _none = Ref(NoneModule)
      val sym = Symbol.newVal(Symbol.currentOwner, s"${field.name}Read", field.tpe, Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym)
      init -> ref
    else if field.tpe.isIterable then
      val tpe1 = field.tpe.iterableArgument
      val collectionType = field.tpe.iterableBaseType
      val collectionCompanion = collectionType.typeSymbol.companionModule
      val newBuilderMethod = collectionCompanion.method("newBuilder").head
      val builderTpe = builderType.appliedTo(List(tpe1, field.tpe))
      val sym = Symbol.newVal(Symbol.currentOwner, s"${field.name}Read", builderTpe, Flags.EmptyFlags, Symbol.noSymbol)
      val rhs = Select(Ref(collectionCompanion), newBuilderMethod)
      val init = ValDef(sym, Some(rhs))
      val ref = Ref(sym)
      init -> ref
    else
      val _none = Ref(NoneModule)
      val sym = Symbol.newVal(Symbol.currentOwner, s"${field.name}Read", OptionType.appliedTo(field.tpe), Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym)
      init -> ref

  def resTerm(ref: Ref, field: FieldInfo): Term =
    if field.tpe.isOption then ref
    else if field.tpe.isIterable then
      Select(ref, ref.tpe.termSymbol.method("result").head)
    else
      val error = s"missing required field `${field.name}: ${field.tpe.typeSymbol.name}`"
      val exception = '{ throw new RuntimeException(${Expr(error)}) }.unseal
      val orElse = field.defaultValue.getOrElse(exception)
      ref.select(OptionClass.method("getOrElse").head)
        .appliedToType(field.tpe)
        .appliedTo(orElse) // ref.getOrElse[feld.tpe](orElse)
  
  def findCodec(t: TypeRepr): Term = 
    val tpe = TypeRepr.of[MessageCodec].appliedTo(t)
    Implicits.search(tpe) match {
      case x: ImplicitSearchSuccess => x.tree
      case _: ImplicitSearchFailure => throwError(s"could not find implicit codec for `${t.typeSymbol.fullName}`")
    }

  def classApply(t: TypeRepr, params: List[Term]): Term =
    t match
      case x: TermRef => Ident(x)
      case x: TypeRef =>
        val companion = x.typeSymbol.companionModule
        val applyMethod = companion.method("apply").head
        Apply(Select(Ref(companion), applyMethod), params)
      case x: AppliedType =>
        val companion = x.typeSymbol.companionModule
        val applyMethod = companion.method("apply").head
        Ref(companion).select(applyMethod)
          .appliedToTypes(x.typeArgs)
          .appliedToArgs(params)

  def increment(x: Ref, y: Expr[Int]): Assign =  Assign(x, '{ ${x.seal.asExprOf[Int]} + ${y} }.unseal)
}