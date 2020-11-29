package zd
package proto

import proto.api.{MessageCodec, Prepare, N, RestrictedN}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._, report._
import scala.collection.immutable.ArraySeq
import zd.proto.Bytes

trait Common3 {
  implicit val qctx: QuoteContext
  import qctx.reflect.{_, given}
  import qctx.reflect.defn._

  private[proto] case class FieldInfo(
    name: String
  , num: Int
  , tpe: TypeRepr
  , getter: Term => Term
  , sizeSym: Symbol
  , prepareSym: Symbol
  , prepareOptionSym: Symbol
  , prepareArraySym: Symbol
  , defaultValue: Option[Term]
  ) {
    def tag: Int = num << 3 | wireType(tpe)
  }

  def wireType(t: TypeRepr): Int =
    if      t.isInt || t.isLong || t.isBoolean then 0
    else if t.isDouble then 1
    else if t.isFloat then 5
    else if t.isOption then wireType(t.optionArgument)
    else if t.isString || 
            t.isArrayByte || 
            t.isArraySeqByte || 
            t.isBytesType || 
            t.isCaseType ||
            t.isSealedTrait ||
            t.isIterable then 2
    else 2

  def writeFun(os: Expr[CodedOutputStream], t: TypeRepr, getterTerm: Term): Expr[Unit] =
    val getValue = getterTerm.seal
    if      t.isInt then '{ ${os}.writeInt32NoTag(${getValue.asExprOf[Int]}) }
    else if t.isLong then '{ ${os}.writeInt64NoTag(${getValue.asExprOf[Long]}) }
    else if t.isBoolean then '{ ${os}.writeBoolNoTag(${getValue.asExprOf[Boolean]}) }
    else if t.isDouble then '{ ${os}.writeDoubleNoTag(${getValue.asExprOf[Double]}) }
    else if t.isFloat then '{ ${os}.writeFloatNoTag(${getValue.asExprOf[Float]}) }
    else if t.isString then '{ ${os}.writeStringNoTag(${getValue.asExprOf[String]}) }
    else if t.isArrayByte then '{ ${os}.writeByteArrayNoTag(${getValue.asExprOf[Array[Byte]]}) }
    else if t.isArraySeqByte then '{ ${os}.writeByteArrayNoTag(${getValue.asExprOf[ArraySeq[Byte]]}.toArray[Byte]) }
    else if t.isBytesType then '{ ${os}.writeByteArrayNoTag(${getValue.asExprOf[Bytes]}.unsafeArray) }
    else throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  def sizeFun(t: TypeRepr, getterTerm: Term): Expr[Int] =
    val CodedOutputStreamRef = Ref(TypeRepr.of[CodedOutputStream].typeSymbol.companionModule)
    val getValue = getterTerm.seal
    if      t.isInt then '{ CodedOutputStream.computeInt32SizeNoTag(${getValue.asExprOf[Int]}) }
    else if t.isLong then '{ CodedOutputStream.computeInt64SizeNoTag(${getValue.asExprOf[Long]}) }
    else if t.isBoolean then Expr(1)
    else if t.isDouble then Expr(8)
    else if t.isFloat then Expr(4)
    else if t.isString then '{ CodedOutputStream.computeStringSizeNoTag(${getValue.asExprOf[String]}) }
    else if t.isArrayByte then '{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.asExprOf[Array[Byte]]}) }
    else if t.isArraySeqByte then '{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.asExprOf[ArraySeq[Byte]]}.toArray[Byte]) }
    else if t.isBytesType then '{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.asExprOf[Bytes]}.unsafeArray) }
    else throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  def readFun(t: TypeRepr, is: Expr[CodedInputStream]): Term =
    if      t.isInt then '{ ${is}.readInt32 }.unseal
    else if t.isLong then '{ ${is}.readInt64 }.unseal
    else if t.isBoolean then '{ ${is}.readBool }.unseal
    else if t.isDouble then '{ ${is}.readDouble }.unseal
    else if t.isFloat then '{ ${is}.readFloat }.unseal
    else if t.isString then '{ ${is}.readString }.unseal
    else if t.isArrayByte then '{ ${is}.readByteArray }.unseal
    else if t.isArraySeqByte then '{ ArraySeq.unsafeWrapArray(${is}.readByteArray) }.unseal
    else if t.isBytesType then '{ Bytes.unsafeWrap(${is}.readByteArray) }.unseal
    else throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  val ArrayByteType: TypeRepr = TypeRepr.of[Array[Byte]]
  val ArraySeqByteType: TypeRepr = TypeRepr.of[ArraySeq[Byte]]
  val BytesType: TypeRepr = TypeRepr.of[Bytes]
  val NTpe: TypeRepr = TypeRepr.of[N]
  val RestrictedNType: TypeRepr = TypeRepr.of[RestrictedN]
  val ItetableType: TypeRepr = TypeRepr.of[scala.collection.Iterable[_]]
  val PrepareType: TypeRepr = TypeRepr.of[Prepare]
  val CodedInputStreamType: TypeRepr = TypeRepr.of[CodedInputStream]
  
  extension (s: Symbol)
    def caseClassValueParams: List[Symbol] = s.primaryConstructor.paramSymss.find(_.headOption.fold(false)( _.isTerm)).getOrElse(Nil)
    def tpe: TypeRepr = 
      s.tree match
        case x: ClassDef => x.constructor.returnTpt.tpe
        case Bind(_, pattern: Term) => pattern.tpe

  def unitLiteral: Literal = Literal(Constant.Unit())
  def defaultMethodName(i: Int): String = s"$$lessinit$$greater$$default$$${i+1}"

  def unitExpr: Expr[Unit] = unitLiteral.seal.asExprOf[Unit]

  def builderType: TypeRepr = TypeRepr.of[scala.collection.mutable.Builder]
    
  def OptionType: TypeRepr = TypeRepr.of[Option]

  def Some_Apply(tpe: TypeRepr, value: Term): Term =
    Select.unique(Ref(SomeModule.companionModule), "apply")
      .appliedToType(tpe)
      .appliedTo(value)

  val commonTypes: List[TypeRepr] =
    TypeRepr.of[String] :: TypeRepr.of[Int] :: TypeRepr.of[Long] :: TypeRepr.of[Boolean] :: TypeRepr.of[Double] :: TypeRepr.of[Float] :: ArrayByteType :: ArraySeqByteType :: BytesType :: Nil 

  extension (t: TypeRepr)
    def isNType: Boolean = t =:= NTpe
    def isCaseClass: Boolean = t.typeSymbol.flags.is(Flags.Case)
    def isCaseObject: Boolean = t.termSymbol.flags.is(Flags.Case)
    def isCaseType: Boolean = t.isCaseClass || t.isCaseObject
    def isSealedTrait: Boolean = t.typeSymbol.flags.is(Flags.Sealed) && t.typeSymbol.flags.is(Flags.Trait)
    def isIterable: Boolean = t <:< ItetableType && !t.isArraySeqByte
    def isString: Boolean = t =:= TypeRepr.of[String]
    def isInt: Boolean = t =:= TypeRepr.of[Int]
    def isLong: Boolean = t =:= TypeRepr.of[Long]
    def isBoolean: Boolean = t =:= TypeRepr.of[Boolean]
    def isDouble: Boolean = t =:= TypeRepr.of[Double]
    def isFloat: Boolean = t =:= TypeRepr.of[Float]
    def isArrayByte: Boolean = t =:= ArrayByteType
    def isArraySeqByte: Boolean = t =:= ArraySeqByteType
    def isBytesType: Boolean = t =:= BytesType
    def isCommonType: Boolean = commonTypes.exists(_ =:= t)

    def typeArgsToReplace: Map[String, TypeRepr] =
      t.typeSymbol.primaryConstructor.paramSymss
      .find(_.headOption.fold(false)( _.isType))
      .map(_.map(_.name).zip(t.typeArgs)).getOrElse(Nil)
      .toMap

    def isOption: Boolean = t match
      case AppliedType(t1, _) if t1.typeSymbol == OptionClass => true
      case _ => false

    def typeArgs: List[TypeRepr] = t match
      case AppliedType(t1, args)  => args.map(_.asInstanceOf[TypeRepr])//TODO
      case _ => Nil

    def optionArgument: TypeRepr = t match
      case AppliedType(t1, args) if t1.typeSymbol == OptionClass => args.head.asInstanceOf[TypeRepr]
      case _ => throwError(s"It isn't Option type: ${t.typeSymbol.name}")

    def iterableArgument: TypeRepr = t.baseType(ItetableType.typeSymbol) match
      case AppliedType(_, args) if t.isIterable => args.head.asInstanceOf[TypeRepr]
      case _ => throwError(s"It isn't Iterable type: ${t.typeSymbol.name}")

    def iterableBaseType: TypeRepr = t match
      case AppliedType(t1, _) if t.isIterable => t1
      case _ => throwError(s"It isn't Iterable type: ${t.typeSymbol.name}")

    def restrictedNums: List[Int] =
      val aName = RestrictedNType.typeSymbol.name
      val tName = t.typeSymbol.fullName
      t.typeSymbol.annots.collect{ case Apply(Select(New(tpt),_), List(Typed(Repeated(args,_),_))) if tpt.tpe =:= RestrictedNType => args } match
        case List(Nil) => throwError(s"empty annotation ${aName} for `${tName}`")
        case List(xs) =>
          val nums = xs.collect{
            case Literal(Constant.Int(n)) => n
            case x => throwError(s"wrong annotation ${aName} for `${tName}` $x")
          }
          if (nums.size != nums.distinct.size) throwError(s"nums not unique in annotation ${aName} for `${tName}`")
          nums
        case Nil => Nil
        case _ => throwError(s"multiple ${aName} annotations applied for `${tName}`")

  def mkIfStatement(branches: List[(Term, Term)], elseBranch: Term): Term =
    branches match
      case (cond, thenp) :: xs =>
        If(cond, thenp, mkIfStatement(xs, elseBranch))
      case Nil => elseBranch
}