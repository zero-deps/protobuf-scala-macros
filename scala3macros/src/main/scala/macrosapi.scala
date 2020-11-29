package zd
package proto

import scala.language.experimental.macros
import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._, report._
import scala.collection.immutable.ArraySeq
import zd.proto.Bytes

object macrosapi {

  def caseCodecAuto[A]: MessageCodec[A] = macro Impl.caseCodecAuto[A]
  inline def caseCodecAuto[A]: MessageCodec[A] = ${Macro.caseCodecAuto[A]}

  def caseCodecNums[A](nums: (String, Int)*): MessageCodec[A] = macro Impl.caseCodecString[A]
  inline def caseCodecNums[A](inline nums: (String, Int)*): MessageCodec[A] = ${Macro.caseCodecNums[A]('nums)}

  def caseCodecIdx[A]: MessageCodec[A] = macro Impl.caseCodecIdx[A]
  inline def caseCodecIdx[A]: MessageCodec[A] = ${Macro.caseCodecIdx[A]}

  def classCodecAuto[A]: MessageCodec[A] = macro Impl.classCodecAuto[A]
  inline def classCodecAuto[A]: MessageCodec[A] = ${Macro.classCodecAuto[A]}

  def classCodecNums[A](nums: (String, Int)*)(constructor: Any): MessageCodec[A] = macro Impl.classCodecString[A]
  inline def classCodecNums[A](nums: (String, Int)*)(constructor: Any): MessageCodec[A] = ${Macro.classCodecNums[A]('nums)}

  def sealedTraitCodecAuto[A]: MessageCodec[A] = macro Impl.sealedTraitCodecAuto[A]
  inline def sealedTraitCodecAuto[A]: MessageCodec[A] = ${Macro.sealedTraitCodecAuto[A]}

  def sealedTraitCodecNums[A](nums: (String, Int)*): MessageCodec[A] = macro Impl.sealedTraitCodecString[A]
  inline def sealedTraitCodecNums[A](nums: (String, Int)*): MessageCodec[A] = ${Macro.sealedTraitCodecNums[A]('nums)}

}

object Macro {
  def caseCodecAuto[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl3().caseCodecAuto[A]
  def caseCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]])(using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl3().caseCodecNums[A](numsExpr)
  def caseCodecIdx[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl3().caseCodecIdx[A]
  def classCodecAuto[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl3().classCodecAuto[A]
  def classCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]])(using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl3().classCodecNums[A](numsExpr)
  def enumByN[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl3().enumByN[A]
  def sealedTraitCodecAuto[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl3().sealedTraitCodecAuto[A]
  def sealedTraitCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]])(using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl3().sealedTraitCodecNums[A](numsExpr)
}

private class Impl3(using val qctx: QuoteContext) extends BuildCodec3 {
  import qctx.reflect.{_, given}
  import qctx.reflect.defn._

  def caseCodecAuto[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val aType = getCaseClassType[A]
    val aTypeSymbol = aType.typeSymbol
    val typeName = aTypeSymbol.fullName
    val params: List[Symbol] = aTypeSymbol.caseClassValueParams
    val nums: List[(String, Int)] = params.map(p =>
      p.annots.collect{ case Apply(Select(New(tpt),_), List(Literal(Constant.Int(num)))) if tpt.tpe.isNType => p.name -> num } match {
        case List(x) => x
        case Nil => throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
        case _ => throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
      }
    )
    messageCodec(aType, nums, params, restrictDefaults=true)
  }

  def caseCodecNums[A: quoted.Type](numsExpr: Expr[Seq[(String, Int)]]): Expr[MessageCodec[A]] = {
    val nums: Seq[(String, Int)] = numsExpr match {
      case Varargs(argExprs) =>
        argExprs.collect{
          case '{ ($x:String, $y:Int) } => x.unseal -> y.unseal
          case '{ ($x:String) -> ($y:Int) } => x.unseal -> y.unseal
        }.collect{
          case (Literal(Constant.String(name)), Literal(Constant.Int(num))) => name -> num
        }
      case _ => Seq()
    }
    val aType = getCaseClassType[A]
    val aTypeSymbol = aType.typeSymbol
    val params: List[Symbol] = aTypeSymbol.caseClassValueParams
    messageCodec(aType, nums, params, restrictDefaults=true)
  }

  def caseCodecIdx[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val aType = getCaseClassType[A]
    val aTypeSymbol = aType.typeSymbol
    val params: List[Symbol] = aTypeSymbol.caseClassValueParams
    val nums: List[(String, Int)] = params.zipWithIndex.map{case (p, idx) => (p.name, idx + 1) }
    messageCodec(aType, nums, params, restrictDefaults=false)
  }

  def messageCodec[A: quoted.Type](a_tpe: TypeRepr, nums: Seq[(String, Int)], cParams: List[Symbol], restrictDefaults: Boolean)(using ctx: Context): Expr[MessageCodec[A]] = {
    val aTypeSym = a_tpe.typeSymbol
    val aTypeCompanionSym = aTypeSym.companionModule
    val typeName = aTypeSym.fullName
    
    if (nums.exists(_._2 < 1)) throwError(s"nums ${nums} should be > 0")
    if (nums.size != cParams.size) throwError(s"nums size ${nums} not equal to `${typeName}` constructor params size ${cParams.size}")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) throwError(s"nums ${nums} should be unique")
    val restrictedNums = a_tpe.restrictedNums
    val typeArgsToReplace: Map[String, TypeRepr] = a_tpe.typeArgsToReplace

    val fields: List[FieldInfo] = cParams.zipWithIndex.map{ case (s, i) =>
      val (name, tpe) = s.tree match  
        case ValDef(v_name, v_tpt, v_rhs) => 
          typeArgsToReplace.get(v_tpt.tpe.typeSymbol.name) match
            case Some(typeArg) => (v_name, typeArg)
            case None => (v_name, v_tpt.tpe)
        case _ => throwError(s"wrong param definition of case class `${typeName}`")
      
      val defaultValue: Option[Term] = aTypeCompanionSym.method(defaultMethodName(i)) match
        case List(x) =>
          if tpe.isOption && restrictDefaults then throwError(s"`${name}: ${tpe.typeSymbol.fullName}`: default value for Option isn't allowed")
          else if tpe.isIterable && restrictDefaults then throwError(s"`${name}: ${tpe.typeSymbol.fullName}`: default value for collections isn't allowed")
          else Some(Select(Ref(aTypeCompanionSym), x))
        case _ => None
      val num: Int =
        nums.collectFirst{ case (name1, num1) if name1 == name =>
          if restrictedNums.contains(num1) then throwError(s"num ${num1} for `${typeName}` is restricted") 
          else num1
        }.getOrElse{
          throwError(s"missing num for `${name}: ${typeName}`")
        }
      FieldInfo(
        name = name
      , num = num
      , tpe = tpe
      , getter = (a: Term) => Select.unique(a, name)
      , sizeSym = Symbol.newVal(Symbol.currentOwner, s"${name}Size", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(Symbol.currentOwner, s"${name}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , prepareOptionSym = Symbol.newVal(Symbol.currentOwner, s"${name}Prepare", OptionType.appliedTo(PrepareType), Flags.Mutable, Symbol.noSymbol)
      , prepareArraySym = Symbol.newVal(Symbol.currentOwner, s"${name}Prepare", TypeRepr.of[Array[Prepare]], Flags.Mutable, Symbol.noSymbol)
      , defaultValue = defaultValue
      )
    }
    '{ 
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ${ prepareImpl('a, fields) }
        def read(is: CodedInputStream): A = ${ readImpl(a_tpe, fields, 'is).asExprOf[A] }
        val nums: Map[String, Int] = Map()
        val aType: String = ""
      }
    }
  }

  def classCodecAuto[A: quoted.Type]: Expr[MessageCodec[A]] = {
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ???
        def read(is: CodedInputStream): A = ???
        val nums: Map[String, Int] = Map()
        val aType: String = ""
      }
    }
  }

  def sealedTraitCodecNums[A: quoted.Type](numsExpr: Expr[Seq[(String, Int)]]): Expr[MessageCodec[A]] = {
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ???
        def read(is: CodedInputStream): A = ???
        val nums: Map[String, Int] = Map()
        val aType: String = ""
      }
    }
  }

  def enumByN[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val ctx = summon[Context]
    val t = summon[quoted.Type[A]]
    val aType = t.unseal.tpe
    val aTypeSym = aType.typeSymbol
    val typeName = aTypeSym.fullName
    val xs = aTypeSym.children
    val restrictedN: List[Int] = aType.restrictedNums
    xs.map{ x =>
      val num: Int =
        x.annots.collect{
          case Apply(Select(New(tpt),_), List(Literal(Constant.Int(num1)))) if tpt.tpe.isNType => num1
        } match {
          case List(num1) if restrictedN.contains(num1) => throwError(s"num ${num1} for `${typeName}` is restricted") 
          case List(num1) => num1
          case Nil => throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
          case _ => throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
        }
      // val field = FieldInfo(
      //   name = s"field${num}"
      // , num = num
      // , tpe = ???
      // , tpt = ???
      // , getter = aTypeSym //?
      // , sizeSym = Symbol.newVal(ctx.owner, s"field${num}Size", IntType, Flags.Mutable, Symbol.noSymbol)
      // , prepareSym = Symbol.newVal(ctx.owner, s"field${num}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      // , prepareOptionSym = Symbol.newVal(ctx.owner, s"field${num}Prepare", appliedOptionType(PrepareType), Flags.Mutable, Symbol.noSymbol)
      // , prepareArraySym = Symbol.newVal(ctx.owner, s"field${num}Prepare", typeOf[Array[Prepare]], Flags.Mutable, Symbol.noSymbol)
      // , defaultValue = None
      // )
      // prepareImpl(x, List(field))
    }
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ???
        def read(is: CodedInputStream): A = ???
        val nums: Map[String, Int] = Map()
        val aType: String = ""
      }
    }
  }

  def sealedTraitCodecAuto[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val aType = getSealedTrait[A]
    val aTypeSymbol = aType.typeSymbol
    val typeName = aTypeSymbol.fullName
    val xs = aTypeSymbol.children
    val nums: List[(TypeRepr, Int)] = xs.map{ x =>
      x.annots.collect{ case Apply(Select(New(tpt),_), List(Literal(Constant.Int(num)))) if tpt.tpe.isNType => x.tpe -> num } match
        case List(x) => x
        case Nil => throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
        case _ => throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
    }
    sealedTraitCodec(aType, nums)
  }

  def classCodecNums[A: quoted.Type](numsExpr: Expr[Seq[(String, Int)]]): Expr[MessageCodec[A]] = {
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ???
        def read(is: CodedInputStream): A = ???
        val nums: Map[String, Int] = Map()
        val aType: String = ""
      }
    }
  }

  def sealedTraitCodec[A: quoted.Type](a_tpe: TypeRepr, nums: Seq[(TypeRepr, Int)]): Expr[MessageCodec[A]] = {
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val subclasses = aTypeSymbol.children

    if (subclasses.size <= 0) throwError(s"required at least 1 subclass for `${typeName}`")
    if (nums.size != subclasses.size) throwError(s"`${typeName}` subclasses ${subclasses.size} count != nums definition ${nums.size}")
    if (nums.exists(_._2 < 1)) throwError(s"nums for ${typeName} should be > 0")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) throwError(s"nums for ${typeName} should be unique")
    val restrictedNums = a_tpe.restrictedNums

    val fields: List[FieldInfo] = subclasses.map{ s =>
      val tpe = s.tpe
      val num: Int = nums.collectFirst{ case (tpe1, num) if tpe =:= tpe1 => num }.getOrElse(throwError(s"missing num for class `${tpe}` of trait `${a_tpe}`"))
      if (restrictedNums.contains(num)) throwError(s"num ${num} is restricted for class `${tpe}` of trait `${a_tpe}`")
    
      FieldInfo(
        name = s.fullName
      , num = num
      , tpe = tpe
      , getter = 
          if s.isTerm then
            (a: Term) => Ref(s)
          else
            (a: Term) => Select.unique(a, "asInstanceOf").appliedToType(tpe)
      , sizeSym = Symbol.newVal(Symbol.currentOwner, s"field${num}Size", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(Symbol.currentOwner, s"field${num}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , prepareOptionSym = Symbol.noSymbol
      , prepareArraySym = Symbol.noSymbol
      , defaultValue = None
      )
    }
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ${ prepareTrait('a, fields) }
        def read(is: CodedInputStream): A = ${ readImpl(a_tpe, fields, 'is).asExprOf[A] }
        val nums: Map[String, Int] = Map()
        val aType: String = ""
      }
    }
  }

  private def getSealedTrait[A: quoted.Type]: TypeRepr = {
    val t = summon[quoted.Type[A]]
    val tpe = t.unseal.tpe
    if tpe.isSealedTrait then tpe else throwError(s"`${tpe.typeSymbol.fullName}` is not a sealed trait. Make sure that you specify codec type explicitly.\nExample:\n implicit val codecName: MessageCodec[SealedTraitTypeHere] = ...\n\n")
  }

  private def getCaseClassType[A: quoted.Type]: TypeRepr = {
    val t = summon[quoted.Type[A]]
    val tpe = t.unseal.tpe
    if tpe.isCaseType then tpe else throwError(s"`${tpe.typeSymbol.fullName}` is not a case class")
  }

}