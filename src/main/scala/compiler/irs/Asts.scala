package compiler.irs

import compiler.reporting.Position
import identifiers.*
import lang.Types.*
import lang.Types.PrimitiveType.*
import lang.{Device, FunctionSignature, Keyword, Operator, TypeConversion}

object Asts {

  sealed abstract class Ast {
    // Positions are propagated by the TreeParser
    // Each AST is assigned the position of its leftmost token (by the map method of TreeParser)
    private var positionOpt: Option[Position] = None

    final def setPosition(posOpt: Option[Position]): Unit = {
      positionOpt = posOpt
    }

    final def setPosition(pos: Position): Unit = {
      setPosition(Some(pos))
    }

    final def getPosition: Option[Position] = positionOpt

    def children: List[Ast]

    final def collect[T](f: Ast => T): List[T] = {
      f(this) :: children.flatMap(_.collect(f))
    }

    /**
     * Crashes the compiler if at least 1 expression in the AST has no type (i.e. `expr.getTypeOpt.isEmpty`)
     */
    final def assertAllTypesAreSet(): Unit = {
      collect {
        case expr: Expr => assert(expr.getTypeOpt.isDefined, s"no type set for $expr")
        case _ => ()
      }
    }
  }

  sealed abstract class Statement extends Ast

  sealed abstract class Expr extends Statement {
    private var tpeOpt: Option[Type] = None

    /**
     * Set the type that has been inferred for this expression
     *
     * WARNING: since subclasses are allowed to override [[getTypeOpt]], setting a type might have no effect
     */
    def setTypeOpt(tpe: Option[Type]): Expr = {
      tpeOpt = tpe
      this
    }

    /**
     * See [[setTypeOpt]]
     */
    final def setType(tpe: Type): Expr = {
      setTypeOpt(Some(tpe))
    }

    /**
     * @return the type that has been inferred for this expression
     *
     *         WARNING: might be overriden by subclasses if they are able to infer their type from the types of their children
     */
    def getTypeOpt: Option[Type] = tpeOpt

    /**
     * See [[getTypeOpt]]
     */
    final def getType: Type = {
      getTypeOpt match
        case Some(tpe) => tpe
        case None => throw new NoSuchElementException(s"type missing in $this")
    }

  }

  /**
   * Code source (most of the time a file)
   */
  final case class Source(defs: List[TopLevelDef]) extends Ast {
    private var name: String = "<missing name>"

    override def children: List[Ast] = defs

    def setName(name: String): Source = {
      this.name = name
      this
    }

    def getName: String = name
  }

  /**
   * Block (scope):
   * {{{
   *   {
   *    ...
   *   }
   * }}}
   */
  final case class Block(stats: List[Statement]) extends Statement {
    override def children: List[Ast] = stats
  }

  sealed abstract class TopLevelDef extends Ast
  
  sealed trait TypeDefTree extends TopLevelDef {
    def name: TypeIdentifier
    def directSupertypes: Seq[TypeIdentifier]
    def isInterface: Boolean
  }
  
  sealed trait ModuleOrPackageDefTree extends TypeDefTree {
    def functions: List[FunDef]
    def isPackage: Boolean
  }
  
  final case class PackageDef(packageName: TypeIdentifier, functions: List[FunDef]) extends ModuleOrPackageDefTree {
    override def name: TypeIdentifier = packageName
    override def directSupertypes: Seq[TypeIdentifier] = Nil
    override def isInterface: Boolean = false
    override def children: List[Ast] = functions
    override def isPackage: Boolean = true
  }

  final case class ModuleDef(
                              moduleName: TypeIdentifier,
                              imports: List[Import],
                              functions: List[FunDef]
                            ) extends ModuleOrPackageDefTree {
    override def name: TypeIdentifier = moduleName
    override def directSupertypes: Seq[TypeIdentifier] = Nil
    override def isInterface: Boolean = false
    override def children: List[Ast] = imports ++ functions
    override def isPackage: Boolean = false
  }

  sealed trait Import extends Ast

  final case class ParamImport(paramId: FunOrVarId, paramType: Type) extends Import {
    override def children: List[Ast] = Nil
  }

  final case class PackageImport(packageId: TypeIdentifier) extends Import {
    override def children: List[Ast] = Nil
  }

  final case class DeviceImport(device: Device) extends Import {
    override def children: List[Ast] = Nil
  }


  /**
   * Structure (`struct`) or interface definition
   */
  final case class StructDef(
                              structName: TypeIdentifier,
                              fields: List[Param],
                              directSupertypes: Seq[TypeIdentifier],
                              isInterface: Boolean
                            ) extends TypeDefTree {
    override def name: TypeIdentifier = structName
    override def children: List[Ast] = fields
  }

  /**
   * Function definition
   */
  final case class FunDef(funName: FunOrVarId, params: List[Param], optRetType: Option[Type], body: Block) extends Ast {
    val signature: FunctionSignature = FunctionSignature(funName, params.map(_.tpe), optRetType.getOrElse(VoidType))

    override def children: List[Ast] = params :+ body
  }

  /**
   * Parameter of a function or field of a struct
   */
  final case class Param(paramNameOpt: Option[FunOrVarId], tpe: Type, isReassignable: Boolean) extends Ast {
    override def children: List[Ast] = Nil
  }

  /**
   * Constant definition
   */
  final case class ConstDef(constName: FunOrVarId, tpeOpt: Option[Type], value: Literal) extends TopLevelDef {
    override def children: List[Ast] = List(value)
  }

  /**
   * `val` or `var` definition
   *
   * @param optType        type, if explicitely given
   * @param rhs            expression producing the value that will be assigned to the local
   * @param isReassignable `true` if `var`, `false` if `val`
   */
  final case class LocalDef(localName: FunOrVarId, var optType: Option[Type], rhs: Expr, isReassignable: Boolean) extends Statement {
    val keyword: Keyword = if isReassignable then Keyword.Var else Keyword.Val

    override def children: List[Ast] = List(rhs)
  }

  sealed abstract class Literal extends Expr {
    val value: Any

    final override def children: List[Ast] = Nil

    override def getTypeOpt: Option[Type]
  }

  sealed trait NumericLiteral extends Literal

  sealed trait NonNumericLiteral extends Literal

  /**
   * Integer literal
   */
  final case class IntLit(value: Int) extends NumericLiteral {
    override def getTypeOpt: Option[Type] = Some(IntType)
  }

  /**
   * Double literal
   */
  final case class DoubleLit(value: Double) extends NumericLiteral {
    override def getTypeOpt: Option[Type] = Some(DoubleType)
  }

  /**
   * Char literal
   */
  final case class CharLit(value: Char) extends NonNumericLiteral {
    override def getTypeOpt: Option[Type] = Some(CharType)
  }

  /**
   * Bool (boolean) literal
   */
  final case class BoolLit(value: Boolean) extends NonNumericLiteral {
    override def getTypeOpt: Option[Type] = Some(BoolType)
  }

  /**
   * String literal
   */
  final case class StringLit(value: String) extends NonNumericLiteral {
    override def getTypeOpt: Option[Type] = Some(StringType)
  }
  
  final case class RegionCreation() extends Expr {
    override def children: List[Ast] = Nil

    override def getTypeOpt: Option[Type] = Some(PrimitiveType.RegionType)
  }

  /**
   * Occurence of a variable (`val`, `var`, function parameter, etc.)
   */
  final case class VariableRef(name: FunOrVarId) extends Expr {
    override def children: List[Ast] = Nil
  }
  
  final case class MeRef() extends Expr {
    override def children: List[Ast] = Nil
  }
  
  final case class PackageRef(pkgName: TypeIdentifier) extends Expr {
    override def children: List[Ast] = Nil
  }

  final case class DeviceRef(device: Device) extends Expr {
    override def children: List[Ast] = Nil
  }
  
  /**
   * Function call: `callee(args)`
   */
  final case class Call(receiverOpt: Option[Expr], function: FunOrVarId, args: List[Expr]) extends Expr {
    private var _sig: Option[(NamedType, FunctionSignature)] = None
    
    def resolve(receiverType: NamedType, sig: FunctionSignature): Unit = {
      _sig = Some((receiverType, sig))
    }
    
    def getSignature: Option[(NamedType, FunctionSignature)] = _sig
    
    override def children: List[Ast] = receiverOpt.toList ++ args
  }

  /**
   * Array indexing: `indexed[arg]`
   */
  final case class Indexing(indexed: Expr, arg: Expr) extends Expr {
    override def children: List[Ast] = List(indexed, arg)
  }

  /**
   * Initialization of an (empty) array
   */
  final case class ArrayInit(region: Expr, elemType: Type, size: Expr) extends Expr {
    override def children: List[Ast] = List(region, size)
  }

  /**
   * Initialization of an array that contains all the elements in `arrayElems` (in order)
   */
  final case class FilledArrayInit(regionOpt: Option[Expr], arrayElems: List[Expr]) extends Expr {
    override def children: List[Ast] = regionOpt.toList ++ arrayElems
  }
  
  final case class StructOrModuleInstantiation(regionOpt: Option[Expr], typeId: TypeIdentifier, args: List[Expr]) extends Expr {
    override def children: List[Ast] = regionOpt.toList ++ args
  }

  /**
   * Unary operator
   */
  final case class UnaryOp(operator: Operator, operand: Expr) extends Expr {
    override def children: List[Ast] = List(operand)
  }

  /**
   * Binary operator
   */
  final case class BinaryOp(lhs: Expr, operator: Operator, rhs: Expr) extends Expr with SmartCastsAware {

    override def cond: Expr = lhs

    override def thenBr: Statement = rhs
    
    override def children: List[Ast] = List(lhs, rhs)
  }
  
  /**
   * Access to a struct field: `lhs.select`
   */
  final case class Select(lhs: Expr, selected: FunOrVarId) extends Expr {
    override def children: List[Ast] = List(lhs)
  }

  sealed abstract class Assignment extends Statement

  /**
   * Assignment of a value to a variable (or struct field, or in an array): `lhs = rhs`
   */
  final case class VarAssig(lhs: Expr, rhs: Expr) extends Assignment {
    override def children: List[Ast] = List(lhs, rhs)
  }

  /**
   * In-place mutation of a variable (or struct field, or in an array), e.g. `x += 1`
   *
   * @param op the operator <b>without =</b>, e.g. `+` in a `+=` expression
   */
  final case class VarModif(lhs: Expr, rhs: Expr, op: Operator) extends Assignment {
    override def children: List[Ast] = List(lhs, rhs)
  }

  /**
   * If-then-else:
   * {{{
   *   if cond {
   *     thenBr
   *   } else {
   *     elseBr
   *   }
   * }}}
   */
  final case class IfThenElse(cond: Expr, thenBr: Statement, elseBrOpt: Option[Statement]) extends Statement with Conditional {
    override def children: List[Ast] = List(cond, thenBr) ++ elseBrOpt
  }

  /**
   * Ternary operator:
   * {{{
   *   when cond then thenBr else elseBr
   * }}}
   */
  final case class Ternary(cond: Expr, thenBr: Expr, elseBr: Expr) extends Expr with Conditional {
    override def children: List[Ast] = List(cond, thenBr, elseBr)
    override def elseBrOpt: Option[Statement] = Some(elseBr)
  }

  /**
   * While loop:
   * {{{
   *   while cond {
   *     body stats
   *   }
   * }}}
   */
  final case class WhileLoop(cond: Expr, body: Statement) extends Statement {
    override def children: List[Ast] = List(cond, body)
  }

  /**
   * For loop:
   * {{{
   *   for initStats; cond; stepStats {
   *     body stats
   *   }
   * }}}
   */
  final case class ForLoop(
                            initStats: List[LocalDef | Assignment],
                            cond: Expr,
                            stepStats: List[Assignment],
                            body: Block
                          ) extends Statement {
    override def children: List[Ast] = initStats ++ List(cond) ++ stepStats :+ body
  }

  /**
   * `return` statement, with or without return value
   */
  final case class ReturnStat(optVal: Option[Expr]) extends Statement {
    def getRetType: Option[Type] = if optVal.isDefined then optVal.get.getTypeOpt else Some(VoidType)

    override def children: List[Ast] = optVal.toList
  }

  /**
   * Cast, e.g. `x as Int`
   */
  final case class Cast(expr: Expr, tpe: Type) extends Expr {
    private var _isTransparentCast: Boolean = false
    
    def isTransparentCast: Boolean = _isTransparentCast

    def markTransparent(): Unit = {
      _isTransparentCast = true
    }
    
    override def children: List[Ast] = List(expr)
  }

  /**
   * Type test, e.g. `x is Foo`
   */
  final case class TypeTest(expr: Expr, tpe: Type) extends Expr {
    override def children: List[Ast] = List(expr)
  }

  /**
   * `panic` statement
   */
  final case class PanicStat(msg: Expr) extends Statement {
    override def children: List[Ast] = List(msg)
  }

  final case class EnclosedStat(capabilities: List[Expr], body: Block) extends Statement {
    override def children: List[Ast] = capabilities :+ body
  }

  /**
   * Node generated by lowering
   *
   * @param stats statements to be executed before [[expr]]
   * @param expr  the expression to be executed after [[stats]]. Its return value will be the return value of the whole Sequence
   */
  final case class Sequence(stats: List[Statement], expr: Expr) extends Expr {
    override def children: List[Ast] = stats :+ expr

    override def getTypeOpt: Option[Type] = expr.getTypeOpt
  }
  
  trait SmartCastsAware extends Ast {
    private var smartCasts: Map[FunOrVarId, Type] = Map.empty

    def cond: Expr
    def thenBr: Statement
    
    def setSmartCasts(smartcasts: Map[FunOrVarId, Type]): Unit = {
      this.smartCasts = smartcasts
    }

    def getSmartCasts: Map[FunOrVarId, Type] = smartCasts
  }
  
  trait Conditional extends SmartCastsAware {
    def cond: Expr
    def thenBr: Statement
    def elseBrOpt: Option[Statement]
  }

}
