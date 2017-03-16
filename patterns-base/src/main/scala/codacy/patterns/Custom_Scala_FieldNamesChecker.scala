package codacy.patterns

import codacy.base.{Pattern, Result}

import scala.meta._
import scala.util.matching.Regex

class Custom_Scala_FieldNamesChecker(configuration: Custom_Scala_FieldNamesChecker.Configuration) extends Pattern{

  def this() = this(Custom_Scala_FieldNamesChecker.Configuration())

  private[this] def applicableRegex(insideObject: Boolean): Regex = if (insideObject && !configuration.includeObjects) {
    configuration.objectRegex
  } else {
    configuration.regex
  }

  override def apply(tree: Tree): Set[Result] = {
    checkVar(tree).map{ case t => Result(message(t),t) }.toSet
  }

  private[this] def checkVar(tree: Tree): Seq[Tree] = {
    tree.collect {
      //var definitions
      case t@q"..$_ var ..$patsnel: $tpe = $expr" if configuration.includeEnums || ! isEnumValDefRegexOrDecl(t.parent,expr,tpe) =>
        conflictingNames(patsnel)
      //var declarations
      case t@q"..$_ var ..$pnamesnel: $tpe" if configuration.includeEnums || ! isEnumValDefRegexOrDecl(t.parent,tpe=Option(tpe)) =>
        conflictingNames(pnamesnel)
      //objects
      case t@q"..$mods object $name" => checkConstant(t)
      //val definitions
      case t@q"..$_ val ..$patsnel: $tpe = $expr" if configuration.includeEnums || ! isEnumValDefRegexOrDecl(t.parent,Option(expr),tpe) =>
        conflictingNames(patsnel)
      //val declarations
      case t@q"..$_ val ..$pnamesnel: $tpe" if configuration.includeEnums || ! isEnumValDefRegexOrDecl(t.parent,tpe=Option(tpe)) =>
        conflictingNames(pnamesnel)
      //parameter values
      case t@param"..$mods $paramname: $atpeopt = $expropt" if isConflictingName(insideObject = false)(paramname) =>
        List(paramname)
    }.flatten
  }

  private[this] def checkConstant(tree: Tree): Seq[Tree] = {
    tree.collect{
      //var definitions
      case t@q"..$_ var ..$patsnel: $tpe = $expr" if configuration.includeEnums || ! isEnumValDefRegexOrDecl(t.parent,expr,tpe) =>
        conflictingNames(patsnel)
      //var declarations
      case t@q"..$_ var ..$pnamesnel: $tpe" if configuration.includeEnums || ! isEnumValDefRegexOrDecl(t.parent,tpe=Option(tpe)) =>
        conflictingNames(pnamesnel)
      //classes
      case t@q"..$mods class $name" => checkVar(t)
      //traits
      case t@q"..$mods trait $name" => checkVar(t)
      //val definitions
      case t@q"..$_ val ..$patsnel: $tpe = $expr" if configuration.includeEnums || ! isEnumValDefRegexOrDecl(t.parent,Option(expr),tpe) =>
        conflictingNames(patsnel, insideObject = true)
      //val declarations
      case t@q"..$_ val ..$pnamesnel: $tpe" if configuration.includeEnums || ! isEnumValDefRegexOrDecl(t.parent,tpe=Option(tpe)) =>
        conflictingNames(pnamesnel)
      //parameter values
      case t@param"..$mods $paramname: $atpeopt = $expropt" if isConflictingName(insideObject = false)(paramname) =>
        List(paramname)
    }.flatten
  }

  private[this] def isEnumValDefRegexOrDecl(parent:Option[Tree],expr:Option[Tree]=None,tpe:Option[Tree]=None) = {
    val extendsEnumeration:Boolean = parent.collect{ case template"{ ..$stats } with ..${ctorcalls:Seq[Ctor.Call]} { $param => ..$stats2 }" =>
      ctorcalls.exists{
        case ctor"Enumeration" => true
        case _ => false
      }
    }.getOrElse(false)

    lazy val exprIsValue:Boolean = expr.exists{
      case q"${Term.Name(value)}" => value == "Value"
      case q"${Term.Apply(Term.Name(name), _)}" => name == "Value"
      case _ => false
    }

    lazy val exprIsRegex:Boolean = expr.exists{
      case q"${lit: Lit}.r" => true
      case _ => false
    }

    lazy val tpeIsValue:Boolean = tpe.exists{
      case t"${Type.Name(value)}" => value == "Value"
      case _ => false
    }

    exprIsRegex || (extendsEnumeration && (exprIsValue || tpeIsValue))
  }


  private[this] def conflictingNames(trees: Seq[Tree], insideObject: Boolean = false) = {
    trees.collect{
      //tuples
      case p"(..$exprsnel)" =>
        exprsnel.filter( isConflictingName(insideObject) )
      case single if isConflictingName(insideObject)(single) =>
        List(single)
    }.flatten
  }

  private[this] def isEscaped(name: Term.Name):Boolean = {
    name.tokens.collectFirst{ case id:Token.Ident => id.syntax }.
      exists{ case value => value.startsWith("`") && value.endsWith("`")}
  }

  private[this] def isConflictingName(insideObject: Boolean)(tree: Tree): Boolean = {
    Option(tree).collect{
      case q"${term: Pat.Var.Term}" if ! isEscaped(term.name) =>
        term.name.value
      case q"${name: Term.Name}" if ! isEscaped(name) =>
        name.value
    }.exists( name => ! applicableRegex(insideObject).pattern.matcher(name).matches() )
  }

  private[this] def message(tree:Tree) = Message(s"Field name '$tree' does not match the regular expression")
}

object Custom_Scala_FieldNamesChecker{
  case class Configuration(regex:Regex="""^[a-z][A-Za-z0-9]*$""".r,
                           objectRegex:Regex="""^[A-Za-z][A-Za-z0-9]*$""".r,
                           includeEnums:Boolean = false,
                           includeObjects: Boolean = false)
}
