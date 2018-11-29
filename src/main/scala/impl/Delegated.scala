package impl

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class Delegated extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegatedImpl.impl
}

object DelegatedImpl {

  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def modifiedClass(classDecl: ClassDef, compDeclOpt: Option[ModuleDef]): c.Expr[Any] = {
      val q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$methods }" = classDecl
      val funcName = TermName(c.freshName("func"))
      val instanceName = TermName(c.freshName("instance"))
      val newMethods = methods.collect {
        case x: DefDef => delegateMethod(x, funcName, instanceName)
      }
      val xx = tpname match {
        case x: TypeName => x.toTermName
      }
      val paramNames = classDecl.tparams.map(_.name)

      val obj =
        q"""object $xx {
                implicit def delegateProducer[..$tparams]: impl.DelegateProducer[$tpname[..$paramNames]] = {
                  new impl.DelegateProducer[$tpname[..$paramNames]] {
                      override def delegate($instanceName: $tpname[..$paramNames]): $tpname[..$paramNames] = {
                        new $tpname[..$paramNames] {
                            ..$newMethods
                        }
                      }
                  }
                }
            }"""
      val e =
        q"""
        $classDecl
        $obj
        """
      println(e)
      c.Expr(e)
    }

    def delegateMethod(d: DefDef, funcName: TermName, instanceName: TermName): DefDef = {
      val q"$mods def $tname[..$tparams](...$paramss): $tpt = $body" = d
      val appliedParams = d.vparamss.map { x =>
        x.map(_.name)
      }
      val printExpr = q"println(10)"
      val expr = q"$printExpr; $instanceName.$tname[..$tparams](...$appliedParams)"
      val newMods = withOverride(d.mods)
      q"$newMods def $tname[..$tparams](...$paramss): $tpt = {$expr}".asInstanceOf[DefDef]
    }

    def withOverride(mods: Modifiers): Modifiers = {
      val newFlags = mods.flags | Flag.OVERRIDE
      val newMods = Modifiers.apply(newFlags, mods.privateWithin, mods.annotations)
      newMods
    }

    annottees.map(_.tree) match {
      case (classDecl: ClassDef) :: Nil => modifiedClass(classDecl, None)
      case (classDecl: ClassDef) :: (compDecl: ModuleDef) :: Nil => modifiedClass(classDecl, Some(compDecl))
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }

  }


}

