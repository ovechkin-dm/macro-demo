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
      val implicitDefName = TermName(c.freshName("producer"))
      val newMethods = methods.collect {
        case x: DefDef if x.mods.hasFlag(Flag.DEFERRED) =>
          wrapMethod(x, funcName, instanceName)
        case x: DefDef =>
          delegateMethod(x, funcName, instanceName)
      }
      val xx = tpname match {
        case x: TypeName => x.toTermName
      }
      val paramNames = classDecl.tparams.map(_.name)
      val producer =
        q"""
            implicit def $implicitDefName[..$tparams]: impl.DelegateProducer[$tpname[..$paramNames]] = {
              new impl.DelegateProducer[$tpname[..$paramNames]] {
                  override def delegate($instanceName: $tpname[..$paramNames])
                      ($funcName: (impl.DelegateProducer.MethodInfo, () => Any) => Any): $tpname[..$paramNames] = {
                    new $tpname[..$paramNames] {
                        ..$newMethods
                    }
                  }
              }
            }
         """
      val obj = buildCompanionObject(compDeclOpt, xx, producer.asInstanceOf[DefDef])
      val e =
        q"""
           $classDecl
           $obj
        """
      c.Expr(e)
    }

    def buildCompanionObject(src: Option[ModuleDef], traitName: TermName, producer: DefDef): ModuleDef = {
      src match {
        case Some(obj) =>
          val q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" = obj
          val extendedBody =
            q"""
               ..$body
               $producer
             """
          val result = q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$extendedBody }"
          result.asInstanceOf[ModuleDef]
        case None =>
          q"""
              object $traitName {
                $producer
              }
           """.asInstanceOf[ModuleDef]
      }
    }

    def wrapMethod(d: DefDef, funcName: TermName, instanceName: TermName): Tree = {
      val argListName = TermName(c.freshName("argList"))
      val methodInfoName = TermName(c.freshName("methodInfo"))
      val lazyFuncName = TermName(c.freshName("lazyFunc"))
      val resultName = TermName(c.freshName("result"))
      val q"$mods def $tname[..$tparams](...$paramss): $tpt = $body" = d
      val appliedParams = d.vparamss.map { x =>
        x.map(_.name)
      }
      val argListExp = q"val $argListName: List[Any] = List(..${appliedParams.flatten})"
      val methodName = d.name.decodedName.toString
      val methodInfo = q"""val $methodInfoName = new impl.DelegateProducer.MethodInfo($methodName, 0, $argListName)"""
      val delegatedMethodFunc = q"val $lazyFuncName = () => $instanceName.$tname[..$tparams](...$appliedParams)"
      val funcApply = q"val $resultName = $funcName.apply($methodInfoName, $lazyFuncName)"
      val newMods = withOverride(d.mods)
      q"""
         $newMods def $tname[..$tparams](...$paramss): $tpt = {
            $argListExp
            $methodInfo
            $delegatedMethodFunc
            $funcApply
            $resultName.asInstanceOf[$tpt]
         }
       """
    }

    def delegateMethod(d: DefDef, funcName: TermName, instanceName: TermName): Tree = {
      val q"$mods def $tname[..$tparams](...$paramss): $tpt = $body" = d
      val appliedParams = d.vparamss.map { x =>
        x.map(_.name)
      }
      val expr = q"$instanceName.$tname[..$tparams](...$appliedParams)"
      val newMods = withOverride(d.mods)
      q"$newMods def $tname[..$tparams](...$paramss): $tpt = {$expr}"
    }


    def withOverride(mods: Modifiers): Modifiers = {
      val newFlags = Flag.OVERRIDE
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

