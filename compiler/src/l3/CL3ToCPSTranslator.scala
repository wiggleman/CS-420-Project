package l3

import l3.{ SymbolicCL3TreeModule => S }
import l3.{ HighCPSTreeModule => H }

import CL3Literal._ // ???

object CL3ToCPSTranslator extends (S.Tree => H.Tree) {
  def apply(tree: S.Tree): H.Tree =
    transform(tree)(_ => H.Halt(IntLit(L3Int(0))))

  private def transform(tree: S.Tree)(ctx: H.Atom => H.Tree): H.Tree = 
    given Position = tree.pos // ???
    
    tree match {
      case S.Let(bdgs: Seq[(S.Name, S.Tree)], body: S.Tree) =>
        bdgs.foldRight(transform(body)(ctx)) // ⟦e⟧ C, i.e., C[e]
                      ((b, t) => transform(b._2)
                                          ((a: H.Atom) => H.LetP(b._1, L3ValuePrimitive.Id, Seq(a), t)))
      case S.LetRec(functions: Seq[S.Fun], body: S.Tree) => {
        val funs = functions.map {
          case S.Fun(name: S.Name, args: Seq[S.Name], body: S.Tree) => {
            val ret = Symbol.fresh("ret")
            H.Fun(name, ret, args, transform(body)
                                            ((a: H.Atom) => H.AppC(ret, Seq(a))))
          }
        }
        H.LetF(funs, transform(body)(ctx))
      }
      /*
      case S.App(fun: S.Tree, args: Seq[S.Tree]) =>
        transform(fun)
                 ((a: H.Atom) => {
                   val ret = Symbol.fresh("ret")
                   val v   = Symbol.fresh("v")
                   args.foldRight(H.LetC(Seq(H.Cnt(ret, Seq(v), ctx(v))),
                                         H.AppF(a, ret, Seq.empty[H.Atom])))
                                 ((b, t) => transform(b)
                                                     ((c: H.Atom) => {
                                                       t match 
                                                         case H.LetC(cnts, H.AppF(fun, retC, args)) =>
                                                           H.LetC(cnts, H.AppF(fun, retC, c +: args)) // Still valid after copied?
                                                         // Case match not exhaustive!
                                                     }))})
      */
      case S.App(fun: S.Tree, args: Seq[S.Tree]) =>{
        //@annotation.tailrec
        def appTransform(es: Seq[S.Tree])(as: Seq[H.Atom]): H.Tree = {
          es match {
            case Seq() => {
              val ret = Symbol.fresh("ret")
              val v   = Symbol.fresh("v")
              H.LetC(Seq(H.Cnt(ret, Seq(v), ctx(v))),
                     H.AppF(as.head, ret, as.tail))
            }
            case e +: es => transform(e)
                                     ((a: H.Atom) => appTransform(es)(as :+ a))
          }
        }
        appTransform(fun +: args)(Seq.empty[H.Atom])
      }
      case S.If(S.Prim(prim: S.Primitive, args: Seq[S.Tree]),
                thenE: S.Tree, elseE: S.Tree) => {
        val c = Symbol.fresh("c")
        val r = Symbol.fresh("r")
        val thenC = Symbol.fresh("thenC")
        val elseC = Symbol.fresh("elseC")

        def primTransform(es: Seq[S.Tree])(as: Seq[H.Atom]): H.Tree = {
          es match {
            case Seq() => {
              H.If(prim.asInstanceOf[L3TestPrimitive], as, thenC, elseC)
            }
            case e +: es => transform(e)
                                     ((a: H.Atom) => primTransform(es)(as :+ a))
          }
        }
        H.LetC(Seq(H.Cnt(c, Seq(r), ctx(r)),
                   H.Cnt(thenC, Seq.empty[Symbol], transform(thenE)((a: H.Atom) => H.AppC(c, Seq(a)))),
                   H.Cnt(elseC, Seq.empty[Symbol], transform(elseE)((a: H.Atom) => H.AppC(c, Seq(a))))),
               /*
               args.foldRight(H.If(prim.asInstanceOf[L3TestPrimitive], Seq.empty[H.Atom], thenC, elseC))
                             ((e, t) => transform(e)
                                                 ((a: H.Atom) => {
                                                   t match
                                                     case H.If(cond, args, thenC, elseC) =>
                                                       H.If(cond, a +: args, thenC, elseC)
                                                     // Case match not exhaustive!
                                                 }))
               */
               primTransform(args)(Seq.empty[H.Atom]))
      }
      case S.If(cond: S.Tree, thenE: S.Tree, elseE: S.Tree) =>
        transform(S.If(S.Prim(L3TestPrimitive.Eq,
                              Seq(cond, S.Lit(CL3Literal.BooleanLit(false)))),
                       elseE,
                       thenE))
                 (ctx)
      case S.Prim(prim: L3TestPrimitive, args: Seq[S.Tree]) =>
        transform(S.If(S.Prim(prim, args),
                       S.Lit(CL3Literal.BooleanLit(true)),
                       S.Lit(CL3Literal.BooleanLit(false))))
                 (ctx)
      case S.Prim(prim: S.Primitive, args: Seq[S.Tree]) => {
        val n = Symbol.fresh("n")

        def primTransform(es: Seq[S.Tree])(as: Seq[H.Atom]): H.Tree = {
          es match {
            case Seq() =>
              H.LetP(n, prim.asInstanceOf[L3ValuePrimitive], as, ctx(n))
            case e +: es => transform(e)
                                     ((a: H.Atom) => primTransform(es)(as :+ a))
          }
        }
        primTransform(args)(Seq.empty[H.Atom])
      }
      case S.Halt(arg: S.Tree) =>
        transform(arg)(ctx)
      case S.Ident(value) => 
        ctx(value)
      case S.Lit(value) => 
        ctx(value)
    }
}

