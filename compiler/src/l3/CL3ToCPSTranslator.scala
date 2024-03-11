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
      case S.Ident(value) => 
        ctx(value)
      case S.Lit(value) => 
        ctx(value)
    }
}

