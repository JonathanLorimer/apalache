package at.forsyte.apalache.tla.bmcmt.rules

import at.forsyte.apalache.tla.bmcmt._
import at.forsyte.apalache.tla.bmcmt.types.BoolT
import at.forsyte.apalache.tla.lir.oper.{TlaArithOper, TlaOper}
import at.forsyte.apalache.tla.lir.values.TlaBool
import at.forsyte.apalache.tla.lir.{OperEx, TlaEx, ValEx}

/**
  * Integer comparisons: <, <=, >, >=. For equality and inequality, check EqRule and NeqRule.
  * Implements SE-INT-CMP1.
  *
  * @author Igor Konnov
  */
class IntCmpRule(rewriter: SymbStateRewriter) extends RewritingRule {
  private val simplifier = new ConstSimplifierForSmt()

  override def isApplicable(symbState: SymbState): Boolean = {
    symbState.ex match {
      case OperEx(TlaArithOper.lt, _, _)
           | OperEx(TlaArithOper.le, _, _)
           | OperEx(TlaArithOper.gt, _, _)
           | OperEx(TlaArithOper.ge, _, _)
      => true

      case _ => false
    }
  }

  override def apply(state: SymbState): SymbState = state.ex match {
    case OperEx(oper: TlaArithOper, left, right)
      if (oper == TlaArithOper.lt || oper == TlaArithOper.le
        || oper == TlaArithOper.gt || oper == TlaArithOper.ge)
    =>
      rewriteGeneral(state, simplifier.simplify(state.ex))

    case _ =>
      throw new RewriterException("%s is not applicable".format(getClass.getSimpleName))
  }

  private def rewriteGeneral(state: SymbState, ex: TlaEx) = ex match {
    case ValEx(TlaBool(value)) =>
      // keep the simplified expression
      val finalState = rewriter.rewriteUntilDone(state.setRex(ex).setTheory(BoolTheory()))
      rewriter.coerce(finalState, state.theory)

    case OperEx(oper, left, right) =>
      val leftState = rewriter.rewriteUntilDone(state.setTheory(CellTheory()).setRex(left))
      val rightState = rewriter.rewriteUntilDone(leftState.setTheory(CellTheory()).setRex(right))
      // compare integers directly in SMT
      var arena = rightState.arena.appendCell(BoolT())
      val eqPred = arena.topCell
      val cons =
        OperEx(TlaOper.eq,
          eqPred.toNameEx,
          OperEx(oper, leftState.ex, rightState.ex))
      rewriter.solverContext.assertGroundExpr(cons)
      val finalState = rightState.setArena(arena).setTheory(CellTheory()).setRex(eqPred.toNameEx)
      rewriter.coerce(finalState, state.theory)

    case _ =>
      throw new RewriterException("It should not happen. Report a bug")
  }
}
