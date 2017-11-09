// \phi : ϕ
// \delta : δ
// \in : ∈
// \squsubset : ⊏
// \iff : ⇔

package at.forsyte.apalache.tla.assignments

import java.io._

import at.forsyte.apalache.tla.lir.actions.TlaActionOper
import at.forsyte.apalache.tla.lir.oper.{TlaBoolOper, TlaSetOper}
import at.forsyte.apalache.tla.lir.plugins.UniqueDB
import at.forsyte.apalache.tla.lir.{UID, _}
import com.microsoft.z3._

import scala.collection.immutable.{Map, Set}

/**
  * Object equipped with methods for solving the assignment problem.
  *
  * =Instructions For Use=
  *   1. Extract a set of variables and the next formula from your TLA specification.
  *   1. Do one of the following:
  *     a. To produce a good assignment strategy, call [[[[assignmentSolver#getOrder(p_vars:scala\.collection\.immutable\.Set[at\.forsyte\.apalache\.tla\.lir\.NameEx],p_phi:at\.forsyte\.apalache\.tla\.lir\.OperEx,p_fileName:String):Option[Seq[(at\.forsyte\.apalache\.tla\.lir\.UID,Boolean)]]* getOrder]]]].
  *     a. To produce an SMT file or specification for later use, call [[assignmentSolver#makeSpec makeSpec]].
  */
object assignmentSolver{

  /** Symbol to be used for variable names in SMT. */
  protected var m_varSym = "A"
  /** Symbol to be used for the function name in SMT. */
  protected var m_fnSym = "R"

  /**
    * Should be immutable, since the names are arbitrary anyway.
    * Otherwise, there could be problems with names changing mid-execution.
    */
  //  def varSym : String = m_varSym
  //  def varSym_=( p_sym : String ) : Unit = {
  //    m_varSym = p_sym
  //  }
  //
  //  def fnSym : String = m_fnSym
  //  def fnSym_=( p_sym : String ) : Unit = {
  //    m_fnSym = p_sym
  //  }

  /**
    * Intermediate class for internal use. Represents ϕ formulas as trees.
    */
  protected abstract class BoolFormula
  protected case class False() extends BoolFormula
  protected case class And( args : BoolFormula* ) extends BoolFormula
  protected case class Or( args : BoolFormula* ) extends BoolFormula
  protected case class Neg( arg : BoolFormula ) extends BoolFormula
  protected case class Implies( LHS : BoolFormula, RHS : BoolFormula ) extends BoolFormula
  protected case class Variable( id: Int ) extends BoolFormula
  protected case class LtFns( i: Int, j: Int )  extends BoolFormula // ( R( i ) < R( j ) )
  protected case class NeFns( i: Int, j: Int )  extends BoolFormula // ( R( i ) != R( j ) )


  /**
    * Transforms BoolFormula expressions into SMTLIBv2 format recursively.
    * @param phi The formula being transformed.
    * @return The SMTLIBv2 representation of `phi`. Does not include a top-level assertion command.
    */
  protected def toSmt2( phi : BoolFormula ) : String = {
    phi match{
      case False() =>
        /* return */ "false" //"( false )"
      case And( args@_* ) =>
        /* return */ "( and %s )".format( args.map( toSmt2 ).mkString(" ") )
      case Or( args@_* ) =>
        /* return */  "( or %s )".format( args.map( toSmt2 ).mkString(" ") )
      case Neg( arg : BoolFormula ) =>
        /* return */ "( not %s )".format( toSmt2( arg ) )
      case Implies( lhs, rhs ) =>
        /* return */ "( => %s %s )".format( toSmt2( lhs ), toSmt2( rhs ) )
      case Variable( id: Int ) =>
        /* return */ m_varSym + "_" + id //"( %s_%s )".format( varSym, id )
      case LtFns( i: Int, j: Int ) =>
        /* return */ "( < ( %s %s ) ( %s %s ) )".format( m_fnSym, i, m_fnSym, j )
      case NeFns( i: Int, j: Int ) =>
        /* return */ "( not ( = ( %s %s ) ( %s %s ) ) )".format( m_fnSym, i, m_fnSym, j )
    }
  }

  /**
    * Removes spurious branches from the BoolFormula tree to obtain a
    * logically equivalent but smaller formula.
    *
    * Assumes input formulas are generated by the [[delta]] function or equivalent.
    * @param phi The formula being transformed.
    * @return A new BoolFormula, logically equivalent to `phi`, with simpler structure.
    */
  protected def simplify( phi: BoolFormula ) : BoolFormula = {
    phi match {
      /**
        * Recursively simplify branches first.
        * If any branch is false, the whole formula is false.
        * It is important to recurse first,
        * since otherwise false-simplification would not propagate upward.
        */
      case And( args@_*) => {
        val newargs = args.map(simplify)
        if ( newargs.contains( False() ) )
          /* return */ False()
        else
          /* return */ And( newargs:_* )
      }
      /**
        * Recursively simplify, then drop all False() branches.
        * Afterwards, if the new tree has too few branches prune accordingly.
        */
      case Or( args@_*) => {
        val newargs = args.map(simplify).filterNot( _ == False() )
        newargs.size match {
          case 0 =>
            /* return */ False()
          case 1 =>
            /* return */ newargs.head
          case _ =>
            /* return */ Or( newargs:_* )
        }
      }
      case _ =>
        /* return */phi
    }
  }

  /**
    * Constructs the δ,,v,,(ϕ) formulas.
    * @param v The variable deciding leaf terms.
    * @param phi The formula being transformed.
    * @return The transformed formula.
    * @deprecated
    */
  protected def delta( v: NameEx )( phi: OperEx ) : BoolFormula = {
    // assume well-formedndess, i.e. only and/or/in
    phi.oper match {
      case TlaBoolOper.and => Or( phi.args.map( arg => delta(v)( arg.asInstanceOf[OperEx] ) ):_* )
      case TlaBoolOper.or => And( phi.args.map( arg => delta(v)( arg.asInstanceOf[OperEx] ) ):_* )
      case TlaSetOper.in => {
        phi.args.head match {
          case OperEx( TlaActionOper.prime, NameEx( name ) ) => {
            if( v == NameEx( name ) )
              Variable( phi.ID.id )
            else
              False()
          }
          case _ => False()
        }
      }
      case _ => False()
    }
  }

  /**
    * Finds the LHS primed variable of a well-formed formula (given by the ID).
    *
    * Well-formed means that the left hand side is a single primed variable.
    * Undefined behaviour if the formula is not well-formed or if the ID is invalid.
    * @param i The UID of a formula, assumed to be valid.
    * @return A TLA expression with the name of the variable, unprimed.
    * @see [[rvars]]
    */
  protected def lvar( i : Int ) : NameEx = UniqueDB.apply( UID( i ) ).
    head.asInstanceOf[OperEx].args.
    head.asInstanceOf[OperEx].args.
    head.asInstanceOf[NameEx]

  /**
    * Extracts all primed subexpressions within a given expression, regardless of nesting depth.
    * @param ex An arbitrary TLA expression.
    * @return A set of unprimed names. Each name appears uniquely, regardless of
    *         multiple occurrences with different UIDs.
    */
  protected def findPrimes( ex : TlaEx ) : Set[NameEx] = {
    ex match {
      case OperEx( TlaActionOper.prime, NameEx(n) ) =>
        /* return */ Set(NameEx(n))
      case OperEx( _, args@_* ) =>
        /* return */ args.map( findPrimes ).fold( Set[NameEx]() ){ (a,b) => a ++ b }
      case _ =>
        /* return */ Set[NameEx]()
    }
  }

  /**
    * Finds the RHS primed variables of an arbitrary formula (given by the ID).
    *
    * The RHS does not need to be well-formed. Undefined behaviour if the ID is invalid.
    * @param i The UID of a formula, assumed to be valid.
    * @return A set of expressions with the names of the variables, unprimed.
    * @see [[lvar]]
    */
  protected def rvars( i : Int ) : Set[NameEx] =
    findPrimes(
      UniqueDB(UID(i)).head.asInstanceOf[OperEx].args.tail.head // 2nd arg is the RHS
    )


  /**
    * The ⊏ binary relation.
    *
    * i ⊏ j ⇔ [[lvar]]( i ) ∈ [[rvars]]( j ).
    * @param i The UID of the first term.
    * @param j The UID of the second term.
    * @return `true` iff the relation is satisfied.
    */
  protected def sqsubset( i: Int, j: Int ): Boolean =
    rvars( j ).contains( lvar( i ) )

  /**
    * Main method.
    *
    * Extracts all relevant information in a single pass.
    * We assume the input is preprocessed, all terms that are not of the form a' ∈ B
    * are ignored for assignment (that includes all a' = ... terms).
    * @param p_phi The next-step formula.
    * @param p_vars The set of all variable names relevant to the spec.
    * @return A triple `( seen, D, deltas )` where `seen` is the set of all
    *         leaf IDs, `D` is the set of dependent indices and `deltas` is a map storing
    *         δ,,v,,(ϕ) for every v.
    */
  protected def massProcess( p_phi : TlaEx, p_vars: Set[NameEx] )
    : ( Set[Int], Set[(Int,Int)], Map[NameEx,BoolFormula] ) = {
    val ( seen, deps, _, deltas ) = innerMassProcess(p_phi,p_vars)
    /* return */ (seen, deps, deltas.map( pa => (pa._1, simplify(pa._2) ) ) )
  }

  /**
    * Recursive submethod called within [[massProcess]].
    * @return An additional extra set of independent indices, as bookkeeping,
    *         which is discarded in the return of [[massProcess]].
    */
  protected def innerMassProcess( p_phi : TlaEx, p_vars: Set[NameEx] )
                : ( Set[Int], Set[(Int,Int)], Set[(Int,Int)], Map[NameEx,BoolFormula] ) = {

    /** We name the default arguments to return at irrelevant terms  */
    val defaultMap = ( for {v <- p_vars} yield (v, False()) ).toMap
    val defaultArgs = ( Set[Int](), Set[(Int,Int)](), Set[(Int,Int)](), defaultMap )

    p_phi match {
      /**
        * The δ_v function is defined only on boolean conjunctions/disjunctions
        * and leafs ( ∈-expressions ). At a leaf with id i, if [[lvar]](i) is v then
        * the δ_v formula evaluates to a fresh boolean variable A_i. Otherwise, we compute the
        * δ for all branches and flip the boolean connective.
        *
        * The (in)dependece sets are directly computable at leafs, for a leaf with id i
        * the dep. set D is {(i,i)} and the indep. set I is {}.
        * Otherwise, at a boolean connective, we first compute all (D_j,I_j) pairs for all children
        * and the set S' of all seen ids so far. To obtain (D_i,I_i) at the call level, recall that
        * D_i \cup I_i = S' \times S' = S, as each pair is either dependent of independent.
        * Let D_i' be the union of all D_j, similarly for I_i'. We need to determine whether
        * all elements of U = S \ ( D_i' \cup I_i') should belong to D_i or I_i.
        * If the connective is AND, then the first common ancestor of all pairs in U is this AND
        * node and therefore all pairs in U are dependent, since any branch will include all
        * subbranches of an AND node. Then, D_i = S \ I_i' and I_i = I_i'.
        * Conversely, if the node is an OR, D_i = D_i' and I_i = S \ D_i'.
        *
        * The set of seen elements is simply the union of all such child sets.
        *
        */
      case OperEx(oper, args@_*) =>
        oper match{
          /** Recursive case */
          case TlaBoolOper.and | TlaBoolOper.or =>
            /** First, process all children */
            val processedArgs : Seq[ ( Set[Int],
                                       Set[(Int,Int)],
                                       Set[(Int,Int)],
                                       Map[NameEx,BoolFormula] ) ]
                = args.map( innerMassProcess( _, p_vars) )

            /** Next, compute a map from each v to a sequence of all child delta_v formulas  */
            val newMapArgs : Map[ NameEx, Seq[BoolFormula] ] =
              ( for { v <- p_vars } yield
                ( v,
                  processedArgs.map(
                    /** Take the current delta_v. We know none of them are None by construction */
                    _._4.get(v).head
                  )
                )
              ).toMap

            /* Old: used to include filterNot( _.isEmpty )

            processedArgs.map(
              _._4.get(v)
            ).filterNot(
              _.isEmpty
            ).map(
              _.head
            )
            */

            /**
              * The rest we obtain by folding and taking the unions of all components.
              * The default arguments are empty sets as not to impact the result.
              */
            val ( seen, depSet, indepSet, _ ) = processedArgs.fold(
              defaultArgs
            ){
              ( a,b ) => ( a._1 ++ b._1,
                           a._2 ++ b._2,
                           a._3 ++ b._3,
                           defaultArgs._4 // irrelevant
              )
            }

            /** Deltas flip boolean connectives */
            val newMap : Map[NameEx,BoolFormula] =
              (
                for { v <- p_vars }
                  yield ( v,
                          if (oper == TlaBoolOper.and)
                            Or(newMapArgs(v):_*)
                          else
                            And(newMapArgs(v):_*)
                        )
              ).toMap

            /** S is the set of all index pairs which we will be certain about after this step */
            val S : Set[(Int,Int)] = for { x <- seen; y <- seen } yield (x,y)

            /** One set is unchanged, the other is the remeining elements from S */
            oper match {
              case TlaBoolOper.and => ( seen, S -- indepSet, indepSet, newMap )
              case TlaBoolOper.or  => ( seen, depSet, S -- depSet, newMap )
            }
          /** Base case */
          case TlaSetOper.in =>
            /** First, we check for well-formed expr., i.e. a' \in B */
            args.head match {
              case OperEx( TlaActionOper.prime, nameEx ) =>
                val n : Int = p_phi.ID.id
                /** Use the definition of delta_v for base cases */
                val newmap =
                  ( for { v <- p_vars }
                      yield ( v,
                              if( nameEx == v )
                                Variable( n )
                              else
                                False()
                            )
                  ).toMap
                /** If well formed, S = {n}, D = {(n,n)}, I = {}, deltas = newmap */
                /* return */ ( Set[Int](n), Set[(Int,Int)]( (n,n) ), Set[(Int,Int)](), newmap )
              case _ =>
                /** If not well-formed, ignore and return empty sets/trivial maps */
                /* return */ defaultArgs
            }
          /** Other case */
          case _ =>
            /** If the term is of any other form it is just ignored */
            /* return */ defaultArgs
        }
      /** Not an operator. We know the top-level call is on an OperEx. Other terms are ignored */
      case _ =>
        /* return */ defaultArgs
    }
  }

  /**
    * Given a Next formula and a set of variables, produces an SMT formula that
    * is satisfiable iff the assignment problem for the given formula has a solution.
    * @param p_vars The set of variables declared by the specification.
    * @param p_phi The Next formula.
    * @param p_fileName Optional parameter, if `p_fileName` is nonempty, a file with the
    *                   complete specification is produced, including set-logic,
    *                   check-sat and get-model commands. Set to empty by default.
    * @param p_completeSpec Optional parameter, if `true`, produces a complete .smt2
    *                       specification, with set-logic, check-sat and get-model commands.
    *                       If false, will only produce the assertions and declarations.
    *                       Set to false by default.
    * @return An SMTLIBv2 string to be used alone or passed to the z3 API. Contains at least all
    *         relevant declarations and assertions.
    */
  def makeSpec( p_vars: Set[NameEx],
                p_phi : OperEx,
                p_fileName : String = "",
                p_completeSpec : Boolean = false
              ) : String = {

    /** Extract the list of leaf ids, the dependency set and the delta mapping */
    val ( seen, deps, deltas ) = massProcess(p_phi, p_vars)

    /**
      * We need two subsets of deps, one where the \sqsubset relation holds
      * and one where lvars match for constructing \phi_R and \phi^\exists!^
      * respectively.
      */
    val D_sqss = deps.filter( pa =>  sqsubset( pa._1, pa._2 ) )
    val D_exOne = deps.filter( pa => pa._1 < pa._2 && lvar( pa._1 ) == lvar( pa._2) )

    /** \phi_A */
    val aargs = deltas.values
    val aargsSMT = aargs.map( toSmt2 )

    /** \phi_R */
    val rargs =
      for { (i,j) <- D_sqss }
      yield
        Implies(
          And( Variable( i ),
               Variable( j )
          ),
          LtFns( i, j )
        )
    val rargsSMT = rargs.map( toSmt2 )

    /** \phi_R^{inj}^ */
    val injargs = for { i <- seen; j <- seen if i < j} yield NeFns(i, j)
    val injargsSMT = injargs.map( toSmt2 )

    /** \phi^{\exists!}^ */
    val exOneargs =
      for { (i,j) <- D_exOne }
        yield Neg( And( Variable( i ), Variable( j ) ) )
    val exOneargsSMT = exOneargs.map( toSmt2 )

    /** The constant/funciton declaration commands */
    val typedecls = seen.map( "( declare-fun %s_%s () Bool )".format( m_varSym, _ ) ).mkString("\n")
    val fndecls = "\n( declare-fun %s ( Int ) Int )\n".format( m_fnSym )

    /** Assert all of the constraints, as defined in \phi_S^{good}^ */
    val constraints = ( aargsSMT ++ rargsSMT ++ injargsSMT ++ exOneargsSMT ).toSeq.map(
      str => "( assert %s )".format( str )
    ).mkString("\n")

    /** Partial return, sufficient for the z3 API */
    val ret = typedecls + fndecls + constraints

    val logic = "( set-logic QF_UFLIA )\n"
    val end = "\n( check-sat )\n( get-model )\n( exit )"

    /** Possibly produce standalone file */
    if( p_fileName.nonEmpty ) {

      val pw = new PrintWriter(new File( p_fileName ) )
      pw.write(logic + ret + end)
      pw.close()

    }

    /** Return complete or partial spec */
    if( p_completeSpec ){
      /* return */ logic + ret + end
    }
    else{
      /* return */ ret
    }

  }

  /**
    * Allows calls to a com.microsoft.z3.FuncInterp object as if it were a String -> Int function.
    * @param f The funciton interpretation produced by the z3 solver.
    */
  protected class FunWrapper( f: FuncInterp ) {
    /** Return value for arguments outside the relevant subdomain. */
    protected val m_default : Int = f.getElse.asInstanceOf[IntNum].getInt

    /**
      * Internal map, corresponds to the restriction of the function represented by `f`
      * to the relevant subdomain.
      */
    protected val m_map : Map[String, Int] =
      ( for { e: FuncInterp.Entry <- f.getEntries }
          yield (
                  "%s_%s".format( m_varSym, e.getArgs.head ),
                  e.getValue.asInstanceOf[IntNum].getInt
                )
      ).toMap

    /** The wrapper can be called like a function. */
    def apply( arg: String ): Int = m_map.getOrElse( arg, m_default )

    override def toString: String = m_map.toString
  }

  /**
    * Point of access method, presents the solution to the assignment problem for
    * the specification `p_spec`.
    * @param p_spec A SMTLIBv2 specification string, as required by the parser method of
    *               com.microsoft.z3.Context.
    * @return `None`, if the assignment problem has no solution. Otherwise, returns a sequence
    *        of pairs where the UID component refers to an assignment candidate and the boolean
    *        component specifies whether the candidate is chosen as an assignment in the
    *        good assignment strategy found by the internal methods. The pairs are sorted by the
    *        ranking function, in ascending order.
    * @see [[[[getOrder(p_vars:scala\.collection\.immutable\.Set[at\.forsyte\.apalache\.tla\.lir\.NameEx],p_phi:at\.forsyte\.apalache\.tla\.lir\.OperEx,p_fileName:String):Option[Seq[(at\.forsyte\.apalache\.tla\.lir\.UID,Boolean)]]* getOrder]]]]
    */
  def getOrder( p_spec : String ) : Option[Seq[(UID,Boolean)]] = {
    /** Initialize a context and solver */
    val ctx = new Context()
    val s = ctx.mkSolver()
    /** Parse the spec and add it to the solver */
    s.add( ctx.parseSMTLIB2String( p_spec, null, null, null, null ) )

    /** Check sat, if not SAT terminate with None */
    val status = s.check.toString
    if( status != "SATISFIABLE" )
      return None

    /** If SAT, get a model. */
    val m = s.getModel

    /** Extract the rank function. Should be the only (non-const.) function */
    val fnDecl = m.getFuncDecls
    if( fnDecl.size != 1 )
      return None

    /** Wrap the function so it can be used to sort the sequence later. */
    val wrap = new FunWrapper( m.getFuncInterp( fnDecl(0) ) )

    /** Extract all constants and their values */
    val varInterps = m.getConstDecls.map( x => ( m.getConstInterp( x ), x.getName.toString ) )

    /** Sort by rank */
    val sorted = varInterps.sortBy(  x => wrap( x._2 ) )

    /** Convert z3 classes into UIDs and Bools */
    // Note: if anyone can figure out how to do this in a less hack-ish manner, please let me know.
    val ret = sorted.map(
        x => (
          UID( x._2.substring(2).toInt ), // Truncate A_... prefix
          x._1.getBoolValue.toInt == 1 // no .toBool exists, cast to int then compare.
        )
      )

    /* return */ Some( ret )
  }

  /**
    * Point of access method, presents the solution to the assignment problem for
    * the specification with variables `p_vars` and a Next-formula `p_phi`.
    * @param p_vars The set of variables declared by the specification.
    * @param p_phi The Next formula.
    * @param p_fileName Optional parameter, if `p_fileName` is nonempty, a file with the complete
    *                   specification is also produced. Set to empty by default.
    * @return `None`, if the assignment problem has no solution. Otherwise, returns a sequence
    *        of pairs where the UID component refers to an assignment candidate and the boolean
    *        component specifies whether the candidate is chosen as an assignment in the
    *        good assignment strategy found by the internal methods. The pairs are sorted by the
    *        ranking function, in ascending order.
    * @see [[makeSpec]], [[[[getOrder(p_spec:String):Option[Seq[(at\.forsyte\.apalache\.tla\.lir\.UID,Boolean)]]* getOrder]]]]
    */
  def getOrder( p_vars: Set[NameEx],
                p_phi : OperEx,
                p_fileName : String = ""
              ) : Option[Seq[(UID,Boolean)]] = {
    val spec = makeSpec(p_vars,p_phi,p_fileName)
    /* return */ getOrder( spec )
  }

}
