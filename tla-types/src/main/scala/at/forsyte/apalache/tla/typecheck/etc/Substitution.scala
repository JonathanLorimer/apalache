package at.forsyte.apalache.tla.typecheck.etc

import at.forsyte.apalache.tla.lir.{
  BoolT1, ConstT1, FunT1, IntT1, OperT1, RealT1, RecT1, SeqT1, SetT1, SparseTupT1, StrT1, TlaType1, TupT1, VarT1
}

/**
 * A substitution from type variables to types.
 *
 * @param mapping a mapping from variable names to types.
 */
class Substitution(val mapping: Map[EqClass, TlaType1]) {
  // map every variable to its equivalence class (assuming that the classes are disjoint)
  private lazy val varToClass = mapping.keys.foldLeft(Map[Int, EqClass]()) { (map, cls) =>
    map ++ cls.typeVars.map(_ -> cls)
  }

  def apply(tp: TlaType1): TlaType1 = {
    sub(tp)
  }

  /**
   * Substitute variables with the types that are assigned in the context.
   *
   * @param tp a type term
   * @return the type term in which the variables have been substituted
   */
  def sub(tp: TlaType1): TlaType1 = {
    tp match {
      case VarT1(no) =>
        if (varToClass.contains(no)) {
          mapping(varToClass(no))
        } else {
          tp
        }

      case IntT1() | BoolT1() | RealT1() | StrT1() | ConstT1(_) =>
        tp

      case SetT1(elem) =>
        SetT1(sub(elem))

      case SeqT1(elem) =>
        SeqT1(sub(elem))

      case TupT1(elems @ _*) =>
        TupT1(elems.map(sub): _*)

      case SparseTupT1(fieldTypes) =>
        SparseTupT1(fieldTypes.map(kv => (kv._1, sub(kv._2))))

      case RecT1(fieldTypes) =>
        RecT1(fieldTypes.map(kv => (kv._1, sub(kv._2))))

      case FunT1(arg, res) =>
        FunT1(sub(arg), sub(res))

      case OperT1(args, res) =>
        OperT1(args.map(sub), sub(res))
    }
  }

  override def toString: String = {
    def cls(c: EqClass): String = c.typeVars.map(VarT1(_).toString).mkString(", ")

    "Sub{%s}".format(String.join(", ", mapping.toSeq.map(p => "[%s] -> %s".format(cls(p._1), p._2)): _*))
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Substitution]

  // Comparison of two substitutions is expensive, but we mainly use it for testing.
  // We use structural equality of equivalence classes instead of shallow comparison by ids.
  override def equals(other: Any): Boolean = other match {
    case that: Substitution =>
      (mapping.size == that.mapping.size) && mapping.forall { case (lcls, ltype) =>
        that.mapping.exists { case (rcls, rtype) =>
          lcls.deepEquals(rcls) && ltype == rtype
        }
      }

    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(mapping)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Substitution {
  val empty = new Substitution(Map.empty)

  def apply(elems: (EqClass, TlaType1)*): Substitution = {
    new Substitution(Map(elems: _*))
  }

  def apply(context: Map[EqClass, TlaType1]): Substitution = {
    new Substitution(context)
  }
}
