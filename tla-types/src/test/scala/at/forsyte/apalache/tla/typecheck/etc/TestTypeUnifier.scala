package at.forsyte.apalache.tla.typecheck.etc

import at.forsyte.apalache.tla.lir.{BoolT1, ConstT1, FunT1, IntT1, OperT1, RealT1, RecT1, SeqT1, SetT1, StrT1, VarT1}
import at.forsyte.apalache.io.typecheck.parser.{DefaultType1Parser, Type1Parser}
import org.junit.runner.RunWith
import org.scalatest.easymock.EasyMockSugar
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterEach, FunSuite}

@RunWith(classOf[JUnitRunner])
class TestTypeUnifier extends FunSuite with EasyMockSugar with BeforeAndAfterEach with EtcBuilder {
  private val parser: Type1Parser = DefaultType1Parser
  private var unifier: TypeUnifier = _

  override protected def beforeEach(): Unit = {
    unifier = new TypeUnifier()
  }

  test("unifying monotypes") {
    assert(unifier.unify(Substitution.empty, ConstT1("F"), ConstT1("F")).contains((Substitution.empty, ConstT1("F"))))
    assert(unifier.unify(Substitution.empty, IntT1(), IntT1()).contains((Substitution.empty, IntT1())))
    assert(unifier.unify(Substitution.empty, BoolT1(), BoolT1()).contains((Substitution.empty, BoolT1())))
    assert(unifier.unify(Substitution.empty, StrT1(), StrT1()).contains((Substitution.empty, StrT1())))
    assert(unifier.unify(Substitution.empty, RealT1(), RealT1()).contains((Substitution.empty, RealT1())))
    val intToInt = parser("Int -> Int")
    assert(unifier.unify(Substitution.empty, intToInt, intToInt).contains((Substitution.empty, intToInt)))
    val intAndBoolToInt = parser("(Int, Bool) => Int")
    assert(
        unifier
          .unify(Substitution.empty, intAndBoolToInt, intAndBoolToInt)
          .contains((Substitution.empty, intAndBoolToInt)))
    val tup1 = parser("<<Int, Bool>>")
    assert(unifier.unify(Substitution.empty, tup1, tup1).contains((Substitution.empty, tup1)))

    val sparse1 = parser("{2: Int, 4: Bool}")
    val sparse2 = parser("{3: Str}")
    val sparse3 = parser("{2: Int, 3: Str, 4: Bool}")
    val sparse4 = parser("{1: Int}")
    val sparse5 = parser("{1: Int, 2: Bool}")
    assert(unifier.unify(Substitution.empty, sparse1, sparse2).contains((Substitution.empty, sparse3)))
    assert(unifier.unify(Substitution.empty, tup1, sparse4).contains((Substitution.empty, tup1)))
    assert(unifier.unify(Substitution.empty, sparse4, tup1).contains((Substitution.empty, tup1)))

    val rec1 = parser("[foo: Bool, bar: Int]")
    val rec2 = parser("[baz: Str]")
    val rec3 = parser("[foo: Bool, bar: Int, baz: Str]")
    assert(unifier.unify(Substitution.empty, rec1, rec2).contains((Substitution.empty, rec3)))
  }

  test("non-unifying monotypes") {
    assert(unifier.unify(Substitution.empty, ConstT1("F"), BoolT1()).isEmpty)
    assert(unifier.unify(Substitution.empty, ConstT1("F"), ConstT1("G")).isEmpty)
    assert(unifier.unify(Substitution.empty, IntT1(), BoolT1()).isEmpty)
    assert(unifier.unify(Substitution.empty, BoolT1(), StrT1()).isEmpty)
    assert(unifier.unify(Substitution.empty, StrT1(), IntT1()).isEmpty)
    assert(unifier.unify(Substitution.empty, RealT1(), IntT1()).isEmpty)

    val intToInt = parser("Int -> Int")
    val boolToInt = parser("Bool -> Int")
    assert(unifier.unify(Substitution.empty, intToInt, boolToInt).isEmpty)
    val intAndBoolToInt = parser("(Int, Bool) => Int")
    val boolAndIntToInt = parser("(Bool, Int) => Int")
    assert(unifier.unify(Substitution.empty, intAndBoolToInt, boolAndIntToInt).isEmpty)
    val tup1 = parser("<<Int, Bool>>")
    val tup2 = parser("<<Int, Str>>")
    val tup3 = parser("<<Str, Int>>")
    assert(unifier.unify(Substitution.empty, tup1, tup2).isEmpty)
    assert(unifier.unify(Substitution.empty, tup2, tup3).isEmpty)

    val sparse1 = parser("{2: Int, 4: Bool}")
    val sparse2 = parser("{2: Int, 4: Int}")
    assert(unifier.unify(Substitution.empty, sparse1, sparse2).isEmpty)
    // a sparse tuple is not allowed to extend the tuple domain
    assert(unifier.unify(Substitution.empty, tup3, sparse1).isEmpty)

    val rec1 = parser("[foo: Bool, bar: Int]")
    val rec2 = parser("[foo: Bool, bar: Bool]")
    assert(unifier.unify(Substitution.empty, rec1, rec2).isEmpty)
  }

  test("unifying polytypes") {
    assert(
        unifier
          .unify(Substitution.empty, VarT1(0), IntT1())
          .contains((Substitution(EqClass(0) -> IntT1()), IntT1())))
    assert(
        unifier
          .unify(Substitution.empty, FunT1(VarT1(0), IntT1()), FunT1(BoolT1(), VarT1(1)))
          .contains((Substitution(EqClass(0) -> BoolT1(), EqClass(1) -> IntT1()), FunT1(BoolT1(), IntT1()))))
    assert(
        unifier
          .unify(Substitution.empty, VarT1(0), ConstT1("ID"))
          .contains((Substitution(EqClass(0) -> ConstT1("ID")), ConstT1("ID"))))
    assert(
        unifier
          .unify(Substitution.empty, ConstT1("ID"), VarT1(0))
          .contains((Substitution(EqClass(0) -> ConstT1("ID")), ConstT1("ID"))))

    val rec1 = parser("[foo: Bool]")
    val rec2 = parser("[bar: Int]")
    val rec3 = parser("[foo: Bool, bar: Int]")
    assert(
        unifier
          .unify(Substitution(EqClass(0) -> rec1), VarT1(0), rec2)
          .contains((Substitution(EqClass(0) -> rec3), rec3)))
  }

  test("unifying tricky polytypes") {
    assert(
        unifier
          .unify(Substitution.empty, VarT1(0), VarT1(0))
          .contains((Substitution(EqClass(0) -> VarT1(0)), VarT1(0))))
    assert(
        unifier
          .unify(Substitution(EqClass(0) -> IntT1()), VarT1(0), VarT1(0))
          .contains((Substitution(EqClass(0) -> IntT1()), IntT1())))
    assert(
        unifier
          .unify(Substitution.empty, VarT1(0), VarT1(1))
          .contains((Substitution(EqClass(Set(0, 1)) -> VarT1(1)), VarT1(1))))
    assert(
        unifier
          .unify(Substitution(EqClass(1) -> IntT1()), VarT1(0), VarT1(1))
          .contains((Substitution(EqClass(Set(0, 1)) -> IntT1()), IntT1())))
    // unify <<a, b>> and <<b, a>>
    assert(
        unifier
          .unify(Substitution.empty, parser("<<a, b>>"), parser("<<b, a>>"))
          .contains((Substitution(EqClass(Set(0, 1)) -> VarT1(1)), parser("<<b, b>>"))))
    // there is no problem with the cycle a -> b -> c -> a
    val expectedSub = Substitution(EqClass(Set(0, 1, 2)) -> VarT1(2))
    assert(
        unifier
          .unify(Substitution.empty, parser("<<a, b, c>>"), parser("<<b, c, a>>"))
          .contains((expectedSub, parser("<<c, c, c>>"))))
  }

  test("unifying Seq(a) and Seq(Int)") {
    assert(
        unifier
          .unify(Substitution.empty, SeqT1(VarT1(0)), SeqT1(IntT1()))
          .contains((Substitution(EqClass(0) -> IntT1()), SeqT1(IntT1()))))
  }

  test("unifying a => Set(a) and Int => b") {
    assert(
        unifier
          .unify(Substitution.empty, OperT1(Seq(VarT1(0)), SetT1(VarT1(0))), OperT1(Seq(IntT1()), VarT1(1)))
          .contains((Substitution(EqClass(0) -> IntT1(), EqClass(1) -> SetT1(VarT1(0))),
                  OperT1(Seq(IntT1()), SetT1(IntT1())))))
  }

  test("non-unifying polytypes") {
    // a and Set(a) must be non-unifiable
    assert(unifier.unify(Substitution.empty, parser("a"), parser("Set(a)")).isEmpty)
  }

  test("unifying with transitivity") {
    val expectedSubstitution = Substitution(EqClass(Set(1, 2)) -> parser("PERSON"))
    assert(
        unifier
          .unify(Substitution.empty, parser("Set(b) -> Set(b)"), parser("Set(c) -> Set(PERSON)"))
          .contains((expectedSubstitution, parser("Set(PERSON) -> Set(PERSON)"))))
  }

  // regression
  test("unifying variables via sets") {
    val sub = Substitution(EqClass(1003) -> SetT1(VarT1(0)), EqClass(1004) -> SetT1(VarT1(1005)))
    val expected =
      Substitution(EqClass(Set(1003, 1004)) -> SetT1(VarT1(1005)), EqClass(Set(0, 1005)) -> VarT1(1005))
    assert(
        unifier
          .unify(sub, VarT1(1004), VarT1(1003))
          .contains((expected, SetT1(VarT1(1005)))))
  }

  // regression
  test("unifying variables via operators") {
    val sub = Substitution(
        EqClass(Set(1000, 1004)) -> VarT1(1000),
        EqClass(1005) -> OperT1(Seq(VarT1(1000)), VarT1(1001)),
        EqClass(1006) -> VarT1(1000)
    ) ////
    val expected = Substitution(
        EqClass(Set(1000, 1001, 1004, 1006)) -> VarT1(1006),
        EqClass(1005) -> OperT1(Seq(VarT1(1006)), VarT1(1006))
    ) ////
    assert(
        unifier
          .unify(sub, VarT1(1005), OperT1(Seq(VarT1(1004)), VarT1(1006)))
          .contains((expected, OperT1(Seq(VarT1(1006)), VarT1(1006)))))
  }

  // regression
  test("merge equivalence classes of a -> b, b -> a") {
    val expectedSub = Substitution(EqClass(Set(0, 1)) -> VarT1(1))
    val result = unifier.unify(Substitution(EqClass(0) -> VarT1("b"), EqClass(1) -> VarT1("a")), VarT1("a"), VarT1("b"))
    assert(result.contains((expectedSub, VarT1(1))))
  }

  // regression
  test("no stack overflow #925") {
    val oper1 = OperT1(Seq(VarT1("a")), RecT1("f" -> VarT1("a")))
    val oper2 = OperT1(Seq(VarT1("a")), RecT1("f" -> SetT1(VarT1("a"))))
    assert(unifier.unify(Substitution(), oper1, oper2).isEmpty)
  }
}
