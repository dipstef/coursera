package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for (
      (name, exprSignal) <- namedExpressions
    ) yield (name, Signal(eval( exprSignal(), namedExpressions - name)) )
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

      def compute(expr: Expr) = eval(expr, references)

      expr match {
        case Literal(v) => v
        case Ref(name) => compute(getReferenceExpr(name, references))
        case Plus(expr1, expr2) => compute(expr1) + compute(expr2)
        case Minus(expr1, expr2) => compute(expr1) - compute(expr2)
        case Times(expr1, expr2) => compute(expr1) * compute(expr2)
        case Divide(expr1, expr2) => compute(expr1) / compute(expr2)
      }

  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] { Literal(Double.NaN)} { exprSignal => exprSignal() }
  }
}
