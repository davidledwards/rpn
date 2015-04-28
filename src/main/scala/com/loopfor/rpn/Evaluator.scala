package com.loopfor.rpn

import scala.annotation.tailrec
import scala.math.{max, min, pow}
import scala.util.Try

/**
 * An evaluator that computes the result of an instruction sequence.
 */
trait Evaluator {
  def apply(codes: Stream[Code]): Try[Double]
}

private class BasicEvaluator(val resolver: String => Double) extends Evaluator {
  import BasicEvaluator._

  def apply(codes: Stream[Code]): Try[Double] = Try {
    @tailrec def evaluate(codes: Stream[Code],
                          stack: Seq[Double],
                          syms: Map[String, Double]): Double = codes.headOption match {
      case Some(DeclareSymbolCode(name)) =>
        evaluate(codes.tail, stack, syms + (name -> resolver(name)))
      case Some(PushSymbolCode(name)) =>
        val st = (syms get name) match {
          case Some(v) => v +: stack
          case None => throw new Exception(s"$name: symbol not bound")
        }
        evaluate(codes.tail, st, syms)
      case Some(PushCode(v)) =>
        evaluate(codes.tail, v +: stack, syms)
      case Some(c: ScalarCode) =>
        val (vs, rest) = stack splitAt c.args
        val st = if (vs.size == c.args)
          (vs.reverse reduceLeft scalarOp(c.getClass)) +: rest
        else
          throw new Exception(s"evaluator stack underflow")
        evaluate(codes.tail, st, syms)
      case Some(_) =>
        evaluate(codes.tail, stack, syms)
      case None =>
        stack match {
          case Seq(r) => r
          case _ => throw new Exception(s"evaluator stack size should be 1, but is ${stack.size}")
        }
    }
    evaluate(codes, Seq.empty, Map.empty)
  }
}

object BasicEvaluator {
  def apply(resolver: String => Double): Evaluator = new BasicEvaluator(resolver)
  def apply(codes: Stream[Code])(resolver: String => Double): Try[Double] = apply(resolver)(codes)

  val scalarOp: Map[Class[_ <: ScalarCode], (Double, Double) => Double] = Map(
    classOf[AddCode] -> { _ + _ },
    classOf[SubtractCode] -> { _ - _ },
    classOf[MultiplyCode] -> { _ * _ },
    classOf[DivideCode] -> { _ / _ },
    classOf[MinCode] -> { min(_, _) },
    classOf[MaxCode] -> { max(_, _) },
    ModuloCode.getClass -> { _ % _ },
    PowerCode.getClass -> { pow(_, _) }
    )
}