/*
 * Copyright 2015 David Edwards
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.loopfor.rpn

import scala.annotation.tailrec

/**
 * An optimizer that transforms a sequence of instructions into another sequence of
 * instructions.
 */
trait Optimizer extends (Seq[Code] => Seq[Code])

private class BasicOptimizer extends Optimizer {
  def apply(codes: Seq[Code]): Seq[Code] = {
    @tailrec def optimize(codes: Seq[Code]): Seq[Code] = {
      val to = transform(codes)
      if (to.size < codes.size) optimize(to) else to
    }
    optimize(codes)
  }

  private val transform =
    (combineDynamicOperators _) andThen
    (flattenDynamicOperators _) andThen
    (evaluateLiteralExpressions _)

  private val dynamicCtors: Map[Class[_ <: DynamicOperatorCode], Int => DynamicOperatorCode] = Map(
        classOf[AddCode] -> AddCode.apply _,
        classOf[SubtractCode] -> SubtractCode.apply _,
        classOf[MultiplyCode] -> MultiplyCode.apply _,
        classOf[DivideCode] -> DivideCode.apply _,
        classOf[MinCode] -> MinCode.apply _,
        classOf[MaxCode] -> MaxCode.apply _
        )

  private val operatorCtors: Map[Class[_ <: Code], Int => Code] = Map(
        ModuloCode.getClass -> { _: Int => ModuloCode },
        PowerCode.getClass -> { _: Int => PowerCode }
        ) ++ dynamicCtors

  /**
   * An optimization that combines a series of identical operations as they appear in the
   * original source.
   * 
   * Consider the following input: `x + y + z`
   * 
   * The parser generates an AST that first evaluates `x + y`, then evaluates the result of
   * that expression and `z`. The corresponding bytecode follows:
   * {{{
   * push x
   * push y
   * add 2
   * push z
   * add 2
   * }}}
   * 
   * The `add 2` instruction tells the interpreter to pop `2` elements from the evaluation
   * stack, compute the sum, and push the result onto the stack. Since the `add` instruction
   * can operate on any number of arguments, both `add` operations can be combined into a
   * single instruction:
   * {{{
   * push x
   * push y
   * push z
   * add 3
   * }}}
   * 
   * A slightly more complicated example illustrates the same principle. Consider the input,
   * `a + (b * c) + d`, and the corresponding bytecode:
   * {{{
   * push a
   * push b
   * push c
   * mul 2
   * add 2
   * push d
   * add 2
   * }}}
   * 
   * Similar to the first scenario, both `add` operations can be combined even though the
   * intervening expression `b * c` exists. Note that adjacency of instructions is not
   * relevant, but rather the equivalence of the evaluation stack frame depth. In other
   * words, all operations of the same type at the same frame can be combined into a single
   * operation.
   * 
   * The algorithm works by simulating execution using an evaluation stack, maintaining a
   * set of instructions that are candidates for elimination. If another instruction at the
   * same frame depth is encountered, the original instruction is replaced with a `nop` and
   * the current instruction modified to evaluate additional elements on the stack. Once
   * all instructions have been evaluated, the set of revisions are applied, resulting in
   * a new sequence of instructions.
   */
  private def combineDynamicOperators(codes: Seq[Code]): Seq[Code] = {
    trait Frame
    case object Ineligible extends Frame
    case class Eligible(op: String, pos: Int, code: DynamicOperatorCode) extends Frame

    val (_, _, revs) = codes.foldLeft((0,
                                       Seq.empty[Frame],
                                       Map.empty[Int, Code])) {
      case ((pos, frames, revs), code) =>
        val (_frames, _revs) = code match {
          case PushSymbolCode(_) | PushCode(_) =>
            (Ineligible +: frames, revs)
          case c: DynamicOperatorCode =>
            // Consider only last argument on stack frame as potentially eligible, and only
            // if operator names match. The intuition behind examining only the last argument,
            // which is actually the first when viewed from a left-to-right evaluation
            // standpoint, is that the prior eligible operator would have occurred at the
            // same frame depth as the current operator.
            val (_code, _revs) = (frames take c.args).last match {
              case Eligible(op, _pos, _code) if op == c.op =>
                // Create modified instruction to reflect increase in number of arguments,
                // and turn eligible instruction into `nop` by including in revision set.
                val _c = dynamicCtors(c.getClass)(c.args + _code.args - 1)
                (_c, revs + (_pos -> NopCode) + (pos -> _c))
              case _ =>
                (c, revs)
            }
            (Eligible(_code.op, pos, _code) +: (frames drop c.args), _revs)
          case c: OperatorCode =>
            (Ineligible +: (frames drop c.args), revs)
          case _ =>
            (frames, revs)
        }
        (pos + 1, _frames, _revs)
    }
    revise(codes, revs)
  }

  /**
   * An optimization that flattens identical operations adjacent to each other in the
   * instruction sequence.
   * 
   * This optimization is similar to [[combineDynamicOperators]] in that operations are
   * essentially combined, but instead it looks for special cases in which identical operations
   * occur in adjacent frames on the evaluation stack.
   * 
   * Consider the input, `x * (y * z)`, and the corresponding bytecode:
   * {{{
   * push x
   * push y
   * push z
   * mul 2
   * mul 2
   * }}}
   * 
   * Note that both `mul` instructions occur in adjacent positions. At first glance, it may
   * appear as though [[combineDynamicOperators]] would eliminate one of the operations, but
   * each occurs at a different frame on the evaluation stack.
   * 
   * The intuition behind this optimization is that the first `mul 2` would push its result
   * onto the stack, only to be removed for evaluation by the second `mul 2` instruction. So,
   * rather than performing an intermediate calculation, the first can be eliminated in lieu of
   * a single `mul 3` instruction. In general, any number of adjacent identical instructions
   * can be reduced to a single instruction.
   * 
   * One may notice that this phase optimizes right-to-left evaluation scenarios, but only for
   * those operators with the associative property, i.e. evaluation can be left-to-right or
   * right-to-left. This becomes more clear with another example: `a * (b * (c * d))`.
   * The original instruction sequence follows:
   * {{{
   * push a
   * push b
   * push c
   * push d
   * mul 2
   * mul 2
   * mul 2
   * }}}
   * 
   * In essence, the parentheses are being removed and evaluated in a left-to-right manner
   * by eliminating all but the last `mul` instruction:
   * {{{
   * push a
   * push b
   * push c
   * push d
   * mul 4
   * }}}
   * 
   * The algorithm works by stepping through each associative operator instruction, finding
   * adjacent identical pairs, and eliminating all but the final instruction, which is then
   * modified to reflect the combined number of arguments.
   */
  private def flattenDynamicOperators(codes: Seq[Code]): Seq[Code] = {
    val (_, _, revs) = codes.foldLeft((0,
                                       Option.empty[DynamicOperatorCode],
                                       Map.empty[Int, Code])) {
      case ((pos, prior, revs), code) =>
        val (p, r) = code match {
          case c: DynamicOperatorCode if c.isAssociative =>
            prior match {
              case Some(p) if (c.op == p.op) =>
                // Current instruction matches previous instruction, so do the following:
                // - replace last instruction with `nop`, which may overwrite revision from
                //   prior instruction evaluation
                // - modify current instruction to reflect combined argument count
                val rep = dynamicCtors(c.getClass)(c.args + p.args - 1)
                (Some(rep), revs + (pos - 1 -> NopCode) + (pos -> rep))
              case _ => (Some(c), revs)
            }
          case _ => (None, revs)
        }
        (pos + 1, p, r)
    }
    revise(codes, revs)
  }

  /**
   * An optimization that evaluates literal expressions.
   * 
   * This optimization finds expressions containing only literal values and reduces them to a
   * single value, thereby eliminating the need for the interpreter to perform the computation.
   * 
   * Consider the input, `x + 1 + y + 2`, which produces the following sequence of
   * unoptimized instructions:
   * {{{
   * push x
   * push 1
   * add 2
   * push y
   * add 2
   * push 2
   * add 2
   * }}}
   * 
   * Applying the [[combineDynamicOperators]] optimization produces the following:
   * {{{
   * push x
   * push 1
   * push y
   * push 2
   * add 4
   * }}}
   * 
   * In either case, there is still opportunity to further optimize. Had the original input
   * been written as `x + y + 1 + 2`, it becomes more clear that `1 + 2` could be replaced
   * with `3`. The purpose of this optimization phase is to find such expressions and reduce
   * them to a single value.
   * 
   * In the latter optimized case above, applying this optimization reduces the instruction
   * sequence to the following:
   * {{{
   * push x
   * push y
   * push 3
   * add 3
   * }}}
   * 
   * The algorithm works by simulating execution of the instruction sequence using an evaluation
   * stack, though recording only literal values. As operations are encountered, the optimizer
   * peeks into the evaluation stack to determine if two or more literals are present, and if so,
   * eliminates the `push` instruction corresponding to each literal in lieu of a single `push`.
   * When an optimization is detected, the evaluation terminates and revisions are applied to
   * the original sequence of instructions. This process repeats itself until a complete
   * evaluation yields no new optimizations.
   * 
   * Note that an expression consisting entirely of literals will always be reduced to a single
   * `push` instruction containing the computed value.
   */
  private def evaluateLiteralExpressions(codes: Seq[Code]): Seq[Code] = {
    trait Value
    case class NumberValue(value: Double, pos: Int) extends Value
    case object ArbitraryValue extends Value

    def analyze(pos: Int, codes: Seq[Code], stack: Seq[Value]): Map[Int, Code] = {
      def inspect(code: OperatorCode): (Map[Int, Code], Seq[Value]) = {
        // Extract only those arguments representing literal values.
        val nums = for (arg @ NumberValue(_, _) <- (stack take code.args).reverse) yield arg

        // Revisions can only be applied if the operator is commutative and more than one argument
        // is a literal, or for any operator if all arguments are literals.
        val revs = if ((nums.size > 1 && code.isCommutative) || nums.size == code.args) {
          import Evaluator.operators

          // Precompute value by applying operator function to literals in left-to-right order.
          val value = (for (NumberValue(v, _) <- nums) yield v) reduceLeft operators(code.getClass)

          // Replace all but last `push` instruction with `nop`.
          val rs = nums.init.foldLeft(Map.empty[Int, Code]) { case (r, num) => r + (num.pos -> NopCode) }

          // Eliminate operation if entire expression is composed of literals, otherwise modify
          // operation to reflect reduction in arguments.
          rs + (nums.last.pos -> PushCode(value)) +
            (if (nums.size == code.args) (pos -> NopCode)
             else (pos -> operatorCtors(code.getClass)(code.args - nums.size + 1)))
        } else
          Map.empty[Int, Code]

        // Arbitrary value is always pushed after dropping operator arguments since all operators
        // push result of its computation.
        (revs, ArbitraryValue +: (stack drop code.args))
      }

      codes.headOption match {
        case Some(DeclareSymbolCode(_)) => analyze(pos + 1, codes.tail, stack)
        case Some(PushSymbolCode(_)) => analyze(pos + 1, codes.tail, ArbitraryValue +: stack)
        case Some(PushCode(v)) => analyze(pos + 1, codes.tail, NumberValue(v, pos) +: stack)
        case Some(c: OperatorCode) =>
          val (revs, s) = inspect(c)
          if (revs.size == 0) analyze(pos + 1, codes.tail, s) else revs
        case None => Map.empty
        case _ => analyze(pos + 1, codes.tail, stack)
      }
    }

    @tailrec def evaluate(codes: Seq[Code]): Seq[Code] = {
      val revs = analyze(0, codes, Seq.empty)
      if (revs.isEmpty) codes else evaluate(revise(codes, revs))
    }
    evaluate(codes)
  }

  private def revise(codes: Seq[Code], revs: Map[Int, Code]): Seq[Code] = {
    if (revs.isEmpty)
      codes
    else {
      val (_, cs) = ((0, Seq.empty[Code]) /: codes) { case ((pos, revised), code) =>
        (pos + 1, (revs get pos) match {
          case Some(NopCode) => revised
          case Some(c) => c +: revised
          case None => code +: revised
        })
      }
      cs.reverse
    }
  }
}

object Optimizer {
  def apply(): Optimizer = new BasicOptimizer
  def apply(codes: Seq[Code]): Seq[Code] = apply()(codes)

  val disabled: Optimizer = new Optimizer {
    def apply(codes: Seq[Code]): Seq[Code] = codes
  }
}
