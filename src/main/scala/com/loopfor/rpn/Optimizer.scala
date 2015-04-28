package com.loopfor.rpn

import scala.annotation.tailrec
import scala.util.{Success, Try}

/**
 * An optimizer that transforms a sequence of instructions into another sequence of
 * instructions.
 */
trait Optimizer {
  def apply(codes: Seq[Code]): Try[Seq[Code]]
}

private class BasicOptimizer extends Optimizer {
  def apply(codes: Seq[Code]): Try[Seq[Code]] = Try {
    @tailrec def optimize(codes: Seq[Code]): Seq[Code] = {
      val to = transform(codes)
      if (to.size < codes.size) optimize(to) else to
    }
    optimize(codes)
  }

  private val transform = (combineDynamicScalars _) andThen (flattenDynamicScalars _) andThen (evaluateLiteralExpressions _)

  private val dynamicCtors: Map[Class[_ <: DynamicScalarCode], Int => DynamicScalarCode] = Map(
        classOf[AddCode] -> AddCode.apply _,
        classOf[SubtractCode] -> SubtractCode.apply _,
        classOf[MultiplyCode] -> MultiplyCode.apply _,
        classOf[DivideCode] -> DivideCode.apply _,
        classOf[MinCode] -> MinCode.apply _,
        classOf[MaxCode] -> MaxCode.apply _
        )

  private val scalarCtors: Map[Class[_ <: Code], Int => Code] = Map(
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
  private def combineDynamicScalars(codes: Seq[Code]): Seq[Code] = {
    val (_, _, _, revs) = codes.foldLeft((0, 0,
                                          Map.empty[(String, Int), (Int, DynamicScalarCode)],
                                          Map.empty[Int, Code])) {
      case ((pos, frame, cands, revs), code) =>
        // Only candidates whose frame is not larger than the current frame are eligible
        // for elimination.
        lazy val eligible = cands filter { case ((_, f), _) => f <= frame }

        def evaluate(code: DynamicScalarCode) = (eligible get (code.op, frame)) match {
          case Some((p, c)) =>
            // Found matching candidate on current frame, so do the following:
            // - remove candidate from eligibility set and add `nop` to revision set
            // - add modified instruction both as candidate for future consideration and
            //   to revision set
            val rep = dynamicCtors(code.getClass)(c.args + code.args - 1)
            (eligible - ((code.op, frame)) + ((rep.op, frame) -> (pos, rep)),
                  revs + (p -> NopCode) + (pos -> rep))
          case None =>
            // No matching candidate on current frame, so add this instruction for future
            // consideration.
            (eligible + ((code.op, frame) -> (pos, code)), revs)
        }

        val (f, c, r) = code match {
          case PushSymbolCode(_) => (1, cands, revs)
          case PushCode(_) => (1, cands, revs)
          case c: DynamicScalarCode =>
            val (_cands, _revs) = evaluate(c)
            (1 - c.args, _cands, _revs)
          case c: ScalarCode => (1 - c.args, eligible, revs)
          case _ => (0, cands, revs)
        }
        (pos + 1, frame + f, c, r)
    }
    revise(codes, revs)
  }

  /**
   * An optimization that flattens identical operations adjacent to each other in the
   * instruction sequence.
   * 
   * This optimization is similar to [[combineDynamicScalars]] in that operations are
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
   * appear as though [[combineDynamicScalars]] would eliminate one of the operations, but each
   * occurs at a different frame on the evaluation stack.
   * 
   * The intuition behind this optimization is that the first `mul 2` would push its result
   * onto the stack, only to be removed for evaluation by the second `mul 2` instruction. So,
   * rather than performing an intermediate calculation, the first can be eliminated in lieu of
   * a single `mul 3` instruction. In general, any number of adjacent identical instructions
   * can be reduced to a single instruction.
   * 
   * One may notice that this phase optimizes for right-to-left associativity, which becomes
   * more clear with another example: `a * (b * (c * d))`. The original instruction sequence
   * follows:
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
   * The algorithm works by stepping through each instruction, finding adjacent identical
   * pairs, and eliminating all but the final instruction, which is then modified to reflect
   * the combined number of arguments.
   */
  private def flattenDynamicScalars(codes: Seq[Code]): Seq[Code] = {
    val (_, _, revs) = codes.foldLeft((0,
                                       Option.empty[DynamicScalarCode],
                                       Map.empty[Int, Code])) {
      case ((pos, prior, revs), code) =>
        val (p, r) = code match {
          case c: DynamicScalarCode =>
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
   * Applying the [[combineDynamicScalars]] optimization produces the following:
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
      def inspect(code: ScalarCode): (Map[Int, Code], Seq[Value]) = {
        val nums = for (arg @ NumberValue(_, _) <- (stack take code.args).reverse) yield arg
        val revs = if (nums.size > 1) {
          import BasicEvaluator.scalarOp
          // Expression with multiple literals is detected, so do the following:
          // - replace all but last `push` instruction with `nop`
          // - modify last `push` with precomputed value
          // - eliminate entire operation if entire expression composed of literals
          // - otherwise, modify operation to reflect reduction in arguments
          val value = (for (NumberValue(v, _) <- nums) yield v) reduceLeft scalarOp(code.getClass)
          val rs = nums.init.foldLeft(Map.empty[Int, Code]) { case (r, num) => r + (num.pos -> NopCode) }
          rs + (nums.last.pos -> PushCode(value)) +
            (if (nums.size == code.args) (pos -> NopCode)
             else (pos -> scalarCtors(code.getClass)(code.args - nums.size + 1)))
        } else
          Map.empty[Int, Code]
        (revs, stack drop code.args)
      }

      codes.headOption match {
        case Some(DeclareSymbolCode(_)) => analyze(pos + 1, codes.tail, stack)
        case Some(PushSymbolCode(_)) => analyze(pos + 1, codes.tail, ArbitraryValue +: stack)
        case Some(PushCode(v)) => analyze(pos + 1, codes.tail, NumberValue(v, pos) +: stack)
        case Some(c: ScalarCode) =>
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

object BasicOptimizer {
  def apply(): Optimizer = new BasicOptimizer
  def apply(codes: Seq[Code]): Try[Seq[Code]] = apply()(codes)
}

object DisabledOptimizer {
  def apply(): Optimizer = new Optimizer {
    def apply(codes: Seq[Code]): Try[Seq[Code]] = Success(codes)
  }
  def apply(codes: Seq[Code]): Try[Seq[Code]] = apply()(codes)
}
