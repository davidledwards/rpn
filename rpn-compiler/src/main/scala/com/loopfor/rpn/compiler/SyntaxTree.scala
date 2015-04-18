package com.loopfor.rpn.compiler

sealed trait Expr

case class SymbolExpr(name: String) extends Expr
case class NumberExpr(value: Double) extends Expr
case class AddExpr(l: Expr, r: Expr) extends Expr
case class SubtractExpr(l: Expr, r: Expr) extends Expr
case class MultiplyExpr(l: Expr, r: Expr) extends Expr
case class DivideExpr(l: Expr, r: Expr) extends Expr
