package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object SegmentedRedundantBinaryNumbers {

  sealed trait Digit {
    val value: Long
    val multiplier: Long = 2L
  }

  case object Zero extends Digit {
    val value = 0L
  }

  case class Ones(n: Int) extends Digit {
    override val multiplier = 1L << n

    val value = multiplier - 1
  }

  case object Two extends Digit {
    val value = 2L
  }

  case class Threes(n: Int) extends Digit {
    override val multiplier = 1L << n

    val value = 3 * (multiplier - 1)
  }

  case object Four extends Digit {
    val value = 4L
  }

  type Number = List[Digit]

  def ones(n: Int, ds: Number): Number = (n, ds) match {
    case (0, _) => ds
    case (_, Ones(m) :: ds1) => Ones(n + m) :: ds1
    case _ => Ones(n) :: ds
  }

  def threes(n: Int, ds: Number): Number = (n, ds) match {
    case (0, _) => ds
    case (_, Threes(m) :: ds1) => Threes(n + m) :: ds1
    case _ => Threes(n) :: ds
  }

  def simpleInc(x: Number): Number = x match {
    case Nil => ones(1, Nil)
    case Zero :: ds => ones(1, ds)
    case Ones(n) :: ds => Two :: ones(n - 1, ds)
    case Two :: ds => threes(1, ds)
    case Threes(n) :: ds => Four :: threes(n - 1, ds)
    case Four :: ds => Two :: simpleInc(ds)
  }

  def simpleDec(x: Number): Number = x match {
    case Nil => throw new IllegalArgumentException("Cannot decrement zero")
    case Zero :: ds => ones(1, simpleDec(ds))
    case Ones(n) :: ds => Zero :: ones(n - 1, ds)
    case Two :: ds => ones(1, ds)
    case Threes(n) :: ds => Two :: threes(n - 1, ds)
    case Four :: ds => threes(1, ds)
  }

  def fixup(x: Number) = x match {
    case Zero :: Nil => Nil
    case Zero :: ds => Two :: simpleDec(ds)
    case Ones(n) :: Zero :: ds => Ones(n) :: Zero :: simpleDec(ds)
    case Threes(n) :: Four :: ds => Threes(n) :: Two :: simpleInc(ds)
    case Four :: ds => Two :: simpleInc(ds)
    case _ => x
  }

  def inc(x: Number) = fixup(simpleInc(x))

  def dec(x: Number) = fixup(simpleDec(x))

  implicit class NumberOps(ds: Number) {
    def asLong: Long =
      ds.foldRight(0L)((d, a) => a * d.multiplier + d.value)
  }


  def toNumber(x: Long): Number = {
    def twice(ds: Number): Number = ds match {
      case Nil => Nil
      case _ => Two :: dec(ds)
    }

    def toNumber(x: Long, ds: Number): Number = x match {
      case _ if x < 0 => throw new IllegalArgumentException(s"toNumber($x)")
      case 0 => Nil
      case _ =>
        val rounded = twice(toNumber(x / 2, ds))
        if (x % 2 == 1) inc(rounded) else rounded
    }

    toNumber(x, Nil)
  }
}
