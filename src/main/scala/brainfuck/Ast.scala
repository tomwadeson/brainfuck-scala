package brainfuck

sealed trait Instruction

object Instruction {
  case object IncrementPointer                           extends Instruction
  case object DecrementPointer                           extends Instruction
  case object IncrementValueAtPointer                    extends Instruction
  case object DecrementValueAtPointer                    extends Instruction
  case object OutputValueAtPointer                       extends Instruction
  case object ReadAndSetValueAtPointer                   extends Instruction
  case class DoWhileValueAtPointerNonZero(loop: Program) extends Instruction
}

final case class Program(value: List[Instruction]) extends AnyVal
