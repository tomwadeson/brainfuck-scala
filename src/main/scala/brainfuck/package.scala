import cats.data.StateT
import cats.effect.IO

package object brainfuck {
  type Brainfuck[A] = StateT[IO, Machine, A]
}
