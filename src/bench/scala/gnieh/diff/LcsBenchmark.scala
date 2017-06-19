package gnieh.diff

import org.scalameter.api._

import scala.util.Random

class LcsBenchmark extends Bench.LocalTime {

  private val random = new Random

  private val inputs = Gen.range("size")(100, 1000, 200).map { size =>
    def seq = (0 until size).map(_ => random.nextString(5))

    seq -> seq
  }

  def measureLcs(alg: Lcs[String]): Unit = measure method "lcs" in {
    using(inputs) in { case (s1, s2) =>
      alg.lcs(s1, s2)
    }
  }

  performance of "Patience" in {
    measureLcs(new Patience[String])
  }

  performance of "MyersLcs" in {
    measureLcs(new MyersLcs[String])

  }

  performance of "DynamicProgLcs" in {
    measureLcs(new DynamicProgLcs[String])
  }

}
