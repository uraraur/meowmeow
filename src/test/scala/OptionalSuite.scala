package com.tkroman.kpi.y2022.l1

import munit.FunSuite

class OptionalSuite extends FunSuite {
  test("fold on Some") {
    val expected = 10
    val actual = fold(Optional.Some(5), 0, (_, a) => a * 2)
    assertEquals(actual, expected)
  }
  test("fold on None") {
    val expected = 0
    val actual = fold(Optional.None: Optional[Int], 0, (_, a) => a * 2)
    assertEquals(actual, expected)
  }
}
