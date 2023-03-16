// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  val stop1 = Stop("Downtown Crossing", List("Red"))
  val stop1copy = Stop("Downtown Crossing", List("Red"))
  val stop2 = Stop("Downtown Crossing", List("Orange"))
  val stop3 = Stop("Alewife", List("Red"))
  val stop4 = Stop("Chinatown", List("Orange"))
  val stop5 = Stop("Park Street", List("Red"))
  val stop6 = Stop("Park Street", List("Green C"))
  val stop7 = Stop("Government Center", List("Green C"))
  val stop8 = Stop("Government Center", List("Blue"))
  val stop9 = Stop("State", List("Blue"))
  val stop10 = Stop("State", List("Orange"))
  val stops = List(
    stop1,
    stop1copy,
    stop2,
    stop3,
    stop4,
    stop5,
    stop6,
    stop7,
    stop8,
    stop9,
    stop10
  )
  val collapsedStops = collapseStops(stops)
  println(collapsedStops)
  val route1 = Route("Red", "Red Line", List(stop1, stop5))
  val route2 = Route("Green C", "Green Line C", List(stop1, stop1copy, stop2))
  val route3 = Route("Blue", "Blue Line", List(stop1, stop1copy))
  val routes = List(route1, route2, route3)
  println(mostStops(routes))
  println(leastStops(routes))
  println("All connecting")
  println(connectingStops(collapsedStops))
  val s1 = stop1
  val s2 = stop2
  println(s"Get path between $s1 and $s2")
  val path = getLeastConnectionsLeastStopsPath(s1, s2, stops, List())
  println(path)

  /*HashMap(
      "Red"-> HashSet("Mattapan", "Orange", "Green-B", "Green-C", "Green-D", "Green-E"),
      "Blue"-> HashSet("Orange","Green-B", "Green-C", "Green-D", "Green-E"),
      "Orange"-> HashSet("Blue", "Orange", "Green-B", "Green-C", "Green-D", "Green-E"),
      "Green-B"-> HashSet("Blue", "Orange", "Red", "Green-C", "Green-D", "Green-E"),
      "Green-C"-> HashSet("Blue", "Orange", "Green-B", "Red", "Green-D", "Green-E"),
      "Green-D"-> HashSet("Blue", "Orange", "Green-B", "Green-C", "Red", "Green-E"),
      "Green-E"-> HashSet("Blue", "Orange", "Green-B", "Green-C", "Green-D", "Red"),
      "Mattapan"-> HashSet("Red"),
      )*/
}
