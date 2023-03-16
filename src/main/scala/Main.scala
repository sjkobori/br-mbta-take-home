package routeanalysis

import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.Request
import org.http4s.Headers
import org.http4s.headers._
import org.http4s.MediaType
import org.http4s.implicits.uri
import org.http4s._
import org.http4s.headers.`Content-Type`
import org.http4s.dsl.io._
import org.http4s.FormDataDecoder.formEntityDecoder
import org.http4s.circe._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.client.Client

import cats._, cats.effect._, cats.implicits._, cats.data._
import cats.syntax.all._

import org.typelevel.ci.CIString
import org.typelevel.ci.CIStringSyntax

import scala.io.StdIn.readLine
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import scala.collection.mutable.Seq

val apiKey = "76a2a7f98ff74de48413d2de37b7ebc0"

object RouteAnalysis extends IOApp.Simple {

  val run: IO[Unit] = EmberClientBuilder
    .default[IO]
    .build
    .use(client =>
      for {
        routeResp <- requestRoutes(client)
        stopsResp <- requestStops(routeResp.routes.map(_.id), client)
        // configure data
        stops <- IO(getStops(stopsResp))
        routes <- IO(getRoutes(routeResp.routes, stops))

        // display most, least, and connection stops
        _ <- IO(printMostStops(routes))
        _ <- IO(printLeastStops(routes))
        _ <- IO(printConnectingStops(stops))
        // _ <- IO(print(s"All stops: $stops"))

        // handle inputs
        // should really be an input loop with error handling
        // but since it was not the focus I did not prioritize it
        _ <- IO(print("Enter starting stop: "))
        startingStop <- IO(getStopByName(readLine(), stops))
        _ <- IO(print("Enter destination stop: "))
        destinationStop <- IO(getStopByName(readLine(), stops))
        _ <- IO(println(s"Start: $startingStop, Destination: $destinationStop"))
        _ <- IO(
          println(
            "Path: " +
              getLeastConnectionsPath(
                startingStop,
                destinationStop,
                stops,
                routes
              )
          )
        )
      } yield ()
    )
}

/** Gets stop by name and returns first element of stops if invalid name (not
  * safe or desirable)
  *
  * @param name
  * @param stops
  */
def getStopByName(name: String, stops: List[Stop]): Stop = {
  stops
    .filter(s => s.name == name)
    .headOption
    .getOrElse(stops.head) // not a clean way of doing this but I dont want
  // to prioritize input sanitizing over other features
}

/** Requests routes responses from MBTA API
  *
  * @param client
  */
def requestRoutes(client: Client[IO]): IO[RoutesResponse] = for {
  routesRequest <- IO(
    Request[IO](
      // using mbta's filter mainly since the MBTA already has a very specific
      // api schema that I feel would be hard to generalize to other transit systems
      // the way I have decoupled the request logic from the logic of creating
      // the connections makes me confident that only another api schema adaptor
      // would need to be made in order to allow this to work with another system
      // given this, I believe it makes more sense to allow the mbta to give me their
      // data with any imperfections present rather than misrepresent the data more
      // accurately
      // An example of thier inconsistency is that the Green line is separated into
      // discrete stops, but the Red line is not, despite it having multiple paths.
      // In short, I would rather be consistent with their inaccuracies rather than try to
      // fix their data
      uri = uri"https://api-v3.mbta.com/routes"
        .withQueryParam("filter[type]", "0,1")
        .withQueryParam("api_key", apiKey),
      headers = Headers(
        Host("api-v3.mbta.com", 443),
        Accept(MediaType("*", "*")),
        Connection(ci"keep-alive")
      )
    )
  )
  routesResponse <- client.expect(routesRequest)(jsonOf[IO, RoutesResponse])
} yield routesResponse

/** Requests stops responses from MBTA API
  *
  * @param route_ids
  * @param client
  */
def requestStops(
    route_ids: List[String],
    client: Client[IO]
): IO[List[StopsResponse]] = {
  val stopsRequest = (route_id: String) =>
    Request[IO](
      uri = uri"https://api-v3.mbta.com/stops"
        .withQueryParam("filter[route]", route_id)
        .withQueryParam("include", "route")
        .withQueryParam("fields[stop]", "name")
        .withQueryParam("api_key", apiKey),
      headers = Headers(
        Host("api-v3.mbta.com", 443),
        Accept(MediaType("*", "*")),
        Connection(ci"keep-alive")
      )
    )
  // all route ids
  route_ids
    .map[IO[StopsResponse]](id =>
      for {
        sr <- client.expect(stopsRequest(id))(jsonOf[IO, StopsResponse])
      } yield sr
    )
    .sequence
}

/** Converts stop responses into stops
  *
  * @param responses
  */
def getStops(responses: List[StopsResponse]): List[Stop] = {
  collapseStops(
    responses
      .map(l => l.stops.map(s => Stop(s.name, List(s.route_id))))
      .flatten
  )
}

/** Converts route responses and stops into a list of routes
  * @param responses
  * @param stops
  */
def getRoutes(
    responses: List[RouteResponse],
    stops: List[Stop]
): List[Route] = {
  // for each of the responses
  responses.map(resp => {
    // search through list, if stop has a corresponding route id, add it to the route
    val routeStops = stops.filter(s => s.route_ids.contains(resp.id))
    Route(resp.id, resp.long_name, routeStops)
  })
}

/** Groups stops by name and adds their distinct routes togther
  * @param stops
  * @return
  *   list of uniquely named stops and their routes
  */
def collapseStops(stops: List[Stop]): List[Stop] = {
  var stopMap = new HashMap[String, Stop]()
  stops.foreach((s) => {
    stopMap
      .get(s.name)
      .fold(stopMap += (s.name -> s))(x =>
        stopMap
          .update(s.name, Stop(s.name, (x.route_ids ::: s.route_ids).distinct))
      )
  })
  stopMap.values.toList
}

/** Returns the routes with the mosts stops
  * @param routes
  */
def mostStops(routes: List[Route]): List[Route] = {
  // does not cover case where routes is empty but should
  val maxNumStops = routes.map(_.stops.length).max
  routes.filter((_.stops.length == maxNumStops))
}

/** Prints the routes with the most stops
  *
  * @param routes
  */
def printMostStops(routes: List[Route]): Unit = {
  val routesWithMostStops = mostStops(routes)
  val routeNames = routesWithMostStops.map(_.long_name)
  val numStops = routesWithMostStops.headOption.map(_.stops.size).getOrElse(0)
  println(s"The subway route(s) with the most stops: $routeNames")
  println(s"The max number of stops are $numStops")
}

/** Returns the routes with the least stops
  *
  * @param routes
  */
def leastStops(routes: List[Route]): List[Route] = {
  // does not cover case where routes is empty but should
  val minNumStops = routes.map(_.stops.length).min
  return routes.filter((_.stops.length == minNumStops))
}

/** Prints the routes with the most stops
  *
  * @param routes
  */
def printLeastStops(routes: List[Route]): Unit = {
  val routesWithMostStops = leastStops(routes)
  val routeNames = routesWithMostStops.map(_.long_name)
  val numStops = routesWithMostStops.headOption.map(_.stops.size).getOrElse(0)
  println(s"The subway route(s) with the least stops: $routeNames")
  println(s"The min number of stops are $numStops")
}

/** Returns all stops which have more than one route passing through
  *
  * @param stops
  */
def connectingStops(stops: List[Stop]): List[Stop] =
  stops.filter(_.route_ids.length > 1)

/** Prints all stops which have more than one route passing through
  *
  * @param stops
  */
def printConnectingStops(stops: List[Stop]): Unit = {
  val cStops = connectingStops(stops)
  println(s"These are all of the connecting stops:")
  cStops.foreach(s => println(s"$s"))
}

/** Gets the path (routes) with the least amount of connections from source stop
  * to destination stop
  *
  * @param src
  * @param dest
  * @param stops
  * @param routes
  */
def getLeastConnectionsPath(
    src: Stop,
    dest: Stop,
    stops: List[Stop],
    routes: List[Route]
): List[String] = {
  val cStops = connectingStops(stops)

  // go through connecting stops
  // each route has a set of routes
  var routeConnection = new HashMap[String, HashSet[String]]()
  val routeIds = cStops.map(_.route_ids).flatten.distinct
  routeConnection.addAll(routeIds.map(r => (r -> HashSet[String]())))

  // for each connecting stop
  // for each line on it, get or add the hashmap (line -> connections)
  // for each route add it to hashmap if not same as line
  cStops.foreach(cs =>
    cs.route_ids.foreach(
      // adding to hashset
      r =>
        cs.route_ids
          .filter(n => n != r)
          .foreach(n => routeConnection.getOrElse(r, HashSet[String]()).add(n))
    )
  )

  var paths = List[List[String]]()
  src.route_ids.foreach(sid => {
    dest.route_ids.foreach(did => {
      paths = paths :+ findShortestPath(sid, did, routeConnection)
    })
  })
  // does not cover case where routes is empty but should
  val minPathLength = paths.map(p => p.length).min
  paths.filter(p => p.length == minPathLength).head
}

/** Finds the shortest path between source and destination nodes using the
  * graph, hm
  * @param s
  * @param d
  * @param hm
  */
def findShortestPath(
    s: String,
    d: String,
    hm: HashMap[String, HashSet[String]]
): List[String] = {
  val visited = new HashMap[String, Boolean]
  visited.addAll(hm.keys.map(k => (k -> false)))
  val toVisit = new Queue[String]
  // from source
  val distances = new HashMap[String, Int]
  distances.addAll(hm.keys.map(k => (k -> 0)))
  // path taken
  val parents = new HashMap[String, String]
  parents.addAll(hm.keys.map(k => (k -> k)))

  toVisit.enqueue(s)
  while (toVisit.nonEmpty) {
    val current = toVisit.dequeue
    visited.addOne(current -> true)
    val nonVisitedNeighbors =
      hm.getOrElse(current, new HashSet[String])
        .filter(r => !visited.getOrElse(r, true))
    nonVisitedNeighbors.foreach(n => {
      visited.addOne(n -> true)
      // update distance
      distances.updateWith(n)(x => Some(distances.getOrElse(n, 0) + 1))
      // update parents
      parents.update(n, current)
      toVisit.enqueue(n)
    })

  }
  // find shortest path
  var path = List[String]()
  var destination = d
  while (parents.getOrElse(destination, destination) != destination) {
    path = (destination +: path)
    destination = parents.getOrElse(destination, destination)
  }
  path = destination +: path
  path
}
