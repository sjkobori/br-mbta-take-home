package routeanalysis

import org.http4s.Uri
import org.http4s.client.Client
// for REPL or mdoc use only!
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

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.generic.semiauto._

import scala.io.StdIn.readLine
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import scala.collection.mutable.Seq

final case class Stop(
    name: String,
    route_ids: List[String]
)
final case class StopResponse(
    name: String,
    route_id: String
)

object StopResponse {
  implicit val encoder: Encoder[StopResponse] = deriveEncoder[StopResponse]
  implicit val decoder: Decoder[StopResponse] = new Decoder[StopResponse] {
    final def apply(c: HCursor): Decoder.Result[StopResponse] =
      for {
        name <- c.downField("attributes").downField("name").as[String]
        route_id <- c
          .downField("relationships")
          .downField("route")
          .downField("data")
          .downField("id")
          .as[String]
      } yield {
        new StopResponse(name, route_id)
      }
  }
}

final case class StopsResponse(
    stops: List[StopResponse]
)

object StopsResponse {
  implicit val encoder: Encoder[StopsResponse] = deriveEncoder[StopsResponse]
  implicit val decoder: Decoder[StopsResponse] = new Decoder[StopsResponse] {
    final def apply(c: HCursor): Decoder.Result[StopsResponse] =
      for {
        stops <- c.downField("data").as[List[StopResponse]]
      } yield {
        new StopsResponse(stops)
      }
  }
}

final case class Route(
    id: String,
    long_name: String,
    stops: List[Stop]
)

final case class RouteResponse(
    id: String,
    long_name: String
)

object RouteResponse {
  implicit val encoder: Encoder[RouteResponse] = deriveEncoder[RouteResponse]
  implicit val decoder: Decoder[RouteResponse] = new Decoder[RouteResponse] {
    final def apply(c: HCursor): Decoder.Result[RouteResponse] =
      for {
        id <- c.downField("id").as[String]
        ln <- c.downField("attributes").downField("long_name").as[String]
      } yield {
        new RouteResponse(id, ln)
      }
  }
}

final case class RoutesResponse(
    routes: List[RouteResponse]
)

object RoutesResponse {
  implicit val encoder: Encoder[RoutesResponse] = deriveEncoder[RoutesResponse]
  implicit val decoder: Decoder[RoutesResponse] = new Decoder[RoutesResponse] {
    final def apply(c: HCursor): Decoder.Result[RoutesResponse] =
      for {
        routes <- c.downField("data").as[List[RouteResponse]]
      } yield {
        new RoutesResponse(routes)
      }
  }
}

val apiKey = "76a2a7f98ff74de48413d2de37b7ebc0"

val routesRequest = Request[IO](
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

object RouteAnalysis extends IOApp.Simple {
  def requestRoutes(
      request: Request[IO],
      client: Client[IO]
  ): IO[RoutesResponse] = for {
    routesResponse <- client.expect(request)(jsonOf[IO, RoutesResponse])
  } yield routesResponse
  def requestStops(
      route_ids: List[String],
      client: Client[IO]
  ): IO[List[StopsResponse]] =
    // all route ids
    route_ids
      .map[IO[StopsResponse]](id =>
        for {
          sr <- client.expect(stopsRequest(id))(jsonOf[IO, StopsResponse])
        } yield sr
      )
      .sequence

  def getStops(responses: List[StopsResponse]): List[Stop] = {
    collapseStops(
      responses
        .map(l => l.stops.map(s => Stop(s.name, List(s.route_id))))
        .flatten
    )
  }
  def getRoutes(
      responses: List[RouteResponse],
      stops: List[Stop]
  ): List[Route] = {
    // for each of the responses
    responses.map(resp => {
      // search through list, if stop has a curresponding route id, add it to the route
      val routeStops = stops.filter(s => s.route_ids.contains(resp.id))
      Route(resp.id, resp.long_name, routeStops)
    })
  }

  val run: IO[Unit] = EmberClientBuilder
    .default[IO]
    .build
    .use(client =>
      for {
        routeResp <- requestRoutes(routesRequest, client)
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
        _ <- IO(print("Enter starting stop: "))
        startingStop <- IO(getStopByName(readLine(), stops))
        _ <- IO(print("Enter destination stop: "))
        destinationStop <- IO(getStopByName(readLine(), stops))
        _ <- IO(println(s"Start: $startingStop, Destination: $destinationStop"))
        _ <- IO(
          println(
            "Path: " +
              getLeastConnectionsLeastStopsPath(
                startingStop,
                destinationStop,
                stops,
                routes
              )
          )
        )
      } yield ()
    )

  def getStopByName(name: String, stops: List[Stop]): Stop = {
    stops
      .filter(s => s.name == name)
      .headOption
      .getOrElse(stops.head) // not a clean way of doing this but I dont want
    // to prioritize input sanitizing over other features
  }
}

private def collapseStops(stops: List[Stop]): List[Stop] = {
  var stopMap = new HashMap[String, Stop]()
  stops.foreach((s) => {
    stopMap
      .get(s.name)
      .fold(stopMap += (s.name -> s))(x =>
        stopMap
          .update(s.name, Stop(s.name, (x.route_ids ::: s.route_ids).distinct))
      )
  })
  return stopMap.values.toList
}

private def mostStops(routes: List[Route]): List[Route] = {
  val maxNumStops = routes.map(_.stops.length).max
  return routes.filter((_.stops.length == maxNumStops))
}

def printMostStops(routes: List[Route]): Unit = {
  val routesWithMostStops = mostStops(routes)
  val routeNames = routesWithMostStops.map(_.long_name)
  val numStops = routesWithMostStops.headOption.map(_.stops.size).getOrElse(0)
  println(s"The subway route(s) with the most stops: $routeNames")
  println(s"The max number of stops are $numStops")
}

private def leastStops(routes: List[Route]): List[Route] = {
  val minNumStops = routes.map(_.stops.length).min
  return routes.filter((_.stops.length == minNumStops))
}

def printLeastStops(routes: List[Route]): Unit = {
  val routesWithMostStops = leastStops(routes)
  val routeNames = routesWithMostStops.map(_.long_name)
  val numStops = routesWithMostStops.headOption.map(_.stops.size).getOrElse(0)
  println(s"The subway route(s) with the least stops: $routeNames")
  println(s"The min number of stops are $numStops")
}

private def connectingStops(stops: List[Stop]): List[Stop] =
  stops.filter(_.route_ids.length > 1)

def printConnectingStops(stops: List[Stop]): Unit = {
  val cStops = connectingStops(stops)
  println(s"These are all of the connecting stops:")
  cStops.foreach(s => println(s"$s"))
}

def getLeastConnectionsLeastStopsPath(
    srcs: Stop,
    dests: Stop,
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
  srcs.route_ids.foreach(src => {
    dests.route_ids.foreach(dest => {
      paths = paths :+ findPath(src, dest, routeConnection)
    })
  })
  val minPathLength = paths.map(p => p.length).min
  paths.filter(p => p.length == minPathLength).head
}

private def findPath(
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
