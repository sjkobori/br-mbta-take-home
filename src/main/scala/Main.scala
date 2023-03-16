package routeanalysis

import org.http4s.client.JavaNetClientBuilder
import cats.syntax.all._
import org.http4s.Uri
import org.http4s.client.Client
import cats.effect.unsafe.IORuntime
implicit val runtime: IORuntime = IORuntime.global
// for REPL or mdoc use only!
import org.http4s.Request
import org.http4s.Headers
import org.http4s.headers._
import org.http4s.MediaType
import org.http4s.implicits.uri
import org.http4s._
import org.http4s.headers.`Content-Type`
import org.http4s.dsl.io._
import cats._, cats.effect._, cats.implicits._, cats.data._
import org.typelevel.ci.CIString
import org.typelevel.ci.CIStringSyntax
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import scala.collection.mutable.Seq
import io.circe.{ Decoder, Encoder, HCursor, Json }
import cats.effect.{IO, IOApp}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.client.Client
import scala.io.StdIn.readLine
import io.circe.generic.semiauto._
import org.http4s.FormDataDecoder.formEntityDecoder
import org.http4s.circe._


final case class Stop(
    name : String,
    route_ids : List[String],
)
final case class StopResponse(
    name : String,
    route_id : String,
)

object StopResponse {
  implicit val encoder: Encoder[StopResponse] = deriveEncoder[StopResponse]
  implicit val decoder: Decoder[StopResponse] = new Decoder[StopResponse] {
  final def apply(c: HCursor): Decoder.Result[StopResponse] =
    for {
      name <- c.downField("attributes").downField("name").as[String]
      route_id <- c.downField("relationships")
                   .downField("route")
                   .downField("data")
                   .downField("id").as[String]
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
    id : String,
    long_name : String,
    stops : List[Stop],
)

final case class RouteResponse(
    id : String,
    long_name : String,
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
  uri = uri"https://api-v3.mbta.com/routes?filter[type]=0,1&api_key=76a2a7f98ff74de48413d2de37b7ebc0",
  headers = Headers(
    Host("api-v3.mbta.com", 80),
    Accept(MediaType("*", "*")),
    Connection(ci"keep-alive")
  )
)

val stopsRequest = (route_id : String) => Request[IO](
  uri = uri"https://api-v3.mbta.com/stops"
    .withQueryParam("filter[route]", route_id)
    .withQueryParam("include", "route")
    .withQueryParam("fields[stop]", "name")
    .withQueryParam("api_key", apiKey),
  headers = Headers(
    Host("api-v3.mbta.com", 80),
    Accept(MediaType("*", "*")),
    Connection(ci"keep-alive")
  )
)


object Hello extends IOApp.Simple {


  def requestRoutes(request: Request[IO], client: Client[IO]): IO[RoutesResponse] = for {
    routesResponse <- client.expect(request)(jsonOf[IO, RoutesResponse])
  } yield routesResponse
  def requestStops(route_ids: List[String], client: Client[IO]): IO[List[StopsResponse]] = 
    //all route ids, 
    route_ids.map[IO[StopsResponse]](id => for {
      sr <- client.expect(stopsRequest(id))(jsonOf[IO, StopsResponse])
    } yield sr).sequence

  def getStops(responses : List[StopsResponse]) : List[Stop] = {
    collapseStops(responses.map(l => 
      l.stops.map(s => Stop(s.name, List(s.route_id)))).flatten)
  }
  def getRoutes(responses : List[RouteResponse], stops: List[Stop]) : List[Route] = {
    //for each of the responses
    responses.map(resp => {
      //search through list, if stop has a curresponding route id, add it to the route
      val routeStops = stops.filter(s => s.route_ids.contains(resp.id))
      Route(resp.id, resp.long_name, routeStops)
    })
  }

  def printRoutes(client: Client[IO]): IO[Unit] = for {
    _ <- requestRoutes(routesRequest, client).flatMap(IO.println)
   // getStops(List("Red", "Orange"), client).flatMap(IO.println)

   // IO.pure(print("Enter your first name: "))
    //val firstName = IO.pure(readLine())

   // IO.pure(print("Enter your last name: "))
    //IO.pure("Hello " + readLine()).flatMap(IO.println)

   // IO.pure(println(s"Your name is $firstName $lastName"))
  } yield ()
  def printStops(client: Client[IO]): IO[Unit] = for {
    rStops <- requestStops(List("Red", "Orange"), client).flatMap(IO.println)
  } yield ()

  val run: IO[Unit] = EmberClientBuilder
    .default[IO]
    .build
    .use(client => for {
      routeResp <- requestRoutes(routesRequest,client)
      stopsResp <- requestStops(routeResp.routes.map(_.id), client)
      //configure data 
      stops <- IO(getStops(stopsResp))
      routes <- IO(getRoutes(routeResp.routes, stops))

      _ <- IO(printMostStops(routes))
      _ <- IO(printLeastStops(routes))
      _ <- IO(printConnectingStops(stops))
      //handle inputs
      //_ <- IO(print(s"All stops: $stops"))
      
      _ <- IO(print("Enter starting stop: "))
          startingStop <- IO(getStopByName(readLine(), stops))
      _ <- IO(print("Enter destination stop: "))
          destinationStop <- IO(getStopByName(readLine(), stops))
      _ <- IO(println(s"Start: $startingStop, Destination: $destinationStop"))
      _ <- IO(println(getLeastConnectionsLeastStopsPath(
            startingStop,
            destinationStop, 
            stops, 
            routes)))
    } yield ())
    
    def getStopByName(name : String, stops : List[Stop]): Stop = {
      stops.filter(s => s.name == name).head
    }
    //print("Enter your first name: ")
    //val firstName = readLine()

    //print("Enter your last name: ")
    //val lastName = readLine()

    //println(s"Your name is $firstName $lastName")
}

/*
@main def hello: Unit = 
  //println("Hello world!")
  //val httpClient: Client[IO] = JavaNetClientBuilder[IO].create
  //val helloEmber: IO[String] =
 // httpClient.expect[String](request)

  //val response = helloEmber.unsafeRunSync();
 // println(response)
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
 val stops = List(stop1,stop1copy,stop2,stop3,stop4,stop5,stop6, stop7, stop8,stop9, stop10)
 val collapsedStops = collapseStops(stops)
 println(collapsedStops)
 val route1 = Route("Red", "Red Line", List(stop1, stop5))
 val route2 = Route("Green C", "Green Line C", List(stop1,stop1copy,stop2))
 val route3 = Route("Blue", "Blue Line", List(stop1,stop1copy))
 val routes = List(route1, route2,route3)
 println(mostStops(routes))
 println(leastStops(routes))
 println("All connecting")
 println(connectingStops(collapsedStops))
 val s1 = stop1
 val s2 = stop2
 println(s"Get path between $s1 and $s2")
 val path = getLeastConnectionsLeastStopsPath(s1, s2, stops, List())
 println(path)

*/


private def collapseStops(stops : List[Stop]) : List[Stop] = {
  var stopMap = new HashMap[String, Stop]()
  stops.foreach((s) => {
    stopMap.get(s.name).fold(stopMap+=(s.name -> s))(
      x => stopMap.update(s.name, Stop(s.name, (x.route_ids ::: s.route_ids).distinct))
    )})
    return stopMap.values.toList
}

private def mostStops(routes : List[Route]) : List[Route] = {
  val maxNumStops = routes.map(_.stops.length).max
  return routes.filter((_.stops.length == maxNumStops))
}

def printMostStops(routes : List[Route]): Unit = {
    val routesWithMostStops = mostStops(routes)
    val routeNames = routesWithMostStops.map(_.long_name)
    val numStops = routesWithMostStops.headOption.map(_.stops.size).getOrElse(0)
    println(s"The subway route(s) with the most stops: $routeNames")
    println(s"The max number of stops are $numStops")
}

private def leastStops(routes : List[Route]) : List[Route] = {
  val minNumStops = routes.map(_.stops.length).min
  return routes.filter((_.stops.length == minNumStops))
}

def printLeastStops(routes : List[Route]): Unit = {
    val routesWithMostStops = leastStops(routes)
    val routeNames = routesWithMostStops.map(_.long_name)
    val numStops = routesWithMostStops.headOption.map(_.stops.size).getOrElse(0)
    println(s"The subway route(s) with the least stops: $routeNames")
    println(s"The min number of stops are $numStops")
}

private def connectingStops(stops : List[Stop]) : List[Stop] = 
  stops.filter(_.route_ids.length > 1)

def printConnectingStops(stops : List[Stop]): Unit = {
    val cStops = connectingStops(stops)
    println(s"These are all of the connecting stops:")
    cStops.foreach(s => println(s"$s"))
}

def getLeastConnectionsLeastStopsPath(srcs:Stop, dests:Stop, 
                stops: List[Stop],
                routes : List[Route]) : List[String] = {
    val cStops = connectingStops(stops)

    //go through connecting stops
    //each route has a set of routes
    var routeConnection = new HashMap[String, HashSet[String]]()
    val routeIds = cStops.map(_.route_ids).flatten.distinct
    routeConnection.addAll(routeIds.map(r => (r -> HashSet[String]())))
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
    //for each connecting stop
    cStops.foreach(cs => cs.route_ids.foreach(
      //adding to hashset
      r => cs.route_ids.filter(n => n != r).foreach(
        n => routeConnection.getOrElse(r, HashSet[String]()).add(n)
      ) 
    ))
    //for each line on it, get or add the hashmap (line -> connections)

    //for each route add it to hashmap if not same as line
    var paths = List[List[String]]()
    srcs.route_ids.foreach(src => {
      dests.route_ids.foreach(
        dest => {
          paths = paths :+ findPath(src, dest, routeConnection)
        }
      )
    })
    println(paths)
    val minPathLength = paths.map(p => p.length).min
    paths.filter(p => p.length == minPathLength).head
}

private def findPath(s : String, 
                     d: String,
                     hm : HashMap[String, HashSet[String]]): List[String] = {
                      val visited = new HashMap[String, Boolean]
                      visited.addAll(hm.keys.map(k => (k -> false)))
                      val toVisit = new Queue[String]
                      //from source
                      val distances = new HashMap[String, Int]
                      distances.addAll(hm.keys.map(k => (k -> 0)))
                      //path taken
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
                            //update distance
                            distances.updateWith(n)(x => 
                              Some(distances.getOrElse(n, 0) + 1))
                            //update parents
                            parents.update(n, current)
                            toVisit.enqueue(n)
                        })
                        
                      }
                      //find shortest path
                      var path = List[String]()
                      var destination = d
                      while (parents.getOrElse(destination, destination) != destination) {
                        path = (destination +: path)
                        destination = parents.getOrElse(destination, destination)
                      }
                      path = destination +: path
                      path
                }

private def bfs(s : List[String], 
                d: List[String], 
                hm : HashMap[String, HashSet[String]]): List[String] = 
                  bfsHelper(s, d, hm, List())

private def bfsHelper(s : List[String], 
                d: List[String], 
                hm : HashMap[String, HashSet[String]],
                currentPath : List[String]): List[String] = ??? /*
                  {
  //if any source matches any destination
  //append source to list
  


  s.foreach(start => {if (d.contains(start)) return currentPath :+ start
                     else if (hm.nonEmpty) {
                      //check everything in the hashset not in the current path
                     //for each key in the hashmap which is not in the current path
                     val filtered = hm.filter((k,v) => !currentPath.contains(k))
                     filtered.foreach((k,v) =>
                        return bfsHelper(v.toList, d, filtered, currentPath)
                     )}
                     else return List[String]()
                     
                  })
                  return List[String]()
}
*/
private def bfsInterative(s : String, 
                d: List[String], 
                hm : HashMap[String, HashSet[String]]) : List[String] = {
                  val currentPath = List[String]()
                  return currentPath
                }