package routeanalysis
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.generic.semiauto._

//For encoding and decoding responses from json

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
