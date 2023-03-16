package routeanalysis
final case class Route(
    id : String,
    long_name : String,
    stops : List[Stop],
)
/*
object Route {
    implicit val decoder: Decoder[Route] = deriveDecoder[Route]
    implicit val encoder: Encoder[Route] = deriveEncoder[Route]
}*/

final case class Stop(
    name : String,
    route_ids : List[String],
)


/*
object Stop {
    implicit val decoder: Decoder[Stop] = deriveDecoder[Stop]
    implicit val encoder: Encoder[Stop] = deriveEncoder[Stop]
}*/

class RouteStopGraph(routes: List[Route], stops : List[Stop]) {

}