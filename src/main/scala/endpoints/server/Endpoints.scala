package endpoints.server

import endpoints.algebra
import endpoints.algebra.{HttpRequest, HttpResponse}

/**
 * Server interpreter for endpoints
 *
 * From the server point of view, a communication endpoint with a request
 * carrying an information of type `A` and a response carrying an information
 * of type `B` is a function `(A => B) => HttpRequest => HttpResponse`. Provided
 * the business logic that turns an `A` into a `B`, we get a function that,
 * given an incoming HTTP request, decodes the `A` information that it contains
 * and returns an HTTP response containing the `B` information.
 */
trait Endpoints extends algebra.Endpoints with Requests with Responses {

  type Endpoint[A, B] = (A => B) => HttpRequest => HttpResponse

  def endpoint[A, B](req: Request[A], resp: Response[B]): Endpoint[A, B] =
    f => httpRequest =>
      req(httpRequest).map(resp compose f)
        .getOrElse(HttpResponse(NotFound, s"Resource ${httpRequest.path} not found"))

}

trait Requests extends algebra.Requests {

  // Function decoding an `A` value from the paths segments
  type Path[A] = List[String] => Option[(A, List[String])]

  def staticPathSegment(segment: String): Path[Unit] = {
    case s :: ss if s == segment => Some(((), ss))
    case _ => None
  }
  def segment[A](implicit segmentA: Segment[A]): Path[A] = {
    case s :: ss => segmentA(s).map((_, ss))
    case _       => None
  }
  def pairablePath = new algebra.Pairable[Path] {
    def (fa: Path[A]) zip [A, B] (fb: Path[B]): Path[(A, B)] =
      segments =>
        for {
          (a, segments2) <- fa(segments)
          (b, segments3) <- fb(segments2)
        } yield ((a, b), segments3)
  }

  // Function decoding an `A` value from a single path segment
  type Segment[A] = String => Option[A]
  def segmentString = Some(_)
  def partialTransformSegment = new algebra.PartialTransform[Segment] {
    def (fa: Segment[A])xmapPartial[A, B](f: A => Option[B])(g: B => A): Segment[B] =
      s => fa(s).flatMap(f)
  }

  type Request[A] = HttpRequest => Option[A]

  def get[A](path: Path[A]): Request[A] = { httpRequest =>
    val segments = httpRequest.path.split("/").toList
    path(if (segments.isEmpty) List("") else segments).flatMap {
      case (a, Nil) => Some(a)
      case _        => None
    }
  }

  def partialTransformRequest = new algebra.PartialTransform[Request] {
    def (fa: Request[A])xmapPartial[A, B](f: A => Option[B])(g: B => A): Request[B] =
      s => fa(s).flatMap(f)
  }

}

trait Responses extends algebra.Responses {

  type ResponseEntity[A] = A => String

  def partialTransformResponseEntity = new algebra.PartialTransform[ResponseEntity] {
    def (fa: ResponseEntity[A])xmapPartial[A, B](f: A => Option[B])(g: B => A): ResponseEntity[B] =
      fa compose g
  }

  def emptyResponse = _ => ""
  def responseEntity[A](implicit m: EntityMarshaller[A]): ResponseEntity[A] = m

  type EntityMarshaller[A] = A => String

  def entityMarshallerString = s => s

  def partialTransformEntityMarshaller = new algebra.PartialTransform[EntityMarshaller] {
    def (fa: EntityMarshaller[A])xmapPartial[A, B](f: A => Option[B])(g: B => A): EntityMarshaller[B] =
      fa compose g
  }

  type Response[A] = A => HttpResponse

  def response[A](status: Status, entity: ResponseEntity[A]): Response[A] =
    a => HttpResponse(status, entity(a))

  def disjoinableResponse = new algebra.Disjoinable[Response] {
    def (fa: Response[A])xor[A, B](fb: Response[B]): Response[Either[A, B]] = {
      case Left(a)  => fa(a)
      case Right(b) => fb(b)
    }
  }
  def partialTransformResponse = new algebra.PartialTransform[Response] {
    def (fa: Response[A])xmapPartial[A, B](f: A => Option[B])(g: B => A): Response[B] =
      fa compose g
  }

}
