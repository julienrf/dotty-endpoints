package endpoints.algebra

import scala.util.Try

/**
 * Algebra interface providing vocabulary for defining HTTP endpoints
 */
trait Endpoints extends Requests with Responses {

  /** Endpoint carrying information of type `A` in its request, and `B` in its response */
  type Endpoint[A, B]
  /** An endpoint with the given `request` and `response` */
  def endpoint[A, B](request: Request[A], response: Response[B]): Endpoint[A, B]

}

/**
 * Algebra interface providing vocabulary for defining HTTP requests
 */
trait Requests {

  /** URL path carrying an information of type `A` */
  type Path[A]
  /** An empty path */
  final def path: Path[Unit] = staticPathSegment("")
  /** A path segment with the given `segment` value */
  def staticPathSegment(segment: String): Path[Unit]
  /** A path carrying an information of type `A` */
  def segment[A](implicit s: Segment[A]): Path[A]
  /** Ability to chain paths */
  def pairablePath: Pairable[Path]
  implied for Pairable[Path] = pairablePath
  /** Convenient syntax for chaining paths
    * {{{
    *   // “/foo/42”
    *   path / "foo" / segment[Int]
    * }}}
    */
  final def (pA: Path[A]) / [A, B](pB: Path[B]): Path[(A, B)] = pA zip pB
  final def (pA: Path[A]) / [A](s: String): Path[(A, Unit)] = pA zip staticPathSegment(s)

  /** A path segment carrying an information of type `A` */
  type Segment[A]
  /** Ability to transform information carried in path segments */
  def partialTransformSegment: PartialTransform[Segment]
  implied for PartialTransform[Segment] = partialTransformSegment
  /** Ability to carry an information of type `String` in a path segment */
  def segmentString: Segment[String]
  implied for Segment[String] = segmentString
  /** Ability to carry an information of type `Int` in a path segment */
  implied for Segment[Int] = segmentString.xmapPartial(s => Try(s.toInt).toOption)(_.toString)

  /** An HTTP request containing an information of type `A` */
  type Request[A]
  /** A request that uses the GET verb and the given `path` */
  def get[A](path: Path[A]): Request[A]
  /** Ability to transform information carried in requests */
  def partialTransformRequest: PartialTransform[Request]
  implied for PartialTransform[Request] = partialTransformRequest

}

trait Responses {

  /** HTTP response status */
  type Status = Int
  def Ok: Status = 200
  def BadRequest: Status = 400
  def NotFound: Status = 404
  def InternalServerError: Status = 500

  /** Response entity carrying an information of type `A` */
  type ResponseEntity[A]
  /** Ability to transform information carried by a response entity */
  def partialTransformResponseEntity: PartialTransform[ResponseEntity]
  implied for PartialTransform[ResponseEntity] = partialTransformResponseEntity
  /** An empty response entity */
  def emptyResponse: ResponseEntity[Unit]
  /** A response entity with a text content */
  final def textResponse: ResponseEntity[String] = responseEntity
  /** A response entity carrying an information of type `A` */
  def responseEntity[A](implicit m: EntityMarshaller[A]): ResponseEntity[A]

  /** Marshalling between type `A` and response entity */
  type EntityMarshaller[A]
  /** Marshalling for type `String` */
  def entityMarshallerString: EntityMarshaller[String]
  implied for EntityMarshaller[String] = entityMarshallerString
  /** Ability to transform the information carried by a entity marshaller */
  def partialTransformEntityMarshaller: PartialTransform[EntityMarshaller]
  implied for PartialTransform[EntityMarshaller] = partialTransformEntityMarshaller

  /** Response carrying an information of type `A` */
  type Response[A]
  /** A response with the given `status` and `entity` */
  def response[A](status: Status, entity: ResponseEntity[A]): Response[A]
  /** Convenient syntax for defining a response with an `Ok` status */
  final def ok[A](entity: ResponseEntity[A]): Response[A] = response(Ok, entity)
  /** Ability to define alternative responses */
  def disjoinableResponse: Disjoinable[Response]
  implied for Disjoinable[Response] = disjoinableResponse
  /** Ability to transform information carried by responses */
  def partialTransformResponse: PartialTransform[Response]
  implied for PartialTransform[Response] = partialTransformResponse
  /** Convenient syntax for defining a response that may not carry an information of type `A` */
  final def (resp: Response[A]) orNotFound[A]: Response[Option[A]] =
    (response(NotFound, emptyResponse) |+| resp).xmap(_.toOption)(_.toRight(()))

}

/**
 * Ability for a type constructor `F` to transform a value of type `F[A]` into
 * a value of type `F[B]` given a partial function `A => Option[B]` and a total
 * function `B => A`
 */
trait PartialTransform[F[_]] {
  def (fa: F[A]) xmapPartial [A, B](f: A => Option[B])(g: B => A): F[B]
  def (fa: F[A]) xmap [A, B](f: A => B)(g: B => A): F[B] = fa.xmapPartial(a => Some(f(a)))(g)
}

/**
 * Ability for a type constructor `F` to transform a value of type `F[A]` and
 * a value of type `F[B]` into a value of type `F[(A, B)]`
 */
trait Pairable[F[_]] {
  def (fa: F[A]) zip [A, B] (fb: F[B]): F[(A, B)]
  /** Symbolic alias for `zip` */
  inline final def (fa: F[A]) |*| [A, B] (fb: F[B]): F[(A, B)] = fa zip fb
}

/**
 * Ability for a type constructor `F` to transform a value of type `F[A]` and
 * a value of type `F[B]` into a value of type `F[Either[A, B]]`
 */
trait Disjoinable[F[_]] {
  def (fa: F[A]) xor [A, B] (fb: F[B]): F[Either[A, B]]
  /** Symbolic alias for `xor` */
  inline final def (fa: F[A]) |+| [A, B] (fb: F[B]): F[Either[A, B]] = fa xor fb
}

// Dummy HTTP request model
case class HttpRequest(path: String)
case class HttpResponse(status: Int, entity: String)
