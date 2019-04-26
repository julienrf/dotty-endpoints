package endpoints.client

import endpoints.algebra
import endpoints.algebra.{Disjoinable, HttpRequest, HttpResponse, Pairable, PartialTransform}

/**
 * Client interpreter for endpoints
 * 
 * From the client point of view, a communication endpoint with a request carrying
 * an information of type `A` and a response carrying an information of type `B` is
 * a function `A => Option[B]`. Provided a value of type `A`, we (hopefully) get a
 * response of type `B`.
 */
trait Endpoints extends algebra.Endpoints with Requests with Responses {

  type Endpoint[A, B] = A => Option[B]

  def endpoint[A, B](request: Request[A], response: Response[B]): Endpoint[A, B] =
    response compose server compose request

  // Pretend that there is a server that turns our requests into responses
  lazy val server: HttpRequest => HttpResponse

}

trait Requests extends algebra.Requests {

  type Path[A] = A => List[String]
  def staticPathSegment(segment: String) = _ => segment :: Nil
  def segment[A](implicit s: Segment[A]) = a => s(a) :: Nil

  def pairablePath = new Pairable[Path] {
    def (fa: Path[A]) zip [A, B](fb: Path[B]) = (a, b) => fa(a) ++ fb(b)
  }

  type Segment[A] = A => String
  def segmentString =
    s => java.net.URLEncoder.encode(s, "utf8")
  def partialTransformSegment = new PartialTransform[Segment] {
    def (fa: Segment[A]) xmapPartial [A, B](f: A => Option[B])(g: B => A) = fa compose g
  }

  type Request[A] = A => HttpRequest
  def partialTransformRequest = new PartialTransform[Request] {
    def (fa: Request[A]) xmapPartial [A, B](f: A => Option[B])(g: B => A) = fa compose g
  }
  def get[A](path: Path[A]) = a => HttpRequest(path(a).mkString("/"))

}

trait Responses extends algebra.Responses {

  type ResponseEntity[A] = String => Option[A]
  def partialTransformResponseEntity = new PartialTransform[ResponseEntity] {
    def (fa: ResponseEntity[A]) xmapPartial [A, B](f: A => Option[B])(g: B => A) =
      s => fa(s).flatMap(f)
  }
  def emptyResponse = _ => Some(())
  def responseEntity[A](implicit m: EntityMarshaller[A]) = m

  type EntityMarshaller[A] = String => Option[A]
  def entityMarshallerString = Some(_)
  def partialTransformEntityMarshaller = new PartialTransform[EntityMarshaller] {
    def (fa: EntityMarshaller[A]) xmapPartial [A, B](f: A => Option[B])(g: B => A) =
      s => fa(s).flatMap(f)
  }

  type Response[A] = HttpResponse => Option[A]
  def response[A](status: Status, entity: ResponseEntity[A]) =
    resp => if (resp.status == status) entity(resp.entity) else None
  def disjoinableResponse = new Disjoinable[Response] {
    def (fa: Response[A]) xor [A, B] (fb: Response[B]): Response[Either[A, B]] =
      resp => fa(resp).map(Left(_)).orElse(fb(resp).map(Right(_)))
  }
  def partialTransformResponse = new PartialTransform[Response] {
    def (fa: Response[A]) xmapPartial [A, B](f: A => Option[B])(g: B => A) =
      resp => fa(resp).flatMap(f)
  }

}
