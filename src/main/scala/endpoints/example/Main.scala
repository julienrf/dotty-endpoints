package endpoints.example

import endpoints.{algebra, client, server}
import endpoints.algebra.HttpRequest

case class User(name: String)

// Endpoint definitions
trait Endpoints extends algebra.Endpoints {

  // An endpoint that returns an user given its id
  val getUser = endpoint(
    get(path / "user" / segment[Int]),
    ok(responseEntity[User]).orNotFound
  )

  // Ability to marshall our `User` data type to and from HTTP responses
  implied given (m: EntityMarshaller[String]) for EntityMarshaller[User] =
    m.xmap(User)(_.name)

}

// Endpoint server implementations
object EndpointsServer extends Endpoints with server.Endpoints {

  // In this example, our database only has one user (Paul), whose id is 42
  val server = getUser {
    case (((), ()), 42) => Some(User("Paul"))
    case _ => None
  }

}

// Endpoint client implementation
object EndpointsClient extends Endpoints with client.Endpoints {

  lazy val server = EndpointsServer.server

}

object Main {
  def main(args: Array[String]): Unit = {
    // Client usage
    println(EndpointsClient.getUser((((), ()), 42))) // Some(Some(User(Paul)))
    println(EndpointsClient.getUser((((), ()), 43))) // Some(None)

    // Low-level HTTP requests
    println(EndpointsServer.server(HttpRequest("/user/42"))) // HttpResponse(200, "Paul")
    println(EndpointsServer.server(HttpRequest("/user/43"))) // HttpResponse(404, "")
    println(EndpointsServer.server(HttpRequest("/unknown"))) // HttpResponse(404, "Resource /unknown not found")
  }
}
