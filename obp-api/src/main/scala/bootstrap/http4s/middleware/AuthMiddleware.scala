package bootstrap.http4s.middleware

import bootstrap.http4s.CallContextKeyProvider.callContextKey
import bootstrap.http4s.RestHelperChecks._
import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import code.api.util.ErrorMessages.{UnknownError, UserNotLoggedIn}
import net.liftweb.common._
import org.http4s.headers.`Content-Type`
import org.http4s.{Request, _}

object AuthMiddleware {
  def securedEndpoint(
    routes: HttpRoutes[IO]
  ): HttpRoutes[IO] = Kleisli { req: Request[IO] =>
    req.attributes.lookup(callContextKey) match {
      case Some(cc) =>
        OptionT.liftF(checkAuth(req, cc)).flatMap {
          case (Some(user), updatedCtx) =>
            routes(req.withAttribute(callContextKey, updatedCtx.copy(user = Full(user))))
          case (None, _) =>
            val errorJson = s"""{"code": 403, "message": "$UserNotLoggedIn"}"""
            OptionT.pure[IO](
              Response[IO](status = Status.Forbidden)
                .withEntity(errorJson)
                .withContentType(`Content-Type`(MediaType.application.json))
            )
        }
      case None =>
        val errorJson = s"""{"code": 500, "message": "$UnknownError CallContext missing"}"""
        OptionT.pure[IO](
          Response[IO](status = Status.InternalServerError)
            .withEntity(errorJson)
            .withContentType(`Content-Type`(MediaType.application.json))
        )
    }
  }
}
