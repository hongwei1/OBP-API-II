package bootstrap.http4s.middleware

import bootstrap.http4s.CallContextKeyProvider.callContextKey
import cats.data.Kleisli
import cats.effect.IO
import code.api.util.APIUtil.nameOfSpellingParam
import code.api.util.{APIUtil, CallContext}
import code.api.v1_3_0.APIMethods130
import code.util.Helper.MdcLoggable
import net.liftweb.common._
import net.liftweb.http.provider.HTTPParam
import org.http4s.{Request, _}

import java.util.UUID

object CallContextMiddleware extends MdcLoggable{

  /**
   * Middleware to inject a new CallContext into every incoming request.
   * This allows downstream routes to retrieve CallContext from Vault attributes.
   */
  def withCallContext(routes: HttpRoutes[IO]): HttpRoutes[IO] = Kleisli { req: Request[IO] =>
  
    val url = java.net.URLDecoder.decode(req.uri.renderString, "UTF-8")
    val verb = req.method.name
    val reqHeaders = req.headers.headers.map(h => HTTPParam(h.name.toString, List(h.value)))
    val authorizationHeaderValue = reqHeaders.find(_.name.equalsIgnoreCase("Authorization")).flatMap(_.values.headOption)
    val params: Map[String, String] = req.uri.query.params
    val ipAddress = req.remote.map(_.host.toString).getOrElse("")
    val correlationId = UUID.randomUUID().toString
    val sessionId = UUID.randomUUID().toString

    val spellingHeader = reqHeaders.find(_.name.equalsIgnoreCase(nameOfSpellingParam()))
    val spellingHeaderValue = spellingHeader.flatMap(_.values.headOption)

    //        val body: Box[String] = req.as[String].attempt.unsafeRunSync() match {
    //          case Right(value) => Full(value)
    //          case Left(_)      => Empty
    //        }
    //TODO. this body may need to changed to http4s body, it is Stream type in http4s
    val body: Box[String] = Full("")

    val (standard, version, resourceDocUrl) = APIUtil.extractResourceDocFields(url)

    val resourceDoc = APIMethods130.resourceDocs.find(doc =>
      doc.implementedInApiVersion.apiShortVersion == version &&
      doc.implementedInApiVersion.apiStandard == standard &&
      doc.requestUrl == resourceDocUrl
    ) match {
      case Some(resourceDoc) => 
        Some(resourceDoc)
      case None => 
        logger.error(s"ResourceDoc not found for URL: $url")
        None
    }
    
    val callContext = CallContext(
      url = url,
      httpBody = body,
      spelling = spellingHeaderValue,
      verb = verb,
      authReqHeaderField = authorizationHeaderValue,
      directLoginParams = params,
      oAuthParams = params,
      requestHeaders = reqHeaders,
      ipAddress = ipAddress,
      correlationId = correlationId,
      sessionId = Some(sessionId),
      implementedInVersion = version,
      resourceDocument = resourceDoc
    )
    
    val updatedReq = req.withAttributes(
      req.attributes.insert(callContextKey, callContext) // Inject into request attributes
    )
    routes(updatedReq)
  }
}
