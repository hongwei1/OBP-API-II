package code.api.v1_3_0

import bootstrap.http4s.CallContextKeyProvider.callContextKey
import bootstrap.http4s.middleware.JsonErrorHandlerMiddleware.executeWithErrorHandling
import cats.effect._
import cats.syntax.all._
import code.api.Constant._
import code.api.ResourceDocs1_4_0.SwaggerDefinitionsJSON._
import code.api.util.APIUtil._
import code.api.util.ApiTag._
import code.api.util.ErrorMessages._
import code.api.util.{ApiRole, CustomJsonFormats, NewStyle}
import code.api.v1_2_1.JSONFactory
import com.github.dwickern.macros.NameOf.nameOf
import com.openbankproject.commons.ExecutionContext.Implicits.global
import com.openbankproject.commons.model.{BankId, User}
import com.openbankproject.commons.util.{ApiVersion, ApiVersionStatus, ScannedApiVersion}
import net.liftweb.json.{Extraction, Formats, prettyRender}
import org.http4s.HttpRoutes
import org.http4s.dsl.io._

import scala.collection.mutable.ArrayBuffer
object APIMethods130 {

  implicit val formats: Formats = CustomJsonFormats.formats
  implicit def convertAnyToJsonString(any: Any): String = prettyRender(Extraction.decompose(any))

  val implementedInApiVersion: ScannedApiVersion = ApiVersion.v1_3_0
  val versionStatus = ApiVersionStatus.DEPRECATED.toString
  val resourceDocs = ArrayBuffer[ResourceDoc]()

  object Implementations1_3_0 {

    // Common prefix: /obp/v1.3.0
    val prefixPath = Root / ApiPathZero.toString / implementedInApiVersion.toString

    resourceDocs += ResourceDoc(
      null,
      implementedInApiVersion,
      nameOf(root),
      "GET",
      "/root",
      "Get API Info (root)",
      """Returns information about:
        |
        |* API version
        |* Hosted by information
        |* Git Commit""",
      EmptyBody,
      apiInfoJSON,
      List(UnknownError, "no connector set"),
      apiTagApi :: Nil,
      http4sPartialFunction = Some(root)
    )
    
    // Route: GET /obp/v1.3.0/root
    val root: HttpRoutes[IO] = HttpRoutes.of[IO] {
      case req@GET -> `prefixPath` / "root" =>
        val json: String = JSONFactory.getApiInfoJSON(implementedInApiVersion, versionStatus)
        Ok(json)
    }

    resourceDocs += ResourceDoc(
      null,
      implementedInApiVersion,
      nameOf(getCardsRoute),
      "GET",
      "/cards",
      "Get cards for the current user",
      "Returns data about all the physical cards a user has been issued. These could be debit cards, credit cards, etc.",
      EmptyBody,
      physicalCardsJSON,
      List(UserNotLoggedIn, UnknownError),
      List(apiTagCard),
      http4sPartialFunction = Some(getCardsRoute))
    // Route: GET /obp/v1.3.0/cards
    val getCardsRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
      case req @ GET -> `prefixPath` / "cards" =>
        val callContext = req.attributes.lookup(callContextKey)
        val user: User = callContext.flatMap(_.user.headOption).getOrElse(throw new IllegalArgumentException(UserNotLoggedIn))
        val obpResponse = for {
          (cards, updatedCtx) <- NewStyle.function.getPhysicalCardsForUser(user, callContext)
          json: String = JSONFactory1_3_0.createPhysicalCardsJSON(cards, user)
        } yield (json, updatedCtx)

        executeWithErrorHandling(obpResponse)
    }


    resourceDocs += ResourceDoc(
      null,
      implementedInApiVersion,
      nameOf(getCardsForBankRoute),
      "GET",
      "/banks/BANK_ID/cards",
      "Get cards for the specified bank",
      "",
      EmptyBody,
      physicalCardsJSON,
      List(UserNotLoggedIn,BankNotFound, UnknownError),
      List(apiTagCard),
      http4sPartialFunction = Some(getCardsForBankRoute)
    )
    // Route: GET /obp/v1.3.0/banks/BANK_ID/cards
    val getCardsForBankRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
      case req @ GET -> `prefixPath` / "banks" / bankId / "cards" =>
        val callContext = req.attributes.lookup(callContextKey)
        val user: User = callContext.flatMap(_.user.headOption).getOrElse(throw new IllegalArgumentException(UserNotLoggedIn))
        val obpResponse = for {
            httpParams <- NewStyle.function.extractHttpParamsFromUrl(callContext.map(_.url).getOrElse(req.uri.toString))
            (queryParams, ctx1) <- createQueriesByHttpParamsFuture(httpParams, callContext)
            _ <- NewStyle.function.hasEntitlement(bankId, ctx1.map(_.user.head.userId).head, ApiRole.canGetCardsForBank, ctx1)
            (bank, ctx2) <- NewStyle.function.getBank(BankId(bankId), ctx1)
            (cards, ctx3) <- NewStyle.function.getPhysicalCardsForBank(bank, ctx1.map(_.user.head).head, queryParams, ctx2)
            json: String = JSONFactory1_3_0.createPhysicalCardsJSON(cards, ctx1.map(_.user.head).head)
          } yield (json, ctx3)
        
          executeWithErrorHandling(obpResponse)
        }

    // All routes combined
    val allRoutes: HttpRoutes[IO] =
      root <+> 
        getCardsRoute <+> 
        getCardsForBankRoute
  }
 
}
