import cats.effect._
import org.http4s.client._
import org.http4s.ember.client._
import org.http4s._
import org.http4s.implicits._
import org.http4s.circe._
import org.http4s.dsl.io._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import java.io._


object  GitHub extends IOApp {
    
    final case class Event(user: String, eventType: String, repo: String)

    given Decoder[Event] with 
        def apply(c: HCursor): Decoder.Result[Event] = 
            for 
                username  <- c.downField("actor").downField("login").as[String]
                eventType <- c.downField("type").as[String]
                repo      <- c.downField("repo").downField("name").as[String]
            yield Event(username, eventType, repo)

    given Decoder[List[Event]] = Decoder.decodeList[Event]

    val clientResource : Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
    
    def fetchData: IO[List[Event]] = clientResource.use{ client => 
        val request = Request[IO](
            method = Method.GET,
            uri = uri"https://api.github.com/users/kamranahmedse/events"
        )  

        client.expect[Json](request).flatMap{response =>
            decode[List[Event]](response.noSpaces ) match {
                case Right(event) => IO.pure(event) // Convert Event to a String
                case Left(error)  => IO.raiseError(new RuntimeException(error.getMessage)) // Handle decoding failure
            }
        }
    }
    def handlePushEvents(args: List[Event]): List[String] = 
        args.filter(event => event.eventType == "PushEvent")
            .groupBy(event => event.repo)
            .view
            .mapValues(_.size)
            .toMap
            .map((k,v) => s"Pushed $v commits to $k")
            .toList

    def handleIssueEvents(args: List[Event]): List[String] = 
        args.filter(event => event.eventType == "IssuesEvent")
            .groupBy(event => event.repo)
            .view
            .mapValues(_.size)
            .toMap
            .map((k,v) => s"opened $v issues to $k")
            .toList

    def run(args: List[String]): IO[ExitCode] = 
        fetchData.flatMap{res => 
            //TODO: refactor the code and handle the rest of API
            val pushEvents = handlePushEvents(res)
            val issueEvents = handleIssueEvents(res)
            IO{
                pushEvents.foreach(println)
                issueEvents.foreach(println)
                //TO
            }
        
        }.as(ExitCode.Success)
}