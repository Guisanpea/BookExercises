package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorCapabilities extends App {
  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message: String => println(s"[$self] I have received $message")
      case number: Int => println(s"[simple actor] I have received a NUMBER: $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received something SPECIAL: $contents")
      case SendMessageToYourself(content) =>
        self ! content
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"
  simpleActor ! 42

  case class SpecialMessage(contents: String)
  simpleActor ! SpecialMessage("some special content")

  case class SendMessageToYourself(content: String)
  simpleActor ! SendMessageToYourself("I am a proud actor")

}
