package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  class WordCountActor extends Actor {
    var totalWords = 0

    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")

  wordCounter ! "I am learning Akka and it's pretty damn cool!"
  anotherWordCounter ! 123

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi my name is $name")
      case _ =>
    }
  }

  object Person {
    def props(name: String) = Props(new Person(name))
  }

  // Bad practice
  val person = actorSystem.actorOf(Props(new Person("Bob")))
  person ! "hi"

  // Good practice
  val personNice = actorSystem.actorOf(Person.props("Bob"))
}
