package part2actors

import akka.actor.ActorSystem

object ActorsIntro extends App {
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)
}
