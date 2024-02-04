import akka.actor.{Actor, ActorSystem, Props}

import scala.concurrent.duration.*

case class Estudiante(nombre: String, calificacion: Double)

object Ejem_reactivo extends App {
  val actorSystem = ActorSystem("actorSystem")
  import actorSystem.dispatcher

  class Profesor extends Actor {
    override def receive: Receive =
      case Estudiante(nombre, calificacion) =>
        if (calificacion >= 7) {
          context.system.scheduler.scheduleOnce(5.seconds){
            println(s"El estudiante $nombre ha aprobado con una calificación de $calificacion")
          }
        } else {
          println(s"El estudiante $nombre no ha aprobado. Su calificación fue $calificacion")
        }
  }

  val profesor = actorSystem.actorOf(Props(new Profesor), "profesor")
  profesor ! Estudiante("Ana", 7.5)
  profesor ! Estudiante("Juan", 5.5)
  profesor ! Estudiante("Ruben", 8)
}
