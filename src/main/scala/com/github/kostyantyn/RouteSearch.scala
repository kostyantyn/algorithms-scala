package com.github.kostyantyn

case class Ticket(from:String, to:String)

class RouteSearch(val tickets:List[Ticket]) {
  type Tickets = List[Ticket]
  type Route   = List[Ticket]
  type Routes  = List[Route]

  def search(route:Ticket):Routes = {
    def searchExtendedRoutes(allTickets:Tickets, allRoutes:Routes, lastTicket:Ticket, routeAcc:Route):Routes = {
      if (allTickets.nonEmpty && routeAcc.contains(allTickets.head)) {
        searchExtendedRoutes(allTickets.tail, allRoutes, lastTicket, routeAcc)
      } else {
        allTickets match {
          case Ticket(lastTicket.to, route.to)::_  => allRoutes :+ (routeAcc :+ allTickets.head)
          case Ticket(lastTicket.to, _)::_         =>
            searchExtendedRoutes(
              allTickets.tail,
              searchExtendedRoutes(tickets, allRoutes, allTickets.head, routeAcc :+ allTickets.head),
              lastTicket,
              routeAcc
            )
          case Ticket(_,_)::_                      => searchExtendedRoutes(allTickets.tail, allRoutes, lastTicket, routeAcc)
          case Nil                                 => allRoutes
        }
      }
    }

    def searchAcc(allTickets:Tickets, acc:Routes):Routes = {
      allTickets match {
        case Ticket(route.from, route.to)::_ => searchAcc(allTickets.tail, acc :+ List(allTickets.head))
        case Ticket(route.from,_)::_         => searchAcc(allTickets.tail, acc ::: searchExtendedRoutes(tickets, Nil, allTickets.head, List(allTickets.head)))
        case Ticket(_,_)::_                  => searchAcc(allTickets.tail, acc)
        case Nil                             => acc
      }
    }
    searchAcc(tickets, Nil)
  }
}
