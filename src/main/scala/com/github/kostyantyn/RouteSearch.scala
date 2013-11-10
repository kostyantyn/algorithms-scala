package com.github.kostyantyn

case class Ticket(from: String, to: String)

class RouteSearch(val tickets: List[Ticket]) {
  type Tickets = List[Ticket]
  type Route   = List[Ticket]
  type Routes  = List[Route]

  def search(route: Ticket): Routes = {
    def searchExtendedRoutes(allTickets: Tickets,
                             allRoutes: Routes,
                             lastTicket: Ticket,
                             routeAcc: Route,
                             nextTicketsIteration: Tickets): Routes = allTickets match {
      case Ticket(lastTicket.to, route.to)::_  =>
        allRoutes :+ (routeAcc :+ allTickets.head)
      case Ticket(lastTicket.to, _)::_ =>
        searchExtendedRoutes(
          allTickets.tail,
          searchExtendedRoutes(
            nextTicketsIteration ::: allTickets.tail,
            allRoutes,
            allTickets.head,
            routeAcc :+ allTickets.head,
            Nil
          ),
          lastTicket,
          routeAcc,
          allTickets.head :: nextTicketsIteration
        )
      case Ticket(_,_)::_ =>
        searchExtendedRoutes(allTickets.tail, allRoutes, lastTicket, routeAcc, allTickets.head :: nextTicketsIteration)
      case Nil => allRoutes
    }

    def searchAcc(allTickets: Tickets, acc: Routes, nextTicketsIteration: Tickets): Routes = allTickets match {
      case Ticket(route.from, route.to)::_ =>
        searchAcc(
          allTickets.tail,
          acc :+ List(allTickets.head),
          nextTicketsIteration :+ allTickets.head
        )
      case Ticket(route.from,_)::_ =>
        val extendedRoutes = searchExtendedRoutes(
          nextTicketsIteration ::: allTickets.tail,
          Nil,
          allTickets.head,
          List(allTickets.head),
          Nil
        )
        searchAcc(allTickets.tail, acc ::: extendedRoutes, Nil)
      case Ticket(_,_)::_ => searchAcc(allTickets.tail, acc, nextTicketsIteration :+ allTickets.head)
      case Nil => acc
    }
    searchAcc(tickets, Nil, Nil)
  }
}
