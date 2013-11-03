package com.github.kostyantyn

import org.scalatest.FunSpec

class RouteSearchSpec extends FunSpec {
  describe("Search") {
    it("should find direct route from one ticket") {
      val search = new RouteSearch(List(Ticket("Lviv", "Kiev")))
      val result = search.search(Ticket("Lviv", "Kiev"))
      assert(result == List(List(Ticket("Lviv", "Kiev"))))
    }

    it("should find direct route from three tickets") {
      val search = new RouteSearch(List(
        Ticket("Lviv", "Kiev"),
        Ticket("Krakow", "Kiev"),
        Ticket("Milan", "London")
      ))
      val result = search.search(Ticket("Krakow", "Kiev"))
      assert(result == List(List(Ticket("Krakow", "Kiev"))))
    }

    it("should find a route of two destinations") {
      val search = new RouteSearch(List(
        Ticket("Lviv", "Kiev"),
        Ticket("Kiev", "Krakow"),
        Ticket("Milan", "London")
      ))
      val result = search.search(Ticket("Lviv", "Krakow"))
      assert(result == List(List(Ticket("Lviv", "Kiev"), Ticket("Kiev", "Krakow"))))
    }

    it("should find several multiple destinations") {
      val search = new RouteSearch(List(
        Ticket("Lviv", "Kiev"),
        Ticket("Kiev", "Krakow"),
        Ticket("Krakow", "London"),
        Ticket("Kiev", "London"),
        Ticket("Lviv", "London"),
        Ticket("Kiev", "Milan")
      ))
      val result = search.search(Ticket("Lviv", "London"))
      assert(result == List(
        List(Ticket("Lviv", "Kiev"), Ticket("Kiev", "Krakow"), Ticket("Krakow", "London")),
        List(Ticket("Lviv", "Kiev"), Ticket("Kiev", "London")),
        List(Ticket("Lviv", "London"))
      ))
    }

    it("should return blank list in cannot find route") {
      val search = new RouteSearch(List(
        Ticket("Lviv", "Kiev"),
        Ticket("Kiev", "Krakow"),
        Ticket("Krakow", "London"),
        Ticket("Kiev", "London"),
        Ticket("Lviv", "London"),
        Ticket("Kiev", "Milan")
      ))
      val result = search.search(Ticket("Lviv", "Paris"))
      assert(result == Nil)
    }
  }
}
