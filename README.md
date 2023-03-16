## Thought Process
choose what language to use:
	scala 	- institute uses scala and why I was chosen
			- not familiar with testing framework
	c# 		- more recently used
			- not familiar with testing framework, but Mircosoft documents well (mostly)
			
	first attempt:
		Pick scala
		Try out ScalaTest
		Realize that we will need to use a networking library
			http4s is what I have used before, so lets use that
		Realize that the API component and functionality should be 
		separated from the internal logic.
		-> write the tests for logic first
			(can just grab json from api for "dummy" data)
		-> write logic 
		-> write API connections
		
		
	Question 1: 
		Query MBTA routes api and filter only light and heavy rails
		List their "long names" on the console
		
		filter spec:
			- resulting list be a subset of the total list
			- results should only contain rails types heavy and light
			- results should have the all same heavy and light rails as total
			
	Question 2:
		Extend to display route with:
			- most stops 
			- fewest stops
		Extend to display each stop which connects two or more 
		routes and which routes by "long name"
		
		most stops spec:
			- route returned is in the list
			- if list has elements, should return at least one result
			- given all routes, no other route has more stops
		
		least stops spec:
			- route returned is in the list
			- if list has elements, should return at least one result
			- given all routes, no other route has less stops
			
		stops with 2 connections spec:
			- each stop returned has at least two routes
			- each stop returned only has each route once (uniqueness in routes)
			- stops returned is a subset of all stops
			- same stops which are on more than one route from all
		
	Question 3:
		Extend for user input to specify two stops
		-> return routes in order that would need 
		to be take in order to go from stop 1 to stop 2
		
		path 2 stops spec:
			- if both stops are on same route, only that route should be on the listif both stops are on same route, only that route should be on the list
			- if a path exists there should be a path found between these two stops
			- there should not be any other path which has less transfers and less stops
			- the first route must have the first stop on it
			- the last route must have the last stop on it
			- non first routes must have a connection stop shared with the previous route
			- non last routes must have a connection stop shared with the next route
## sbt project compiled with Scala 3
### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).
